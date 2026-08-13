! This file is part of mctc-lib.
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!     http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> @file mctc/csrlist/type.f90
!> Compressed Sparse Row neighbour list implementation.

!> Implementation of a sparse neighbour map in compressed sparse row format.
!>
!> A symmetric neighbour map given in dense format like
!>
!>   |   | 1 | 2 | 3 | 4 | 5 | 6 |
!>   |---|---|---|---|---|---|---|
!>   | 1 |   | x |   | x | x |   |
!>   | 2 | x |   | x |   | x | x |
!>   | 3 |   | x |   | x |   | x |
!>   | 4 | x |   | x |   | x | x |
!>   | 5 | x | x |   | x |   |   |
!>   | 6 |   | x | x | x |   |   |
!>
!> Is stored in two compressed array identifying the neighbouring atom `nlat`
!> and its cell index `nltr`. Two index arrays `inl` for the offset
!> and `nnl` for the number of entries map the atomic index to the row index.
!>
!> ```
!> inl   =     1,       4,          8,      11,         15,      18,     21
!> nlat  =     2, 4, 5, 1, 3, 5, 6, 2, 4, 6, 1, 3, 5, 6, 1, 2, 4, 2, 3, 4
!> nltr  =     1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
!> ```
!> The first element of the 'nlat' and 'nltr' arrays is the diagonal entry of
!> the atom itself, which is always present. One can choose to store either
!> the full matrix (complete mode) or only upper triangular. Note, that the
!> sparsity pattern is symmetric, but one can use it to store assymmetric
!> matrices with symmetric sparsity pattern. The indexing can be accessed
!> `nlat(inl(i):inl(i+1)-1)` to make it transferable for the CSR-support
!> libraries (e.g. MKL, cuSPARSE, etc.).
!> `nimg` saves the closest images of cross-interaction in the periodic system,
!> `tridx` saves its translation index.

module mctc_csrlist_type
   use iso_fortran_env, only : int64
   use mctc_env, only : wp, timer_type, format_time
   use mctc_io, only : structure_type
   use mctc_io_resize, only : resize
   use mctc_cutoff, only: get_lattice_points
   use mctc_wignerseitz, only : wignerseitz_cell
   implicit none
   private

   public :: csr_list, new_csr_list, compute_grid, get_linked_cell

   !> @class csr_list
   !> Neighbourlist in CSR format
   type :: csr_list
      !> Realspace cutoff for neighbourlist generation
      real(wp), allocatable :: cutoff
      !> Complete asymmetric neighbour list flag
      logical :: complete
      !> Offset index in the neighbour map
      integer, allocatable :: inl(:)
      !> Number of neighbours for each atom
      integer, allocatable :: nnl(:)
      !> Index of the neighbouring atom
      integer, allocatable :: nlat(:)
      !> Cell index of the neighbouring atom
      integer, allocatable :: nltr(:)
      !> Lattice translation vector
      real(wp), allocatable :: trans(:, :)
      !> Wigner-Seitz cell type
      type(wignerseitz_cell), allocatable :: wsc
   end type csr_list

   ! Default input
   real(wp), parameter :: cutoff_def = 29.0_wp
   real(wp), parameter :: trans_def(3, 1) = 0.0_wp
   logical, parameter :: complete_def = .false.
   integer, parameter :: init_size = 10
   real(wp), parameter :: buffer = 0.1_wp
   real(wp), parameter :: eps = tiny(1.0_wp)
   real(wp), parameter :: thr = sqrt(epsilon(0.0_wp))
   real(wp), parameter :: tol = 0.01_wp

contains

   !> Create new neighbourlist for a given geometry and cutoff
   subroutine new_csr_list(self, mol, wsc, cutoff, trans, complete)
      !> Instance of the neighbourlist
      type(csr_list), intent(out) :: self
      !> Structure type
      type(structure_type), intent(in) :: mol
      !> Wigner-Seitz cell type
      type(wignerseitz_cell), intent(inout), allocatable, optional :: wsc
      !> Realspace cutoff for neighbourlist generation
      real(wp), intent(in), optional :: cutoff
      !> Lattice translation vectors for periodic systems
      real(wp), intent(in), optional :: trans(:, :)
      !> Flag for complete neighbourlist generation
      logical, intent(in), optional :: complete

      allocate(self%cutoff)
      if (present(cutoff)) then
         self%cutoff = cutoff
      else
         self%cutoff = cutoff_def
      end if

      if (present(complete)) then
         self%complete = complete
      else
         self%complete = complete_def
      end if

      allocate(self%inl(mol%nat+1), source=0)

      if (any(mol%periodic)) then
         if (present(wsc)) then
            call generate_wsc(self, mol, wsc)
            call move_alloc(wsc, self%wsc)
         else if (present(trans)) then
            self%trans = trans
            call generate_hybrid(self, mol)
         end if
      else
         if (present(trans)) then
            self%trans = trans
         else
            allocate(self%trans, source=trans_def)
         end if
         call generate_hybrid(self, mol)
      end if

   end subroutine new_csr_list

!> Generator of the CSR-based Hybrid Neighbour List
   subroutine generate_hybrid(self, mol)
      use omp_lib

      !> Instance of the neighbourlist
      type(csr_list), intent(inout) :: self
      !> Molecular structure data
      type(structure_type), intent(in) :: mol

      integer :: iat, jat, itr, img, jc
      integer :: ix, iy, iz, jx, jy, jz, di, dj, dk, d
      integer, allocatable :: head(:), nxt(:)
      integer :: n_xyz(3)
      real(wp) :: r2, vec(3), cutoff2, cell_w(3), min_xyz(3), max_xyz(3)
      real(wp) :: vol, dens, det, lat_inv(3, 3), fract(3)
      integer(int64) :: prob
      integer, allocatable :: ccount(:)

      ! Dynamic stencil search bounds per dimension
      integer :: kmin(3), kmax(3)

      ! Statistical estimation variables
      integer :: nz_count, median

      ! OpenMP specific variables
      integer(int64) :: thr_mem
      integer :: nthr, tid
      integer :: start_idx, local_offset, n_neigh
      integer, allocatable :: thr_img(:)
      integer, allocatable :: thr_inl(:)
      integer, allocatable :: atrack(:)
      integer, allocatable :: thr_nlat(:,:)
      integer, allocatable :: thr_nltr(:,:)

      img = 0
      cutoff2 = self%cutoff**2

      ! 1. Generate linked-cell grid
      if (any(mol%periodic)) then
         call compute_grid(mol, self%cutoff, det, n_xyz, lat_inv=lat_inv)
      else
         call compute_grid(mol, self%cutoff, det, n_xyz, cell_w=cell_w)
      end if
      ! Dynamic Stencil bounds prevent duplicate cell visits when grid dimensions (N_xyz) are < 3
      do d = 1, 3
         select case (n_xyz(d))
          case (1)
            kmin(d) = 0; kmax(d) = 0
          case (2)
            kmin(d) = 0; kmax(d) = 1
          case default
            kmin(d) = -1; kmax(d) = 1
         end select
      end do

      ! 2. Build Linked Cell List
      allocate(head(product(n_xyz)), source=0)
      allocate(ccount(product(n_xyz)), source=0)
      allocate(nxt(mol%nat), source=0)

      if (any(mol%periodic)) then
         call get_linked_cell(mol, n_xyz, head, nxt, ccount, lat_inv=lat_inv)
      else
         call get_linked_cell(mol, n_xyz, head, nxt, ccount, cell_w=cell_w)
      end if

      ! 3. Perform basic statistical analysis to estimate the `nlat` size
      nz_count = count(ccount > 0)
      call get_median(ccount, median)

      vol = det * real(nz_count, wp) / real(product(n_xyz), wp)
      dens = real(median, wp) * real(product(n_xyz), wp) / vol
      prob = ceiling(dens * self%cutoff**3.0_wp * 4.0_wp)
      if (self%complete) prob = prob * 2

      ! 4. OpenMP Setup
      !$omp parallel
      !$omp master
      nthr = omp_get_num_threads()
      !$omp end master
      !$omp end parallel

      allocate(thr_img(nthr), source=0)
      allocate(atrack(mol%nat), source=0)
      allocate(thr_inl(mol%nat), source=0)

      thr_mem = max(init_size * mol%nat, &
      & int(real((prob * mol%nat), wp) / real(nthr, wp), int64) * size(self%trans, 2))
      allocate(thr_nlat(thr_mem, nthr))
      if (any(mol%periodic)) allocate(thr_nltr(thr_mem, nthr))

      ! 5. Search Loop
      if (any(mol%periodic)) then
         ! Periodic Branch

         !$omp parallel do schedule(guided) &
         !$omp private(iat, tid, ix, iy, iz, dk, dj, di, fract) &
         !$omp private(jx, jy, jz, jc, jat, itr, vec, r2) &
         !$omp shared(mol, self, head, nxt, thr_img, thr_inl) &
         !$omp shared(thr_nlat, thr_nltr, cell_w, lat_inv, min_xyz) &
         !$omp shared(n_xyz, kmin, kmax, cutoff2, atrack, thr_mem)
         do iat = 1, mol%nat
            tid = omp_get_thread_num() + 1
            atrack(iat) = tid
            thr_inl(iat) = thr_img(tid)

            ! Inject Diagonal (self-interaction) at position 1
            thr_img(tid) = thr_img(tid) + 1
            if (thr_img(tid) > thr_mem) error stop "CSR list size overflow!"
            thr_nlat(thr_img(tid), tid) = iat
            thr_nltr(thr_img(tid), tid) = 1

            fract(:) = matmul(lat_inv, mol%xyz(:, iat))
            fract(:) = fract(:) - floor(fract(:))
            ix = min(n_xyz(1), max(1, int(fract(1) * n_xyz(1)) + 1))
            iy = min(n_xyz(2), max(1, int(fract(2) * n_xyz(2)) + 1))
            iz = min(n_xyz(3), max(1, int(fract(3) * n_xyz(3)) + 1))

            ! Dynamically clamped stencil loops prevent duplicate cell evaluations
            do dk = kmin(3), kmax(3)
               jz = modulo(iz + dk - 1, n_xyz(3)) + 1

               do dj = kmin(2), kmax(2)
                  jy = modulo(iy + dj - 1, n_xyz(2)) + 1

                  do di = kmin(1), kmax(1)
                     jx = modulo(ix + di - 1, n_xyz(1)) + 1
                     jc = jx + n_xyz(1)*(jy-1) + n_xyz(1)*n_xyz(2)*(jz-1)
                     jat = head(jc)

                     do while (jat > 0)
                        ! Upper triangle condition: skip lower triangle if incomplete
                        if (self%complete .or. jat >= iat) then
                           do itr = 1, size(self%trans, 2)
                              ! Skip diagonal element identity shift (already injected at start of row)
                              if (iat == jat .and. itr == 1) cycle

                              vec(:) = mol%xyz(:, iat) - mol%xyz(:, jat) - self%trans(:, itr)
                              r2 = sum(vec**2)

                              if (r2 < epsilon(cutoff2) .or. r2 > cutoff2) cycle

                              thr_img(tid) = thr_img(tid) + 1
                              if (thr_img(tid) > thr_mem) error stop "CSR list size overflow!"

                              thr_nlat(thr_img(tid), tid) = jat
                              thr_nltr(thr_img(tid), tid) = itr
                           end do
                        end if
                        jat = nxt(jat)
                     end do
                  end do
               end do
            end do

            ! Temporarily store neighbor count for atom iat at position (iat + 1)
            self%inl(iat + 1) = thr_img(tid) - thr_inl(iat)
         end do
         !$omp end parallel do

      else
         ! Non-periodic Branch
         min_xyz = minval(mol%xyz, dim=2) - buffer

         !$omp parallel do schedule(guided) &
         !$omp private(iat, tid, ix, iy, iz, dk, dj, di) &
         !$omp private(jx, jy, jz, jc, jat, vec, r2) &
         !$omp shared(mol, self, head, nxt, thr_img, thr_inl) &
         !$omp shared(thr_nlat, cell_w, min_xyz, n_xyz, cutoff2) &
         !$omp shared(atrack, thr_mem, kmin, kmax)
         do iat = 1, mol%nat
            tid = omp_get_thread_num() + 1
            atrack(iat) = tid
            thr_inl(iat) = thr_img(tid)

            ! Inject Diagonal (self-interaction) at position 1
            thr_img(tid) = thr_img(tid) + 1
            if (thr_img(tid) > thr_mem) error stop "CSR list size overflow!"
            thr_nlat(thr_img(tid), tid) = iat

            ix = min(n_xyz(1), max(1, int((mol%xyz(1, iat) - min_xyz(1)) / cell_w(1)) + 1))
            iy = min(n_xyz(2), max(1, int((mol%xyz(2, iat) - min_xyz(2)) / cell_w(2)) + 1))
            iz = min(n_xyz(3), max(1, int((mol%xyz(3, iat) - min_xyz(3)) / cell_w(3)) + 1))

            do dk = kmin(3), kmax(3); do dj = kmin(2), kmax(2); do di = kmin(1), kmax(1)
                     jx = ix + di; jy = iy + dj; jz = iz + dk

                     if (jx < 1 .or. jx > n_xyz(1) .or. &
                        jy < 1 .or. jy > n_xyz(2) .or. &
                        jz < 1 .or. jz > n_xyz(3)) cycle

                     jc = jx + n_xyz(1)*(jy-1) + n_xyz(1)*n_xyz(2)*(jz-1)
                     jat = head(jc)

                     do while (jat > 0)
                        ! Skip lower triangle if incomplete
                        if ( self%complete .or. jat > iat) then
                           vec(:) = mol%xyz(:, iat) - mol%xyz(:, jat)
                           r2 = sum(vec**2)

                           if (r2 < epsilon(cutoff2) .or. r2 > cutoff2) then
                              jat = nxt(jat); cycle
                           end if

                           thr_img(tid) = thr_img(tid) + 1
                           if (thr_img(tid) > thr_mem) error stop "CSR list size overflow!"

                           thr_nlat(thr_img(tid), tid) = jat
                        end if
                        jat = nxt(jat)
                     end do
                  end do; end do; end do
            self%inl(iat + 1) = thr_img(tid) - thr_inl(iat)
         end do
         !$omp end parallel do
      end if

      ! 6. CSR Pointer array construction
      self%inl(1) = 1
      do iat = 1, mol%nat
         self%inl(iat + 1) = self%inl(iat) + self%inl(iat + 1)
      end do
      img = self%inl(mol%nat + 1) - 1

      ! 7. CSR List Resizing & Stream Copying
      call resize(self%nlat, img)
      if (any(mol%periodic)) call resize(self%nltr, img)

      !$omp parallel do schedule(static) private(iat, tid, start_idx, local_offset, n_neigh) &
      !$omp shared(mol, self, atrack, thr_inl, thr_nlat, thr_nltr)
      do iat = 1, mol%nat
         tid = atrack(iat)
         start_idx = self%inl(iat)
         local_offset = thr_inl(iat)
         n_neigh = self%inl(iat + 1) - self%inl(iat)

         if (n_neigh > 0) then
            self%nlat(start_idx : start_idx + n_neigh - 1) = &
               thr_nlat(local_offset + 1 : local_offset + n_neigh, tid)

            if (any(mol%periodic)) then
               self%nltr(start_idx : start_idx + n_neigh - 1) = &
                  thr_nltr(local_offset + 1 : local_offset + n_neigh, tid)
            end if
         end if
      end do
      !$omp end parallel do

      if (allocated(head)) deallocate(head)
      if (allocated(nxt)) deallocate(nxt)
      deallocate(thr_img, atrack, thr_inl, thr_nlat, ccount)
      if (allocated(thr_nltr)) deallocate(thr_nltr)

   end subroutine generate_hybrid

!> Generator of the CSR-based Hybrid Neighbour List for Periodic systems using Wigner-Seitz Cell Search
   subroutine generate_wsc(self, mol, wsc)
      use omp_lib

      !> Instance of the neighbourlist
      type(csr_list), intent(inout) :: self
      !> Molecular structure data
      type(structure_type), intent(in) :: mol
      !> Wigner-Seitz cell type
      type(wignerseitz_cell), intent(inout) :: wsc

      integer :: iat, jat, img, jc
      integer :: ix, iy, iz, jx, jy, jz, di, dj, dk, d
      integer, allocatable :: head(:), nxt(:), ccount(:)

      integer :: n_xyz(3), ntr, median, nz_count
      real(wp) :: cutoff2, r2_min, dens
      real(wp) :: lat_inv(3, 3), det
      real(wp) :: fract(3)
      real(wp), allocatable :: trans(:, :)
      real(wp) :: vec(3), zero_vec(3)
      real(wp) :: vol
      integer :: prob
      integer :: trptr, total_self_nimg, start_count

      integer :: kmin(3), kmax(3)

      ! OpenMP specific variables
      integer :: nthr, tid, t, t_start, t_size, t_tr_start
      integer(int64) :: thr_mem, thr_maxtr

      ! Thread tracking arrays
      integer, allocatable :: thr_img(:), thr_trptr(:)
      integer, allocatable :: thr_start(:), thr_trstart(:)
      integer, allocatable :: thr_nimg_max(:)

      ! Thread-local storage buffers
      integer, allocatable :: thr_nlat(:,:), thr_nimg(:,:), thr_itr(:,:)
      integer, allocatable :: thr_tridx(:,:)

      ! Loop-private search variables for OMP
      integer :: nimg_count
      integer :: tridx_arr(27)

      zero_vec = 0.0_wp
      trptr = 0

      ! 1. Lattice setup and jacket translations
      call get_lattice_points(mol%periodic, mol%lattice, sqrt(epsilon(0.0_wp)), trans)
      ntr = size(trans, 2)
      if (allocated(self%trans)) deallocate(self%trans)
      allocate(self%trans, source=trans)
      allocate(wsc%trans, source=trans)

      cutoff2 = self%cutoff**2
      img = 0
      wsc%nimg_max = 0

      ! 2. Grid Sizing calculation based on lattice geometry and cutoff
      call compute_grid(mol, self%cutoff, det, n_xyz, lat_inv=lat_inv)

      ! 3. Build Linked Cell List using Atomic Capture
      allocate(head(product(n_xyz)), source=0)
      allocate(nxt(mol%nat), source=0)
      allocate(ccount(product(n_xyz)), source=0)

      call get_linked_cell(mol, n_xyz, head, nxt, ccount, lat_inv)

      ! Calculate median linked-cell population for non-zero cell counts
      nz_count = count(ccount > 0)
      call get_median(ccount, median)

      ! Dynamic Stencil bounds prevent duplicate cell visits when grid dimensions (N_xyz) are < 3
      do d = 1, 3
         select case (n_xyz(d))
          case (1)
            kmin(d) = 0; kmax(d) = 0
          case (2)
            kmin(d) = 0; kmax(d) = 1
          case default
            kmin(d) = -1; kmax(d) = 1
         end select
      end do

      ! 4. Setup OpenMP Thread-Local Environments
      !$omp parallel
      !$omp master
      nthr = omp_get_num_threads()
      !$omp end master
      !$omp end parallel

      vol = abs(det) * real(count(head /= 0), wp) / real(product(n_xyz), wp)
      dens = real(median, wp) * real(product(n_xyz), wp) / vol
      prob = ceiling(dens * self%cutoff**3.0_wp * 4.0_wp)
      if (self%complete) prob = prob * 2

      thr_mem = max(init_size * mol%nat, &
      & int(real((prob * mol%nat), wp) / real(nthr, wp), int64) * size(self%trans, 2))
      thr_maxtr = thr_mem * 6

      ! Allocate thread metrics
      allocate(thr_img(nthr), source=0)
      allocate(thr_trptr(nthr), source=0)
      allocate(thr_nimg_max(nthr), source=0)

      ! Allocate Thread-local buffers
      allocate(thr_nlat(thr_mem, nthr))
      allocate(thr_nimg(thr_mem, nthr))
      allocate(thr_itr(thr_mem, nthr))
      allocate(thr_tridx(thr_maxtr, nthr))

      ! Pre-allocate CSR row pointer array (size = nat + 1)
      if (allocated(self%inl)) deallocate(self%inl)
      allocate(self%inl(mol%nat + 1), source=0)

      ! 5. Threaded Loop Search
      !$omp parallel do schedule(static) &
      !$omp private(iat, tid, start_count, fract, ix, iy, iz, dk, dj, &
      !$omp        di, jx, jy, jz, jc, jat, vec, nimg_count, tridx_arr, r2_min, total_self_nimg) &
      !$omp shared(mol, self, head, nxt, thr_img, thr_trptr, &
      !$omp        thr_nlat, thr_nimg, thr_itr, thr_tridx, thr_nimg_max, &
      !$omp        trans, zero_vec, cutoff2, n_xyz, lat_inv, kmin, kmax, thr_mem, thr_maxtr)
      do iat = 1, mol%nat
         tid = omp_get_thread_num() + 1
         start_count = thr_img(tid)

         ! A. Search for diagonal periodic self-interactions
         call get_wsc_pairs(trans, zero_vec, nimg_count, tridx_arr, r2_min)
         if (nimg_count > 0 .and. r2_min <= cutoff2) then
            total_self_nimg = 1 + nimg_count
         else
            total_self_nimg = 1
         end if

         thr_img(tid) = thr_img(tid) + 1
         if (thr_img(tid) > thr_mem) error stop "CSR list size overflow!"
         if (thr_trptr(tid) + total_self_nimg > thr_maxtr) error stop "Translation index list overflow"

         ! Insert diagonal element at first position of current row
         thr_nlat(thr_img(tid), tid) = iat
         thr_nimg(thr_img(tid), tid) = total_self_nimg
         thr_itr(thr_img(tid), tid) = thr_trptr(tid) + 1
         thr_nimg_max(tid) = max(thr_nimg_max(tid), total_self_nimg)
         thr_tridx(thr_trptr(tid) + 1, tid) = 1

         ! Append remaining periodic self-images if present
         if (total_self_nimg > 1) then
            thr_tridx(thr_trptr(tid) + 2 : thr_trptr(tid) + total_self_nimg, tid) = tridx_arr(1:nimg_count)
         end if

         thr_trptr(tid) = thr_trptr(tid) + total_self_nimg

         ! B. Search for off-diagonal periodic interactions
         fract(:) = matmul(lat_inv, mol%xyz(:, iat))
         fract(:) = fract(:) - floor(fract(:))
         ix = min(n_xyz(1), max(1, int(fract(1) * n_xyz(1)) + 1))
         iy = min(n_xyz(2), max(1, int(fract(2) * n_xyz(2)) + 1))
         iz = min(n_xyz(3), max(1, int(fract(3) * n_xyz(3)) + 1))

         ! Traversing cells using dynamically clamped stencil bounds
         do dk = kmin(3), kmax(3)
            jz = modulo(iz + dk - 1, n_xyz(3)) + 1

            do dj = kmin(2), kmax(2)
               jy = modulo(iy + dj - 1, n_xyz(2)) + 1

               do di = kmin(1), kmax(1)
                  jx = modulo(ix + di - 1, n_xyz(1)) + 1

                  jc = jx + n_xyz(1)*(jy-1) + n_xyz(1)*n_xyz(2)*(jz-1)
                  jat = head(jc)

                  do while (jat > 0)
                     ! Upper Triangular (jat > iat) vs Complete Mode (all jat /= iat)
                     if (self%complete .or. jat > iat) then
                        vec(:) = mol%xyz(:, iat) - mol%xyz(:, jat)
                        call get_wsc_pairs(trans, vec, nimg_count, tridx_arr, r2_min)

                        if (nimg_count > 0 .and. r2_min <= cutoff2) then
                           thr_img(tid) = thr_img(tid) + 1

                           if (thr_img(tid) > thr_mem) error stop "CSR list size overflow"
                           if (thr_trptr(tid) + nimg_count > thr_maxtr) error stop "Translation index list overflow"

                           thr_nlat(thr_img(tid), tid) = jat
                           thr_nimg(thr_img(tid), tid) = nimg_count
                           thr_itr(thr_img(tid), tid) = thr_trptr(tid) + 1
                           thr_nimg_max(tid) = max(thr_nimg_max(tid), nimg_count)

                           thr_tridx(thr_trptr(tid) + 1 : thr_trptr(tid) + nimg_count, tid) = tridx_arr(1:nimg_count)
                           thr_trptr(tid) = thr_trptr(tid) + nimg_count
                        end if
                     end if

                     jat = nxt(jat)
                  end do
               end do
            end do
         end do

         ! Temporarily store neighbor count for atom iat at position (iat + 1)
         self%inl(iat + 1) = thr_img(tid) - start_count
      end do
      !$omp end parallel do

      ! 6. The Stitching Phase
      allocate(thr_start(nthr), source=0)
      allocate(thr_trstart(nthr), source=0)
      thr_start(1) = 0
      thr_trstart(1) = 0

      do t = 2, nthr
         thr_start(t) = thr_start(t-1) + thr_img(t-1)
         thr_trstart(t) = thr_trstart(t-1) + thr_trptr(t-1)
      end do

      img = thr_start(nthr) + thr_img(nthr)
      trptr = thr_trstart(nthr) + thr_trptr(nthr)
      wsc%nimg_max = maxval(thr_nimg_max)

      ! CSR Pointer array construction
      self%inl(1) = 1
      do iat = 1, mol%nat
         self%inl(iat + 1) = self%inl(iat) + self%inl(iat + 1)
      end do

      ! 7. CSR List Resizing & Stream Copying
      call resize(self%nlat, img)
      call resize(wsc%nimg_list, img)
      call resize(wsc%itr_list, img + 1)
      call resize(wsc%tridx_list, trptr)

      !$omp parallel private(tid, t_start, t_size, t_tr_start) &
      !$omp shared(self, wsc, thr_start, thr_trstart, thr_img, thr_trptr, &
      !$omp        thr_nlat, thr_nimg, thr_itr, thr_tridx)
      tid = omp_get_thread_num() + 1
      t_start = thr_start(tid)
      t_size = thr_img(tid)
      t_tr_start = thr_trstart(tid)

      if (t_size > 0) then
         self%nlat(t_start + 1 : t_start + t_size) = thr_nlat(1 : t_size, tid)
         wsc%nimg_list(t_start + 1 : t_start + t_size) = thr_nimg(1 : t_size, tid)
         wsc%itr_list(t_start + 1 : t_start + t_size) = thr_itr(1 : t_size, tid) + t_tr_start
      end if

      if (thr_trptr(tid) > 0) then
         wsc%tridx_list(t_tr_start + 1 : t_tr_start + thr_trptr(tid)) = thr_tridx(1 : thr_trptr(tid), tid)
      end if
      !$omp end parallel
      wsc%itr_list(img + 1) = trptr + 1

      deallocate(head, nxt, ccount)
      deallocate(thr_img, thr_trptr)
      deallocate(thr_start, thr_trstart)
      deallocate(thr_nimg_max)
      deallocate(thr_nlat, thr_nimg, thr_itr, thr_tridx)

   end subroutine generate_wsc

   subroutine get_wsc_pairs(trans, rij, iws, list, min_r2)
      !> Translation vectors
      real(wp), intent(in) :: trans(:, :)
      !> Interatomic vector
      real(wp), intent(in) :: rij(3)
      !> Number of images for a pair
      integer, intent(out) :: iws
      !> List of image indices for a pair
      integer, intent(out) :: list(:)
      !> Minimum squared distance found
      real(wp), intent(out) :: min_r2

      real(wp) :: dx, dy, dz, r2
      integer :: itr, ntr, img

      ntr = size(trans, 2)
      iws = 0
      img = 0
      min_r2 = huge(1.0_wp)

      do itr = 1, ntr
         dx = rij(1) - trans(1, itr)
         dy = rij(2) - trans(2, itr)
         dz = rij(3) - trans(3, itr)
         r2 = dx*dx + dy*dy + dz*dz

         if (r2 < thr) cycle
         img = img + 1

         if (r2 < min_r2 - tol) then
            ! Found a strictly better minimum
            min_r2 = r2
            iws = 1
            list(1) = img
         else if (r2 < min_r2 + tol) then
            ! Within tolerance: record degeneracy
            iws = iws + 1
            list(iws) = img
         end if
      end do

   end subroutine get_wsc_pairs

   !> Computes safe linked cell grid sub-divisions for any crystal class
   subroutine compute_grid(mol, cutoff, det, n_xyz, lat_inv, cell_w)
      !> Stucture type
      type(structure_type), intent(in) :: mol
      !> Interaction cutoff radius
      real(wp), intent(in) :: cutoff
      !> Determinant (Volume) of the lattice
      real(wp), intent(out) :: det
      !> Output: Number of grid subdivisions along each axis
      integer, intent(out) :: n_xyz(3)
      !> Inverse of the lattice matrix
      real(wp), intent(out), optional :: lat_inv(3, 3)
      !> Width of each grid cell
      real(wp), intent(out), optional :: cell_w(3)

      real(wp) :: L_vec, H(3), cross_ij(3), lattice(3, 3)
      real(wp) :: dot_12, dot_23, dot_31, max_xyz(3), min_xyz(3)
      integer  :: i

      if (any(mol%periodic)) then
         ! Inverse Lattice Matrix
         lattice = mol%lattice
         det = lattice(1,1)*(lattice(2,2)*lattice(3,3) - lattice(2,3)*lattice(3,2)) - &
            lattice(1,2)*(lattice(2,1)*lattice(3,3) - lattice(2,3)*lattice(3,1)) + &
            lattice(1,3)*(lattice(2,1)*lattice(3,2) - lattice(2,2)*lattice(3,1))

         if (present(lat_inv)) then
            lat_inv(1,1) =  (lattice(2,2)*lattice(3,3) - lattice(2,3)*lattice(3,2)) / det
            lat_inv(1,2) = -(lattice(1,2)*lattice(3,3) - lattice(1,3)*lattice(3,2)) / det
            lat_inv(1,3) =  (lattice(1,2)*lattice(2,3) - lattice(1,3)*lattice(2,2)) / det
            lat_inv(2,1) = -(lattice(2,1)*lattice(3,3) - lattice(2,3)*lattice(3,1)) / det
            lat_inv(2,2) =  (lattice(1,1)*lattice(3,3) - lattice(1,3)*lattice(3,1)) / det
            lat_inv(2,3) = -(lattice(1,1)*lattice(2,3) - lattice(1,3)*lattice(2,1)) / det
            lat_inv(3,1) =  (lattice(2,1)*lattice(3,2) - lattice(2,2)*lattice(3,1)) / det
            lat_inv(3,2) = -(lattice(1,1)*lattice(3,2) - lattice(1,2)*lattice(3,1)) / det
            lat_inv(3,3) =  (lattice(1,1)*lattice(2,2) - lattice(1,2)*lattice(2,1)) / det
         end if

         ! Calculates strict perpendicular heights via reciprocal cross products

         ! Perpendicular height H1 (normal to a2 x a3)
         cross_ij(1) = lattice(2,2)*lattice(3,3) - lattice(3,2)*lattice(2,3)
         cross_ij(2) = lattice(3,2)*lattice(1,3) - lattice(1,2)*lattice(3,3)
         cross_ij(3) = lattice(1,2)*lattice(2,3) - lattice(2,2)*lattice(1,3)
         H(1) = abs(det) / sqrt(sum(cross_ij**2))

         ! Perpendicular height H2 (normal to a3 x a1)
         cross_ij(1) = lattice(2,3)*lattice(3,1) - lattice(3,3)*lattice(2,1)
         cross_ij(2) = lattice(3,3)*lattice(1,1) - lattice(1,3)*lattice(3,1)
         cross_ij(3) = lattice(1,3)*lattice(2,1) - lattice(2,3)*lattice(1,1)
         H(2) = abs(det) / sqrt(sum(cross_ij**2))

         ! Perpendicular height H3 (normal to a1 x a2)
         cross_ij(1) = lattice(2,1)*lattice(3,2) - lattice(3,1)*lattice(2,2)
         cross_ij(2) = lattice(3,1)*lattice(1,2) - lattice(1,1)*lattice(3,2)
         cross_ij(3) = lattice(1,1)*lattice(2,2) - lattice(2,1)*lattice(1,2)
         H(3) = abs(det) / sqrt(sum(cross_ij**2))

         ! Map cells dynamically to the strict real-space thickness
         do i = 1, 3
            n_xyz(i) = max(1, floor(H(i) / cutoff))
         end do
      else
         min_xyz = minval(mol%xyz, dim=2) - buffer
         max_xyz = maxval(mol%xyz, dim=2) + buffer

         ! Number of cells: must be at least 1, and cell width >= cutoff
         n_xyz = max(1, floor((max_xyz - min_xyz) / (cutoff + eps)))
         if (present(cell_w)) then
            cell_w = (max_xyz - min_xyz) / (real(n_xyz, wp) + eps) + eps
         end if
         det = product(max_xyz - min_xyz)
      end if

   end subroutine compute_grid

   subroutine get_linked_cell(mol, n_xyz, head, nxt, ccount, lat_inv, cell_w)
      !> Stucture type
      type(structure_type), intent(in) :: mol
      !> Output: Number of grid subdivisions along each axis
      integer, intent(in) :: n_xyz(3)
      !> Linked cell list head array
      integer, intent(out) :: head(:)
      !> Linked cell list next array
      integer, intent(out) :: nxt(:)
      !> Cell population counts
      integer, intent(out) :: ccount(:)
      !> Inverse of the lattice matrix
      real(wp), intent(in), optional :: lat_inv(3, 3)
      !> Width of each grid cell
      real(wp), intent(in), optional :: cell_w(3)

      integer :: iat, ix, iy, iz, ic
      real(wp) :: fract(3), min_xyz(3), max_xyz(3)

      if (any(mol%periodic) .and. present(lat_inv)) then
         !$omp parallel do private(iat, fract, ix, iy, iz, ic) &
         !$omp shared(mol, lat_inv, n_xyz, head, nxt, ccount)
         do iat = 1, mol%nat
            fract(:) = matmul(lat_inv, mol%xyz(:, iat))
            fract(:) = fract(:) - floor(fract(:))

            ix = min(n_xyz(1), max(1, int(fract(1) * n_xyz(1)) + 1))
            iy = min(n_xyz(2), max(1, int(fract(2) * n_xyz(2)) + 1))
            iz = min(n_xyz(3), max(1, int(fract(3) * n_xyz(3)) + 1))

            ic = ix + n_xyz(1)*(iy-1) + n_xyz(1)*n_xyz(2)*(iz-1)

            !$omp atomic capture
            nxt(iat) = head(ic)
            head(ic) = iat
            !$omp end atomic

            !$omp atomic
            ccount(ic) = ccount(ic) + 1
         end do
         !$omp end parallel do
      else if (present(cell_w)) then
         min_xyz = minval(mol%xyz, dim=2) - buffer
         max_xyz = maxval(mol%xyz, dim=2) + buffer

         !$omp parallel do private(iat, ix, iy, iz, ic) &
         !$omp shared(mol, n_xyz, min_xyz, cell_w, head, nxt, ccount)
         do iat = 1, mol%nat
            ix = min(n_xyz(1), max(1, int((mol%xyz(1, iat) - min_xyz(1)) / cell_w(1)) + 1))
            iy = min(n_xyz(2), max(1, int((mol%xyz(2, iat) - min_xyz(2)) / cell_w(2)) + 1))
            iz = min(n_xyz(3), max(1, int((mol%xyz(3, iat) - min_xyz(3)) / cell_w(3)) + 1))

            ic = ix + n_xyz(1)*(iy-1) + n_xyz(1)*n_xyz(2)*(iz-1)

            !$omp atomic capture
            nxt(iat) = head(ic)
            head(ic) = iat
            !$omp end atomic

            !$omp atomic
            ccount(ic) = ccount(ic) + 1
         end do
         !$omp end parallel do
      end if

   end subroutine get_linked_cell

   subroutine get_median(cells, median)
      integer, intent(in) :: cells(:)
      integer, intent(out) :: median

      integer :: nz_count, max_cell_val, cumulative_sum, i_cell
      integer, allocatable :: hist(:)

      nz_count = count(cells > 0)

      if (nz_count > 0) then
         max_cell_val = maxval(cells)
         allocate(hist(1:max_cell_val), source=0)

         ! Build histogram of population frequencies for non-zero cells
         do i_cell = 1, size(cells)
            if (cells(i_cell) > 0) then
               hist(cells(i_cell)) = hist(cells(i_cell)) + 1
            end if
         end do

         cumulative_sum = 0
         median = 1
         do i_cell = 1, max_cell_val
            cumulative_sum = cumulative_sum + hist(i_cell)
            if (cumulative_sum >= (nz_count + 1) / 2) then
               median = i_cell
               exit
            end if
         end do

         deallocate(hist)
      else
         median = 0
      end if

   end subroutine get_median

end module mctc_csrlist_type
