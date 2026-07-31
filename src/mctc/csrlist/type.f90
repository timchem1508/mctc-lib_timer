! This file is part of multicharge.
! SPDX-Identifier: Apache-2.0
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
   implicit none
   private

   public :: csr_list, new_csr_list

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
      !> Wiegner-Seitz cell parameters
      !> Maximal number of interaction images
      integer :: nimg_max
      !> Pointer index of the Wiegner-Seitz translation vector for each neighbour
      integer, allocatable :: itr(:)
      !> Number of images per cross-interaction
      integer, allocatable :: nimg(:)
      !> Primitive unit cell image index per cross-interaction
      integer, allocatable :: tridx(:)
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
   subroutine new_csr_list(self, mol, cutoff, trans, complete)
      !> Instance of the neighbourlist
      type(csr_list), intent(out) :: self
      !> Structure type
      type(structure_type), intent(in) :: mol
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
         call generate_wsc(self, mol)
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
      use iso_fortran_env, only: int64

      !> Instance of the neighbourlist
      type(csr_list), intent(inout) :: self
      !> Molecular structure data
      type(structure_type), intent(in) :: mol

      integer :: iat, jat, itr, img, ic, jc
      integer :: ix, iy, iz, jx, jy, jz, di, dj, dk
      integer, allocatable :: head(:), nxt(:)
      integer :: n_xyz(3)
      real(wp) :: r2, vec(3), cutoff2, cell_w(3), min_xyz(3), max_xyz(3)
      real(wp) :: vol, dens
      integer(int64) :: prob
      integer, allocatable :: cell_count(:)

      ! Statistical estimation variables
      integer :: nz_count, median_val
      integer, allocatable :: hist(:)

      ! OpenMP specific variables
      integer(int64) :: max_neigh_per_thread
      integer :: nthreads, tid
      integer :: start_idx, local_offset, n_neigh
      ! Track current neighbor count per thread
      integer, allocatable :: thread_img(:)
      ! Start index in thread local buffer per atom
      integer, allocatable :: thread_inl(:)
      ! Track which thread processed which atom
      integer, allocatable :: atrack(:)
      ! Thread-local neighbor storage [max_neigh, nthreads]
      integer, allocatable :: thread_nlat(:,:)
      ! Thread-local translation storage [max_neigh, nthreads]
      integer, allocatable :: thread_nltr(:,:)

      img = 0
      cutoff2 = self%cutoff**2

      ! 1. Define the grid boundaries and dimensions
      min_xyz = minval(mol%xyz, dim=2) - buffer
      max_xyz = maxval(mol%xyz, dim=2) + buffer

      ! Number of cells: must be at least 1, and cell width >= cutoff
      n_xyz = max(1, floor((max_xyz - min_xyz) / (self%cutoff + eps)))
      cell_w = (max_xyz - min_xyz) / (real(n_xyz, wp) + eps) + eps

      ! 2. Build the Linked List using Atomic Capture
      allocate(head(product(n_xyz)), source=0)
      allocate(cell_count(product(n_xyz)), source=0)
      allocate(nxt(mol%nat), source=0)

      !$omp parallel do private(iat, ix, iy, iz, ic) &
      !$omp shared(mol, n_xyz, min_xyz, cell_w, head, nxt, cell_count)
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
         cell_count(ic) = cell_count(ic) + 1
      end do
      !$omp end parallel do

      ! Linked-cell statistics: median and non-zero cell count
      nz_count = count(cell_count > 0)
      call get_median(cell_count, median_val)

      ! Estimate the number of neighbors based on median cell population and volume
      vol = product(max_xyz - min_xyz) * real(nz_count, wp) / real(product(n_xyz), wp)
      dens = real(median_val, wp) * real(product(n_xyz), wp) / vol
      prob = max(int(init_size, int64), int(ceiling(dens * self%cutoff**3.0_wp * 4.0_wp), int64))
      ! Double buffer estimate if complete matrix mode is enabled
      if (self%complete) prob = prob * 2

      ! 3. Setup OpenMP Thread-Local Environments
      !$omp parallel
      !$omp master
      nthreads = omp_get_num_threads()
      !$omp end master
      !$omp end parallel

      allocate(thread_img(nthreads), source=0)
      allocate(atrack(mol%nat), source=0)
      allocate(thread_inl(mol%nat), source=0)

      ! Estimate a safe thread-local allocation size with a safety buffer
      max_neigh_per_thread = int(real((prob * mol%nat), wp) / real(nthreads, wp), int64)
      allocate(thread_nlat(max_neigh_per_thread, nthreads))
      if (any(mol%periodic)) allocate(thread_nltr(max_neigh_per_thread, nthreads))

      ! 4. Threaded Loop Search over nearby cells
      if (any(mol%periodic)) then
         ! Periodic branch

         !$omp parallel do schedule(guided) &
         !$omp private(iat, tid, ix, iy, iz, dk, dj, di) &
         !$omp private(jx, jy, jz, jc, jat, itr, vec, r2) &
         !$omp shared(mol, self, head, nxt, thread_img, thread_inl) &
         !$omp shared(thread_nlat, thread_nltr, cell_w, min_xyz) &
         !$omp shared(n_xyz, cutoff2, atrack, max_neigh_per_thread)
         do iat = 1, mol%nat
            tid = omp_get_thread_num() + 1
            atrack(iat) = tid
            thread_inl(iat) = thread_img(tid)

            ! Inject the diagonal element (self-interaction) as the first neighbour
            thread_img(tid) = thread_img(tid) + 1
            if (thread_img(tid) > max_neigh_per_thread) then
               error stop "Thread local neighbor buffer overflow! Increase safety margin."
            end if
            thread_nlat(thread_img(tid), tid) = iat
            thread_nltr(thread_img(tid), tid) = 1

            ! Off-diagonal neighbor search within the cutoff radius
            ix = min(n_xyz(1), max(1, int((mol%xyz(1, iat) - min_xyz(1)) / cell_w(1)) + 1))
            iy = min(n_xyz(2), max(1, int((mol%xyz(2, iat) - min_xyz(2)) / cell_w(2)) + 1))
            iz = min(n_xyz(3), max(1, int((mol%xyz(3, iat) - min_xyz(3)) / cell_w(3)) + 1))

            do dk = -1, 1; do dj = -1, 1; do di = -1, 1
                     jx = ix + di; jy = iy + dj; jz = iz + dk

                     if (jx < 1 .or. jx > n_xyz(1) .or. &
                        jy < 1 .or. jy > n_xyz(2) .or. &
                        jz < 1 .or. jz > n_xyz(3)) cycle

                     jc = jx + n_xyz(1)*(jy-1) + n_xyz(1)*n_xyz(2)*(jz-1)
                     jat = head(jc)

                     do while (jat > 0)
                        if (jat > iat .or. self%complete) then
                           jat = nxt(jat); cycle
                        end if

                        do itr = 1, size(self%trans, 2)
                           vec(:) = mol%xyz(:, iat) - mol%xyz(:, jat) - self%trans(:, itr)
                           r2 = sum(vec**2)

                           if (r2 < epsilon(cutoff2) .or. r2 > cutoff2) cycle

                           thread_img(tid) = thread_img(tid) + 1

                           if (thread_img(tid) > max_neigh_per_thread) then
                              error stop "Thread local neighbor buffer overflow! Increase safety margin."
                           end if

                           thread_nlat(thread_img(tid), tid) = jat
                           thread_nltr(thread_img(tid), tid) = itr
                        end do
                        jat = nxt(jat)
                     end do
                  end do; end do; end do
            ! Temporarily store the neighbor count in `inl(iat + 1)`
            self%inl(iat + 1) = thread_img(tid) - thread_inl(iat)
         end do
         !$omp end parallel do

      else
         ! Non-periodic branch
         !$omp parallel do schedule(guided) &
         !$omp private(iat, tid, ix, iy, iz, dk, dj, di) &
         !$omp private(jx, jy, jz, jc, jat, vec, r2) &
         !$omp shared(mol, self, head, nxt, thread_img, thread_inl) &
         !$omp shared(thread_nlat, cell_w, min_xyz, n_xyz, cutoff2) &
         !$omp shared(atrack, max_neigh_per_thread)
         do iat = 1, mol%nat
            tid = omp_get_thread_num() + 1
            atrack(iat) = tid
            thread_inl(iat) = thread_img(tid)

            ! Inject the diagonal element (self-interaction) as the first neighbour
            thread_img(tid) = thread_img(tid) + 1
            if (thread_img(tid) > max_neigh_per_thread) then
               error stop "Thread local neighbor buffer overflow! Increase safety margin."
            end if
            thread_nlat(thread_img(tid), tid) = iat

            ! Off-diagonal neighbor search within the cutoff radius
            ix = min(n_xyz(1), max(1, int((mol%xyz(1, iat) - min_xyz(1)) / cell_w(1)) + 1))
            iy = min(n_xyz(2), max(1, int((mol%xyz(2, iat) - min_xyz(2)) / cell_w(2)) + 1))
            iz = min(n_xyz(3), max(1, int((mol%xyz(3, iat) - min_xyz(3)) / cell_w(3)) + 1))

            do dk = -1, 1; do dj = -1, 1; do di = -1, 1
                     jx = ix + di; jy = iy + dj; jz = iz + dk

                     if (jx < 1 .or. jx > n_xyz(1) .or. &
                        jy < 1 .or. jy > n_xyz(2) .or. &
                        jz < 1 .or. jz > n_xyz(3)) cycle

                     jc = jx + n_xyz(1)*(jy-1) + n_xyz(1)*n_xyz(2)*(jz-1)
                     jat = head(jc)

                     do while (jat > 0)
                        if (jat > iat .or. self%complete) then
                           jat = nxt(jat); cycle
                        end if

                        vec(:) = mol%xyz(:, iat) - mol%xyz(:, jat)
                        r2 = sum(vec**2)

                        if (r2 < epsilon(cutoff2) .or. r2 > cutoff2) then
                           jat = nxt(jat); cycle
                        end if

                        thread_img(tid) = thread_img(tid) + 1

                        if (thread_img(tid) > max_neigh_per_thread) then
                           write(*, *) "Thread ", tid, " exceeded max_neigh_per_thread: ", thread_img(tid), " > ", max_neigh_per_thread
                           error stop "Thread local neighbor buffer overflow! Increase safety margin."
                        end if

                        thread_nlat(thread_img(tid), tid) = jat
                        jat = nxt(jat)
                     end do
                  end do; end do; end do
            ! Temporarily store the neighbor count in self%inl
            self%inl(iat + 1) = thread_img(tid) - thread_inl(iat)
         end do
         !$omp end parallel do
      end if

      ! 5. The Stitching Phase

      ! Reconstruction of the actual `self%inl` pointer
      self%inl(1) = 1
      do iat = 1, mol%nat
         self%inl(iat + 1) = self%inl(iat) + self%inl(iat + 1)
      end do

      ! Reallocate global storage targets to the exact final size
      img = self%inl(mol%nat + 1) - 1
      call resize(self%nlat, img)
      if (any(mol%periodic)) call resize(self%nltr, img)

      ! Parallel scatter-copy data from thread buffers into the sorted global CSR structure
      !$omp parallel do schedule(static) private(iat, tid, start_idx, local_offset, n_neigh) &
      !$omp shared(mol, self, atrack, thread_inl, thread_nlat, thread_nltr)
      do iat = 1, mol%nat
         tid = atrack(iat)
         start_idx = self%inl(iat)
         local_offset = thread_inl(iat)
         n_neigh = self%inl(iat + 1) - self%inl(iat)

         if (n_neigh > 0) then
            ! The first element copied here will naturally be the explicitly added diagonal `iat`
            self%nlat(start_idx : start_idx + n_neigh - 1) = &
               thread_nlat(local_offset + 1 : local_offset + n_neigh, tid)

            if (any(mol%periodic)) then
               self%nltr(start_idx : start_idx + n_neigh - 1) = &
                  thread_nltr(local_offset + 1 : local_offset + n_neigh, tid)
            end if
         end if
      end do
      !$omp end parallel do

      ! 6. Cleanup Memory
      if (allocated(head)) deallocate(head)
      if (allocated(nxt)) deallocate(nxt)
      deallocate(thread_img, atrack, thread_inl, thread_nlat, cell_count)
      if (allocated(thread_nltr)) deallocate(thread_nltr)

   end subroutine generate_hybrid

!> Generator of the CSR-based Hybrid Neighbour List for Periodic systems using Wigner-Seitz Cell Search
   subroutine generate_wsc(self, mol)
      use omp_lib
      use iso_fortran_env, only: int64

      !> Instance of the neighbourlist
      type(csr_list), intent(inout) :: self
      !> Molecular structure data
      type(structure_type), intent(in) :: mol

      integer :: iat, jat, img, ic, jc
      integer :: ix, iy, iz, jx, jy, jz, di, dj, dk
      integer, allocatable :: head(:), nxt(:), cell_count(:)

      integer :: n_xyz(3), ntr, median_val, nz_count
      real(wp) :: cutoff2, r2_min, dens
      real(wp) :: lat_inv(3, 3), det
      real(wp) :: fract(3), xyz_wrap(3, mol%nat)
      real(wp), allocatable :: trans(:, :)
      real(wp) :: vec(3), zero_vec(3)
      real(wp) :: vol
      integer :: prob
      integer :: ptr_tr, total_self_nimg, start_count

      ! OpenMP specific variables
      integer :: nthreads, tid, t, t_start, t_size, t_tr_start
      integer(int64) :: max_neigh_per_thread, max_tr_per_thread

      ! Thread tracking arrays
      integer, allocatable :: thread_img(:), thread_ptr_tr(:)
      integer, allocatable :: thread_global_start(:), thread_global_tr_start(:)
      integer, allocatable :: thread_nimg_max(:)
      logical, allocatable :: thread_checked(:,:)

      ! Thread-local storage buffers
      integer, allocatable :: thread_nlat(:,:), thread_nimg(:,:), thread_itr(:,:)
      integer, allocatable :: thread_tridx(:,:)

      ! Loop-private search variables for OMP
      integer :: nimg_count
      integer :: tridx_arr(27)

      zero_vec = 0.0_wp
      ptr_tr = 0

      ! 1. Lattice setup and jacket translations
      call get_lattice_points(mol%periodic, mol%lattice, sqrt(epsilon(0.0_wp)), trans)
      ntr = size(trans, 2)
      if (allocated(self%trans)) deallocate(self%trans)
      allocate(self%trans, source=trans)

      cutoff2 = self%cutoff**2
      img = 0
      self%nimg_max = 0

      ! 2. Grid Sizing calculation based on lattice geometry and cutoff
      call compute_grid(mol%lattice, self%cutoff, lat_inv, det, n_xyz)

      ! 3. Build Linked Cell List using Atomic Capture
      allocate(head(product(n_xyz)), source=0)
      allocate(nxt(mol%nat), source=0)
      allocate(cell_count(product(n_xyz)), source=0)

      !$omp parallel do private(iat, fract, ix, iy, iz, ic) &
      !$omp shared(mol, lat_inv, n_xyz, head, nxt, xyz_wrap, cell_count)
      do iat = 1, mol%nat
         fract(:) = matmul(lat_inv, mol%xyz(:, iat))
         fract(:) = fract(:) - floor(fract(:))
         xyz_wrap(:, iat) = matmul(mol%lattice, fract)

         ix = min(n_xyz(1), max(1, int(fract(1) * n_xyz(1)) + 1))
         iy = min(n_xyz(2), max(1, int(fract(2) * n_xyz(2)) + 1))
         iz = min(n_xyz(3), max(1, int(fract(3) * n_xyz(3)) + 1))

         ic = ix + n_xyz(1)*(iy-1) + n_xyz(1)*n_xyz(2)*(iz-1)

         !$omp atomic capture
         nxt(iat) = head(ic)
         head(ic) = iat
         !$omp end atomic

         !$omp atomic
         cell_count(ic) = cell_count(ic) + 1
      end do
      !$omp end parallel do

      ! Calculate median linked-cell population for non-zero cell counts
      nz_count = count(cell_count > 0)
      call get_median(cell_count, median_val)

      ! 4. Setup OpenMP Thread-Local Environments
      !$omp parallel
      !$omp master
      nthreads = omp_get_num_threads()
      !$omp end master
      !$omp end parallel

      ! Estimate safe memory bounds based on density
      vol = abs(det) * real(count(head /= 0), wp) / real(product(n_xyz), wp)
      dens = real(median_val, wp) * real(product(n_xyz), wp) / vol
      prob = max(init_size, ceiling(dens * self%cutoff**3.0_wp * 4.0_wp))

      ! Double buffer estimate if complete matrix mode is enabled
      if (self%complete) prob = prob * 2
      write(*, *) " estimated neighbors: ", prob*mol%nat

      max_neigh_per_thread = int(real((prob * mol%nat), wp) / real(nthreads, wp), int64)
      max_tr_per_thread = max_neigh_per_thread * 6 + 100

      ! Allocate thread metrics
      allocate(thread_img(nthreads), source=0)
      allocate(thread_ptr_tr(nthreads), source=0)

      allocate(thread_global_start(nthreads), source=0)
      allocate(thread_global_tr_start(nthreads), source=0)

      allocate(thread_nimg_max(nthreads), source=0)
      allocate(thread_checked(mol%nat, nthreads), source=.false.)

      ! Allocate Thread-local buffers
      allocate(thread_nlat(max_neigh_per_thread, nthreads))
      allocate(thread_nimg(max_neigh_per_thread, nthreads))
      allocate(thread_itr(max_neigh_per_thread, nthreads))
      allocate(thread_tridx(max_tr_per_thread, nthreads))

      ! Pre-allocate CSR row pointer array (size = nat + 1)
      if (allocated(self%inl)) deallocate(self%inl)
      allocate(self%inl(mol%nat + 1), source=0)

      ! 5. Threaded Loop Search (Static scheduling preserves row ordering)
      !$omp parallel do schedule(static) &
      !$omp private(iat, tid, start_count, fract, ix, iy, iz, dk, dj, di, jx, jy, jz, jc, jat, vec, nimg_count, tridx_arr, r2_min, total_self_nimg) &
      !$omp shared(mol, self, head, nxt, thread_img, thread_ptr_tr, thread_checked, &
      !$omp        thread_nlat, thread_nimg, thread_itr, thread_tridx, thread_nimg_max, &
      !$omp        trans, zero_vec, cutoff2, n_xyz, lat_inv, max_neigh_per_thread, max_tr_per_thread)
      do iat = 1, mol%nat
         tid = omp_get_thread_num() + 1
         start_count = thread_img(tid)

         ! ------------------------------------------------------------------
         ! A. PRE-INSERT DIAGONAL ENTRY (j = iat) WITH ALL SELF-TRANSLATIONS
         ! ------------------------------------------------------------------
         call get_wsc_pairs(trans, zero_vec, nimg_count, tridx_arr, r2_min)

         ! Total translations for j = iat: 1 (Identity R=0) + periodic self-images within cutoff
         if (nimg_count > 0 .and. r2_min <= cutoff2) then
            total_self_nimg = 1 + nimg_count
         else
            total_self_nimg = 1
         end if

         thread_img(tid) = thread_img(tid) + 1
         if (thread_img(tid) > max_neigh_per_thread) error stop "Cross-image buffer overflow"
         if (thread_ptr_tr(tid) + total_self_nimg > max_tr_per_thread) error stop "Tridx buffer overflow"

         thread_nlat(thread_img(tid), tid) = iat
         thread_nimg(thread_img(tid), tid) = total_self_nimg
         ! Store 1-based local index into translation array
         thread_itr(thread_img(tid), tid) = thread_ptr_tr(tid) + 1
         thread_nimg_max(tid) = max(thread_nimg_max(tid), total_self_nimg)

         ! First translation is Identity (index 1)
         thread_tridx(thread_ptr_tr(tid) + 1, tid) = 1

         ! Append remaining periodic self-images (R /= 0) if present
         if (total_self_nimg > 1) then
            thread_tridx(thread_ptr_tr(tid) + 2 : thread_ptr_tr(tid) + total_self_nimg, tid) = tridx_arr(1:nimg_count)
         end if

         thread_ptr_tr(tid) = thread_ptr_tr(tid) + total_self_nimg

         ! ------------------------------------------------------------------
         ! B. CROSS-ATOM NEIGHBOR SEARCH
         ! ------------------------------------------------------------------
         fract(:) = matmul(lat_inv, mol%xyz(:, iat))
         fract(:) = fract(:) - floor(fract(:))
         ix = min(n_xyz(1), max(1, int(fract(1) * n_xyz(1)) + 1))
         iy = min(n_xyz(2), max(1, int(fract(2) * n_xyz(2)) + 1))
         iz = min(n_xyz(3), max(1, int(fract(3) * n_xyz(3)) + 1))

         ! Mark iat as checked so j = iat is skipped in cell search loop
         thread_checked(iat, tid) = .true.

         do dk = -1, 1; do dj = -1, 1; do di = -1, 1
                  jx = modulo(ix + di - 1, n_xyz(1)) + 1
                  jy = modulo(iy + dj - 1, n_xyz(2)) + 1
                  jz = modulo(iz + dk - 1, n_xyz(3)) + 1
                  jc = jx + n_xyz(1)*(jy-1) + n_xyz(1)*n_xyz(2)*(jz-1)
                  jat = head(jc)

                  do while (jat > 0)
                     if (.not. thread_checked(jat, tid)) then
                        thread_checked(jat, tid) = .true.

                        ! Upper Triangular (jat > iat) vs Complete Mode (all jat /= iat)
                        if (self%complete .or. jat > iat) then
                           vec(:) = mol%xyz(:, iat) - mol%xyz(:, jat)
                           call get_wsc_pairs(trans, vec, nimg_count, tridx_arr, r2_min)

                           if (nimg_count > 0 .and. r2_min <= cutoff2) then
                              thread_img(tid) = thread_img(tid) + 1

                              if (thread_img(tid) > max_neigh_per_thread) error stop "Cross-image buffer overflow"
                              if (thread_ptr_tr(tid) + nimg_count > max_tr_per_thread) error stop "Tridx buffer overflow"

                              thread_nlat(thread_img(tid), tid) = jat
                              thread_nimg(thread_img(tid), tid) = nimg_count
                              ! Store 1-based local index into translation array
                              thread_itr(thread_img(tid), tid) = thread_ptr_tr(tid) + 1
                              thread_nimg_max(tid) = max(thread_nimg_max(tid), nimg_count)

                              thread_tridx(thread_ptr_tr(tid) + 1 : thread_ptr_tr(tid) + nimg_count, tid) = tridx_arr(1:nimg_count)
                              thread_ptr_tr(tid) = thread_ptr_tr(tid) + nimg_count
                           end if
                        end if
                     end if
                     jat = nxt(jat)
                  end do
               end do; end do; end do

         ! Clean up local checked array for next atom
         thread_checked(iat, tid) = .false.
         do dk = -1, 1; do dj = -1, 1; do di = -1, 1
                  jx = modulo(ix + di - 1, n_xyz(1)) + 1
                  jy = modulo(iy + dj - 1, n_xyz(2)) + 1
                  jz = modulo(iz + dk - 1, n_xyz(3)) + 1
                  jc = jx + n_xyz(1)*(jy-1) + n_xyz(1)*n_xyz(2)*(jz-1)
                  jat = head(jc)
                  do while (jat > 0)
                     thread_checked(jat, tid) = .false.
                     jat = nxt(jat)
                  end do
               end do; end do; end do

         ! Temporarily store neighbor count for atom iat at position (iat + 1)
         self%inl(iat + 1) = thread_img(tid) - start_count
      end do
      !$omp end parallel do

      ! 6. The Stitching Phase (Prefix Sums)
      thread_global_start(1) = 0
      thread_global_tr_start(1) = 0

      do t = 2, nthreads
         thread_global_start(t) = thread_global_start(t-1) + thread_img(t-1)
         thread_global_tr_start(t) = thread_global_tr_start(t-1) + thread_ptr_tr(t-1)
      end do

      img = thread_global_start(nthreads) + thread_img(nthreads)
      ptr_tr = thread_global_tr_start(nthreads) + thread_ptr_tr(nthreads)
      self%nimg_max = maxval(thread_nimg_max)

      ! Construct standard 1-based CSR Row Pointer Array in-place via prefix sum
      self%inl(1) = 1
      do iat = 1, mol%nat
         self%inl(iat + 1) = self%inl(iat) + self%inl(iat + 1)
      end do

      ! 7. Target Array Resizing & Stream Copying
      call resize(self%nlat, img)
      call resize(self%nimg, img)
      call resize(self%itr, img + 1) ! Standard CSR row pointer format (size = nnz + 1)
      call resize(self%tridx, ptr_tr)

      !$omp parallel private(tid, t_start, t_size, t_tr_start) &
      !$omp shared(self, thread_global_start, thread_global_tr_start, thread_img, thread_ptr_tr, &
      !$omp        thread_nlat, thread_nimg, thread_itr, thread_tridx)
      tid = omp_get_thread_num() + 1
      t_start = thread_global_start(tid)
      t_size = thread_img(tid)
      t_tr_start = thread_global_tr_start(tid)

      if (t_size > 0) then
         self%nlat(t_start + 1 : t_start + t_size) = thread_nlat(1 : t_size, tid)
         self%nimg(t_start + 1 : t_start + t_size) = thread_nimg(1 : t_size, tid)
         ! Offset local 1-based translation pointer by global thread translation start
         self%itr(t_start + 1 : t_start + t_size) = thread_itr(1 : t_size, tid) + t_tr_start
      end if

      if (thread_ptr_tr(tid) > 0) then
         self%tridx(t_tr_start + 1 : t_tr_start + thread_ptr_tr(tid)) = thread_tridx(1 : thread_ptr_tr(tid), tid)
      end if
      !$omp end parallel

      ! Final CSR boundary element
      self%itr(img + 1) = ptr_tr + 1

      ! 8. Cleanup Memory
      deallocate(head, nxt, cell_count)
      deallocate(thread_img, thread_ptr_tr)
      deallocate(thread_global_start, thread_global_tr_start)
      deallocate(thread_nimg_max, thread_checked)
      deallocate(thread_nlat, thread_nimg, thread_itr, thread_tridx)

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
   subroutine compute_grid(lattice, cutoff, lat_inv, det, n_xyz)
      !> Lattice vectors [A1 | A2 | A3]
      real(wp), intent(in) :: lattice(3, 3)
      !> Interaction cutoff radius
      real(wp), intent(in) :: cutoff
      !> Determinant (Volume) of the lattice
      real(wp), intent(out) :: det
      !> Inverse of the lattice matrix
      real(wp), intent(out) :: lat_inv(3, 3)
      !> Output: Number of grid subdivisions along each axis
      integer, intent(out) :: n_xyz(3)

      real(wp) :: L_vec, H(3), cross_ij(3)
      real(wp) :: dot_12, dot_23, dot_31
      integer  :: i

      ! Inverse Lattice Matrix
      det = lattice(1,1)*(lattice(2,2)*lattice(3,3) - lattice(2,3)*lattice(3,2)) - &
         lattice(1,2)*(lattice(2,1)*lattice(3,3) - lattice(2,3)*lattice(3,1)) + &
         lattice(1,3)*(lattice(2,1)*lattice(3,2) - lattice(2,2)*lattice(3,1))

      lat_inv(1,1) =  (lattice(2,2)*lattice(3,3) - lattice(2,3)*lattice(3,2)) / det
      lat_inv(1,2) = -(lattice(1,2)*lattice(3,3) - lattice(1,3)*lattice(3,2)) / det
      lat_inv(1,3) =  (lattice(1,2)*lattice(2,3) - lattice(1,3)*lattice(2,2)) / det
      lat_inv(2,1) = -(lattice(2,1)*lattice(3,3) - lattice(2,3)*lattice(3,1)) / det
      lat_inv(2,2) =  (lattice(1,1)*lattice(3,3) - lattice(1,3)*lattice(3,1)) / det
      lat_inv(2,3) = -(lattice(1,1)*lattice(2,3) - lattice(1,3)*lattice(2,1)) / det
      lat_inv(3,1) =  (lattice(2,1)*lattice(3,2) - lattice(2,2)*lattice(3,1)) / det
      lat_inv(3,2) = -(lattice(1,1)*lattice(3,2) - lattice(1,2)*lattice(3,1)) / det
      lat_inv(3,3) =  (lattice(1,1)*lattice(2,2) - lattice(1,2)*lattice(2,1)) / det

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
         n_xyz(i) = max(2, floor(H(i) / cutoff))
      end do

   end subroutine compute_grid

   subroutine get_median(cells, median_val)
      integer, intent(in) :: cells(:)
      integer, intent(out) :: median_val

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
         median_val = 1
         do i_cell = 1, max_cell_val
            cumulative_sum = cumulative_sum + hist(i_cell)
            if (cumulative_sum >= (nz_count + 1) / 2) then
               median_val = i_cell
               exit
            end if
         end do

         deallocate(hist)
      else
         median_val = 0
      end if

   end subroutine get_median

end module mctc_csrlist_type
