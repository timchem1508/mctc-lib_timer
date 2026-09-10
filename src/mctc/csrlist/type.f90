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
!> @brief Compressed Sparse Row neighbour list implementation.
!>
!> Implementation of a sparse neighbour map in compressed sparse row (CSR) format.
!>
!> A symmetric neighbour map given in a dense format like:
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
!> is stored in two compressed arrays: `nlat` identifying the neighbouring atom
!> and `nltr` tracking its cell index. Two index arrays, `inl` for the offset
!> and `nnl` for the number of entries, map the atomic index to the row index.
!>
!> ```
!> inl   =     1,       4,          8,      11,         15,      18,     21
!> nlat  =     2, 4, 5, 1, 3, 5, 6, 2, 4, 6, 1, 3, 5, 6, 1, 2, 4, 2, 3, 4
!> nltr  =     1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
!> ```
!>
!> The first element of `nlat` (`nlat(inl(i))`) and `nltr` (`nltr(inl(i))`)
!> represents the diagonal entry of the atom itself, which is always present.
!> You can choose to store either the full matrix (complete mode) or only the
!> upper triangular part. Although the sparsity pattern is symmetric, this format
!> can also be used to store asymmetric matrices with a symmetric sparsity pattern.
!>
!> The slice `nlat(inl(i):inl(i+1)-1)` can be accessed directly to ensure
!> compatibility with standard CSR-support libraries (e.g., MKL, cuSPARSE, etc.).
!>
!> - `nimg`: Stores the closest images of cross-interactions in the periodic system.
!> - `tridx`: Stores the corresponding translation index.

module mctc_csrlist_type
   use mctc_cutoff, only : get_lattice_points
   use mctc_env, only : wp, timer_type, format_time
   use mctc_io, only : structure_type
   use mctc_io_resize, only : resize
   use mctc_io_math, only : matdet_3x3, matinv_3x3, crossprod
   use mctc_wignerseitz, only : wignerseitz_cell, get_pairs
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

   !> Universal thread-local dynamic storage buffer for neighbourlist construction
   type :: thread_buf_type
      !> Allocated capacity of pair-indexed arrays
      integer :: capacity = 0
      !> Allocated capacity of the translation-index array
      integer :: cap_tr = 0
      !> Neighbour atom indices
      integer, allocatable :: nlat(:)
      !> Neighbour translation indices
      integer, allocatable :: nltr(:)
      !> Number of Wigner-Seitz images per neighbour
      integer, allocatable :: nimg(:)
      !> Offset into the Wigner-Seitz image index array
      integer, allocatable :: itr(:)
      !> Wigner-Seitz image indices
      integer, allocatable :: tridx(:)
   end type thread_buf_type



   !> Default real-space cutoff
   real(wp), parameter :: cutoff_def = 29.0_wp

   !> Default non-periodic translation vector
   real(wp), parameter :: trans_def(3, 1) = 0.0_wp

   !> Default incomplete neighbour-list mode
   logical, parameter :: complete_def = .false.

   !> Initial number of neighbours allocated per atom
   integer, parameter :: init_size = 10

   !> Padding applied to non-periodic cell bounds
   real(wp), parameter :: buffer = 0.1_wp

   !> Smallest positive working-precision number
   real(wp), parameter :: eps = tiny(1.0_wp)

   !> Squared-distance threshold for Wigner-Seitz images
   real(wp), parameter :: thr = sqrt(epsilon(0.0_wp))

   !> Tolerance for equivalent Wigner-Seitz image distances
   real(wp), parameter :: tol = 0.01_wp


contains


!> Create a neighbour list for a geometry and cutoff
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
         ! Generate the hybrid neighbour list using the Wigner-Seitz cell
         call generate_hybrid(self, mol, wsc)
         call move_alloc(wsc, self%wsc)

      else if (present(trans)) then
         ! Generate the hybrid neighbour list using the provided lattice translations
         self%trans = trans
         call generate_hybrid(self, mol)
      end if
   else
      ! Generate the hybrid neighbour list for a non-periodic system
      if (present(trans)) then
         self%trans = trans
      else
         allocate(self%trans, source=trans_def)
      end if
      call generate_hybrid(self, mol)
   end if

end subroutine new_csr_list

!> Dynamic expansion routine for pair-indexed arrays
subroutine grow_buffer(buf)

   !> Thread-local buffer to expand
   type(thread_buf_type), intent(inout) :: buf

   integer :: new_capacity

   if (buf%capacity <= 0) then
      new_capacity = 1024
   else
      new_capacity = buf%capacity * 2
   end if

   if (allocated(buf%nlat)) call resize(buf%nlat, new_capacity)
   if (allocated(buf%nltr)) call resize(buf%nltr, new_capacity)
   if (allocated(buf%nimg)) call resize(buf%nimg, new_capacity)
   if (allocated(buf%itr))  call resize(buf%itr,  new_capacity)

   buf%capacity = new_capacity
end subroutine grow_buffer

!> Dynamic expansion routine for translation index array
subroutine grow_buffer_tr(buf, min_needed)

   !> Thread-local buffer to expand
   type(thread_buf_type), intent(inout) :: buf

   !> Minimum number of additional translation indices required
   integer, intent(in) :: min_needed

   integer :: new_capacity

   new_capacity = max(buf%cap_tr + min_needed, buf%cap_tr * 2)
   if (allocated(buf%tridx)) call resize(buf%tridx, new_capacity)
   buf%cap_tr = new_capacity
end subroutine grow_buffer_tr

!> Generate a CSR-based hybrid neighbour list
subroutine generate_hybrid(self, mol, wsc)
   !$ use omp_lib

   !> Instance of the neighbourlist
   type(csr_list), intent(inout) :: self
   !> Molecular structure data
   type(structure_type), intent(in) :: mol
   !> Wigner-Seitz cell type, enables the Wigner-Seitz image search
   type(wignerseitz_cell), intent(inout), optional :: wsc

   !> Periodic geometry and Wigner-Seitz mode switches
   logical :: periodic, use_wsc

   integer :: iat, jat, itr, img, jc, t
   integer :: ix, iy, iz, jx, jy, jz, di, dj, dk, d

   ! Flattened linked-cell heads, indexed as x + nx * (y - 1) + nx * ny * (z - 1)
   integer, allocatable :: head(:), nxt(:)

   ! Number of linked cells along the x, y, and z directions
   integer :: n_xyz(3), ntr
   real(wp) :: r2, vec(3), cutoff2, cell_w(3), min_xyz(3)
   real(wp) :: vol, dens, det, lat_inv(3, 3), fract(3)
   integer :: prob
   integer, allocatable :: ccount(:)

   ! Dynamic stencil search bounds per dimension
   integer :: kmin(3), kmax(3)
   integer :: di_min, di_max, dj_min, dj_max, dk_min, dk_max

   ! Statistical estimation variables
   integer :: nz_count, median

   ! Wigner-Seitz search variables
   real(wp), allocatable :: trans(:, :)
   real(wp) :: zero_vec(3), r2_min
   integer :: trptr, total_self_nimg, nimg_count
   integer :: tridx_arr(27)

   ! OpenMP specific variables
   integer :: thr_mem, thr_maxtr
   integer :: nthr, tid, start_count, thr_size
   integer, allocatable :: thr_img(:), thr_trptr(:), thr_nimg_max(:)
   integer, allocatable :: thr_start(:), thr_trstart(:)
   type(thread_buf_type), allocatable :: thr_buf(:)

   img = 0
   trptr = 0
   zero_vec = 0.0_wp
   cutoff2 = self%cutoff**2

   periodic = any(mol%periodic)
   use_wsc = .false.
   if (periodic) use_wsc = present(wsc)

   ! 1. Generate linked-cell grid. In Wigner-Seitz mode the jacket translations
   ! are generated here and shared with the Wigner-Seitz cell.
   if (use_wsc) then
      call get_lattice_points(mol%periodic, mol%lattice, sqrt(epsilon(0.0_wp)), trans)
      if (allocated(self%trans)) deallocate(self%trans)
      allocate(self%trans, source=trans)
      if (allocated(wsc%trans)) deallocate(wsc%trans)
      allocate(wsc%trans, source=trans)
      deallocate(trans)
      wsc%nimg_max = 0
   end if
   ntr = size(self%trans, 2)

   if (periodic) then
      call compute_grid(mol, self%cutoff, det, n_xyz, lat_inv=lat_inv)
   else
      call compute_grid(mol, self%cutoff, det, n_xyz, cell_w=cell_w)
   end if

   ! Dynamic stencil bounds prevent examination of non-existent
   ! neighboring cells when the grid dimensions are less than three.
   do d = 1, 3
      select case (n_xyz(d))
       case (1)
         ! Only one grid cell in this dimension
         kmin(d) = 0
         kmax(d) = 0
       case (2)
         ! Only two grid cells in this dimension
         kmin(d) = 0
         kmax(d) = 1
       case default
         ! Three or more grid cells in this dimension
         kmin(d) = -1
         kmax(d) = 1
      end select
   end do

   ! 2. Build Linked Cell List
   allocate(head(product(n_xyz)), source=0)
   allocate(ccount(product(n_xyz)), source=0)
   allocate(nxt(mol%nat), source=0)

   if (periodic) then
      call get_linked_cell(mol, n_xyz, head, nxt, ccount, lat_inv=lat_inv)
   else
      call get_linked_cell(mol, n_xyz, head, nxt, ccount, cell_w=cell_w)
   end if

   ! 3. Estimate buffer capacity from occupied-cell density. The median
   ! population excludes empty cells so sparse regions do not reduce the estimate.
   nz_count = count(ccount > 0)
   call get_median(ccount, median)

   vol = abs(det) * real(nz_count, wp) / real(product(n_xyz), wp)
   dens = real(median, wp) * real(product(n_xyz), wp) / vol
   prob = ceiling(4.0_wp * dens * self%cutoff**3.0_wp)
   if (.not. self%complete) prob = (prob + 1) / 2

   ! 4. OpenMP Setup & Allocation
   nthr = 1
   !$ call omp_set_dynamic(.false.)
   !$ nthr = omp_get_max_threads()

   thr_mem = max(int(init_size * mol%nat) / nthr, &
      & int((prob * mol%nat)) / nthr)
   thr_mem = max(100, thr_mem)
   thr_maxtr = thr_mem * 6

   ! Allocate thread metrics
   allocate(thr_img(nthr), source=0)
   allocate(thr_trptr(nthr), source=0)
   allocate(thr_nimg_max(nthr), source=0)

   ! Allocate thread-local buffers, only the arrays the active mode requires
   allocate(thr_buf(nthr))
   do tid = 1, nthr
      thr_buf(tid)%capacity = thr_mem
      allocate(thr_buf(tid)%nlat(thr_buf(tid)%capacity))
      if (use_wsc) then
         thr_buf(tid)%cap_tr = thr_maxtr
         allocate(thr_buf(tid)%nimg(thr_buf(tid)%capacity))
         allocate(thr_buf(tid)%itr(thr_buf(tid)%capacity))
         allocate(thr_buf(tid)%tridx(thr_buf(tid)%cap_tr))
      else if (periodic) then
         allocate(thr_buf(tid)%nltr(thr_buf(tid)%capacity))
      end if
   end do

   ! Pre-allocate CSR row pointer array (size = nat + 1)
   if (allocated(self%inl)) deallocate(self%inl)
   allocate(self%inl(mol%nat + 1), source=0)

   if (.not. periodic) min_xyz = minval(mol%xyz, dim=2) - buffer

   ! 5. Threaded Loop Search
   !$omp parallel do schedule(static) num_threads(nthr) &
   !$omp& private(iat, tid, start_count, fract, ix, iy, iz, dk, dj, di) &
   !$omp& private(jx, jy, jz, jc, jat, itr, vec, r2) &
   !$omp& private(di_min, di_max, dj_min, dj_max, dk_min, dk_max) &
   !$omp& private(nimg_count, tridx_arr, r2_min, total_self_nimg) &
   !$omp& shared(mol, self, head, nxt, thr_img, thr_trptr, thr_buf) &
   !$omp& shared(thr_nimg_max, cell_w, lat_inv, min_xyz, zero_vec) &
   !$omp& shared(n_xyz, kmin, kmax, cutoff2, ntr, periodic, use_wsc)
   do iat = 1, mol%nat
      tid = 1
      !$ tid = omp_get_thread_num() + 1
      start_count = thr_img(tid)

      ! A. Inject Diagonal (self-interaction) at position 1. In Wigner-Seitz
      ! mode the periodic self-images are resolved and recorded alongside it.
      if (use_wsc) then
         call get_pairs(self%trans, zero_vec, nimg_count, tridx_arr, r2_min)
         if (nimg_count > 0 .and. r2_min <= cutoff2) then
            total_self_nimg = nimg_count
         else
            total_self_nimg = 0
         end if
      end if

      thr_img(tid) = thr_img(tid) + 1
      if (thr_img(tid) > thr_buf(tid)%capacity) then
         call grow_buffer(thr_buf(tid))
      end if
      thr_buf(tid)%nlat(thr_img(tid)) = iat

      if (use_wsc) then
         if (thr_trptr(tid) + total_self_nimg > thr_buf(tid)%cap_tr) then
            call grow_buffer_tr(thr_buf(tid), total_self_nimg)
         end if

         thr_buf(tid)%nimg(thr_img(tid)) = total_self_nimg
         thr_buf(tid)%itr(thr_img(tid))  = thr_trptr(tid) + 1
         thr_nimg_max(tid) = max(thr_nimg_max(tid), total_self_nimg)

         ! Append remaining periodic self-images if present
         if (total_self_nimg > 0) then
            thr_buf(tid)%tridx(thr_trptr(tid) + 1 : &
               & thr_trptr(tid) + total_self_nimg) = tridx_arr(1:total_self_nimg)
         end if

         thr_trptr(tid) = thr_trptr(tid) + total_self_nimg
      else if (periodic) then
         thr_buf(tid)%nltr(thr_img(tid)) = 1
      end if

      ! B. Locate the grid cell of the current atom. Periodic systems use the
      ! fractional coordinates within the lattice, molecular ones the grid
      ! bounding box.
      if (periodic) then
         fract(:) = matmul(lat_inv, mol%xyz(:, iat))
         fract(:) = fract(:) - floor(fract(:))
         ix = min(n_xyz(1), max(1, int(fract(1) * n_xyz(1)) + 1))
         iy = min(n_xyz(2), max(1, int(fract(2) * n_xyz(2)) + 1))
         iz = min(n_xyz(3), max(1, int(fract(3) * n_xyz(3)) + 1))

         ! Dynamically clamped stencil loops prevent duplicate cell evaluations
         di_min = kmin(1); di_max = kmax(1)
         dj_min = kmin(2); dj_max = kmax(2)
         dk_min = kmin(3); dk_max = kmax(3)
      else
         ix = min(n_xyz(1), &
            & max(1, int((mol%xyz(1, iat) - min_xyz(1))/cell_w(1)) + 1))
         iy = min(n_xyz(2), &
            & max(1, int((mol%xyz(2, iat) - min_xyz(2))/cell_w(2)) + 1))
         iz = min(n_xyz(3), &
            & max(1, int((mol%xyz(3, iat) - min_xyz(3))/cell_w(3)) + 1))

         ! Dynamic molecular stencil bounds use to avoid exceeding
         ! the physical grid limits during the neighbor search.
         di_min = max(kmin(1), 1 - ix)
         di_max = min(kmax(1), n_xyz(1) - ix)
         dj_min = max(kmin(2), 1 - iy)
         dj_max = min(kmax(2), n_xyz(2) - iy)
         dk_min = max(kmin(3), 1 - iz)
         dk_max = min(kmax(3), n_xyz(3) - iz)
      end if

      ! C. Traverse the stencil cells, wrapping around the grid if periodic
      do dk = dk_min, dk_max
         if (periodic) then
            jz = modulo(iz + dk - 1, n_xyz(3)) + 1
         else
            jz = iz + dk
         end if

         do dj = dj_min, dj_max
            if (periodic) then
               jy = modulo(iy + dj - 1, n_xyz(2)) + 1
            else
               jy = iy + dj
            end if

            do di = di_min, di_max
               if (periodic) then
                  jx = modulo(ix + di - 1, n_xyz(1)) + 1
               else
                  jx = ix + di
               end if

               jc = jx + n_xyz(1)*(jy-1) + n_xyz(1)*n_xyz(2)*(jz-1)
               jat = head(jc)

               do while (jat > 0)
                  if (use_wsc) then
                     ! Upper Triangular (jat > iat) vs Complete Mode (all jat /= iat)
                     if (self%complete .or. jat > iat) then
                        vec(:) = mol%xyz(:, iat) - mol%xyz(:, jat)
                        call get_pairs(self%trans, vec, nimg_count, tridx_arr, r2_min)

                        if (nimg_count > 0 .and. r2_min <= cutoff2) then
                           thr_img(tid) = thr_img(tid) + 1

                           ! Dynamic checks and buffer expansion
                           if (thr_img(tid) > thr_buf(tid)%capacity) then
                              call grow_buffer(thr_buf(tid))
                           end if
                           if (thr_trptr(tid) + nimg_count > thr_buf(tid)%cap_tr) then
                              call grow_buffer_tr(thr_buf(tid), nimg_count)
                           end if

                           thr_buf(tid)%nlat(thr_img(tid)) = jat
                           thr_buf(tid)%nimg(thr_img(tid)) = nimg_count
                           thr_buf(tid)%itr(thr_img(tid))  = thr_trptr(tid) + 1
                           thr_nimg_max(tid) = max(thr_nimg_max(tid), nimg_count)

                           thr_buf(tid)%tridx(thr_trptr(tid) + 1 : &
                              & thr_trptr(tid) + nimg_count) = &
                              tridx_arr(1:nimg_count)
                           thr_trptr(tid) = thr_trptr(tid) + nimg_count
                        end if
                     end if

                  else if (periodic) then
                     ! Upper triangle condition: skip lower triangle if incomplete.
                     ! Equality is allowed because the periodic images of an atom
                     ! with itself are separate entries of its own row.
                     if (self%complete .or. jat >= iat) then
                        do itr = 1, ntr
                           ! Skip diagonal element identity shift
                           if (iat == jat .and. itr == 1) cycle

                           vec(:) = mol%xyz(:, iat) - mol%xyz(:, jat) &
                              & - self%trans(:, itr)
                           r2 = sum(vec**2)

                           if (r2 < epsilon(cutoff2) .or. r2 > cutoff2) cycle

                           thr_img(tid) = thr_img(tid) + 1
                           if (thr_img(tid) > thr_buf(tid)%capacity) then
                              call grow_buffer(thr_buf(tid))
                           end if

                           thr_buf(tid)%nlat(thr_img(tid)) = jat
                           thr_buf(tid)%nltr(thr_img(tid)) = itr
                        end do
                     end if

                  else
                     if (self%complete .or. jat > iat) then
                        vec(:) = mol%xyz(:, iat) - mol%xyz(:, jat)
                        r2 = sum(vec**2)

                        if (r2 >= epsilon(cutoff2) .and. r2 <= cutoff2) then
                           thr_img(tid) = thr_img(tid) + 1
                           if (thr_img(tid) > thr_buf(tid)%capacity) then
                              call grow_buffer(thr_buf(tid))
                           end if

                           thr_buf(tid)%nlat(thr_img(tid)) = jat
                        end if
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
   if (use_wsc) wsc%nimg_max = maxval(thr_nimg_max)

   ! CSR Pointer array construction
   self%inl(1) = 1
   do iat = 1, mol%nat
      self%inl(iat + 1) = self%inl(iat) + self%inl(iat + 1)
   end do

   ! 7. CSR List Resizing & Stream Copying
   call resize(self%nlat, img)
   if (use_wsc) then
      call resize(wsc%nimg_list, img)
      call resize(wsc%itr_list, img + 1)
      call resize(wsc%tridx_list, trptr)
   else if (periodic) then
      call resize(self%nltr, img)
   end if

   !$omp parallel do schedule(static) num_threads(nthr) &
   !$omp& private(tid, thr_size) &
   !$omp& shared(self, thr_start, thr_trstart, thr_img, thr_trptr, thr_buf) &
   !$omp& shared(periodic, use_wsc)
   do tid = 1, nthr
      thr_size = thr_img(tid)

      if (thr_size > 0) then
         self%nlat(thr_start(tid) + 1 : thr_start(tid) + thr_size) = &
            & thr_buf(tid)%nlat(1 : thr_size)

         if (use_wsc) then
            wsc%nimg_list(thr_start(tid) + 1 : thr_start(tid) + thr_size) = &
               & thr_buf(tid)%nimg(1 : thr_size)
            wsc%itr_list(thr_start(tid) + 1 : thr_start(tid) + thr_size) &
               & = thr_buf(tid)%itr(1 : thr_size) + thr_trstart(tid)
         else if (periodic) then
            self%nltr(thr_start(tid) + 1 : thr_start(tid) + thr_size) = &
               & thr_buf(tid)%nltr(1 : thr_size)
         end if
      end if

      if (use_wsc .and. thr_trptr(tid) > 0) then
         wsc%tridx_list(thr_trstart(tid) + 1 : thr_trstart(tid) + thr_trptr(tid)) = &
            thr_buf(tid)%tridx(1 : thr_trptr(tid))
      end if
   end do
   !$omp end parallel do

   if (use_wsc) wsc%itr_list(img + 1) = trptr + 1

   deallocate(head, nxt, ccount)
   deallocate(thr_img, thr_trptr, thr_nimg_max)
   deallocate(thr_start, thr_trstart)
   deallocate(thr_buf)

end subroutine generate_hybrid

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

   real(wp) :: H(3), cross_ij(3), lattice(3, 3)
   real(wp) :: max_xyz(3), min_xyz(3)
   integer  :: i

   if (any(mol%periodic)) then
      ! Inverse Lattice Matrix
      lattice = mol%lattice
      det = matdet_3x3(lattice)

      if (present(lat_inv)) then
         lat_inv = matinv_3x3(lattice)
      end if

      ! Perpendicular height H1 (normal to a2 x a3)
      cross_ij(:) = crossprod(lattice(:,2), lattice(:,3))
      H(1) = abs(det) / sqrt(sum(cross_ij**2))

      ! Perpendicular height H2 (normal to a3 x a1)
      cross_ij(:) = crossprod(lattice(:,3), lattice(:,1))
      H(2) = abs(det) / sqrt(sum(cross_ij**2))

      ! Perpendicular height H3 (normal to a1 x a2)
      cross_ij(:) = crossprod(lattice(:,1), lattice(:,2))
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

   !> Number of linked cells along the x, y, and z directions
   integer, intent(in) :: n_xyz(3)

   !> Flattened cell-to-atom chain heads using x + nx * (y - 1) + nx * ny * (z - 1)
   integer, intent(out) :: head(:)

   !> Next atom in each linked-cell chain, indexed by atom
   integer, intent(out) :: nxt(:)

   !> Population of each flattened linked cell
   integer, intent(out) :: ccount(:)
   !> Inverse of the lattice matrix
   real(wp), intent(in), optional :: lat_inv(3, 3)
   !> Width of each grid cell
   real(wp), intent(in), optional :: cell_w(3)

   integer :: iat, ix, iy, iz, ic
   real(wp) :: fract(3), min_xyz(3), max_xyz(3)

   if (any(mol%periodic) .and. present(lat_inv)) then
      !$omp parallel do private(iat, fract, ix, iy, iz, ic) &
      !$omp& shared(mol, lat_inv, n_xyz, head, nxt, ccount)
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
      !$omp& shared(mol, n_xyz, min_xyz, cell_w, head, nxt, ccount)
      do iat = 1, mol%nat
         ix = min(n_xyz(1), &
            & max(1, int((mol%xyz(1, iat) - min_xyz(1))/cell_w(1)) + 1))
         iy = min(n_xyz(2), &
            & max(1, int((mol%xyz(2, iat) - min_xyz(2))/cell_w(2)) + 1))
         iz = min(n_xyz(3), &
            & max(1, int((mol%xyz(3, iat) - min_xyz(3))/cell_w(3)) + 1))

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

!> Determine the median population among non-empty linked cells
subroutine get_median(cells, median)

   !> Population of each linked cell
   integer, intent(in) :: cells(:)

   !> Median population among non-empty linked cells
   integer, intent(out) :: median

   integer :: nz_count, max_cell_val, cumulative_sum, i_cell
   integer, allocatable :: hist(:)

   nz_count = count(cells > 0)

   if (nz_count > 0) then
      max_cell_val = maxval(cells)
      allocate(hist(1:max_cell_val), source=0)

      ! Build a histogram only for occupied cells; empty cells are excluded from
      ! the median because it estimates the population of cells that are searched.
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
