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

!> @file mctc/ncoord/adjlist/type.f90
!> Sparse neighbour map / adjacency list implementation.

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
!> inl   =  0,       3,          7,      10,         14,      17, 20
!> nnl   =  |  2 ->  |  3 ->     |  2 ->  |  3 ->     |  2 ->  |  |
!> nlat  =     2, 4, 5, 1, 3, 5, 6, 2, 4, 6, 1, 3, 5, 6, 1, 2, 4, 2, 3, 4
!> nltr  =     1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
!> nimg  =     1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
!> tridx =     1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
!> ```
!>
!> An alternative representation would be to store just the offsets in `inl` and
!> additional beyond the last element the total number of neighbors. However,
!> the indexing is from inl(i) to inl(i+1)-1 could be confusing, therefore
!> two arrays are used for clarity. `nimg` saves the closest images of cross-
!> interaction in the periodic system, `tridx` saves its translation index.

module mctc_ncoord_adjlist_type
   use iso_fortran_env, only : int64
   use mctc_env, only : wp, timer_type, format_time
   use mctc_io, only : structure_type
   use mctc_io_resize, only : resize
   use mctc_cutoff, only: get_lattice_points
   implicit none
   private

   public :: adjacency_list, new_adjacency_list

   !> @class adjacency_list
   !> Neighbourlist in CSR format
   type :: adjacency_list
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
      !> Pointer index of the Wiegner-Seitz translation vector for self-interaction
      integer, allocatable :: sitr(:)
      !> Pointer index of the Wiegner-Seitz translation vector for each neighbour
      integer, allocatable :: itr(:)
      !> Number of images per cross-interaction
      integer, allocatable :: nimg(:)
      !> Number of images per self-interaction
      integer, allocatable :: selfnimg(:)
      !> Primitive unit cell image index per cross-interaction
      integer, allocatable :: tridx(:)
      !> Primitive unit cell image index per self-interaction
      integer, allocatable :: selftridx(:)
   end type adjacency_list

   ! Default input
   real(wp), parameter :: cutoff_def = 29.0_wp
   real(wp), parameter :: trans_def(3, 1) = 0.0_wp
   logical, parameter :: complete_def = .false.
   integer, parameter :: init_size = 10
   real(wp), parameter :: buffer = 0.01_wp
   real(wp), parameter :: eps = tiny(1.0_wp)
   real(wp), parameter :: thr = sqrt(epsilon(0.0_wp))
   real(wp), parameter :: tol = 0.01_wp

contains

   !> Create new neighbourlist for a given geometry and cutoff
   subroutine new_adjacency_list(self, mol, cutoff, trans, complete)
      !> Instance of the neighbourlist
      type(adjacency_list), intent(out) :: self
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

      allocate(self%inl(mol%nat), source=0)
      allocate(self%nnl(mol%nat), source=0)

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

   end subroutine new_adjacency_list

!> Parallel Generator for neighbourlist using a Hybrid List approach (Fully Parallelized LAMMPS Style)
   subroutine generate_hybrid(self, mol)
      use omp_lib ! Required for OpenMP runtime functions

      !> Instance of the neighbourlist
      type(adjacency_list), intent(inout) :: self
      !> Molecular structure data
      type(structure_type), intent(in) :: mol

      integer :: iat, jat, itr, img, ic, jc
      integer :: ix, iy, iz, jx, jy, jz, di, dj, dk
      integer, allocatable :: head(:), nxt(:)
      integer :: n_xyz(3)
      real(wp) :: r2, vec(3), cutoff2, cell_w(3), min_xyz(3), max_xyz(3)
      real(wp) :: vol, dens
      integer :: prob

      ! OpenMP specific variables
      integer :: nthreads, tid, max_neigh_per_thread, t, t_start, t_size
      integer, allocatable :: thread_img(:)           ! Track current neighbor count per thread
      integer, allocatable :: thread_global_start(:)  ! Global offset for stitching phase
      integer, allocatable :: alloc_tid(:)            ! Track which thread processed which atom
      integer, allocatable :: thread_nlat(:,:)        ! Thread-local neighbor storage [max_neigh, nthreads]
      integer, allocatable :: thread_nltr(:,:)        ! Thread-local translation storage [max_neigh, nthreads]

      img = 0
      cutoff2 = self%cutoff**2

      ! 1. Define the grid boundaries and dimensions (Serial Baseline)
      min_xyz = minval(mol%xyz, dim=2) - buffer
      max_xyz = maxval(mol%xyz, dim=2) + buffer

      vol = (max_xyz(1)-min_xyz(1)) * (max_xyz(2)-min_xyz(2)) * (max_xyz(3)-min_xyz(3))
      dens = mol%nat / vol
      prob = max(init_size, int(dens * self%cutoff**3.0_wp * 500.0_wp))

      ! Number of cells: must be at least 1, and cell width >= cutoff
      n_xyz = max(1, floor((max_xyz - min_xyz) / (self%cutoff + eps)))
      cell_w = (max_xyz - min_xyz) / (real(n_xyz, wp) + eps) + eps

      ! 2. Build the Linked List IN PARALLEL using Atomic Capture
      allocate(head(product(n_xyz)), source=0)
      allocate(nxt(mol%nat), source=0)

      !$omp parallel do private(iat, ix, iy, iz, ic) &
      !$omp shared(mol, n_xyz, min_xyz, cell_w, head, nxt)
      do iat = 1, mol%nat
         ix = min(n_xyz(1), max(1, int((mol%xyz(1, iat) - min_xyz(1)) / cell_w(1)) + 1))
         iy = min(n_xyz(2), max(1, int((mol%xyz(2, iat) - min_xyz(2)) / cell_w(2)) + 1))
         iz = min(n_xyz(3), max(1, int((mol%xyz(3, iat) - min_xyz(3)) / cell_w(3)) + 1))

         ic = ix + n_xyz(1)*(iy-1) + n_xyz(1)*n_xyz(2)*(iz-1)

         ! CRITICAL: Atomic capture prevents race conditions on head(ic)
         ! while building the cell linked list simultaneously across threads
         !$omp atomic capture
         nxt(iat) = head(ic)
         head(ic) = iat
         !$omp end atomic
      end do
      !$omp end parallel do

      ! 3. Setup OpenMP Thread-Local Environments
      !$omp parallel
      !$omp master
      nthreads = omp_get_num_threads()
      !$omp end master
      !$omp end parallel

      ! Allocate thread tracking metrics
      allocate(thread_img(nthreads), source=0)
      allocate(thread_global_start(nthreads), source=0)
      allocate(alloc_tid(mol%nat), source=0)

      ! Estimate a safe thread-local allocation size with a safety buffer
      max_neigh_per_thread = (prob * mol%nat) / nthreads + 50000
      allocate(thread_nlat(max_neigh_per_thread, nthreads))
      if (any(mol%periodic)) allocate(thread_nltr(max_neigh_per_thread, nthreads))

      ! 4. Threaded Loop Search over nearby cells
      if (any(mol%periodic)) then
         !$omp parallel do schedule(guided) &
         !$omp private(iat, tid, ix, iy, iz, dk, dj, di, jx, jy, jz, jc, jat, itr, vec, r2) &
         !$omp shared(mol, self, head, nxt, thread_img, thread_nlat, thread_nltr, cell_w, min_xyz, n_xyz, cutoff2, alloc_tid, max_neigh_per_thread)
         do iat = 1, mol%nat
            tid = omp_get_thread_num() + 1
            alloc_tid(iat) = tid
            self%inl(iat) = thread_img(tid) ! Temporary store of local thread offset

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

                           ! Thread-safe: local pointer updates
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
            self%nnl(iat) = thread_img(tid) - self%inl(iat)
         end do
         !$omp end parallel do

      else ! Non-periodic branch
         !$omp parallel do schedule(guided) &
         !$omp private(iat, tid, ix, iy, iz, dk, dj, di, jx, jy, jz, jc, jat, vec, r2) &
         !$omp shared(mol, self, head, nxt, thread_img, thread_nlat, cell_w, min_xyz, n_xyz, cutoff2, alloc_tid, max_neigh_per_thread)
         do iat = 1, mol%nat
            tid = omp_get_thread_num() + 1
            alloc_tid(iat) = tid
            self%inl(iat) = thread_img(tid)

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
                           error stop "Thread local neighbor buffer overflow! Increase safety margin."
                        end if

                        thread_nlat(thread_img(tid), tid) = jat
                        jat = nxt(jat)
                     end do
                  end do; end do; end do
            self%nnl(iat) = thread_img(tid) - self%inl(iat)
         end do
         !$omp end parallel do
      end if

      ! 5. The Stitching Phase (Prefix Sum Calculation)
      thread_global_start(1) = 0
      do t = 2, nthreads
         thread_global_start(t) = thread_global_start(t-1) + thread_img(t-1)
      end do
      img = thread_global_start(nthreads) + thread_img(nthreads)

      ! Shift atom internal offsets (`self%inl`) to match the unified global array
      !$omp parallel do private(iat, tid) shared(self, alloc_tid, thread_global_start, mol)
      do iat = 1, mol%nat
         tid = alloc_tid(iat)
         self%inl(iat) = self%inl(iat) + thread_global_start(tid)
      end do
      !$omp end parallel do

      ! Reallocate global storage targets to the exact final size
      call resize(self%nlat, img)
      if (any(mol%periodic)) call resize(self%nltr, img)

      ! Parallel stream-copy data from thread buffers into global structures
      !$omp parallel private(tid, t_start, t_size) shared(self, thread_global_start, thread_img, thread_nlat, thread_nltr, mol)
      tid = omp_get_thread_num() + 1
      t_start = thread_global_start(tid)
      t_size = thread_img(tid)

      if (t_size > 0) then
         self%nlat(t_start + 1 : t_start + t_size) = thread_nlat(1 : t_size, tid)
         if (any(mol%periodic)) then
            self%nltr(t_start + 1 : t_start + t_size) = thread_nltr(1 : t_size, tid)
         end if
      end if
      !$omp end parallel

      ! 6. Cleanup Memory
      if (allocated(head)) deallocate(head)
      if (allocated(nxt)) deallocate(nxt)
      deallocate(thread_img, thread_global_start, alloc_tid, thread_nlat)
      if (allocated(thread_nltr)) deallocate(thread_nltr)

   end subroutine generate_hybrid

!> Generator of the Hybrid List for Periodic Systems based on Wiegner-Seitz Cell (O(N) scaling)
   subroutine generate_wsc(self, mol)
      use omp_lib ! Required for OpenMP runtime functions

      !> Instance of the neighbourlist
      type(adjacency_list), intent(inout) :: self
      !> Molecular structure data
      type(structure_type), intent(in) :: mol

      integer :: iat, jat, img, ic, jc, kat
      integer :: ix, iy, iz, jx, jy, jz, di, dj, dk
      integer, allocatable :: head(:), nxt(:)

      integer :: n_xyz(3), ntr, nimg_count
      integer :: tridx_arr(27)
      real(wp) :: cutoff2, r2_min, dens
      real(wp) :: lat_inv(3, 3), det
      real(wp) :: fract(3), xyz_wrap(3, mol%nat)
      real(wp), allocatable :: trans(:, :)
      real(wp) :: vec(3), zero_vec(3)

      integer :: selfimg, crossimg, prob
      integer :: ptr_tr, ptr_self_tr

      ! OpenMP specific variables
      integer :: nthreads, tid, t, t_start
      integer :: max_neigh_per_thread, max_tr_per_thread, max_self_tr_per_thread
      integer, allocatable :: thread_img(:), thread_ptr_tr(:), thread_ptr_self_tr(:)
      integer, allocatable :: global_img_start(:), global_tr_start(:), global_self_tr_start(:)
      integer, allocatable :: thread_nimg_max(:), alloc_tid(:)

      ! Thread Local Buffers
      integer, allocatable :: thread_nlat(:,:), thread_nimg(:,:), thread_itr(:,:)
      integer, allocatable :: thread_tridx(:,:), thread_selftridx(:,:)

      ! Replaces 'checked' boolean array with an integer stamp to prevent loop-resets
      integer, allocatable :: thread_seen(:,:)

      zero_vec = 0.0_wp
      ptr_tr = 0
      ptr_self_tr = 0

      ! 1. Lattice setup and jacket translations (Serial)
      call get_lattice_points(mol%periodic, mol%lattice, sqrt(epsilon(0.0_wp)), trans)
      ntr = size(trans, 2)
      if (allocated(self%trans)) deallocate(self%trans)
      allocate(self%trans, source=trans)

      cutoff2 = self%cutoff**2
      img = 0
      self%nimg_max = 0

      call compute_grid(mol%lattice, self%cutoff, lat_inv, det, n_xyz, selfimg, crossimg)
      dens = real(mol%nat, wp)/abs(det)
      prob = max(init_size, int(dens * self%cutoff**3.0_wp * 4.0_wp))

      ! 2. Thread Environment Setup
      !$omp parallel
      !$omp master
      nthreads = omp_get_num_threads()
      !$omp end master
      !$omp end parallel

      ! Buffer estimates (oversized to prevent resizing)
      max_neigh_per_thread = ((mol%nat * prob) / nthreads) + 50000
      max_tr_per_thread = max_neigh_per_thread * 4
      max_self_tr_per_thread = (mol%nat / nthreads) * 8 + 5000

      allocate(head(product(n_xyz)), source=0)
      allocate(nxt(mol%nat), source=0)

      allocate(thread_img(nthreads), source=0)
      allocate(thread_ptr_tr(nthreads), source=0)
      allocate(thread_ptr_self_tr(nthreads), source=0)
      allocate(thread_nimg_max(nthreads), source=0)
      allocate(global_img_start(nthreads), source=0)
      allocate(global_tr_start(nthreads), source=0)
      allocate(global_self_tr_start(nthreads), source=0)
      allocate(alloc_tid(mol%nat), source=0)

      allocate(thread_nlat(max_neigh_per_thread, nthreads))
      allocate(thread_nimg(max_neigh_per_thread, nthreads))
      allocate(thread_itr(max_neigh_per_thread, nthreads))
      allocate(thread_tridx(max_tr_per_thread, nthreads))
      allocate(thread_selftridx(max_self_tr_per_thread, nthreads))
      allocate(thread_seen(mol%nat, nthreads), source=0)

      if (.not. allocated(self%sitr)) allocate(self%sitr(mol%nat), source=0)
      if (.not. allocated(self%selfnimg)) allocate(self%selfnimg(mol%nat), source=0)

      ! 3. Build Linked Cell List (Parallelized with Atomic Capture)
      !$omp parallel do private(iat, fract, ix, iy, iz, ic) &
      !$omp shared(mol, lat_inv, xyz_wrap, n_xyz, head, nxt)
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
      end do
      !$omp end parallel do

      ! 4. Main Search Loop (Parallelized)
      !$omp parallel do schedule(guided) &
      !$omp private(iat, tid, fract, ix, iy, iz, dk, dj, di, jx, jy, jz, jc, jat, vec, &
      !$omp nimg_count, tridx_arr, r2_min) &
      !$omp shared(mol, self, head, nxt, lat_inv, trans, zero_vec, cutoff2, n_xyz, &
      !$omp thread_img, thread_ptr_tr, thread_ptr_self_tr, thread_nimg_max, alloc_tid, &
      !$omp thread_nlat, thread_nimg, thread_itr, thread_tridx, thread_selftridx, thread_seen)
      do iat = 1, mol%nat
         tid = omp_get_thread_num() + 1
         alloc_tid(iat) = tid
         self%inl(iat) = thread_img(tid)
         self%sitr(iat) = thread_ptr_self_tr(tid)

         ! Handle self-images
         call get_wsc_pairs(trans, zero_vec, nimg_count, tridx_arr, r2_min)
         if (nimg_count > 0 .and. r2_min <= cutoff2) then
            self%selfnimg(iat) = nimg_count
            thread_nimg_max(tid) = max(thread_nimg_max(tid), nimg_count)

            if (thread_ptr_self_tr(tid) + nimg_count > max_self_tr_per_thread) error stop "Self-TR Buffer Overflow!"

            thread_selftridx(thread_ptr_self_tr(tid)+1 : thread_ptr_self_tr(tid)+nimg_count, tid) = tridx_arr(1:nimg_count)
            thread_ptr_self_tr(tid) = thread_ptr_self_tr(tid) + nimg_count
         end if

         fract(:) = matmul(lat_inv, mol%xyz(:, iat))
         fract(:) = fract(:) - floor(fract(:))
         ix = min(n_xyz(1), max(1, int(fract(1) * n_xyz(1)) + 1))
         iy = min(n_xyz(2), max(1, int(fract(2) * n_xyz(2)) + 1))
         iz = min(n_xyz(3), max(1, int(fract(3) * n_xyz(3)) + 1))

         thread_seen(iat, tid) = iat ! Mark current atom as seen by this thread for this iat cycle

         do dk = -1, 1; do dj = -1, 1; do di = -1, 1
                  jx = modulo(ix + di - 1, n_xyz(1)) + 1
                  jy = modulo(iy + dj - 1, n_xyz(2)) + 1
                  jz = modulo(iz + dk - 1, n_xyz(3)) + 1
                  jc = jx + n_xyz(1)*(jy-1) + n_xyz(1)*n_xyz(2)*(jz-1)
                  jat = head(jc)

                  do while (jat > 0)
                     ! Integer stamping prevents double-loop reset
                     if (thread_seen(jat, tid) /= iat) then
                        thread_seen(jat, tid) = iat

                        if (jat <= iat .or. self%complete) then
                           vec(:) = mol%xyz(:, iat) - mol%xyz(:, jat)
                           call get_wsc_pairs(trans, vec, nimg_count, tridx_arr, r2_min)

                           if (nimg_count > 0 .and. r2_min <= cutoff2) then
                              thread_img(tid) = thread_img(tid) + 1
                              if (thread_img(tid) > max_neigh_per_thread) error stop "Neighbour Buffer Overflow!"
                              if (thread_ptr_tr(tid) + nimg_count > max_tr_per_thread) error stop "TR Buffer Overflow!"

                              thread_nlat(thread_img(tid), tid) = jat
                              thread_nimg(thread_img(tid), tid) = nimg_count
                              thread_itr(thread_img(tid), tid) = thread_ptr_tr(tid)
                              thread_nimg_max(tid) = max(thread_nimg_max(tid), nimg_count)

                              thread_tridx(thread_ptr_tr(tid)+1 : thread_ptr_tr(tid)+nimg_count, tid) = tridx_arr(1:nimg_count)
                              thread_ptr_tr(tid) = thread_ptr_tr(tid) + nimg_count
                           end if
                        end if
                     end if
                     jat = nxt(jat)
                  end do
               end do; end do; end do

         self%nnl(iat) = thread_img(tid) - self%inl(iat)
      end do
      !$omp end parallel do

      ! 5. Prefix Sum Calculation (Stitching Phase)
      global_img_start(1) = 0
      global_tr_start(1) = 0
      global_self_tr_start(1) = 0
      do t = 2, nthreads
         global_img_start(t) = global_img_start(t-1) + thread_img(t-1)
         global_tr_start(t) = global_tr_start(t-1) + thread_ptr_tr(t-1)
         global_self_tr_start(t) = global_self_tr_start(t-1) + thread_ptr_self_tr(t-1)
      end do

      img = global_img_start(nthreads) + thread_img(nthreads)
      ptr_tr = global_tr_start(nthreads) + thread_ptr_tr(nthreads)
      ptr_self_tr = global_self_tr_start(nthreads) + thread_ptr_self_tr(nthreads)
      self%nimg_max = maxval(thread_nimg_max)

      ! Shift global offsets for arrays that rely on internal pointer tracking
      !$omp parallel do private(iat, tid) shared(self, alloc_tid, global_img_start, global_self_tr_start, mol)
      do iat = 1, mol%nat
         tid = alloc_tid(iat)
         self%inl(iat) = self%inl(iat) + global_img_start(tid)
         self%sitr(iat) = self%sitr(iat) + global_self_tr_start(tid)
      end do
      !$omp end parallel do

      ! 6. Final Global Resize and Stream Copy
      call resize(self%nlat, img)
      call resize(self%nimg, img)
      call resize(self%itr, img)
      call resize(self%tridx, ptr_tr)
      call resize(self%selftridx, ptr_self_tr)

      !$omp parallel private(tid) shared(self, thread_nlat, thread_nimg, thread_itr, &
      !$omp thread_tridx, thread_selftridx, global_img_start, global_tr_start, global_self_tr_start)
      tid = omp_get_thread_num() + 1

      if (thread_img(tid) > 0) then
         self%nlat(global_img_start(tid)+1 : global_img_start(tid)+thread_img(tid)) = &
            thread_nlat(1:thread_img(tid), tid)
         self%nimg(global_img_start(tid)+1 : global_img_start(tid)+thread_img(tid)) = &
            thread_nimg(1:thread_img(tid), tid)
         ! Offset local translation pointers to match global space
         self%itr(global_img_start(tid)+1 : global_img_start(tid)+thread_img(tid)) = &
            thread_itr(1:thread_img(tid), tid) + global_tr_start(tid)
      end if

      if (thread_ptr_tr(tid) > 0) then
         self%tridx(global_tr_start(tid)+1 : global_tr_start(tid)+thread_ptr_tr(tid)) = &
            thread_tridx(1:thread_ptr_tr(tid), tid)
      end if

      if (thread_ptr_self_tr(tid) > 0) then
         self%selftridx(global_self_tr_start(tid)+1 : global_self_tr_start(tid)+thread_ptr_self_tr(tid)) = &
            thread_selftridx(1:thread_ptr_self_tr(tid), tid)
      end if
      !$omp end parallel

      ! 7. Cleanup Memory
      deallocate(head, nxt, thread_img, thread_ptr_tr, thread_ptr_self_tr, alloc_tid)
      deallocate(global_img_start, global_tr_start, global_self_tr_start, thread_nimg_max)
      deallocate(thread_nlat, thread_nimg, thread_itr, thread_tridx, thread_selftridx, thread_seen)

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
   subroutine compute_grid(lattice, cutoff, lat_inv, det, n_xyz, selfimg, crossimg)
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
      !> Output: Allocation capacity for self-images
      integer, intent(out) :: selfimg
      !> Output: Allocation capacity for cross-images
      integer, intent(out) :: crossimg

      real(wp) :: L_vec, H(3), cross_ij(3)
      real(wp) :: dot_12, dot_23, dot_31
      integer  :: i

      selfimg = 2
      crossimg = 2

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
         if (cutoff >= 0.05_wp * H(i)) then
            selfimg = 12
            crossimg = 6
         end if
         n_xyz(i) = max(2, floor(H(i) / cutoff))
      end do

   end subroutine compute_grid

end module mctc_ncoord_adjlist_type
