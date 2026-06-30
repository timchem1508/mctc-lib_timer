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

         !$omp atomic capture
         nxt(iat) = head(ic)
         head(ic) = iat
         !$omp end atomic
      end do
      !$omp end parallel do

      vol = product(max_xyz - min_xyz) * real(count(head /= 0), wp) / real(product(n_xyz), wp)
      dens = mol%nat / vol
      prob = max(init_size, ceiling(dens * self%cutoff**3.0_wp * 4.0_wp))
      write(*, *) " estimated neighbors: ", prob*mol%nat

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
      write(*, *) "size of neighborlist: ", size(self%nlat)
      write(*, *) "estimation accuracy ", real(size(self%nlat), wp) / real(prob*mol%nat, wp)
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
      !> Instance of the neighbourlist
      type(adjacency_list), intent(inout) :: self
      !> Molecular structure data
      type(structure_type), intent(in) :: mol

      integer :: iat, jat, img, ic, jc, i, kat
      integer :: ix, iy, iz, jx, jy, jz, di, dj, dk
      integer, allocatable :: head(:), nxt(:)
      logical, allocatable :: checked(:)

      integer :: n_xyz(3), ntr, nimg_count
      integer :: tridx_arr(27)
      real(wp) :: cutoff2, r2_min, dens
      real(wp) :: lat_inv(3, 3), det
      real(wp) :: fract(3), xyz_wrap(3, mol%nat)
      real(wp), allocatable :: trans(:, :)
      real(wp) :: vec(3), zero_vec(3)

      real(wp) :: vol
      integer :: current_capacity, new_size
      integer :: prob
      integer :: ptr_tr, ptr_self_tr

      zero_vec = 0.0_wp
      ptr_tr = 0
      ptr_self_tr = 0

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

      ! 3. Density estimation for initial buffer sizing
      allocate(head(product(n_xyz)), source=0)
      allocate(nxt(mol%nat), source=0)
      allocate(checked(mol%nat), source=.false.)

      ! 5. Build Linked Cell List
      do iat = 1, mol%nat
         fract(:) = matmul(lat_inv, mol%xyz(:, iat))
         fract(:) = fract(:) - floor(fract(:))
         xyz_wrap(:, iat) = matmul(mol%lattice, fract)

         ix = min(n_xyz(1), max(1, int(fract(1) * n_xyz(1)) + 1))
         iy = min(n_xyz(2), max(1, int(fract(2) * n_xyz(2)) + 1))
         iz = min(n_xyz(3), max(1, int(fract(3) * n_xyz(3)) + 1))

         ic = ix + n_xyz(1)*(iy-1) + n_xyz(1)*n_xyz(2)*(iz-1)
         nxt(iat) = head(ic)
         head(ic) = iat
      end do

      vol = abs(det) * real(count(head /= 0)) / real(product(n_xyz))
      dens = real(mol%nat, wp)/abs(det)
      prob = max(init_size, ceiling(dens * self%cutoff**3.0_wp * 4.0_wp))
      current_capacity = mol%nat * prob

      call resize(self%nlat, current_capacity)
      if (allocated(self%nimg)) deallocate(self%nimg)
      if (allocated(self%tridx)) deallocate(self%tridx)
      if (allocated(self%itr)) deallocate(self%itr)
      if (allocated(self%sitr)) deallocate(self%sitr)
      if (allocated(self%selfnimg)) deallocate(self%selfnimg)
      if (allocated(self%selftridx)) deallocate(self%selftridx)

      allocate(self%nimg(current_capacity), source=0)
      allocate(self%itr(current_capacity), source=0)
      allocate(self%tridx(4 * current_capacity), source=0)
      allocate(self%sitr(mol%nat), source=0)
      allocate(self%selfnimg(mol%nat), source=0)
      allocate(self%selftridx(4 * mol%nat), source=0)

      ! 6. Main Search Loop
      do iat = 1, mol%nat
         self%inl(iat) = img
         self%sitr(iat) = ptr_self_tr

         ! Handle self-images
         call get_wsc_pairs(trans, zero_vec, nimg_count, tridx_arr, r2_min)
         if (nimg_count > 0 .and. r2_min <= cutoff2) then
            self%selfnimg(iat) = nimg_count
            self%nimg_max = max(self%nimg_max, nimg_count)

            ! Check capacity of self translation indices
            if (ptr_self_tr + nimg_count > size(self%selftridx)) then
               call resize(self%selftridx, max(size(self%selftridx) * 2, ptr_self_tr + nimg_count))
            end if

            self%selftridx(self%sitr(iat) + 1 : self%sitr(iat) + self%selfnimg(iat)) = tridx_arr(1:nimg_count)
            ptr_self_tr = ptr_self_tr + nimg_count
         end if

         fract(:) = matmul(lat_inv, mol%xyz(:, iat))
         fract(:) = fract(:) - floor(fract(:))
         ix = min(n_xyz(1), max(1, int(fract(1) * n_xyz(1)) + 1))
         iy = min(n_xyz(2), max(1, int(fract(2) * n_xyz(2)) + 1))
         iz = min(n_xyz(3), max(1, int(fract(3) * n_xyz(3)) + 1))

         checked(iat) = .true.

         do dk = -1, 1; do dj = -1, 1; do di = -1, 1
                  jx = modulo(ix + di - 1, n_xyz(1)) + 1
                  jy = modulo(iy + dj - 1, n_xyz(2)) + 1
                  jz = modulo(iz + dk - 1, n_xyz(3)) + 1
                  jc = jx + n_xyz(1)*(jy-1) + n_xyz(1)*n_xyz(2)*(jz-1)
                  jat = head(jc)

                  do while (jat > 0)
                     if (.not. checked(jat)) then
                        checked(jat) = .true.

                        if (jat <= iat .or. self%complete) then
                           ! Compute distance vector using wrapped coordinates
                           vec(:) = mol%xyz(:, iat) - mol%xyz(:, jat)

                           call get_wsc_pairs(trans, vec, nimg_count, tridx_arr, r2_min)

                           if (nimg_count > 0 .and. r2_min <= cutoff2) then
                              img = img + 1

                              ! Check cross-interaction capacity
                              if (img > size(self%nlat)) then
                                 new_size = size(self%nlat) * 2
                                 call resize(self%nlat, new_size)
                                 call resize(self%nimg, new_size)
                                 call resize(self%itr, new_size)
                              end if

                              self%nlat(img) = jat
                              self%nimg(img) = nimg_count
                              self%itr(img) = ptr_tr
                              self%nimg_max = max(self%nimg_max, nimg_count)

                              ! Check capacity of cross translation indices
                              if (ptr_tr + nimg_count > size(self%tridx)) then
                                 call resize(self%tridx, max(size(self%tridx) * 2, ptr_tr + nimg_count))
                              end if

                              self%tridx(self%itr(img) + 1 : self%itr(img) + self%nimg(img)) = tridx_arr(1:nimg_count)
                              ptr_tr = ptr_tr + nimg_count
                           end if
                        end if
                     end if
                     jat = nxt(jat)
                  end do
               end do; end do; end do

         checked(iat) = .false.
         do dk = -1, 1; do dj = -1, 1; do di = -1, 1
                  jx = modulo(ix + di - 1, n_xyz(1)) + 1
                  jy = modulo(iy + dj - 1, n_xyz(2)) + 1
                  jz = modulo(iz + dk - 1, n_xyz(3)) + 1
                  jc = jx + n_xyz(1)*(jy-1) + n_xyz(1)*n_xyz(2)*(jz-1)
                  jat = head(jc)
                  do while (jat > 0)
                     checked(jat) = .false.
                     jat = nxt(jat)
                  end do
               end do; end do; end do

         self%nnl(iat) = img - self%inl(iat)
      end do

      ! Final Cleanup and Exact Sizing for all CSR Arrays
      call resize(self%nlat, img)
      call resize(self%nimg, img)
      call resize(self%itr, img)
      call resize(self%tridx, ptr_tr)
      call resize(self%selftridx, ptr_self_tr)

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

end module mctc_ncoord_adjlist_type
