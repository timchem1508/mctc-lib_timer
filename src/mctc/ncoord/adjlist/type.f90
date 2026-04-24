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
!> ```
!>
!> An alternative representation would be to store just the offsets in `inl` and
!> additional beyond the last element the total number of neighbors. However,
!> the indexing is from inl(i) to inl(i+1)-1 could be confusing, therefore
!> two arrays are used for clarity.

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
      !> Whether a complete or a symmetrical reduced map should be generated
      logical, allocatable :: complete
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
      integer :: nimg_max
      integer, allocatable :: nimg(:)
      integer, allocatable :: selfnimg(:)
      integer, allocatable :: selftridx(:, :)
      integer, allocatable :: tridx(:, :)
   end type adjacency_list


   ! Default input
   real(wp), parameter :: cutoff_0d_def = 29.0_wp
   logical, parameter :: complete_def = .false.
   integer, parameter :: init_size = 10
   real(wp), parameter :: buffer = 0.01_wp
   real(wp), parameter :: grid_def = 0.001_wp

   real(wp), parameter :: eps = tiny(1.0_wp)

   !> Small cutoff threshold to create only closest cells
   real(wp), parameter :: thr = sqrt(epsilon(0.0_wp))

   !> Tolerance to consider equivalent images
   real(wp), parameter :: tol = 0.01_wp

contains

   !> Create new neighbourlist for a given geometry and cutoff
   subroutine new_adjacency_list(self, mol, cutoff, complete)
      type(adjacency_list), intent(out) :: self
      type(structure_type), intent(in) :: mol
      real(wp), intent(in), optional :: cutoff
      logical, intent(in), optional :: complete

      allocate(self%cutoff)
      if (present(cutoff)) then
         self%cutoff = cutoff
      else
         self%cutoff = cutoff_0d_def
      end if

      allocate(self%complete)
      if (present(complete)) then
         self%complete = complete
      else
         self%complete = complete_def
      end if

      allocate(self%inl(mol%nat), source=0)
      allocate(self%nnl(mol%nat), source=0)

      if (any(mol%periodic)) then
         !call generate_pbc(self, mol)
         call generate_3d(self, mol)
      else
         call generate_0d(self, mol)
      end if
   end subroutine new_adjacency_list

   !> Generator for neighbourlist using a Linked Cell List approach (O(N) scaling)
   subroutine generate_0d(self, mol)
      !> Instance of the neighbourlist
      type(adjacency_list), intent(inout) :: self
      !> Molecular structure data
      type(structure_type), intent(in) :: mol


      integer :: iat, jat, itr, img, ic, jc
      integer :: ix, iy, iz, jx, jy, jz, di, dj, dk
      integer, allocatable :: head(:), nxt(:)
      integer :: n_xyz(3)
      real(wp) :: r2, vec(3), cutoff2, cell_w(3), min_xyz(3), max_xyz(3)
      real(wp) :: trans(3, 1) = 0.0_wp

      self%trans = trans

      img = 0
      cutoff2 = self%cutoff**2

      ! 1. Define the grid boundaries and dimensions
      ! We add a small buffer to the bounding box to ensure all atoms are contained
      min_xyz = minval(mol%xyz, dim=2) - buffer
      max_xyz = maxval(mol%xyz, dim=2) + buffer

      ! Number of cells: must be at least 1, and cell width >= cutoff
      n_xyz = max(1, floor((max_xyz - min_xyz) / (self%cutoff + eps)))
      cell_w = (max_xyz - min_xyz) / (real(n_xyz, wp) + eps) + eps

      ! 2. Build the Linked List
      allocate(head(product(n_xyz)), source=0)
      allocate(nxt(mol%nat), source=0)

      do iat = 1, mol%nat
         ix = min(n_xyz(1), max(1, int((mol%xyz(1, iat) - min_xyz(1)) / cell_w(1)) + 1))
         iy = min(n_xyz(2), max(1, int((mol%xyz(2, iat) - min_xyz(2)) / cell_w(2)) + 1))
         iz = min(n_xyz(3), max(1, int((mol%xyz(3, iat) - min_xyz(3)) / cell_w(3)) + 1))

         ic = ix + n_xyz(1)*(iy-1) + n_xyz(1)*n_xyz(2)*(iz-1)
         nxt(iat) = head(ic)
         head(ic) = iat
      end do

      ! Pre-allocate neighbor arrays
      call resize(self%nlat, init_size*mol%nat)

      ! 3. Triple loop search over nearby cells (O(N) time)
      do iat = 1, mol%nat
         self%inl(iat) = img

         ix = min(n_xyz(1), max(1, int((mol%xyz(1, iat) - min_xyz(1)) / cell_w(1)) + 1))
         iy = min(n_xyz(2), max(1, int((mol%xyz(2, iat) - min_xyz(2)) / cell_w(2)) + 1))
         iz = min(n_xyz(3), max(1, int((mol%xyz(3, iat) - min_xyz(3)) / cell_w(3)) + 1))

         ! Check 27 neighboring cells (3x3x3 block)
         do dk = -1, 1; do dj = -1, 1; do di = -1, 1
                  jx = ix + di; jy = iy + dj; jz = iz + dk

                  ! Skip cells outside the defined grid
                  if (jx < 1 .or. jx > n_xyz(1) .or. &
                     jy < 1 .or. jy > n_xyz(2) .or. &
                     jz < 1 .or. jz > n_xyz(3)) cycle

                  jc = jx + n_xyz(1)*(jy-1) + n_xyz(1)*n_xyz(2)*(jz-1)
                  jat = head(jc)

                  do while (jat > 0)

                     ! Symmetrical optimization: skip if jat > iat and complete is false
                     if (.not. self%complete .and. jat > iat) then
                        jat = nxt(jat)
                        cycle
                     end if

                     ! Check all translation images for this atom pair
                     do itr = 1, size(trans, 2)
                        vec(:) = mol%xyz(:, iat) - mol%xyz(:, jat) - trans(:, itr)
                        r2 = sum(vec**2)

                        ! Standard distance check and self-interaction exclusion
                        if (r2 < epsilon(cutoff2) .or. r2 > cutoff2) cycle

                        img = img + 1
                        if (size(self%nlat) < img) call resize(self%nlat)
                        self%nlat(img) = jat
                     end do
                     jat = nxt(jat)
                  end do
               end do; end do; end do
         self%nnl(iat) = img - self%inl(iat)
      end do

      ! Cleanup and final sizing
      if (allocated(head)) deallocate(head)
      if (allocated(nxt)) deallocate(nxt)
      call resize(self%nlat, img)

   end subroutine generate_0d

   subroutine get_wsc_pairs(trans, rij, iws, list, min_r2, is_self)
      real(wp), intent(in) :: trans(:, :)
      real(wp), intent(in) :: rij(3)
      integer, intent(out) :: iws
      integer, intent(out) :: list(:)
      real(wp), intent(out) :: min_r2
      logical, intent(in) :: is_self

      real(wp) :: dx, dy, dz, r2
      integer :: itr, ntr

      ntr = size(trans, 2)
      iws = 0
      min_r2 = huge(1.0_wp)

      do itr = 1, ntr
         dx = rij(1) - trans(1, itr)
         dy = rij(2) - trans(2, itr)
         dz = rij(3) - trans(3, itr)
         r2 = dx*dx + dy*dy + dz*dz

         ! Skip self-interaction (center image) if requested
         if (is_self .and. r2 < thr) cycle

         if (r2 < min_r2 - tol) then
            ! Found a strictly better minimum
            min_r2 = r2
            iws = 1
            list(1) = itr
         else if (r2 < min_r2 + tol) then
            ! Within tolerance: record degeneracy
            iws = iws + 1
            list(iws) = itr
         end if
      end do
   end subroutine get_wsc_pairs

!> Generator for neighbourlist using a Linked Cell List approach for Periodic Systems
subroutine generate_3d(self, mol)
      type(adjacency_list), intent(inout) :: self
      type(structure_type), intent(in) :: mol

      integer :: iat, jat, img, ic, jc, i
      integer :: ix, iy, iz, jx, jy, jz, di, dj, dk
      integer, allocatable :: head(:), nxt(:)
      logical, allocatable :: checked(:)

      integer :: n_xyz(3), ntr, nimg_count
      integer :: tridx_arr(27)
      real(wp) :: cutoff2, r2_min
      real(wp) :: lat_inv(3, 3), det, L_vec
      real(wp) :: fract(3), xyz_wrap(3, mol%nat)
      real(wp), allocatable :: trans(:, :)
      real(wp) :: vec(3), zero_vec(3)
      integer, allocatable :: tmp_nimg(:), tmp_tridx(:,:)
      integer :: current_capacity

      zero_vec = 0.0_wp

      ! 1. Lattice setup and jacket translations
      call get_lattice_points(mol%periodic, mol%lattice, sqrt(epsilon(0.0_wp)), trans)
      ntr = size(trans, 2)
      if (allocated(self%trans)) deallocate(self%trans)
      allocate(self%trans, source=trans)

      cutoff2 = self%cutoff**2
      img = 0
      self%nimg_max = 0

      ! 2. Fast Inverse Lattice Matrix (Cramer's Rule)
      det = mol%lattice(1,1)*(mol%lattice(2,2)*mol%lattice(3,3) - mol%lattice(2,3)*mol%lattice(3,2)) - &
            mol%lattice(1,2)*(mol%lattice(2,1)*mol%lattice(3,3) - mol%lattice(2,3)*mol%lattice(3,1)) + &
            mol%lattice(1,3)*(mol%lattice(2,1)*mol%lattice(3,2) - mol%lattice(2,2)*mol%lattice(3,1))

      lat_inv(1,1) =  (mol%lattice(2,2)*mol%lattice(3,3) - mol%lattice(2,3)*mol%lattice(3,2)) / det
      lat_inv(1,2) = -(mol%lattice(1,2)*mol%lattice(3,3) - mol%lattice(1,3)*mol%lattice(3,2)) / det
      lat_inv(1,3) =  (mol%lattice(1,2)*mol%lattice(2,3) - mol%lattice(1,3)*mol%lattice(2,2)) / det
      lat_inv(2,1) = -(mol%lattice(2,1)*mol%lattice(3,3) - mol%lattice(2,3)*mol%lattice(3,1)) / det
      lat_inv(2,2) =  (mol%lattice(1,1)*mol%lattice(3,3) - mol%lattice(1,3)*mol%lattice(3,1)) / det
      lat_inv(2,3) = -(mol%lattice(1,1)*mol%lattice(2,3) - mol%lattice(1,3)*mol%lattice(2,1)) / det
      lat_inv(3,1) =  (mol%lattice(2,1)*mol%lattice(3,2) - mol%lattice(2,2)*mol%lattice(3,1)) / det
      lat_inv(3,2) = -(mol%lattice(1,1)*mol%lattice(3,2) - mol%lattice(1,2)*mol%lattice(3,1)) / det
      lat_inv(3,3) =  (mol%lattice(1,1)*mol%lattice(2,2) - mol%lattice(1,2)*mol%lattice(2,1)) / det

      ! 3. Grid sizing
      do i = 1, 3
         L_vec = sqrt(sum(mol%lattice(:, i)**2))
         n_xyz(i) = max(2, ceiling(L_vec / self%cutoff))
         if (mod(n_xyz(i), 2) /= 0) n_xyz(i) = n_xyz(i) + 1
      end do

      ! 4. Initial Buffering (Density-based heuristic)
      current_capacity = mol%nat * 64 
      allocate(head(product(n_xyz)), source=0)
      allocate(nxt(mol%nat), source=0)
      allocate(checked(mol%nat), source=.false.)

      call resize(self%nlat, current_capacity)
      if (allocated(self%nimg)) deallocate(self%nimg)
      if (allocated(self%tridx)) deallocate(self%tridx)
      allocate(self%nimg(current_capacity), source=0)
      allocate(self%tridx(ntr, current_capacity), source=0)
      allocate(self%selfnimg(mol%nat), source=0)
      allocate(self%selftridx(ntr, mol%nat), source=0)

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

      ! 6. Main Search Loop
      do iat = 1, mol%nat
         self%inl(iat) = img
         
         ! Handle self-images
         call get_wsc_pairs(trans, zero_vec, nimg_count, tridx_arr, r2_min, is_self=.true.)
         self%selfnimg(iat) = nimg_count
         self%selftridx(1:nimg_count, iat) = tridx_arr(1:nimg_count)
         self%nimg_max = max(self%nimg_max, nimg_count)

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
                  
                  if (self%complete .or. jat <= iat) then
                     vec(:) = xyz_wrap(:, iat) - xyz_wrap(:, jat)
                     call get_wsc_pairs(trans, vec, nimg_count, tridx_arr, r2_min, is_self=.false.)

                     if (nimg_count > 0 .and. r2_min <= cutoff2) then
                        img = img + 1

                        if (img > current_capacity) then
                           current_capacity = current_capacity * 2
                           call resize(self%nlat, current_capacity)
                           call resize(self%nimg, current_capacity)
                           call resize(self%tridx, current_capacity)
                        end if

                        self%nlat(img) = jat
                        self%nimg(img) = nimg_count
                        self%tridx(1:nimg_count, img) = tridx_arr(1:nimg_count)
                        self%nimg_max = max(self%nimg_max, nimg_count)
                     end if
                  end if
               end if
               jat = nxt(jat)
            end do
         end do; end do; end do

         ! Clean 'checked' array efficiently by retracing the same cells
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

      ! 7. Final Clean-up: Trim to exact size
      call resize(self%nlat, img)
      call resize(self%nimg, img)
      call resize(self%tridx, img)

   end subroutine generate_3d

end module mctc_ncoord_adjlist_type

