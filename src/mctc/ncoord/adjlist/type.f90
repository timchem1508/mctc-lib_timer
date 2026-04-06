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
   use mctc_env, only : wp
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
      integer :: nimg_max
      integer, allocatable :: nimg(:)
      integer, allocatable :: tridx(:, :)
      real(wp), allocatable :: trans(:, :)
   end type adjacency_list


   ! Default input
   real(wp), parameter :: cutoff_def = 29.0_wp
   logical, parameter :: complete_def = .false.
   integer, parameter :: init_size = 10
   real(wp), parameter :: buffer = 0.01_wp

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
         call generate_3d(self, mol)
      else
         call generate_0d(self, mol)
      end if
   end subroutine new_adjacency_list

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

      real(wp), allocatable :: trans(:,:)

      img = 0
      cutoff2 = self%cutoff**2
      allocate(trans(3,1), source=0.0_wp)

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

   !> Generator for neighbourlist using a Linked Cell List approach (O(N) scaling)
   subroutine generate_3d(self, mol)
      type(adjacency_list), intent(inout) :: self
      type(structure_type), intent(in) :: mol

      integer :: iat, jat, itr, img, iws, img_loc, ic, jc, ntr, nimg, pos
      integer :: ix, iy, iz, jx, jy, jz, di, dj, dk
      integer, allocatable :: head(:), nxt(:), list(:), tridx_loc(:)
      real(wp), allocatable :: dist(:)
      logical, allocatable :: mask(:)
      integer :: n_xyz(3)
      real(wp) :: r2, vec(3), cutoff2, cell_w(3), min_xyz(3), max_xyz(3)

      if (any(mol%periodic)) then
         call get_lattice_points(mol%periodic, mol%lattice, 25.0_wp, self%trans)
      else
         allocate(self%trans(3, 1))
         self%trans = 0.0_wp
      end if

      ntr = size(self%trans, 2)
      ! Fix: Properly declare and allocate needed temporary arrays
      allocate(dist(ntr), mask(ntr), tridx_loc(ntr), list(ntr))

      img = 0
      self%nimg_max = 0
      cutoff2 = self%cutoff**2

      ! 1. Define the grid boundaries and dimensions
      min_xyz = minval(mol%xyz, dim=2) - buffer
      max_xyz = maxval(mol%xyz, dim=2) + buffer

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
      call resize(self%nltr, init_size*mol%nat)
      call resize(self%nimg, init_size*mol%nat)
      allocate(self%tridx(ntr, init_size*mol%nat))

      ! 3. Triple loop search over nearby cells (O(N) time)
      do iat = 1, mol%nat
         self%inl(iat) = img

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

                     ! Symmetrical optimization
                     if (.not. self%complete .and. jat > iat) then
                        jat = nxt(jat)
                        cycle
                     end if

                     img_loc = 0
                     dist(:) = 0.0_wp
                     tridx_loc(:) = 0

                     ! Check all translation images for this atom pair
                     do itr = 1, ntr
                        vec(:) = mol%xyz(:, iat) - mol%xyz(:, jat) - self%trans(:, itr)
                        r2 = sum(vec**2)

                        ! Standard distance check and self-interaction exclusion
                        if (r2 < thr .or. r2 >= cutoff2) cycle

                        img_loc = img_loc + 1
                        dist(img_loc) = r2
                        tridx_loc(img_loc) = itr
                     end do

                     ! Find equivalent minimum WSC images once distances are gathered
                     if (img_loc /= 0) then
                        mask(1:img_loc) = .true.
                        pos = minloc(dist(1:img_loc), dim=1)

                        r2 = dist(pos)
                        mask(pos) = .false.

                        iws = 1
                        list(iws) = tridx_loc(pos)

                        if (img_loc > 1) then
                           do
                              pos = minloc(dist(1:img_loc), dim=1, mask=mask(1:img_loc))
                              if (pos == 0) exit ! Fortran failsafe if no true masks remain
                              if (abs(dist(pos) - r2) > tol) exit
                              mask(pos) = .false.
                              iws = iws + 1
                              list(iws) = tridx_loc(pos)
                              if (iws == img_loc) exit
                           end do
                        end if

                        nimg = iws
                        self%nimg_max = max(nimg, self%nimg_max)

                        img = img + 1
                        ! Dynamically resize structural arrays
                        if (size(self%nlat) < img) call resize(self%nlat)
                        if (size(self%nltr) < img) call resize(self%nltr)
                        if (size(self%nimg) < img) call resize(self%nimg)

                        ! Fix: Map to 1D structural definitions
                        self%nlat(img) = jat
                        self%nltr(img) = list(1)
                        self%nimg(img) = nimg
                        self%tridx(1:nimg, img) = list(1:nimg)
                     end if

                     jat = nxt(jat)
                  end do
               end do; end do; end do
         self%nnl(iat) = img - self%inl(iat)
      end do

      ! Cleanup and final sizing
      if (allocated(head)) deallocate(head)
      if (allocated(nxt)) deallocate(nxt)
      call resize(self%nlat, img)
      call resize(self%nltr, img)
      call resize(self%nimg, img)

   end subroutine generate_3d

end module mctc_ncoord_adjlist_type
