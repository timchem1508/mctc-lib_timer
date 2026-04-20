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
   real(wp), parameter :: cutoff_def = 29.0_wp
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
         self%cutoff = cutoff_def
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
      real(wp) :: r2, min_cell_width, vec(3), cutoff2, cell_w(3), min_xyz(3), max_xyz(3), trans(3,1)
      integer(int64) :: n_cells

      img = 0
      cutoff2 = self%cutoff**2
      trans = 0.0_wp
      allocate(self%trans, source=trans)

      ! 1. Define the grid boundaries and dimensions
      ! We add a small buffer to the bounding box to ensure all atoms are contained
      min_xyz = minval(mol%xyz, dim=2) - buffer
      max_xyz = maxval(mol%xyz, dim=2) + buffer

      ! Force the cell width to be at least 60.0 to prevent massive empty grids
      min_cell_width = self%cutoff + eps

      ! Number of cells: must be at least 1, and cell width >= cutoff
      n_xyz = max(1, floor((max_xyz - min_xyz) / (min_cell_width + eps)))
      cell_w = (max_xyz - min_xyz) / (real(n_xyz, wp) + eps) + eps

      n_cells = int(n_xyz(1), int64) * int(n_xyz(2), int64) * int(n_xyz(3), int64)

      if (n_cells > 2147483647_int64) then
         write(*,*) "box params", max_xyz(:) - min_xyz(:)
         write(*,*) "Error: Bounding box too large. Linked-cell grid exceeds 32-bit integer limit."
         stop 1
      end if

      allocate(head(n_cells), source=0)

      ! 2. Build the Linked List
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

                     do itr = 1, size(self%trans, 2)
                        vec(:) = mol%xyz(:, iat) - mol%xyz(:, jat) - self%trans(:, itr)
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

   subroutine get_pairs(iws, trans, rij, list, cutoff2)
      integer, intent(out) :: iws
      real(wp), intent(in) :: rij(3)
      real(wp), intent(in) :: trans(:, :)
      integer, intent(out) :: list(:)
      real(wp), intent(in) :: cutoff2

      logical :: mask(size(list))
      real(wp) :: dist(size(list)), vec(3), r2
      integer :: itr, img, pos

      iws = 0
      img = 0
      list(:) = 0
      mask(:) = .true.

      do itr = 1, size(trans, 2)
         vec(:) = rij - trans(:, itr)
         r2 = vec(1)**2 + vec(2)**2 + vec(3)**2
         if (r2 < thr .or. r2 > cutoff2) cycle
         img = img + 1
         dist(img) = r2
      end do

      if (img == 0) return

      pos = minloc(dist(:img), dim=1)

      r2 = dist(pos)
      mask(pos) = .false.

      iws = 1
      list(iws) = pos
      if (img <= iws) return

      do
         pos = minloc(dist(:img), dim=1, mask=mask(:img))
         if (abs(dist(pos) - r2) > tol) exit
         mask(pos) = .false.
         iws = iws + 1
         list(iws) = pos
      end do

   end subroutine get_pairs

   !> Generator for neighbourlist using a Linked Cell List approach (O(N) scaling)
   subroutine generate_3d(self, mol)
      !> Instance of the neighbourlist
      type(adjacency_list), intent(inout) :: self
      !> Molecular structure data
      type(structure_type), intent(in) :: mol

      integer :: iat, jat, itr, img, ic, jc
      integer :: ix, iy, iz, jx, jy, jz, di, dj, dk
      integer, allocatable :: head(:), nxt(:)
      integer :: n_xyz(3)
      real(wp) :: r2, min_cell_width, vec(3), cutoff2, cell_w(3), min_xyz(3), max_xyz(3)
      integer(int64) :: n_cells

      integer :: nimg, iws
      integer, allocatable :: trlist(:)
      real(wp) :: vec_base(3)


      img = 0
      cutoff2 = self%cutoff**2
      call get_lattice_points(mol%periodic, mol%lattice, thr, self%trans)

      ! Allocate list to hold the maximum number of possible translations
      allocate(trlist(size(self%trans, 2)))


      ! 1. Define the grid boundaries and dimensions
      min_xyz = minval(mol%xyz, dim=2) - buffer
      max_xyz = maxval(mol%xyz, dim=2) + buffer

      ! Force the cell width to be at least 60.0 to prevent massive empty grids
      min_cell_width = self%cutoff + eps

      ! Number of cells: must be at least 1, and cell width >= cutoff
      n_xyz = max(1, floor((max_xyz - min_xyz) / (min_cell_width + eps)))
      cell_w = (max_xyz - min_xyz) / (real(n_xyz, wp) + eps) + eps

      n_cells = int(n_xyz(1), int64) * int(n_xyz(2), int64) * int(n_xyz(3), int64)

      allocate(head(n_cells), source=0)

      ! 2. Build the Linked List
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
      if (any(mol%periodic)) then
         call resize(self%nltr, init_size*mol%nat)
         call resize(self%nimg, init_size*mol%nat)
         self%nimg = 0
         allocate(self%tridx((size(self%trans, 2)), init_size*mol%nat))
         self%tridx = 0
         allocate(self%selfnimg(mol%nat))
         self%selfnimg = 0
         allocate(self%selftridx((size(self%trans, 2)), mol%nat))
         self%selftridx = 0
      end if

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

                     if (iat == jat) then
                        ! Raw distance vector for self-image is just 0.0
                        vec_base(:) = 0.0_wp

                        ! Find ONLY the closest periodic images
                        call get_pairs(nimg, self%trans, vec_base, trlist, cutoff2)

                        ! Update selfnimg but DO NOT increment the cross-interaction `img` counter
                        self%selfnimg(iat) = nimg
                        if (nimg > 0) then
                           self%selftridx(1:nimg, iat) = trlist(1:nimg)
                        end if
                     else
                        ! Get the raw distance vector once
                        vec_base(:) = mol%xyz(:, iat) - mol%xyz(:, jat)

                        ! Find ONLY the closest periodic images
                        call get_pairs(nimg, self%trans, vec_base, trlist, cutoff2)

                        if (nimg > 0) then
                           img = img + 1
                           if (size(self%nlat) < img) call resize(self%nlat)
                           if (size(self%nltr) < img) call resize(self%nltr)
                           if (size(self%nimg) < img) call resize(self%nimg)
                           if (size(self%tridx, 2) < img) call resize(self%tridx, img)

                           self%nlat(img) = jat
                           self%nltr(img) = nimg
                           self%nimg(img) = nimg
                           self%tridx(1:nimg, img) = trlist(1:nimg)
                        end if
                     end if

                     jat = nxt(jat)
                  end do
               end do; end do; end do
         self%nnl(iat) = img - self%inl(iat)
      end do

      ! Cleanup and final sizing
      if (allocated(head)) deallocate(head)
      if (allocated(nxt)) deallocate(nxt)
      if (allocated(trlist)) deallocate(trlist)

      call resize(self%nlat, img)

      if (any(mol%periodic)) then
         call resize(self%nltr, img)
         call resize(self%nimg, img)
         call resize(self%tridx, img)
      end if


   end subroutine generate_3d

end module mctc_ncoord_adjlist_type

