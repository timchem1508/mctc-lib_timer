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

!> Utility to find the shortest distance and its degeneracies across 27 translation images
    subroutine get_wsc_pairs(trans, rij, iws, list, min_r2, is_self)
        real(wp), intent(in) :: trans(:, :)
        real(wp), intent(in) :: rij(3)
        integer, intent(out) :: iws
        integer, intent(out) :: list(:)
        real(wp), intent(out) :: min_r2
        logical, intent(in) :: is_self

        real(wp) :: vec(3), r2
        integer :: itr
        real(wp), parameter :: tol = 0.01_wp
        real(wp), parameter :: thr = sqrt(epsilon(0.0_wp))

        iws = 0
        list(:) = 0
        min_r2 = huge(1.0_wp)

        ! Pass 1: find absolute minimum distance
        do itr = 1, size(trans, 2)
            vec(:) = rij - trans(:, itr)
            r2 = vec(1)**2 + vec(2)**2 + vec(3)**2
            
            ! If computing self-interactions, ignore the central 0-translation image 
            ! to enforce finding the first actual periodic interaction
            if (is_self .and. r2 < thr) cycle
            
            if (r2 < min_r2) min_r2 = r2
        end do

        ! Pass 2: map all degeneracies falling within the tolerance
        do itr = 1, size(trans, 2)
            vec(:) = rij - trans(:, itr)
            r2 = vec(1)**2 + vec(2)**2 + vec(3)**2
            
            if (is_self .and. r2 < thr) cycle
            
            if (abs(r2 - min_r2) <= tol) then
                iws = iws + 1
                list(iws) = itr
            end if
        end do
    end subroutine get_wsc_pairs

!> Generator for neighbourlist using a Linked Cell List approach for Periodic Systems
    subroutine generate_3d(self, mol)
        !> Instance of the neighbourlist
        type(adjacency_list), intent(inout) :: self
        !> Molecular structure data
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
        real(wp) :: vec(3)
        integer, allocatable :: tmp_nimg(:), tmp_tridx(:,:)

        ! 1. Get 27 translation vectors for the "jacket" around the central cell
        call get_lattice_points(mol%periodic, mol%lattice, sqrt(epsilon(0.0_wp)), trans)
        ntr = size(trans, 2)
        
        if (allocated(self%trans)) deallocate(self%trans)
        allocate(self%trans(3, ntr))
        self%trans = trans

        cutoff2 = self%cutoff**2
        img = 0
        self%nimg_max = 0

        ! 2. Compute the inverse of the lattice matrix to map Cartesian to Fractional
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

        ! 3. Grid dimensions: force cell width to be a half-lattice parameter divided by a natural number.
        ! This mathematically requires n_xyz to be an even integer.
        do i = 1, 3
            L_vec = sqrt(sum(mol%lattice(:, i)**2))
            n_xyz(i) = max(2, ceiling(L_vec / self%cutoff))
            if (mod(n_xyz(i), 2) /= 0) n_xyz(i) = n_xyz(i) + 1
        end do

        ! 4. Wrap atoms into the central crystallographic cell and build Linked List
        allocate(head(product(n_xyz)), source=0)
        allocate(nxt(mol%nat), source=0)
        allocate(checked(mol%nat), source=.false.)

        do iat = 1, mol%nat
            fract(:) = matmul(lat_inv, mol%xyz(:, iat))
            fract(:) = fract(:) - floor(fract(:)) ! Wrap to [0, 1)
            xyz_wrap(:, iat) = matmul(mol%lattice, fract) ! Wrapped Cartesian

            ix = min(n_xyz(1), max(1, int(fract(1) * n_xyz(1)) + 1))
            iy = min(n_xyz(2), max(1, int(fract(2) * n_xyz(2)) + 1))
            iz = min(n_xyz(3), max(1, int(fract(3) * n_xyz(3)) + 1))

            ic = ix + n_xyz(1)*(iy-1) + n_xyz(1)*n_xyz(2)*(iz-1)
            nxt(iat) = head(ic)
            head(ic) = iat
        end do

        ! Pre-allocate dynamic and fixed arrays
        call resize(self%nlat, mol%nat * 50) 
        allocate(self%nimg(size(self%nlat)), source=0)
        allocate(self%tridx(ntr, size(self%nlat)), source=0)
        allocate(self%selfnimg(mol%nat), source=0)
        allocate(self%selftridx(ntr, mol%nat), source=0)

        ! 5. Triple loop search over the central cell's fractional grid
        do iat = 1, mol%nat
            self%inl(iat) = img
            
            ! --- SELF-INTERACTIONS ---
            ! Check against its own images in the 27-cell jacket
            call get_wsc_pairs(trans, [0.0_wp, 0.0_wp, 0.0_wp], nimg_count, tridx_arr, r2_min, is_self=.true.)
            self%selfnimg(iat) = nimg_count
            self%selftridx(1:nimg_count, iat) = tridx_arr(1:nimg_count)
            self%nimg_max = max(self%nimg_max, nimg_count)

            ! Locate central cell of iat
            fract(:) = matmul(lat_inv, mol%xyz(:, iat))
            fract(:) = fract(:) - floor(fract(:))
            ix = min(n_xyz(1), max(1, int(fract(1) * n_xyz(1)) + 1))
            iy = min(n_xyz(2), max(1, int(fract(2) * n_xyz(2)) + 1))
            iz = min(n_xyz(3), max(1, int(fract(3) * n_xyz(3)) + 1))

            checked(iat) = .true. ! Flag to avoid comparing an atom to itself in cross-interactions

            ! --- CROSS-INTERACTIONS ---
            do dk = -1, 1; do dj = -1, 1; do di = -1, 1
                ! Map adjacent linked cells back to central cell indices (PBC wrapping)
                jx = modulo(ix + di - 1, n_xyz(1)) + 1
                jy = modulo(iy + dj - 1, n_xyz(2)) + 1
                jz = modulo(iz + dk - 1, n_xyz(3)) + 1

                jc = jx + n_xyz(1)*(jy-1) + n_xyz(1)*n_xyz(2)*(jz-1)
                jat = head(jc)

                do while (jat > 0)
                    if (.not. checked(jat)) then
                        checked(jat) = .true. 

                        if (.not. self%complete .and. jat > iat) then
                            ! Skip symmetrical evaluation, but retain `checked = .true.` to prevent repeat hits
                        else
                            ! Distance in the fully wrapped central cell
                            vec(:) = xyz_wrap(:, iat) - xyz_wrap(:, jat)

                            ! Apply Wigner-Seitz logic across the 27 jacket translations
                            call get_wsc_pairs(trans, vec, nimg_count, tridx_arr, r2_min, is_self=.false.)

                            if (nimg_count > 0 .and. r2_min <= cutoff2) then
                                img = img + 1
                                
                                ! Manual resize for parallel dynamic arrays if necessary
                                if (size(self%nlat) < img) then
                                    call resize(self%nlat)
                                    allocate(tmp_nimg(size(self%nlat)))
                                    tmp_nimg(1:img-1) = self%nimg(1:img-1)
                                    call move_alloc(tmp_nimg, self%nimg)

                                    allocate(tmp_tridx(ntr, size(self%nlat)))
                                    tmp_tridx(:, 1:img-1) = self%tridx(:, 1:img-1)
                                    call move_alloc(tmp_tridx, self%tridx)
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

            ! Reset the checked array for the next `iat` evaluation
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

        ! Final Cleanup and Exact Sizing
        call resize(self%nlat, img)
        allocate(tmp_nimg(img))
        tmp_nimg(1:img) = self%nimg(1:img)
        call move_alloc(tmp_nimg, self%nimg)

        allocate(tmp_tridx(ntr, img))
        tmp_tridx(:, 1:img) = self%tridx(:, 1:img)
        call move_alloc(tmp_tridx, self%tridx)

    end subroutine generate_3d

    !> Generator for neighbourlist using a Linked Cell List approach for Periodic Systems
    !> Optimized with a 27/8 "jacket" reduction for shortest-image searching.
    subroutine generate_pbc(self, mol)
        !> Instance of the neighbourlist
        type(adjacency_list), intent(inout) :: self
        !> Molecular structure data
        type(structure_type), intent(in) :: mol

        integer :: iat, jat, img, ic, jc, i, itr
        integer :: ix, iy, iz, jx, jy, jz, di, dj, dk
        integer, allocatable :: head(:), nxt(:)
        logical, allocatable :: checked(:)
        
        integer :: n_xyz(3), ntr, nimg_count
        integer :: tridx_arr(27)
        integer, allocatable :: tr_int(:, :)
        real(wp) :: cutoff2, r2_min
        real(wp) :: lat_inv(3, 3), det, L_vec
        real(wp) :: fract(3)
        real(wp), allocatable :: xyz_wrap(:, :), fract_wrap(:, :)
        real(wp), allocatable :: trans(:, :)
        real(wp) :: vec(3)
        integer, allocatable :: tmp_nimg(:), tmp_tridx(:,:)

        ! 1. Setup Lattice Translations (27-cell jacket)
        call get_lattice_points(mol%periodic, mol%lattice, sqrt(epsilon(0.0_wp)), trans)
        ntr = size(trans, 2)
        
        if (allocated(self%trans)) deallocate(self%trans)
        allocate(self%trans(3, ntr))
        self%trans = trans

        cutoff2 = self%cutoff**2
        img = 0
        self%nimg_max = 0

        ! 2. Compute Inverse Lattice for Cartesian -> Fractional mapping
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

        ! Pre-calculate integer components of translation vectors for masking
        allocate(tr_int(3, ntr))
        do itr = 1, ntr
            fract(:) = matmul(lat_inv, trans(:, itr))
            tr_int(:, itr) = nint(fract(:))
        end do

        ! 3. Define Grid: force even number of cells to respect half-lattice parameter rule
        do i = 1, 3
            L_vec = sqrt(sum(mol%lattice(:, i)**2))
            n_xyz(i) = max(2, ceiling(L_vec / self%cutoff))
            if (mod(n_xyz(i), 2) /= 0) n_xyz(i) = n_xyz(i) + 1
        end do

        ! 4. Build Linked List using wrapped fractional coordinates
        allocate(head(product(n_xyz)), source=0)
        allocate(nxt(mol%nat), source=0)
        allocate(checked(mol%nat), source=.false.)
        allocate(xyz_wrap(3, mol%nat), fract_wrap(3, mol%nat))

        do iat = 1, mol%nat
            fract(:) = matmul(lat_inv, mol%xyz(:, iat))
            fract(:) = fract(:) - floor(fract(:)) 
            fract_wrap(:, iat) = fract(:)
            xyz_wrap(:, iat) = matmul(mol%lattice, fract) 

            ix = min(n_xyz(1), max(1, int(fract(1) * n_xyz(1)) + 1))
            iy = min(n_xyz(2), max(1, int(fract(2) * n_xyz(2)) + 1))
            iz = min(n_xyz(3), max(1, int(fract(3) * n_xyz(3)) + 1))

            ic = ix + n_xyz(1)*(iy-1) + n_xyz(1)*n_xyz(2)*(iz-1)
            nxt(iat) = head(ic)
            head(ic) = iat
        end do

        ! Initial allocation for results
        call resize(self%nlat, mol%nat * 50) 
        allocate(self%nimg(size(self%nlat)), source=0)
        allocate(self%tridx(ntr, size(self%nlat)), source=0)
        allocate(self%selfnimg(mol%nat), source=0)
        allocate(self%selftridx(ntr, mol%nat), source=0)

        ! 5. Search Logic
        do iat = 1, mol%nat
            self%inl(iat) = img
            
            ! Self-interactions: Check only 8 relevant periodic copies of iat
            call get_wsc_pairs_opt(trans, tr_int, fract_wrap(:, iat), [0.0_wp, 0.0_wp, 0.0_wp], &
                                   nimg_count, tridx_arr, r2_min, .true.)
            self%selfnimg(iat) = nimg_count
            self%selftridx(1:nimg_count, iat) = tridx_arr(1:nimg_count)
            self%nimg_max = max(self%nimg_max, nimg_count)

            ! Locate iat's linked cell
            ix = min(n_xyz(1), max(1, int(fract_wrap(1, iat) * n_xyz(1)) + 1))
            iy = min(n_xyz(2), max(1, int(fract_wrap(2, iat) * n_xyz(2)) + 1))
            iz = min(n_xyz(3), max(1, int(fract_wrap(3, iat) * n_xyz(3)) + 1))

            checked(iat) = .true. 

            do dk = -1, 1; do dj = -1, 1; do di = -1, 1
                jx = modulo(ix + di - 1, n_xyz(1)) + 1
                jy = modulo(iy + dj - 1, n_xyz(2)) + 1
                jz = modulo(iz + dk - 1, n_xyz(3)) + 1

                jc = jx + n_xyz(1)*(jy-1) + n_xyz(1)*n_xyz(2)*(iz-1)
                jat = head(jc)

                do while (jat > 0)
                    if (.not. checked(jat)) then
                        checked(jat) = .true. 

                        if (self%complete .or. iat >= jat) then
                            vec(:) = xyz_wrap(:, iat) - xyz_wrap(:, jat)

                            ! Optimized WSC search: checks only 8 images
                            call get_wsc_pairs_opt(trans, tr_int, fract_wrap(:, jat), vec, &
                                                   nimg_count, tridx_arr, r2_min, .false.)

                            if (nimg_count > 0 .and. r2_min <= cutoff2) then
                                img = img + 1
                                if (size(self%nlat) < img) call resize_nimg_tridx(self, ntr)

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

            ! Reset checked array using cell-based traversal for efficiency
            checked(iat) = .false.
            do dk = -1, 1; do dj = -1, 1; do di = -1, 1
                jx = modulo(ix + di - 1, n_xyz(1)) + 1
                jy = modulo(iy + dj - 1, n_xyz(2)) + 1
                jz = modulo(iz + dk - 1, n_xyz(3)) + 1
                jc = jx + n_xyz(1)*(jy-1) + n_xyz(1)*n_xyz(2)*(iz-1)
                jat = head(jc)
                do while (jat > 0); checked(jat) = .false.; jat = nxt(jat); end do
            end do; end do; end do

            self%nnl(iat) = img - self%inl(iat)
        end do

        ! Cleanup
        call resize(self%nlat, img)
        call resize(self%nimg, img)
        ! tridx resize is handled manually to preserve 2D shape (ntr, img)
        allocate(tmp_tridx(ntr, img))
        tmp_tridx(:, 1:img) = self%tridx(:, 1:img)
        call move_alloc(tmp_tridx, self%tridx)

    contains

        subroutine get_wsc_pairs_opt(trans, tr_int, f_j, rij, iws, list, min_r2, is_self)
            real(wp), intent(in) :: trans(:, :), rij(3), f_j(3)
            integer, intent(in) :: tr_int(:, :)
            integer, intent(out) :: iws, list(:)
            real(wp), intent(out) :: min_r2
            logical, intent(in) :: is_self
            real(wp) :: vec(3), r2
            integer :: itr, i, v_min(3), v_max(3)
            logical :: valid(size(trans, 2))
            real(wp), parameter :: tol = 0.01_wp, thr = 1.0e-8_wp

            iws = 0; list(:) = 0; min_r2 = huge(1.0_wp)

            ! Define which 8 cells constitute the jacket for this target position
            do i = 1, 3
                if (f_j(i) < 0.5_wp) then
                    v_min(i) = -1; v_max(i) = 0
                else
                    v_min(i) = 0; v_max(i) = 1
                end if
            end do

            do itr = 1, size(trans, 2)
                valid(itr) = all(tr_int(:, itr) >= v_min .and. tr_int(:, itr) <= v_max)
                if (.not. valid(itr)) cycle
                
                vec(:) = rij - trans(:, itr)
                r2 = sum(vec**2)
                if (is_self .and. r2 < thr) cycle
                if (r2 < min_r2) min_r2 = r2
            end do

            do itr = 1, size(trans, 2)
                if (.not. valid(itr)) cycle
                vec(:) = rij - trans(:, itr)
                r2 = sum(vec**2)
                if (is_self .and. r2 < thr) cycle
                if (abs(r2 - min_r2) <= tol) then
                    iws = iws + 1
                    list(iws) = itr
                end if
            end do
        end subroutine get_wsc_pairs_opt

        subroutine resize_nimg_tridx(self, ntr)
            type(adjacency_list), intent(inout) :: self
            integer, intent(in) :: ntr
            integer, allocatable :: t1(:), t2(:,:)
            integer :: new_size
            call resize(self%nlat) ! standard resize for nlat
            new_size = size(self%nlat)
            allocate(t1(new_size), source=0); t1(1:size(self%nimg)) = self%nimg
            call move_alloc(t1, self%nimg)
            allocate(t2(ntr, new_size), source=0); t2(:, 1:size(self%tridx,2)) = self%tridx
            call move_alloc(t2, self%tridx)
        end subroutine resize_nimg_tridx

    end subroutine generate_pbc

end module mctc_ncoord_adjlist_type

