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

!> Unit tests for compressed sparse row neighbor lists
module test_csrlist
   use mctc_csrlist, only : csr_list, new_csr_list, compute_grid, grid_type, &
      & distance_screening, screening_type
   use mctc_cutoff, only : get_lattice_points, wrap_to_central_cell
   use mctc_env, only : wp, i8, timer_type, format_time
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, &
      & test_failed, check
   use mctc_io_math, only : outerprod
   use mctc_io_resize, only : resize
   use mctc_io_structure, only : structure_type, new
   use mctc_wignerseitz, only : wignerseitz_cell, new_wignerseitz_cell
   use testsuite_structure, only : get_structure
   implicit none
   private

   public :: collect_csrlist

   !> Tolerance for floating-point comparisons
   real(wp), parameter :: thr = 100*epsilon(1.0_wp)

   !> Tolerance for comparing image vectors built from different translations
   real(wp), parameter :: thr2 = sqrt(epsilon(1.0_wp))


   !> Distance screening which additionally rejects every pair involving
   !> one element
   type, extends(distance_screening) :: element_screening

      !> Atomic number of the element excluded from all pairs
      integer :: exclude = 1
   contains

      !> Reject pairs containing the excluded element
      procedure :: check_pair => check_pair_element

   end type element_screening

   !> Screening by a user-defined distance threshold, which the neighbor list
   !> construction leaves untouched in contrast to the distance screening
   type, extends(screening_type) :: radius_screening
   contains

      !> Include pairs within the user-defined threshold
      procedure :: check_pair => check_pair_radius

   end type radius_screening

   !> Screening rejecting every candidate pair
   type, extends(screening_type) :: reject_screening
   contains

      !> Reject every pair
      procedure :: check_pair => check_pair_reject

   end type reject_screening


contains


!> Collect all exported unit tests
subroutine collect_csrlist(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("grid-fullerene-cut-1", test_grid_fullerene_cut_1), &
      & new_unittest("grid-water-cut-1", test_grid_water_cut_1), &
      & new_unittest("grid-x02-1x1x4", test_grid_x02_114), &
      & new_unittest("csr-vs-verlet-mb01", test_list_mb01), &
      & new_unittest("csr-vs-verlet-fullerene-cut-5", test_list_fullerene_cut_5), &
      & new_unittest("csr-vs-verlet-feo2", test_list_feo2), &
      & new_unittest("csr-vs-verlet-x02-1x1x4", test_list_x02_114), &
      & new_unittest("csr-vs-verlet-slab-2d", test_list_slab), &
      & new_unittest("csr-vs-verlet-wire-1d", test_list_wire), &
      & new_unittest("feo2-wsc", test_feo2_wsc), &
      & new_unittest("wsc-parity-skewed", test_wsc_parity_skewed), &
      & new_unittest("wsc-parity-dense-images", test_wsc_parity_dense_images), &
      & new_unittest("wsc-parity-slab-2d", test_wsc_parity_slab), &
      & new_unittest("wsc-parity-wire-1d", test_wsc_parity_wire), &
      & new_unittest("screen-distance-thresh", test_screen_distance_thresh), &
      & new_unittest("screen-element-mb01", test_screen_element_mb01), &
      & new_unittest("screen-element-feo2", test_screen_element_feo2), &
      & new_unittest("screen-radius-nacl", test_screen_radius_nacl), &
      & new_unittest("screen-reject-feo2", test_screen_reject_feo2), &
      & new_unittest("screen-wsc-element-feo2", test_screen_wsc_element_feo2) &
      & ]

end subroutine collect_csrlist

!> Generate reference 1-based CSR list
subroutine gen_verlet(mol, trans, cutoff, row_ptr, col_ind, nltr, complete, screen)

   !> Molecular structure data
   type(structure_type), intent(in) :: mol

   !> Translation vectors for all images
   real(wp), intent(in) :: trans(:, :)

   !> Realspace cutoff for neighborlist generation
   real(wp), intent(in) :: cutoff

   !> Standard 1-based row pointer array of size (nat + 1)
   integer, intent(inout) :: row_ptr(:)

   !> Column indices / neighboring atom indices (nlat)
   integer, allocatable, intent(out) :: col_ind(:)

   !> Cell index of the neighboring atom
   integer, allocatable, intent(out) :: nltr(:)

   !> Whether a complete or a symmetrical upper-triangular map should be generated
   logical, intent(in) :: complete

   !> Screening applied to every off-diagonal pair within the cutoff
   class(screening_type), intent(in), optional :: screen

   integer :: iat, jat, itr, nnz
   real(wp) :: r2, vec(3), cutoff2
   logical :: include

   nnz = 0
   cutoff2 = cutoff**2

   call resize(col_ind, 10*mol%nat)
   call resize(nltr, 10*mol%nat)
   row_ptr(1) = 1

   do iat = 1, mol%nat
      ! 1. Insert Diagonal Element at start of row
      nnz = nnz + 1
      if (size(col_ind) < nnz) call resize(col_ind)
      if (size(nltr) < nnz) call resize(nltr)

      col_ind(nnz) = iat
      nltr(nnz) = 1

      ! 2. Off-Diagonal neighbors (inverse to fit the linked-cell list ordering)
      do jat = 1, mol%nat
         ! Skip lower triangle if incomplete
         if (.not. complete .and. jat < iat) cycle

         do itr = 1, size(trans, 2)
            ! Skip identity self-interaction (already added as diagonal)
            if (iat == jat .and. itr == 1) cycle

            vec(:) = mol%xyz(:, iat) - mol%xyz(:, jat) - trans(:, itr)
            r2 = sum(vec**2)
            if (r2 < epsilon(cutoff2) .or. r2 > cutoff2) cycle
            if (present(screen)) then
               call screen%check_pair(mol, iat, jat, r2, include)
               if (.not. include) cycle
            end if

            nnz = nnz + 1
            if (size(col_ind) < nnz) call resize(col_ind)
            if (size(nltr) < nnz) call resize(nltr)
            col_ind(nnz) = jat
            nltr(nnz) = itr
         end do
      end do
      row_ptr(iat + 1) = nnz + 1
   end do

   call resize(col_ind, nnz)
   call resize(nltr, nnz)

end subroutine gen_verlet

!> Replicate a structure along each lattice-vector direction
subroutine make_supercell(mol, rep)

   !> Structure to replicate
   type(structure_type), intent(inout) :: mol

   !> Replication factors along the three lattice vectors
   integer, intent(in) :: rep(3)

   real(wp), allocatable :: xyz(:, :), lattice(:, :)
   integer, allocatable :: num(:)
   integer :: i, j, k, l, c

   num = [(mol%num(mol%id), i=1, product(rep))]
   lattice = reshape(&
      [rep(1)*mol%lattice(:, 1), rep(2)*mol%lattice(:, 2), rep(3)*mol%lattice(:, 3)], &
      shape(mol%lattice))
   allocate(xyz(3, product(rep)*mol%nat))
   c = 0
   do i = 0, rep(1)-1
      do j = 0, rep(2)-1
         do k = 0, rep(3)-1
            xyz(:, c+1:c+mol%nat) = mol%xyz &
               & + outerprod(matmul(mol%lattice, [real(wp):: i, j, k]), &
               & [(1.0_wp, l=1, mol%nat)])
            c = c + mol%nat
         end do
      end do
   end do

   call new(mol, num, xyz, lattice=lattice)
end subroutine make_supercell

subroutine test_grid_gen(error, mol, cutoff, ref_nxyz)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   !> Molecular structure data
   type(structure_type), intent(in) :: mol

   !> Cutoff radius
   real(wp), intent(in) :: cutoff

   !> Reference grid dimensions
   integer, intent(in) :: ref_nxyz(3)

   type(grid_type) :: grid

   integer :: n_xyz(3)
   real(wp) :: lat_inv(3, 3), cell_w(3), det

   if (any(mol%periodic)) then
      call compute_grid(mol=mol, cutoff=cutoff, grid=grid, det=det, lat_inv=lat_inv)
      if (any(grid%n_xyz /= ref_nxyz)) then
         call test_failed(error, "Grid dimensions do not match reference.")
         write(*,*) "Generated grid:", grid%n_xyz
         write(*,*) "Reference grid:", ref_nxyz
      end if
   else
      call compute_grid(mol=mol, cutoff=cutoff, grid=grid, det=det, cell_w=cell_w)
      if (any(grid%n_xyz /= ref_nxyz)) then
         call test_failed(error, "Grid dimensions do not match reference.")
         write(*,*) "Generated grid:", grid%n_xyz
         write(*,*) "Reference grid:", ref_nxyz
      end if

      if (any(cell_w <= cutoff .and. grid%n_xyz > 1)) then
         call test_failed(error, "Linked-cell width is smaller than cutoff radius.")
         write(*,*) "Generated cell_width:", cell_w
         write(*,*) "Cutoff:", cutoff
      end if

   end if

end subroutine test_grid_gen


subroutine test_wsc(error, mol, cutoff, cmp, ref_list, ref_nimg)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   !> Molecular structure data
   type(structure_type), intent(in) :: mol

   !> Cutoff radius
   real(wp), intent(in) :: cutoff

   !> Whether a complete or a symmetrical reduced map should be generated
   logical, intent(in) :: cmp

   !> Reference neighbor list
   integer, intent(in) :: ref_list(:)

   !> Reference array of the number of translation images
   integer, intent(in) :: ref_nimg(:)

   type(csr_list), allocatable :: list
   type(distance_screening) :: screen
   type(wignerseitz_cell) :: wsc
   integer :: iat, jat, ntr, tridx
   integer(i8) :: itr, kat
   real(wp) :: vec(3)

   allocate(list)
   call new_csr_list(list, screen, mol, error, wsc, cutoff=cutoff, complete=cmp)
   if (allocated(error)) return

   do iat = 1, mol%nat
      do kat = list%inl(iat), list%inl(iat+1) - 1
         jat = list%nlat(kat)
         ntr = wsc%nimg_list(kat)
         if (.not. any(jat == ref_list(list%inl(iat): list%inl(iat+1) - 1))) then
            call test_failed(error, "neighbor list array does not match reference.")
            print"(20a)", "Generated list:"
            print"(10i6)", list%nlat
            print"(20a)", "Reference list:"
            print"(10i6)", ref_list
            exit
         end if
         if (.not. ntr == ref_nimg(kat)) then
            call test_failed(error, &
               & "neighbor list WSC translations number array does not match " // &
               & "reference.")
            print"(20a)", "Generated translations:"
            print"(10i6)", wsc%nimg_list
            print"(20a)", "Reference translations:"
            print"(10i6)", ref_nimg
            exit
         end if
         do itr = wsc%itr_list(kat), wsc%itr_list(kat+1) - 1
            tridx = wsc%tridx_list(itr)
            vec = mol%xyz(:, jat) - mol%xyz(:, iat) &
               & + wsc%trans(:, wsc%tridx_list(itr))
            if (sum(vec**2) >= cutoff**2) then
               call test_failed(error, &
                  & "The pair in the neighbor list is outside the cutoff radius.")
               exit
            end if
         end do
      end do
   end do

end subroutine test_wsc


subroutine test_grid_fullerene_cut_1(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer, parameter :: ref_nxyz(3) = [ 13, 13, 13 ]

   type(structure_type) :: mol

   real(wp), parameter :: cutoff = 1.0_wp

   call get_structure(mol, "fullerene")

   call test_grid_gen(error, mol, cutoff, ref_nxyz)

end subroutine test_grid_fullerene_cut_1


subroutine test_grid_water_cut_1(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer, parameter :: ref_nxyz(3) = [ 1, 3, 1 ]

   type(structure_type) :: mol

   real(wp), parameter :: cutoff = 1.0_wp

   call get_structure(mol, "water")

   call test_grid_gen(error, mol, cutoff, ref_nxyz)

end subroutine test_grid_water_cut_1


subroutine test_grid_x02_114(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer, parameter :: ref_nxyz(3) = [ 1, 1, 3 ]

   type(structure_type) :: mol

   real(wp), parameter :: cutoff = 29.0_wp
   integer, parameter :: supercell(3) = [1, 1, 4]

   call get_structure(mol, "x02")
   call make_supercell(mol, supercell)

   call test_grid_gen(error, mol, cutoff, ref_nxyz)

end subroutine test_grid_x02_114


!> Compare a neighbor list entry by entry against the Verlet reference built
!> with the same screening, matching both the neighbor and its translation
subroutine check_list(error, mol, cutoff, trans, cmp, screen)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   !> Molecular structure data
   type(structure_type), intent(in) :: mol

   !> Cutoff radius
   real(wp), intent(in) :: cutoff

   !> Translation vectors for all images
   real(wp), intent(in) :: trans(:, :)

   !> Whether a complete or a symmetrical reduced map should be generated
   logical, intent(in) :: cmp

   !> Screening applied to the neighbor list and the reference
   class(screening_type), intent(inout) :: screen

   type(csr_list) :: list
   integer, allocatable :: ref_ptr(:), ref_list(:), ref_nltr(:)
   integer :: iat, jat, itr, lat
   integer(i8) :: kat, mat
   logical :: found

   call new_csr_list(list, screen, mol, error, cutoff=cutoff, trans=trans, complete=cmp)
   if (allocated(error)) return

   allocate(ref_ptr(mol%nat + 1))
   call gen_verlet(mol, trans, cutoff, ref_ptr, ref_list, ref_nltr, cmp, screen)

   do iat = 1, mol%nat
      call check(error, int(list%inl(iat + 1) - list%inl(iat)), &
         & ref_ptr(iat + 1) - ref_ptr(iat), &
         & "Number of neighbors does not match the reference")
      if (allocated(error)) return

      call check(error, list%nlat(list%inl(iat)), iat, &
         & "Diagonal entry is not stored first in the row")
      if (allocated(error)) return

      do kat = list%inl(iat), list%inl(iat + 1) - 1
         jat = list%nlat(kat)
         itr = 1
         if (allocated(list%nltr)) itr = list%nltr(kat)

         found = .false.
         do lat = ref_ptr(iat), ref_ptr(iat + 1) - 1
            found = found .or. (ref_list(lat) == jat .and. ref_nltr(lat) == itr)
         end do
         if (.not. found) then
            call test_failed(error, "Neighbor list contains a pair missing in the reference")
            print "(a,3i6)", "Atom, neighbor, translation:", iat, jat, itr
            return
         end if

         ! Equal counts and no duplicates make the rows identical
         do mat = kat + 1, list%inl(iat + 1) - 1
            if (list%nlat(mat) /= jat) cycle
            if (allocated(list%nltr)) then
               if (list%nltr(mat) /= itr) cycle
            end if
            call test_failed(error, "Neighbor list contains duplicates")
            return
         end do
      end do
   end do

end subroutine check_list


!> Check the reduced and the complete neighbor list against the reference
subroutine test_list_gen(error, mol, cutoff, screen)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   !> Molecular structure data
   type(structure_type), intent(in) :: mol

   !> Cutoff radius
   real(wp), intent(in) :: cutoff

   !> Screening of the neighbor list, distance screening if absent
   class(screening_type), intent(inout), optional :: screen

   type(distance_screening) :: dist
   real(wp), allocatable :: trans(:, :)
   logical :: cmp
   integer :: imode

   if (any(mol%periodic)) then
      call get_lattice_points(mol%periodic, mol%lattice, cutoff, trans, error)
      if (allocated(error)) return
   else
      allocate(trans(3, 1), source=0.0_wp)
   end if

   do imode = 1, 2
      cmp = imode == 2
      if (present(screen)) then
         call check_list(error, mol, cutoff, trans, cmp, screen)
      else
         call check_list(error, mol, cutoff, trans, cmp, dist)
      end if
      if (allocated(error)) then
         error%message = trim(merge("complete list: ", "reduced list:  ", cmp)) &
            & // " " // error%message
         return
      end if
   end do

end subroutine test_list_gen


!> Molecule within a single linked cell
subroutine test_list_mb01(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol

   call get_structure(mol, "mindless01")
   call test_list_gen(error, mol, 29.0_wp)

end subroutine test_list_mb01


!> Molecule spread over many linked cells
subroutine test_list_fullerene_cut_5(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol

   call get_structure(mol, "fullerene")
   call test_list_gen(error, mol, 5.0_wp)

end subroutine test_list_fullerene_cut_5


!> Periodic cell much smaller than the cutoff, many periodic self images
subroutine test_list_feo2(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol

   call get_structure(mol, "feo2")
   call test_list_gen(error, mol, 29.0_wp)

end subroutine test_list_feo2


!> Elongated supercell with several linked cells along one lattice vector
subroutine test_list_x02_114(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   integer, parameter :: supercell(3) = [1, 1, 4]

   call get_structure(mol, "x02")
   call make_supercell(mol, supercell)
   call test_list_gen(error, mol, 29.0_wp)

end subroutine test_list_x02_114


subroutine test_list_slab(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol

   call get_slab(mol)
   call test_list_gen(error, mol, 3.5_wp)
   if (allocated(error)) return

   ! Wrapped atoms and three cells along the periodic directions use the
   ! direct translation lookup of the linked cells
   call wrap_to_central_cell(mol%xyz, mol%lattice, mol%periodic)
   call test_list_gen(error, mol, 3.5_wp)

end subroutine test_list_slab


subroutine test_list_wire(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol

   call get_wire(mol)
   call test_list_gen(error, mol, 6.0_wp)
   if (allocated(error)) return

   ! Wrapped atoms and three cells along the periodic direction use the
   ! direct translation lookup of the linked cells
   call wrap_to_central_cell(mol%xyz, mol%lattice, mol%periodic)
   call test_list_gen(error, mol, 3.3_wp)

end subroutine test_list_wire


subroutine test_feo2_wsc(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   real(wp), parameter :: cutoff = 29.0_wp

   ! Reference neighbor list
   integer, parameter :: ref_list(6) = [1, 3, 2, 2, 3, 3]
   ! Reference array of the number of translation images
   integer, parameter :: ref_nimg(6) = [6, 3, 3, 6, 3, 6]

   call get_structure(mol, "feo2")
   call test_wsc(error, mol, cutoff, .false., ref_list, ref_nimg)

end subroutine test_feo2_wsc


!> Compare the CSR Wigner-Seitz images with the dense Wigner-Seitz cell
subroutine test_wsc_parity_gen(error, mol, cutoff)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   !> Molecular structure data
   type(structure_type), intent(in) :: mol

   !> Cutoff radius
   real(wp), intent(in) :: cutoff

   type(csr_list) :: list
   type(distance_screening) :: screen
   type(wignerseitz_cell) :: dense, wsc
   integer :: iat, jat, img, jmg, nimg, nref
   integer(i8) :: kat, itr
   real(wp) :: rij(3), vec(3), ref(3), r2
   logical :: found

   call new_wignerseitz_cell(dense, mol, error)
   if (allocated(error)) return

   call new_csr_list(list, screen, mol, error, wsc, cutoff=cutoff, complete=.true.)
   if (allocated(error)) return

   do iat = 1, mol%nat
      ! Every pair whose nearest image lies within the cutoff must be listed
      nref = 0
      do jat = 1, mol%nat
         if (jat == iat .or. dense%nimg(jat, iat) == 0) cycle
         rij = mol%xyz(:, iat) - mol%xyz(:, jat)
         r2 = sum((rij - dense%trans(:, dense%tridx(1, jat, iat)))**2)
         if (r2 <= cutoff**2) nref = nref + 1
      end do
      call check(error, int(list%inl(iat + 1) - list%inl(iat)) - 1, nref)
      if (allocated(error)) return

      do kat = list%inl(iat), list%inl(iat + 1) - 1
         jat = list%nlat(kat)
         nimg = wsc%nimg_list(kat)
         ! Diagonal entries carry no images if the self images lie beyond the cutoff
         if (jat == iat .and. nimg == 0) cycle

         call check(error, nimg, dense%nimg(jat, iat))
         if (allocated(error)) return

         ! Compare the physical image vectors, which are independent of the
         ! translation ordering
         rij = mol%xyz(:, iat) - mol%xyz(:, jat)
         do img = 0, nimg - 1
            itr = wsc%itr_list(kat) + img
            vec = rij - wsc%trans(:, wsc%tridx_list(itr))
            found = .false.
            do jmg = 1, dense%nimg(jat, iat)
               ref = rij - dense%trans(:, dense%tridx(jmg, jat, iat))
               found = found .or. maxval(abs(vec - ref)) < thr2
            end do
            if (.not. found) then
               call test_failed(error, "CSR Wigner-Seitz image not selected by dense cell")
               return
            end if
         end do
      end do
   end do

end subroutine test_wsc_parity_gen


subroutine test_wsc_parity_skewed(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol

   integer, parameter :: nat = 3
   integer, parameter :: num(nat) = [6, 8, 1]
   real(wp), parameter :: xyz(3, nat) = reshape([ &
      &  0.0_wp, 0.0_wp, 0.0_wp, &
      & 12.5_wp, 0.5_wp, 0.0_wp, &
      & -7.3_wp, 0.9_wp, 11.2_wp], &
      & [3, nat])
   real(wp), parameter :: lattice(3, 3) = reshape([ &
      &  5.0_wp, 0.0_wp, 0.0_wp, &
      & 25.0_wp, 1.0_wp, 0.0_wp, &
      &  0.0_wp, 0.0_wp, 5.0_wp], &
      & [3, 3])

   ! Strongly skewed cell, nearest images lie several translations away
   call new(mol, num, xyz, lattice=lattice, periodic=[.true., .true., .true.])
   call test_wsc_parity_gen(error, mol, 29.0_wp)

end subroutine test_wsc_parity_skewed


subroutine test_wsc_parity_dense_images(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol

   integer, parameter :: nat = 2
   integer, parameter :: num(nat) = [6, 8]
   real(wp), parameter :: xyz(3, nat) = reshape([ &
      & 0.0_wp, 0.0_wp, 0.0_wp, &
      & 0.125_wp, 0.125_wp, 0.05_wp], &
      & [3, nat])
   real(wp), parameter :: lattice(3, 3) = reshape([ &
      & 0.25_wp, 0.0_wp, 0.0_wp, &
      & 0.0_wp, 0.25_wp, 0.0_wp, &
      & 0.0_wp, 0.0_wp, 0.25_wp], &
      & [3, 3])

   ! Small cubic cell, more than 27 images compete within the switching interval
   call new(mol, num, xyz, lattice=lattice, periodic=[.true., .true., .true.])
   call test_wsc_parity_gen(error, mol, 29.0_wp)

end subroutine test_wsc_parity_dense_images


!> Slab periodic in two directions with unwrapped atoms spread perpendicular
!> to the periodic plane, the nonperiodic lattice vector is zero. Atoms 1 and 3
!> are neighbors across a periodic boundary of the central cell.
subroutine get_slab(mol)

   !> Molecular structure data
   type(structure_type), intent(out) :: mol

   integer, parameter :: nat = 8
   integer, parameter :: num(nat) = [6, 8, 1, 1, 6, 8, 1, 7]
   real(wp), parameter :: xyz(3, nat) = reshape([ &
      &  0.5_wp,  0.3_wp,  0.0_wp, &
      &  3.1_wp,  1.8_wp,  2.5_wp, &
      & 23.3_wp,  0.8_wp, -1.2_wp, &
      & -5.6_wp,  7.7_wp,  6.8_wp, &
      &  8.0_wp, -9.5_wp, 12.3_wp, &
      &  2.2_wp, 20.1_wp, 18.9_wp, &
      & 10.1_wp,  5.5_wp, 25.0_wp, &
      & -1.3_wp, -2.6_wp, 29.4_wp], &
      & [3, nat])
   real(wp), parameter :: lattice(3, 3) = reshape([ &
      & 12.4_wp, 0.0_wp, 0.0_wp, &
      & 6.2_wp, 10.7387_wp, 0.0_wp, &
      & 0.0_wp, 0.0_wp, 0.0_wp], &
      & [3, 3])

   call new(mol, num, xyz, lattice=lattice, periodic=[.true., .true., .false.])

end subroutine get_slab


!> Wire periodic along the second lattice vector with unwrapped atoms, the
!> nonperiodic lattice vectors are zero and arbitrary. Atoms 1 and 7 are
!> neighbors across a periodic boundary of the central cell.
subroutine get_wire(mol)

   !> Molecular structure data
   type(structure_type), intent(out) :: mol

   integer, parameter :: nat = 8
   integer, parameter :: num(nat) = [6, 8, 1, 1, 6, 8, 1, 7]
   real(wp), parameter :: xyz(3, nat) = reshape([ &
      &  0.0_wp,  0.0_wp,  0.0_wp, &
      &  1.5_wp, -0.5_wp,  2.0_wp, &
      &  7.2_wp,  9.1_wp, -1.5_wp, &
      & -4.0_wp, -7.5_wp,  3.3_wp, &
      &  2.5_wp,  1.0_wp, -6.0_wp, &
      & 13.0_wp, 15.5_wp,  4.4_wp, &
      & -7.0_wp, -8.5_wp,  0.8_wp, &
      &  5.5_wp, -2.2_wp, -3.7_wp], &
      & [3, nat])
   real(wp), parameter :: lattice(3, 3) = reshape([ &
      & 0.0_wp, 0.0_wp, 0.0_wp, &
      & 6.0_wp, 8.0_wp, 0.0_wp, &
      & 0.0_wp, 0.0_wp, 60.0_wp], &
      & [3, 3])

   call new(mol, num, xyz, lattice=lattice, periodic=[.false., .true., .false.])

end subroutine get_wire


subroutine test_wsc_parity_slab(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol

   ! Cutoffs giving three, two, and one linked cells along the periodic directions
   call get_slab(mol)
   call test_wsc_parity_gen(error, mol, 3.5_wp)
   if (allocated(error)) return

   call test_wsc_parity_gen(error, mol, 5.0_wp)
   if (allocated(error)) return

   call test_wsc_parity_gen(error, mol, 29.0_wp)

end subroutine test_wsc_parity_slab


subroutine test_wsc_parity_wire(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol

   call get_wire(mol)
   call test_wsc_parity_gen(error, mol, 6.0_wp)

end subroutine test_wsc_parity_wire


!> Reject pairs containing the excluded element, otherwise screen by distance
subroutine check_pair_element(self, mol, iatom, jatom, r2, include)

   !> Element screening instance
   class(element_screening), intent(in) :: self

   !> Structure type
   type(structure_type), intent(in) :: mol

   !> Index of the first atom
   integer, intent(in) :: iatom

   !> Index of the second atom
   integer, intent(in) :: jatom

   !> Squared distance between the two atoms
   real(wp), intent(in) :: r2

   !> Return true if the pair should be included
   logical, intent(out) :: include

   call self%distance_screening%check_pair(mol, iatom, jatom, r2, include)
   include = include .and. mol%num(mol%id(iatom)) /= self%exclude &
      & .and. mol%num(mol%id(jatom)) /= self%exclude

end subroutine check_pair_element


!> Include a pair within the user-defined distance threshold
subroutine check_pair_radius(self, mol, iatom, jatom, r2, include)

   !> Radius screening instance
   class(radius_screening), intent(in) :: self

   !> Structure type
   type(structure_type), intent(in) :: mol

   !> Index of the first atom
   integer, intent(in) :: iatom

   !> Index of the second atom
   integer, intent(in) :: jatom

   !> Squared distance between the two atoms
   real(wp), intent(in) :: r2

   !> Return true if the pair should be included
   logical, intent(out) :: include

   include = r2 <= self%thresh

end subroutine check_pair_radius


!> Reject every pair
subroutine check_pair_reject(self, mol, iatom, jatom, r2, include)

   !> Reject screening instance
   class(reject_screening), intent(in) :: self

   !> Structure type
   type(structure_type), intent(in) :: mol

   !> Index of the first atom
   integer, intent(in) :: iatom

   !> Index of the second atom
   integer, intent(in) :: jatom

   !> Squared distance between the two atoms
   real(wp), intent(in) :: r2

   !> Return true if the pair should be included
   logical, intent(out) :: include

   include = .false.

end subroutine check_pair_reject


!> Compare a screened Wigner-Seitz neighbor list against the dense
!> Wigner-Seitz cell with the screening applied to the nearest images
subroutine test_screen_wsc_gen(error, mol, cutoff, screen)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   !> Molecular structure data
   type(structure_type), intent(in) :: mol

   !> Cutoff radius
   real(wp), intent(in) :: cutoff

   !> Screening applied to the neighbor list and the reference
   class(screening_type), intent(inout) :: screen

   type(csr_list) :: list
   type(wignerseitz_cell) :: dense, wsc
   integer :: iat, jat, nimg, nref, nexp
   integer(i8) :: kat
   real(wp) :: rij(3), r2
   logical :: include

   call new_wignerseitz_cell(dense, mol, error)
   if (allocated(error)) return

   call new_csr_list(list, screen, mol, error, wsc, cutoff=cutoff, complete=.true.)
   if (allocated(error)) return

   do iat = 1, mol%nat
      ! Every screened pair whose nearest image lies within the cutoff is listed
      nref = 0
      do jat = 1, mol%nat
         if (jat == iat .or. dense%nimg(jat, iat) == 0) cycle
         rij = mol%xyz(:, iat) - mol%xyz(:, jat)
         r2 = sum((rij - dense%trans(:, dense%tridx(1, jat, iat)))**2)
         if (r2 > cutoff**2) cycle
         call screen%check_pair(mol, iat, jat, r2, include)
         if (include) nref = nref + 1
      end do
      call check(error, int(list%inl(iat + 1) - list%inl(iat)) - 1, nref, &
         & "Number of neighbors does not match the screened reference")
      if (allocated(error)) return

      do kat = list%inl(iat), list%inl(iat + 1) - 1
         jat = list%nlat(kat)
         nimg = wsc%nimg_list(kat)

         ! The image offsets must agree with the stored image count
         call check(error, int(wsc%itr_list(kat + 1) - wsc%itr_list(kat)), nimg, &
            & "Image offsets do not match the image count of a pair")
         if (allocated(error)) return

         ! Expected image count, zero if the nearest image is screened out
         nexp = 0
         if (dense%nimg(jat, iat) > 0) then
            rij = mol%xyz(:, iat) - mol%xyz(:, jat)
            r2 = sum((rij - dense%trans(:, dense%tridx(1, jat, iat)))**2)
            call screen%check_pair(mol, iat, jat, r2, include)
            if (include .and. r2 <= cutoff**2) nexp = dense%nimg(jat, iat)
         end if
         if (jat /= iat .and. nexp == 0) then
            call test_failed(error, "Screened Wigner-Seitz list contains a rejected pair")
            print "(a,2i6)", "Atom, neighbor:", iat, jat
            return
         end if

         call check(error, nimg, nexp, &
            & "Image count does not match the screened dense Wigner-Seitz cell")
         if (allocated(error)) return
      end do
   end do

end subroutine test_screen_wsc_gen


!> The distance screening threshold follows the cutoff, also for extended
!> types, while other screenings keep their user-defined threshold
subroutine test_screen_distance_thresh(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   type(csr_list) :: list
   type(distance_screening) :: dist
   type(element_screening) :: elem
   type(radius_screening) :: radius
   real(wp), parameter :: cutoff = 7.0_wp

   call get_structure(mol, "feo2")

   dist%thresh = 1.0_wp
   call new_csr_list(list, dist, mol, error, cutoff=cutoff)
   if (allocated(error)) return

   call check(error, dist%thresh, cutoff**2, thr=thr, &
      & message="Distance screening threshold is not the squared cutoff")
   if (allocated(error)) return

   elem%thresh = 1.0_wp
   call new_csr_list(list, elem, mol, error, cutoff=cutoff)
   if (allocated(error)) return

   call check(error, elem%thresh, cutoff**2, thr=thr, &
      & message="Extended distance screening threshold is not the squared cutoff")
   if (allocated(error)) return

   radius%thresh = 4.0_wp
   call new_csr_list(list, radius, mol, error, cutoff=cutoff)
   if (allocated(error)) return

   call check(error, radius%thresh, 4.0_wp, thr=thr, &
      & message="User-defined screening threshold was overwritten")

end subroutine test_screen_distance_thresh


!> Element screening on a molecule spread over several linked cells
subroutine test_screen_element_mb01(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   type(element_screening) :: screen

   call get_structure(mol, "mindless01")

   ! Remove every pair with a hydrogen atom
   screen%exclude = 1
   call test_list_gen(error, mol, 5.0_wp, screen)

end subroutine test_screen_element_mb01


!> Element screening rejecting the periodic self images of an element
subroutine test_screen_element_feo2(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   type(element_screening) :: screen

   call get_structure(mol, "feo2")

   ! Remove every pair with an oxygen atom
   screen%exclude = 8
   call test_list_gen(error, mol, 12.0_wp, screen)

end subroutine test_screen_element_feo2


!> User-defined threshold well below the cutoff of the linked cells
subroutine test_screen_radius_nacl(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   type(radius_screening) :: screen

   call get_structure(mol, "nacl")

   screen%thresh = 6.0_wp**2
   call test_list_gen(error, mol, 15.0_wp, screen)

end subroutine test_screen_radius_nacl


!> Rejecting every pair leaves only the diagonal, also for periodic self images
subroutine test_screen_reject_feo2(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   type(reject_screening) :: screen

   call get_structure(mol, "feo2")
   call test_list_gen(error, mol, 12.0_wp, screen)

end subroutine test_screen_reject_feo2


!> Screened Wigner-Seitz list, the diagonal of the oxygen atoms is rejected
!> together with its periodic self images
subroutine test_screen_wsc_element_feo2(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   type(element_screening) :: screen

   call get_structure(mol, "feo2")

   screen%exclude = 8
   call test_screen_wsc_gen(error, mol, 29.0_wp, screen)

end subroutine test_screen_wsc_element_feo2


end module test_csrlist
