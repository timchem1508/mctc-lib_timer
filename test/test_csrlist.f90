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

module test_csrlist
   use iso_fortran_env, only : int64
   use mctc_env, only : wp, timer_type, format_time
   use mctc_io_resize, only : resize
   use mctc_io_structure, only: structure_type, new
   use mctc_cutoff, only: get_lattice_points
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, &
   & test_failed, check
   use testsuite_structure, only : get_structure
   use mctc_csrlist, only : csr_list, new_csr_list, compute_grid, get_linked_cell, gemv_cmp
   implicit none
   private

   public :: collect_csrlist


   real(wp), parameter :: thr = sqrt(epsilon(1.0_wp))

contains


!> Collect all exported unit tests
   subroutine collect_csrlist(testsuite)

      !> Collection of tests
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
      & new_unittest("grid-methane", test_grid_methane), &
      & new_unittest("grid-water", test_grid_water), &
      & new_unittest("grid-fullerene-cut-1", test_grid_fullerene_cut_1), &
      & new_unittest("grid-water-cut-1", test_grid_water_cut_1), &
      & new_unittest("csr-vs-verlet-water", test_list_water), &
      & new_unittest("csr-vs-verlet-methane", test_list_methane), &
      & new_unittest("csr-vs-verlet-fullerene", test_list_fullerene), &
      & new_unittest("distance-fullerene-cut-5", test_distance_fullerene_cut_5), &
      & new_unittest("csr-vs-verlet-mb01", test_list_mb01), &
      & new_unittest("csr-vs-verlet-mb02", test_list_mb02), &
      & new_unittest("csr-vs-verlet-water-complete", test_list_water_complete), &
      & new_unittest("csr-vs-verlet-methane-complete", test_list_methane_complete), &
      & new_unittest("csr-vs-verlet-fullerene-complete", test_list_fullerene_complete), &
      & new_unittest("csr-vs-verlet-mb09-complete", test_list_mb09_complete), &
      & new_unittest("csr-vs-verlet-mb10-complete", test_list_mb10_complete), &
      & new_unittest("grid-nacl", test_grid_nacl), &
      & new_unittest("grid-feo2", test_grid_feo2), &
      & new_unittest("distance-feo2-cut-5", test_distance_feo2_cut_5), &
      & new_unittest("csr-vs-verlet-nacl", test_list_nacl), &
      & new_unittest("csr-vs-verlet-feo2", test_list_feo2), &
      & new_unittest("csr-vs-verlet-x01", test_list_x01), &
      & new_unittest("csr-vs-verlet-x02", test_list_x02), &
      & new_unittest("grid-x02-1x1x4", test_grid_x02_114), &
      & new_unittest("csr-vs-verlet-x02-1x1x4", test_list_x02_114), &
      & new_unittest("csr-vs-verlet-nacl-complete", test_list_nacl_complete), &
      & new_unittest("csr-vs-verlet-feo2-complete", test_list_feo2_complete), &
      & new_unittest("csr-vs-verlet-x04-complete", test_list_x04_complete), &
      & new_unittest("csr-vs-verlet-x05-complete", test_list_x05_complete) &
      & ]

   end subroutine collect_csrlist

!> Generate reference 1-based CSR list
   subroutine gen_verlet(mol, trans, cutoff, row_ptr, col_ind, nltr, complete)
      !> Molecular structure data
      type(structure_type), intent(in) :: mol
      !> Translation vectors for all images
      real(wp), intent(in) :: trans(:, :)
      !> Realspace cutoff for neighbourlist generation
      real(wp), intent(in) :: cutoff
      !> Standard 1-based row pointer array of size (nat + 1)
      integer, intent(inout) :: row_ptr(:)
      !> Column indices / neighbouring atom indices (nlat)
      integer, allocatable, intent(out) :: col_ind(:)
      !> Cell index of the neighbouring atom
      integer, allocatable, intent(out) :: nltr(:)
      !> Whether a complete or a symmetrical upper-triangular map should be generated
      logical, intent(in) :: complete

      integer :: iat, jat, itr, nnz
      real(wp) :: r2, vec(3), cutoff2

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

         ! 2. Off-Diagonal Neighbours (inverse to fit the linked-cell list ordering)
         do jat = 1, mol%nat
            ! Skip lower triangle if incomplete
            if (.not. complete .and. jat < iat) cycle

            do itr = 1, size(trans, 2)
               ! Skip identity self-interaction (already added as diagonal)
               if (iat == jat .and. itr == 1) cycle

               vec(:) = mol%xyz(:, iat) - mol%xyz(:, jat) - trans(:, itr)
               r2 = sum(vec**2)
               if (r2 < epsilon(cutoff2) .or. r2 > cutoff2) cycle

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

   subroutine make_supercell(mol, rep)
      type(structure_type), intent(inout) :: mol
      integer, intent(in) :: rep(3)

      real(wp), allocatable :: xyz(:, :), lattice(:, :)
      integer, allocatable :: num(:)
      integer :: i, j, k, c

      num = reshape(spread([mol%num(mol%id)], 2, product(rep)), [product(rep)*mol%nat])
      lattice = reshape(&
         [rep(1)*mol%lattice(:, 1), rep(2)*mol%lattice(:, 2), rep(3)*mol%lattice(:, 3)], &
         shape(mol%lattice))
      allocate(xyz(3, product(rep)*mol%nat))
      c = 0
      do i = 0, rep(1)-1
         do j = 0, rep(2)-1
            do k = 0, rep(3)-1
               xyz(:, c+1:c+mol%nat) = mol%xyz &
               & + spread(matmul(mol%lattice, [real(wp):: i, j, k]), 2, mol%nat)
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

      integer :: n_xyz(3)
      real(wp) :: lat_inv(3, 3), cell_w(3), det

      if (any(mol%periodic)) then
         call compute_grid(mol=mol, cutoff=cutoff, det=det, n_xyz=n_xyz, lat_inv=lat_inv)
         if (any(n_xyz /= ref_nxyz)) then
            call test_failed(error, "Grid dimensions do not match reference.")
            write(*,*) "Generated grid:", n_xyz
            write(*,*) "Reference grid:", ref_nxyz
         end if
      else
         call compute_grid(mol=mol, cutoff=cutoff, det=det, n_xyz=n_xyz, cell_w=cell_w)
         if (any(n_xyz /= ref_nxyz)) then
            call test_failed(error, "Grid dimensions do not match reference.")
            write(*,*) "Generated grid:", n_xyz
            write(*,*) "Reference grid:", ref_nxyz
         end if

         if (any(cell_w <= cutoff .and. n_xyz > 1)) then
            call test_failed(error, "Linked-cell width is smaller than cutoff radius.")
            write(*,*) "Generated cell_width:", cell_w
            write(*,*) "Cutoff:", cutoff
         end if

      end if

   end subroutine test_grid_gen

   subroutine test_distance(error, mol, cutoff, trans, cmp)
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

      type(csr_list), allocatable   :: list

      real(wp) :: vec(3)
      integer :: iat, jat, kat, itr

      allocate(list)
      call new_csr_list(list, mol, cutoff, trans, cmp)

      do iat = 1, mol%nat
         do kat = list%inl(iat) + 1, list%inl(iat+1) -1
            jat = list%nlat(kat)
            if (allocated(list%nltr)) then
               itr = list%nltr(kat)
            else
               itr = 1
            end if
            vec(:) = mol%xyz(:, iat) - mol%xyz(:, jat) - trans(:, itr)
            if (sum(vec**2) >= cutoff**2) then
               call test_failed(error, "The pair in the neighbour list is outside the cutoff radius.")
               exit
            end if
         end do
      end do


   end subroutine test_distance

   subroutine test_mol_list_gen(error, mol, cutoff, trans, cmp)
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

      type(csr_list), allocatable   :: list

      !> Reference Lists
      integer, allocatable :: ref_ptr(:), ref_list(:), ref_nltr(:)

      allocate(list)
      call new_csr_list(list, mol, cutoff, trans, cmp)
      call sort_csr_rows(list%inl, list%nlat)

      allocate(ref_ptr(mol%nat + 1))
      call gen_verlet(mol, trans, cutoff, ref_ptr, ref_list, ref_nltr, cmp)

      if (any(list%inl /= ref_ptr)) then
         call test_failed(error, "Neighbour list pointer array does not match reference.")
         write(*,*) "Generated pointer:", list%inl
         write(*,*) "Reference pointer:", ref_ptr
      end if

      if (any(list%nlat /= ref_list)) then
         call test_failed(error, "Neighbour list array does not match reference.")
         write(*,*) "Generated list:", list%nlat
         write(*,*) "Reference list:", ref_list
      end if

   end subroutine test_mol_list_gen

   subroutine test_pbc_list_gen(error, mol, cutoff, trans, cmp)
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

      type(csr_list), allocatable   :: list

      !> Reference Lists
      integer, allocatable :: ref_ptr(:), ref_list(:), ref_nltr(:)

      allocate(list)
      call new_csr_list(list, mol, cutoff, trans, cmp)
      call sort_csr_rows(list%inl, list%nlat, list%nltr)

      allocate(ref_ptr(mol%nat + 1))
      call gen_verlet(mol, trans, cutoff, ref_ptr, ref_list, ref_nltr, cmp)

      if (any(list%inl /= ref_ptr)) then
         call test_failed(error, "Neighbour list pointer array does not match reference.")
         write(*,*) "Generated pointer:", list%inl
         write(*,*) "Reference pointer:", ref_ptr
      end if

      if (any(list%nlat /= ref_list)) then
         call test_failed(error, "Neighbour list array does not match reference.")
         write(*,*) "Generated list:", list%nlat
         write(*,*) "Reference list:", ref_list
      end if

      if (any(list%nltr /= ref_nltr)) then
         call test_failed(error, "Neighbour list translations array does not match reference.")
         write(*,*) "Generated translations:", list%nltr
         write(*,*) "Reference translations:", ref_nltr
      end if

   end subroutine test_pbc_list_gen

   subroutine sort_csr_rows(row_ptr, col_ind, nltr)
      integer, intent(in) :: row_ptr(:)
      integer, intent(inout) :: col_ind(:)
      integer, intent(inout), optional :: nltr(:)
      integer :: i, start_idx, end_idx

      do i = 1, size(row_ptr) - 1
         start_idx = row_ptr(i) + 1
         end_idx   = row_ptr(i+1) - 1
         if (end_idx > start_idx) then
            if (present(nltr)) then
               call sort_pair(col_ind(start_idx:end_idx), nltr(start_idx:end_idx))
            else
               call sort_single(col_ind(start_idx:end_idx))
            end if
         end if
      end do
   end subroutine sort_csr_rows

   pure subroutine sort_pair(keys, vals)
      integer, intent(inout) :: keys(:), vals(:)
      integer :: i, j, key_item, val_item
      integer :: n
      n = size(keys)
      do i = 2, n
         key_item = keys(i)
         val_item = vals(i)
         j = i - 1
         do while (j >= 1)
            if (keys(j) <= key_item) exit
            keys(j + 1) = keys(j)
            vals(j + 1) = vals(j)
            j = j - 1
         end do
         keys(j + 1) = key_item
         vals(j + 1) = val_item
      end do
      i = 1
      do while (i <= n)
         j = i
         do while (j <= n)
            if (keys(j) /= keys(i)) exit
            j = j + 1
         end do
         if (j - i > 1) then
            call sort_single(vals(i:j-1))
         end if
         i = j
      end do
   end subroutine sort_pair

   pure subroutine sort_single(arr)
      integer, intent(inout) :: arr(:)
      integer :: i, j, item, n

      n = size(arr)
      do i = 2, n
         item = arr(i)
         j = i - 1
         do while (j >= 1)
            if (arr(j) <= item) exit
            arr(j + 1) = arr(j)
            j = j - 1
         end do
         arr(j + 1) = item
      end do
   end subroutine sort_single

   subroutine test_grid_water(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      !> Reference linked-cell grid dimensions
      integer, parameter :: ref_nxyz(3) = [ 1, 1, 1 ]

      type(structure_type) :: mol
      real(wp), parameter :: cutoff = 29.0_wp
      real(wp), allocatable :: trans(:, :)

      allocate(trans(3, 1))
      trans = 0.0_wp

      call get_structure(mol, "water")

      call test_grid_gen(error, mol, cutoff, ref_nxyz)

   end subroutine test_grid_water

   subroutine test_grid_water_cut_1(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      !> Reference linked-cell grid dimensions
      integer, parameter :: ref_nxyz(3) = [ 1, 3, 1 ]

      type(structure_type) :: mol
      real(wp), parameter :: cutoff = 1.0_wp
      real(wp), allocatable :: trans(:, :)

      allocate(trans(3, 1))
      trans = 0.0_wp

      call get_structure(mol, "water")

      call test_grid_gen(error, mol, cutoff, ref_nxyz)

   end subroutine test_grid_water_cut_1

   subroutine test_grid_fullerene_cut_1(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      !> Reference linked-cell grid dimensions
      integer, parameter :: ref_nxyz(3) = [ 13, 13, 13 ]

      type(structure_type) :: mol
      real(wp), parameter :: cutoff = 1.0_wp
      real(wp), allocatable :: trans(:, :)

      allocate(trans(3, 1))
      trans = 0.0_wp

      call get_structure(mol, "fullerene")

      call test_grid_gen(error, mol, cutoff, ref_nxyz)

   end subroutine test_grid_fullerene_cut_1

   subroutine test_grid_methane(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      !> Reference linked-cell grid dimensions
      integer, parameter :: ref_nxyz(3) = [ 1, 1, 1 ]

      type(structure_type) :: mol
      real(wp), parameter :: cutoff = 29.0_wp
      real(wp), allocatable :: trans(:, :)

      allocate(trans(3, 1))
      trans = 0.0_wp

      call get_structure(mol, "methane")

      call test_grid_gen(error, mol, cutoff, ref_nxyz)

   end subroutine test_grid_methane

   subroutine test_list_water(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      real(wp), parameter :: cutoff = 29.0_wp
      real(wp), allocatable :: trans(:, :)

      integer, parameter :: ref_ptr(4) = [&
      & 1, 4, 6, 7]
      integer, parameter :: ref_list(6) = [&
      & 1, 2, 3, 2, 3, 3]

      allocate(trans(3, 1))
      trans = 0.0_wp

      call get_structure(mol, "water")

      call test_mol_list_gen(error, mol, cutoff, trans=trans, cmp=.false.)

   end subroutine test_list_water

   subroutine test_list_methane(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      real(wp), parameter :: cutoff = 29.0_wp
      real(wp), allocatable :: trans(:, :)

      integer, parameter :: ref_ptr(6) = [&
      & 1, 6, 10, 13, 15, 16]
      integer, parameter :: ref_list(15) = [&
      & 1, 2, 3, 4, 5, 2, 3, 4, 5, 3, &
      & 4, 5, 4, 5, 5]

      allocate(trans(3, 1))
      trans = 0.0_wp

      call get_structure(mol, "methane")

      call test_mol_list_gen(error, mol, cutoff, trans=trans, cmp=.false.)

   end subroutine test_list_methane

   subroutine test_list_fullerene(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      real(wp), parameter :: cutoff = 29.0_wp
      real(wp), allocatable :: trans(:, :)

      allocate(trans(3, 1))
      trans = 0.0_wp

      call get_structure(mol, "fullerene")

      call test_mol_list_gen(error, mol, cutoff, trans=trans, cmp=.false.)

   end subroutine test_list_fullerene

   subroutine test_distance_fullerene_cut_5(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      real(wp), parameter :: cutoff = 5.0_wp
      real(wp), allocatable :: trans(:, :)

      allocate(trans(3, 1))
      trans = 0.0_wp

      call get_structure(mol, "fullerene")

      call test_distance(error, mol, cutoff, trans=trans, cmp=.false.)

   end subroutine test_distance_fullerene_cut_5

   subroutine test_list_mb01(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      real(wp), parameter :: cutoff = 29.0_wp
      real(wp), allocatable :: trans(:, :)

      allocate(trans(3, 1))
      trans = 0.0_wp

      call get_structure(mol, "mindless01")

      call test_mol_list_gen(error, mol, cutoff, trans=trans, cmp=.false.)

   end subroutine test_list_mb01

   subroutine test_list_mb02(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      real(wp), parameter :: cutoff = 29.0_wp
      real(wp), allocatable :: trans(:, :)

      allocate(trans(3, 1))
      trans = 0.0_wp

      call get_structure(mol, "mindless02")

      call test_mol_list_gen(error, mol, cutoff, trans=trans, cmp=.false.)

   end subroutine test_list_mb02

   subroutine test_list_water_complete(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      real(wp), parameter :: cutoff = 29.0_wp
      real(wp), allocatable :: trans(:, :)

      allocate(trans(3, 1))
      trans = 0.0_wp

      call get_structure(mol, "water")

      call test_mol_list_gen(error, mol, cutoff, trans=trans, cmp=.true.)

   end subroutine test_list_water_complete

   subroutine test_list_methane_complete(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      real(wp), parameter :: cutoff = 29.0_wp
      real(wp), allocatable :: trans(:, :)

      allocate(trans(3, 1))
      trans = 0.0_wp

      call get_structure(mol, "methane")

      call test_mol_list_gen(error, mol, cutoff, trans=trans, cmp=.true.)

   end subroutine test_list_methane_complete

   subroutine test_list_fullerene_complete(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      real(wp), parameter :: cutoff = 29.0_wp
      real(wp), allocatable :: trans(:, :)

      allocate(trans(3, 1))
      trans = 0.0_wp

      call get_structure(mol, "fullerene")

      call test_mol_list_gen(error, mol, cutoff, trans=trans, cmp=.true.)

   end subroutine test_list_fullerene_complete

   subroutine test_list_mb09_complete(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      real(wp), parameter :: cutoff = 29.0_wp
      real(wp), allocatable :: trans(:, :)

      allocate(trans(3, 1))
      trans = 0.0_wp

      call get_structure(mol, "mindless09")

      call test_mol_list_gen(error, mol, cutoff, trans=trans, cmp=.true.)

   end subroutine test_list_mb09_complete

   subroutine test_list_mb10_complete(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      real(wp), parameter :: cutoff = 29.0_wp
      real(wp), allocatable :: trans(:, :)

      allocate(trans(3, 1))
      trans = 0.0_wp

      call get_structure(mol, "mindless10")

      call test_mol_list_gen(error, mol, cutoff, trans=trans, cmp=.true.)

   end subroutine test_list_mb10_complete

   subroutine test_grid_nacl(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      !> Reference linked-cell grid dimensions
      integer, parameter :: ref_nxyz(3) = [ 1, 1, 1 ]

      type(structure_type) :: mol
      real(wp), parameter :: cutoff = 29.0_wp
      real(wp), allocatable :: trans(:, :)

      call get_structure(mol, "nacl")
      call get_lattice_points(mol%periodic, mol%lattice, cutoff, trans)

      call test_grid_gen(error, mol, cutoff, ref_nxyz)

   end subroutine test_grid_nacl

   subroutine test_grid_feo2(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      !> Reference linked-cell grid dimensions
      integer, parameter :: ref_nxyz(3) = [ 1, 1, 1 ]

      type(structure_type) :: mol
      real(wp), parameter :: cutoff = 29.0_wp
      real(wp), allocatable :: trans(:, :)

      call get_structure(mol, "feo2")
      call get_lattice_points(mol%periodic, mol%lattice, cutoff, trans)

      call test_grid_gen(error, mol, cutoff, ref_nxyz)

   end subroutine test_grid_feo2

   subroutine test_grid_x02_114(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      !> Reference linked-cell grid dimensions
      integer, parameter :: ref_nxyz(3) = [ 1, 1, 3 ]

      type(structure_type) :: mol
      real(wp), parameter :: cutoff = 29.0_wp
      real(wp), allocatable :: trans(:, :)
      integer, parameter :: supercell(*) = [1, 1, 4]

      call get_structure(mol, "x02")
      call make_supercell(mol, supercell)
      call get_lattice_points(mol%periodic, mol%lattice, cutoff, trans)

      call test_grid_gen(error, mol, cutoff, ref_nxyz)

   end subroutine test_grid_x02_114

   subroutine test_distance_feo2_cut_5(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      real(wp), parameter :: cutoff = 5.0_wp
      real(wp), allocatable :: trans(:, :)

      call get_structure(mol, "feo2")
      call get_lattice_points(mol%periodic, mol%lattice, cutoff, trans)

      call test_distance(error, mol, cutoff, trans=trans, cmp=.false.)

   end subroutine test_distance_feo2_cut_5

   subroutine test_list_nacl(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      real(wp), parameter :: cutoff = 29.0_wp
      real(wp), allocatable :: trans(:, :)

      call get_structure(mol, "nacl")
      call get_lattice_points(mol%periodic, mol%lattice, cutoff, trans)

      call test_pbc_list_gen(error, mol, cutoff, trans=trans, cmp=.false.)

   end subroutine test_list_nacl

   subroutine test_list_feo2(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      real(wp), parameter :: cutoff = 29.0_wp
      real(wp), allocatable :: trans(:, :)

      call get_structure(mol, "feo2")
      call get_lattice_points(mol%periodic, mol%lattice, cutoff, trans)

      call test_pbc_list_gen(error, mol, cutoff, trans=trans, cmp=.false.)

   end subroutine test_list_feo2

   subroutine test_list_x01(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      real(wp), parameter :: cutoff = 29.0_wp
      real(wp), allocatable :: trans(:, :)

      call get_structure(mol, "x01")
      call get_lattice_points(mol%periodic, mol%lattice, cutoff, trans)

      call test_pbc_list_gen(error, mol, cutoff, trans=trans, cmp=.false.)

   end subroutine test_list_x01

   subroutine test_list_x02(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      real(wp), parameter :: cutoff = 29.0_wp
      real(wp), allocatable :: trans(:, :)

      call get_structure(mol, "x02")
      call get_lattice_points(mol%periodic, mol%lattice, cutoff, trans)

      call test_pbc_list_gen(error, mol, cutoff, trans=trans, cmp=.false.)

   end subroutine test_list_x02

   subroutine test_list_x02_114(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      real(wp), parameter :: cutoff = 29.0_wp
      real(wp), allocatable :: trans(:, :)
      integer, parameter :: supercell(*) = [1, 1, 4]

      call get_structure(mol, "x02")
      call make_supercell(mol, supercell)
      call get_lattice_points(mol%periodic, mol%lattice, cutoff, trans)

      call test_pbc_list_gen(error, mol, cutoff, trans=trans, cmp=.false.)

   end subroutine test_list_x02_114

   subroutine test_list_nacl_complete(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      real(wp), parameter :: cutoff = 29.0_wp
      real(wp), allocatable :: trans(:, :)

      call get_structure(mol, "nacl")
      call get_lattice_points(mol%periodic, mol%lattice, cutoff, trans)

      call test_pbc_list_gen(error, mol, cutoff, trans=trans, cmp=.true.)

   end subroutine test_list_nacl_complete

   subroutine test_list_feo2_complete(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      real(wp), parameter :: cutoff = 29.0_wp
      real(wp), allocatable :: trans(:, :)

      call get_structure(mol, "feo2")
      call get_lattice_points(mol%periodic, mol%lattice, cutoff, trans)

      call test_pbc_list_gen(error, mol, cutoff, trans=trans, cmp=.true.)

   end subroutine test_list_feo2_complete

   subroutine test_list_x04_complete(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      real(wp), parameter :: cutoff = 29.0_wp
      real(wp), allocatable :: trans(:, :)

      call get_structure(mol, "x04")
      call get_lattice_points(mol%periodic, mol%lattice, cutoff, trans)

      call test_pbc_list_gen(error, mol, cutoff, trans=trans, cmp=.true.)

   end subroutine test_list_x04_complete

   subroutine test_list_x05_complete(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      real(wp), parameter :: cutoff = 29.0_wp
      real(wp), allocatable :: trans(:, :)

      call get_structure(mol, "x05")
      call get_lattice_points(mol%periodic, mol%lattice, cutoff, trans)

      call test_pbc_list_gen(error, mol, cutoff, trans=trans, cmp=.true.)

   end subroutine test_list_x05_complete

end module test_csrlist
