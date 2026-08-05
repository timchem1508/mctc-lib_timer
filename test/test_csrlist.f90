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
   use mctc_io, only : structure_type
   use mctc_io_resize, only : resize
   use mctc_cutoff, only: get_lattice_points
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, check
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
      & new_unittest("csr-vs-verlet-mb03", test_list_mb03) &
      & ]

   end subroutine collect_csrlist


!> Generate the reference Verlet/CSR neighbourlist
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
      !> Whether a complete or a symmetrical reduced map should be generated
      logical, intent(in) :: complete

      integer :: iat, jat, itr, nnz
      real(wp) :: r2, vec(3), cutoff2

      nnz = 0
      cutoff2 = cutoff**2

      call resize(col_ind, 10*mol%nat)
      call resize(nltr, 10*mol%nat)
      row_ptr(1) = 1

      do iat = 1, mol%nat
         nnz = nnz + 1
         if (size(col_ind) < nnz) call resize(col_ind)
         if (size(nltr) < nnz) call resize(nltr)

         col_ind(nnz) = iat
         nltr(nnz) = 1

         do jat = 1, merge(mol%nat, iat, complete)
            do itr = 1, size(trans, 2)
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

   subroutine test_list_gen(error, mol, cutoff, trans, cmp)
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

      class(csr_list), allocatable   :: list

      !> Reference Lists
      integer, allocatable :: ref_ptr(:), ref_list(:), ref_nltr(:)

      allocate(list)
      call new_csr_list(list, mol, cutoff, trans, cmp)

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

   end subroutine test_list_gen

   subroutine test_list_mb03(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      real(wp), parameter :: cutoff = 29.0_wp
      real(wp), allocatable :: trans(:, :)

      allocate(trans(3, 1))
      trans = 0.0_wp

      call get_structure(mol, "mindless03")

      call test_list_gen(error, mol, cutoff, trans=trans, cmp=.false.)

   end subroutine test_list_mb03


end module test_csrlist
