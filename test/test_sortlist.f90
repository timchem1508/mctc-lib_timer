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

!> Unit tests for sorted neighbor lists
module test_sortlist
   use mctc_csrlist, only : csr_list, new_csr_list, distance_screening, &
      & sort_neighborlist, find_lower_start
   use mctc_env, only : wp, i8
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, &
      & test_failed
   use mctc_io_structure, only : structure_type
   use mctc_wignerseitz, only : wignerseitz_cell
   use testsuite_structure, only : get_structure
   implicit none
   private

   public :: collect_sortlist

contains

!> Collect all exported unit tests
subroutine collect_sortlist(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("sorted-list-fullerene", test_sorted_list_fullerene), &
      & new_unittest("sorted-list-x02", test_sorted_list_x02) &
      & ]

end subroutine collect_sortlist

!> Check the rows of a sorted list against the unsorted list: the atom itself
!> first, its neighbors in descending order with the same entries and cell
!> indices, and the start of the neighbors of lower index
subroutine check_sorted(error, ref, list)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   !> Unsorted neighbor list
   type(csr_list), intent(in) :: ref

   !> Sorted neighbor list
   type(csr_list), intent(in) :: list

   integer :: iat, nat
   integer(i8) :: kat, lat
   integer, allocatable :: reftr(:), listtr(:), cnt(:, :)

   nat = size(ref%inl) - 1
   if (allocated(ref%nltr)) then
      reftr = ref%nltr
      listtr = list%nltr
   else
      allocate(reftr(size(ref%nlat)), listtr(size(list%nlat)), source=1)
   end if
   allocate(cnt(nat, maxval(reftr)), source=0)

   if (any(list%inl /= ref%inl)) then
      call test_failed(error, "Row offsets of the sorted list changed")
      return
   end if

   do iat = 1, nat
      if (list%nlat(list%inl(iat)) /= iat) then
         call test_failed(error, "Row of the sorted list does not start with the atom")
         return
      end if
      do kat = list%inl(iat) + 2, list%inl(iat + 1) - 1
         if (list%nlat(kat) > list%nlat(kat - 1)) then
            call test_failed(error, "Neighbors of the sorted list are not descending")
            return
         end if
      end do

      lat = find_lower_start(list, iat)
      if (any(list%nlat(list%inl(iat):lat - 1) < iat) &
         & .or. any(list%nlat(lat:list%inl(iat + 1) - 1) >= iat)) then
         call test_failed(error, "Wrong start of the neighbors of lower index")
         return
      end if

      ! Each entry of the row with its cell index is in both lists
      do kat = ref%inl(iat), ref%inl(iat + 1) - 1
         cnt(ref%nlat(kat), reftr(kat)) = cnt(ref%nlat(kat), reftr(kat)) + 1
      end do
      do kat = list%inl(iat), list%inl(iat + 1) - 1
         cnt(list%nlat(kat), listtr(kat)) = cnt(list%nlat(kat), listtr(kat)) - 1
      end do
      if (any(cnt /= 0)) then
         call test_failed(error, "Entries of a row changed by the sorting")
         return
      end if
   end do

end subroutine check_sorted

!> The upper triangles of the rows of a sorted complete list of a molecule
!> hold the entries of the half list
subroutine test_sorted_list_fullerene(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   real(wp), parameter :: cutoff = 10.0_wp
   type(structure_type) :: mol
   type(distance_screening) :: screen
   type(csr_list) :: ref, list, half
   integer :: iat

   call get_structure(mol, "fullerene")
   call new_csr_list(ref, screen, mol, error, cutoff=cutoff, complete=.true.)
   if (allocated(error)) return
   call new_csr_list(half, screen, mol, error, cutoff=cutoff)
   if (allocated(error)) return

   list = ref
   call sort_neighborlist(list)
   call check_sorted(error, ref, list)
   if (allocated(error)) return

   do iat = 1, mol%nat
      if (find_lower_start(list, iat) - list%inl(iat) &
         & /= half%inl(iat + 1) - half%inl(iat)) then
         call test_failed(error, "Upper triangle of a row differs from the half list")
         return
      end if
   end do

end subroutine test_sorted_list_fullerene

!> Sorted complete lists of a periodic structure, the cell indices and the
!> Wigner-Seitz images move with their entries
subroutine test_sorted_list_x02(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   real(wp), parameter :: cutoff = 12.0_wp
   type(structure_type) :: mol
   type(distance_screening) :: screen
   type(csr_list) :: ref, list
   type(wignerseitz_cell) :: wscref, wsc
   integer :: iat, nimg
   integer(i8) :: kat, lat

   call get_structure(mol, "x02")

   ! An entry for each image of a neighbor with its cell index
   call new_csr_list(ref, screen, mol, error, cutoff=cutoff, complete=.true.)
   if (allocated(error)) return
   list = ref
   call sort_neighborlist(list)
   call check_sorted(error, ref, list)
   if (allocated(error)) return

   ! An entry for each neighbor with its Wigner-Seitz images
   call new_csr_list(ref, screen, mol, error, wscref, cutoff, complete=.true.)
   if (allocated(error)) return
   list = ref
   wsc = wscref
   call sort_neighborlist(list, wsc)
   call check_sorted(error, ref, list)
   if (allocated(error)) return

   do iat = 1, mol%nat
      do kat = list%inl(iat), list%inl(iat + 1) - 1
         lat = ref%inl(iat) - 1 + findloc(ref%nlat(ref%inl(iat):ref%inl(iat + 1) - 1), &
            & list%nlat(kat), 1, kind=i8)
         nimg = wsc%nimg_list(kat)
         if (nimg /= wscref%nimg_list(lat) &
            & .or. wsc%itr_list(kat + 1) - wsc%itr_list(kat) /= nimg) then
            call test_failed(error, "Number of images of an entry changed")
            return
         end if
         if (any(wsc%tridx_list(wsc%itr_list(kat):wsc%itr_list(kat + 1) - 1) &
            & /= wscref%tridx_list(wscref%itr_list(lat):wscref%itr_list(lat + 1) - 1))) then
            call test_failed(error, "Images of an entry changed")
            return
         end if
      end do
   end do

end subroutine test_sorted_list_x02

end module test_sortlist
