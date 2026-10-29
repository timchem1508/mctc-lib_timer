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

!> Unit tests for the spatial order of the atoms
module test_atomsort
   use mctc_env, only : wp
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, &
      & test_failed, check
   use mctc_io_structure_atomsort, only : get_spatial_order
   implicit none
   private

   public :: collect_atomsort

   !> Default number of atoms per block of the spatial order
   integer, parameter :: bsize_def = 8

contains

!> Collect all exported unit tests
subroutine collect_atomsort(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("spatial-order-chain", test_spatial_order_chain), &
      & new_unittest("spatial-order-input", test_spatial_order_input), &
      & new_unittest("spatial-order-small", test_spatial_order_small) &
      & ]

end subroutine collect_atomsort

!> Chain along x with a shuffled input order, pos(i) is the position of atom i
subroutine get_chain(xyz, pos)

   !> Cartesian coordinates
   real(wp), intent(out) :: xyz(:, :)

   !> Position of each atom along the chain
   integer, intent(out) :: pos(:)

   integer :: iat

   ! Multiples of 7 run through all residues modulo a length not divisible by 7
   pos(:) = [(mod(7 * iat, size(pos)) + 1, iat = 1, size(pos))]
   xyz(:, :) = 0.0_wp
   xyz(1, :) = 2.0_wp * pos

end subroutine get_chain

!> Each block of the spatial order of a shuffled chain holds a block of
!> consecutive atoms of the chain
subroutine test_spatial_order_chain(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer, parameter :: nat = 20, bsize = 4
   integer :: iat, pos(nat)
   integer, allocatable :: order(:)
   real(wp) :: xyz(3, nat)
   logical :: seen(nat)

   call get_chain(xyz, pos)
   call get_spatial_order(xyz, order, bsize)

   call check(error, size(order), nat)
   if (allocated(error)) return

   seen(:) = .false.
   do iat = 1, nat
      if ((pos(order(iat)) - 1) / bsize /= (iat - 1) / bsize) then
         call test_failed(error, "Block of the spatial order holds a distant atom")
         return
      end if
      seen(order(iat)) = .true.
   end do
   if (.not. all(seen)) then
      call test_failed(error, "Spatial order is not a permutation of the atoms")
   end if

end subroutine test_spatial_order_chain

!> The blocks of the spatial order of a cloud of atoms without equal
!> coordinates hold the same atoms for a shuffled input order
subroutine test_spatial_order_input(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer, parameter :: nat = 100
   integer :: iat, perm(nat), blk(nat)
   integer, allocatable :: ref(:), order(:)
   real(wp) :: xyz(3, nat)

   ! Multiples modulo primes above nat give different coordinates on each axis
   do iat = 1, nat
      xyz(:, iat) = 0.5_wp &
         & * [mod(37 * iat, 101), mod(59 * iat, 103), mod(71 * iat, 107)]
      perm(iat) = mod(7 * iat, nat) + 1
   end do

   call get_spatial_order(xyz, ref)
   call get_spatial_order(xyz(:, perm), order)

   call check(error, size(ref), nat)
   if (allocated(error)) return
   call check(error, size(order), nat)
   if (allocated(error)) return

   blk(:) = -1
   do iat = 1, nat
      blk(ref(iat)) = (iat - 1) / bsize_def
   end do
   if (any(blk < 0)) then
      call test_failed(error, "Spatial order is not a permutation of the atoms")
      return
   end if

   do iat = 1, nat
      if (blk(perm(order(iat))) /= (iat - 1) / bsize_def) then
         call test_failed(error, "Blocks of the spatial order depend on the input order")
         return
      end if
   end do

end subroutine test_spatial_order_input

!> Structures without atoms or with at most one block keep the input order,
!> blocks of single atoms sort a shuffled chain
subroutine test_spatial_order_small(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer, parameter :: nat = 20
   integer :: iat, pos(nat)
   integer, allocatable :: order(:)
   real(wp) :: xyz(3, nat)

   call get_chain(xyz, pos)

   call get_spatial_order(xyz(:, :0), order)
   call check(error, size(order), 0)
   if (allocated(error)) return

   call get_spatial_order(xyz(:, :bsize_def), order)
   if (any(order /= [(iat, iat = 1, bsize_def)])) then
      call test_failed(error, "Spatial order of a single block changed the atoms")
      return
   end if

   call get_spatial_order(xyz, order, 1)
   if (any(pos(order) /= [(iat, iat = 1, nat)])) then
      call test_failed(error, "Blocks of single atoms do not sort the chain")
   end if

end subroutine test_spatial_order_small

end module test_atomsort
