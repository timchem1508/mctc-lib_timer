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
!
!> Provides fast routine for sorting atoms within a molecular structure.
module mctc_io_structure_atomsort
   use mctc_env_accuracy, only : wp
   implicit none
   private

   public :: get_spatial_order

   !> Maximum number of atoms per group in the spatial order
   integer, parameter :: max_group = 8

contains

!> Spatial order of the atoms from a recursive bisection of the structure,
!> consecutive blocks of the order contain atoms close to each other
subroutine get_spatial_order(xyz, order, bsize)

   !> Cartesian coordinates
   real(wp), intent(in) :: xyz(:, :)

   !> Atoms in the spatial order, order(i) is the atom at position i
   integer, allocatable, intent(out) :: order(:)

   !> Number of atoms per block, max_group if absent
   integer, intent(in), optional :: bsize

   integer :: iat, nsize

   nsize = max_group
   if (present(bsize)) nsize = bsize
   order = [(iat, iat = 1, size(xyz, 2))]
   call bisect_atoms(xyz, order, nsize)

end subroutine get_spatial_order

!> Recursively split the atoms at the median of the longest side of their
!> bounding box until each part holds at most one block
recursive subroutine bisect_atoms(xyz, order, bsize)

   !> Cartesian coordinates
   real(wp), intent(in) :: xyz(:, :)

   !> Atoms to split, reordered in place
   integer, intent(inout), contiguous :: order(:)

   !> Number of atoms per block
   integer, intent(in) :: bsize

   integer :: iat, nleft, dir
   real(wp) :: lower(3), upper(3)

   if (size(order) <= bsize) return

   lower(:) = xyz(:, order(1))
   upper(:) = lower
   do iat = 2, size(order)
      lower(:) = min(lower, xyz(:, order(iat)))
      upper(:) = max(upper, xyz(:, order(iat)))
   end do
   dir = maxloc(upper - lower, 1)

   nleft = bsize * ((size(order) + bsize - 1) / bsize / 2)
   call select_atoms(xyz(dir, :), order, nleft)
   call bisect_atoms(xyz, order(:nleft), bsize)
   call bisect_atoms(xyz, order(nleft + 1:), bsize)

end subroutine bisect_atoms

!> Partial sort of the atoms by a coordinate (Hoare selection),
!> the first k atoms have no larger coordinate than the others
subroutine select_atoms(coord, order, k)

   !> Coordinate of each atom
   real(wp), intent(in) :: coord(:)

   !> Atoms to reorder
   integer, intent(inout), contiguous :: order(:)

   !> Number of atoms in the first part
   integer, intent(in) :: k

   integer :: lo, hi, i, j, tmp
   real(wp) :: pivot, first, last

   lo = 1
   hi = size(order)
   do while (lo < hi)
      ! Median of the first, k-th and last coordinate of the range
      first = coord(order(lo))
      last = coord(order(hi))
      pivot = max(min(first, last), min(max(first, last), coord(order(k))))
      i = lo
      j = hi
      do while (i <= j)
         do while (coord(order(i)) < pivot)
            i = i + 1
         end do
         do while (pivot < coord(order(j)))
            j = j - 1
         end do
         if (i <= j) then
            tmp = order(i)
            order(i) = order(j)
            order(j) = tmp
            i = i + 1
            j = j - 1
         end if
      end do
      ! The atoms between j and i have the coordinate of the pivot
      if (j < k) lo = i
      if (k < i) hi = j
   end do

end subroutine select_atoms

end module mctc_io_structure_atomsort
