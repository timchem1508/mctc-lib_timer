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

!> @file mctc/csrlist/screening.f90
!> A screening function type for variability of the neighbor lists,
!> definition and related procedures.

!> Abstract screening deciding which atom pairs enter the neighbor list
module mctc_csrlist_screening
   use mctc_env, only : wp
   implicit none
   private

   public :: screening_type

   !> Screening criterion applied to every candidate pair of the neighbor list.
   type, abstract :: screening_type

      !> Screening threshold for the neighbor list
      real(wp) :: thresh
   contains
      !> Check if a pair of atoms should be included in the neighbor list
      procedure(check_pair), deferred :: check_pair
   end type screening_type

   abstract interface
      !> Check if a pair of atoms should be included in the neighbor list.
      subroutine check_pair(self, iatom, jatom, r2, include)
         import :: screening_type, wp

         !> Screening instance
         class(screening_type), intent(in) :: self

         !> Index of the first atom
         integer, intent(in) :: iatom

         !> Index of the second atom
         integer, intent(in) :: jatom

         !> Squared distance between the two atoms
         real(wp), intent(in) :: r2

         !> Return true if the pair should be included
         logical, intent(out) :: include

      end subroutine check_pair
   end interface

end module mctc_csrlist_screening
