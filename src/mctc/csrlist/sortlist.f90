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

!> @file mctc/csrlist/sortlist.f90
!> In-place sorting of the rows of a neighborlist.

!> Sorts the neighbors in each row of a neighborlist in descending order of
!> their index, the atom itself stays first:
!>
!>   row i = i | neighbors j >= i, descending | neighbors j < i, descending
!>
!> The first part of each row of a sorted complete list is the upper triangle
!> of the neighbor map, which serves as a half list, the neighbors of lower
!> index start at `find_lower_start(list, i)`.
module mctc_csrlist_sortlist
   use mctc_csrlist_type, only : csr_list
   use mctc_env, only : i8
   use mctc_wignerseitz, only : wignerseitz_cell
   implicit none
   private

   public :: sort_neighborlist, find_lower_start

contains

!> Sort the neighbors of each atom in descending order of their index, the
!> atom itself stays first in its row. The rows are sorted in place, the cell
!> indices and the Wigner-Seitz images of the entries move with them, entries
!> of the same neighbor keep their order.
subroutine sort_neighborlist(list, wsc)

   !> Neighborlist, its rows are sorted on exit
   type(csr_list), intent(inout) :: list

   !> Wigner-Seitz cell with the images of the entries of the list
   type(wignerseitz_cell), intent(inout), optional :: wsc

   integer :: iat, nat, nrow, kmax, k
   integer(i8) :: ist, ioff, inew, imax
   logical :: wslist
   integer, allocatable :: perm(:), ibuf(:), nimg(:), tbuf(:)
   integer(i8), allocatable :: key(:), work(:), itr(:)

   nat = size(list%inl) - 1
   wslist = .false.
   if (present(wsc)) then
      if (allocated(wsc%nimg_list)) &
         & wslist = size(wsc%nimg_list, kind=i8) == size(list%nlat, kind=i8)
   end if

   ! Longest row and largest number of images of a row for the buffers
   kmax = 0
   imax = 1_i8
   do iat = 1, nat
      kmax = max(kmax, int(list%inl(iat + 1) - list%inl(iat)))
      if (wslist) imax = max(imax, wsc%itr_list(list%inl(iat + 1)) &
         & - wsc%itr_list(list%inl(iat)))
   end do

   !$omp parallel default(none) &
   !$omp shared(list, wsc, nat, kmax, imax, wslist) &
   !$omp private(iat, nrow, k, ist, ioff, inew, perm, ibuf, nimg, tbuf, key, work) &
   !$omp private(itr)
   allocate(key(kmax), work(kmax), perm(kmax), ibuf(kmax))
   if (wslist) allocate(nimg(kmax), itr(kmax), tbuf(imax))
   !$omp do schedule(runtime)
   do iat = 1, nat
      ! Neighbors after the atom itself at ist + 1, ..., ist + nrow
      ist = list%inl(iat)
      nrow = int(list%inl(iat + 1) - ist) - 1
      if (nrow < 2) cycle

      ! The neighbor in the upper bits orders the keys, the lower bits keep
      ! the reversed position, which is unique and keeps equal neighbors in
      ! their order
      do k = 1, nrow
         key(k) = shiftl(int(list%nlat(ist + k), i8), 32) + int(nrow - k, i8)
      end do
      call sort_descending(key(:nrow), work(:nrow))
      do k = 1, nrow
         list%nlat(ist + k) = int(shiftr(key(k), 32))
         perm(k) = nrow - int(iand(key(k), maskr(32, i8)))
      end do

      if (allocated(list%nltr)) then
         ibuf(:nrow) = list%nltr(ist + 1:ist + nrow)
         list%nltr(ist + 1:ist + nrow) = ibuf(perm(:nrow))
      end if

      ! The images of the neighbors follow the ones of the atom itself and
      ! are copied in the new order of the neighbors
      if (wslist) then
         ioff = wsc%itr_list(ist + 1)
         tbuf(:wsc%itr_list(ist + nrow + 1) - ioff) = &
            & wsc%tridx_list(ioff:wsc%itr_list(ist + nrow + 1) - 1)
         itr(:nrow) = wsc%itr_list(ist + 1:ist + nrow) - ioff
         nimg(:nrow) = wsc%nimg_list(ist + 1:ist + nrow)
         inew = ioff
         do k = 1, nrow
            wsc%itr_list(ist + k) = inew
            wsc%nimg_list(ist + k) = nimg(perm(k))
            wsc%tridx_list(inew:inew + nimg(perm(k)) - 1) = &
               & tbuf(itr(perm(k)) + 1:itr(perm(k)) + nimg(perm(k)))
            inew = inew + nimg(perm(k))
         end do
      end if
   end do
   !$omp end do
   deallocate(key, work, perm, ibuf)
   if (wslist) deallocate(nimg, itr, tbuf)
   !$omp end parallel

end subroutine sort_neighborlist

!> Sort keys in descending order by a bottom-up merge sort. The rows of the
!> neighborlist consist of runs of ascending neighbors, one for each linked
!> cell, which the merges handle with few mispredicted branches.
pure subroutine sort_descending(key, work)

   !> Keys to sort
   integer(i8), intent(inout) :: key(:)

   !> Workspace of the size of the keys
   integer(i8), intent(out) :: work(:)

   integer :: n, width, lo, mid, hi, i, j, k

   n = size(key)
   width = 1
   do while (width < n)
      do lo = 1, n, 2 * width
         mid = min(lo + width - 1, n)
         hi = min(lo + 2 * width - 1, n)
         i = lo
         j = mid + 1
         k = lo
         do while (i <= mid .and. j <= hi)
            if (key(j) > key(i)) then
               work(k) = key(j)
               j = j + 1
            else
               work(k) = key(i)
               i = i + 1
            end if
            k = k + 1
         end do
         work(k:k + mid - i) = key(i:mid)
         k = k + mid - i + 1
         work(k:k + hi - j) = key(j:hi)
      end do
      key(:n) = work(:n)
      width = 2 * width
   end do

end subroutine sort_descending

!> Position of the first neighbor of lower index in the row of an atom of a
!> sorted neighborlist by a binary search, the end of the row plus one if
!> there is none. The entries before it, the atom itself and its neighbors
!> of not lower index, form the upper triangle of the row.
pure function find_lower_start(list, iat) result(kat)

   !> Sorted neighborlist
   type(csr_list), intent(in) :: list

   !> Atom of the row
   integer, intent(in) :: iat

   !> Position of the first neighbor of lower index
   integer(i8) :: kat

   integer(i8) :: hi, mid

   kat = list%inl(iat) + 1
   hi = list%inl(iat + 1) - 1
   do while (kat <= hi)
      mid = (kat + hi) / 2
      if (list%nlat(mid) < iat) then
         hi = mid - 1
      else
         kat = mid + 1
      end if
   end do

end function find_lower_start

end module mctc_csrlist_sortlist
