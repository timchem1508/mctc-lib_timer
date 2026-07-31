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

!> @file blascomp.f90
!> Matrix-vector and Matrix-matrix routines for CSR compressed matrices.
module mctc_csrlist_linal
   use mctc_env, only : wp
   use mctc_csrlist_type, only: csr_list
   implicit none
   private

   public :: gemv_cmp

   interface gemv_cmp
      module procedure gemv_cmp_111
      module procedure gemv_cmp_212
   end interface gemv_cmp


contains

!=========================================================
! GEMV 111 - Thread-safe OMP
!=========================================================
   subroutine gemv_cmp_111(list, mlist, mdiag, x, y, alpha, beta, symmetric)
      type(csr_list), intent(in) :: list
      real(wp), intent(in)  :: mlist(:)
      real(wp), intent(in)  :: mdiag(:)
      real(wp), intent(in)  :: x(:)
      real(wp), intent(inout) :: y(:)
      real(wp), intent(in)  :: alpha, beta
      logical, intent(in), optional :: symmetric

      integer :: i, k, j, n
      logical :: is_sym
      real(wp) :: y_tmp_i, a

      is_sym = .true.
      if (present(symmetric)) is_sym = symmetric

      n = size(list%inl) - 1
      if (size(mlist) /= size(list%nlat)) return
      if (is_sym) then
         a = alpha
      else
         a = -alpha
      end if

      !$omp parallel do default(shared) private(i)
      do i = 1, size(y)
         if (beta == 0.0_wp) then
            y(i) = 0.0_wp
         else if (beta /= 1.0_wp) then
            y(i) = y(i) * beta
         end if
      end do
      !$omp end parallel do

      !$omp parallel do default(shared) private(i, k, j, y_tmp_i) reduction(+:y)
      do i = 1, n
         y_tmp_i = 0.0_wp

         ! Diagonal contribution
         y_tmp_i = y_tmp_i + a * mdiag(i) * x(i)

         ! Slot +1 is j = i; off-diagonal interactions start at +2
         do k = list%inl(i) + 1, list%inl(i+1) - 1
            j = list%nlat(k)

            ! Part 1: Contribution to row i (accumulated locally in scalar)
            y_tmp_i = y_tmp_i + a * mlist(k) * x(j)

            ! Part 2: Contribution to row j
            y(j) = y(j) + a * mlist(k) * x(i)
         end do

         ! Apply accumulated row i results to the thread-private y
         y(i) = y(i) + y_tmp_i
      end do
      !$omp end parallel do

   end subroutine gemv_cmp_111

!=========================================================
! GEMV 212 - Thread-safe OMP (Atomics Removed)
!=========================================================
   subroutine gemv_cmp_212(list, mdrij, mdrji, mdrdiag, x, y, alpha, beta)
      type(csr_list), intent(in) :: list
      real(wp), intent(in)  :: mdrij(:,:)
      real(wp), intent(in)  :: mdrji(:,:)
      real(wp), intent(in)  :: mdrdiag(:,:)
      real(wp), intent(in)  :: x(:)
      real(wp), intent(inout) :: y(:,:)
      real(wp), intent(in), optional :: alpha
      real(wp), intent(in), optional :: beta

      real(wp) :: a, b
      integer  :: i, j, k, m, n, nv

      a = 1.0_wp
      if (present(alpha)) a = alpha
      b = 0.0_wp
      if (present(beta)) b = beta

      n = size(list%inl) - 1
      nv = size(y, 1)

      !$omp parallel do default(shared) private(i, m)
      do i = 1, size(y, 2)
         do m = 1, nv
            if (b == 0.0_wp) then
               y(m, i) = 0.0_wp
            else if (b /= 1.0_wp) then
               y(m, i) = y(m, i) * b
            end if
         end do
      end do
      !$omp end parallel do

      !$omp parallel do default(shared) private(i, k, j, m) reduction(+:y)
      do i = 1, n
         do m = 1, nv
            y(m, i) = y(m, i) + a * mdrdiag(m, i) * x(i)
         end do
         ! Slot +1 is j = i; off-diagonal interactions start at +2
         do k = list%inl(i) + 1, list%inl(i+1) - 1
            j = list%nlat(k)
            do m = 1, nv
               y(m, i) = y(m, i) + a * mdrij(m, k) * x(j)
               y(m, j) = y(m, j) + a * mdrji(m, k) * x(i)
            end do
         end do
      end do
      !$omp end parallel do

   end subroutine gemv_cmp_212

!=========================================================
! GEMV 212 DIRECTED - Thread-safe OMP (Atomics Removed)
!=========================================================
   subroutine gemv_cmp_212_dir(list, mlist_drij, mlist_drji, mdiag, x, y, alpha, beta, symmetric)
      type(csr_list), intent(in) :: list
      real(wp), intent(in)  :: mlist_drij(:,:)
      real(wp), intent(in)  :: mlist_drji(:,:)
      real(wp), intent(in)  :: mdiag(:,:)
      real(wp), intent(in)  :: x(:)
      real(wp), intent(inout) :: y(:,:)
      real(wp), intent(in)  :: alpha, beta
      logical, intent(in), optional :: symmetric

      integer :: i, k, j, m, n, nv
      logical :: is_sym

      is_sym = .true.
      if (present(symmetric)) is_sym = symmetric

      n = size(list%inl) - 1
      nv = size(y, 1)

      if (size(mlist_drij, 2) /= size(list%nlat)) return
      if (size(mlist_drji, 2) /= size(list%nlat)) return

      !$omp parallel do default(shared) private(i, m)
      do i = 1, size(y, 2)
         do m = 1, nv
            if (beta == 0.0_wp) then
               y(m, i) = 0.0_wp
            else if (beta /= 1.0_wp) then
               y(m, i) = y(m, i) * beta
            end if
         end do
      end do
      !$omp end parallel do

      !$omp parallel do default(shared) private(i, k, j, m) reduction(+:y)
      do i = 1, n
         ! Diagonal contribution
         if (is_sym) then
            do m = 1, nv
               y(m, i) = y(m, i) + alpha * mdiag(m, i) * x(i)
            end do
         end if

         ! Slot +1 is j = i; off-diagonal interactions start at +2
         do k = list%inl(i) + 1, list%inl(i+1) - 1
            j = list%nlat(k)

            do m = 1, nv
               y(m, i) = y(m, i) + alpha * mlist_drij(m, k) * x(j)

               ! Safe to branch inside the reduction loop without atomics
               if (is_sym) then
                  y(m, j) = y(m, j) + alpha * mlist_drji(m, k) * x(i)
               else
                  y(m, j) = y(m, j) - alpha * mlist_drji(m, k) * x(i)
               end if
            end do
         end do
      end do
      !$omp end parallel do

   end subroutine gemv_cmp_212_dir

end module mctc_csrlist_linal
