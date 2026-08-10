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
      module procedure gemv_cmp_111_standard
      module procedure gemv_cmp_111
      module procedure gemv_cmp_211_standard
   end interface gemv_cmp


contains

!================================================================================
! GEMV 111 - Standard CSR-based SpMV based on CSR list, matrix and vector arrays
!================================================================================
   subroutine gemv_cmp_111_standard(list, mlist, x, y, alpha, beta, symmetric, complete)
      type(csr_list), intent(in) :: list
      real(wp), intent(in)  :: mlist(:)
      real(wp), intent(in)  :: x(:)
      real(wp), intent(inout) :: y(:)
      real(wp), intent(in)  :: alpha, beta
      logical, intent(in), optional :: symmetric
      logical, intent(in), optional :: complete

      integer :: i, k, j, n
      logical :: is_sym, is_cmp
      real(wp) :: y_tmp_i

      is_sym = .true.
      is_cmp = .false.
      if (present(symmetric)) is_sym = symmetric
      if (present(complete)) is_cmp = complete

      n = size(list%inl) - 1
      if (size(mlist) /= size(list%nlat)) return
      !$omp parallel do default(shared) private(i)
      do i = 1, size(y)
         if (beta == 0.0_wp) then
            y(i) = 0.0_wp
         else if (beta /= 1.0_wp) then
            y(i) = y(i) * beta
         end if
      end do
      !$omp end parallel do
      if (is_cmp .or. (.not. is_sym)) then
         !$omp parallel do default(shared) private(i, k, j, y_tmp_i)
         do i = 1, n
            y_tmp_i = 0.0_wp
            do k = list%inl(i), list%inl(i+1) - 1
               j = list%nlat(k)
               y_tmp_i = y_tmp_i + alpha * mlist(k) * x(j)
            end do
            y(i) = y(i) + y_tmp_i
         end do
         !$omp end parallel do
      else
         !$omp parallel do default(shared) private(i, k, j, y_tmp_i) reduction(+:y)
         do i = 1, n
            y_tmp_i = 0.0_wp
            do k = list%inl(i), list%inl(i+1) - 1
               j = list%nlat(k)

               ! Contribution to row i
               y_tmp_i = y_tmp_i + alpha * mlist(k) * x(j)

               ! Contribution to row j
               if (j /= i) then
                  y(j) = y(j) + alpha * mlist(k) * x(i)
               end if
            end do

            y(i) = y(i) + y_tmp_i
         end do
         !$omp end parallel do
      end if

   end subroutine gemv_cmp_111_standard

!==================================================================================================
! GEMV 111 - CSR-based SpMV for symmetric matrix separated on diagonal and off-diagonal components
!==================================================================================================
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
         y_tmp_i = y_tmp_i + a * mdiag(i) * x(i)
         do k = list%inl(i) + 1, list%inl(i+1) - 1
            j = list%nlat(k)
            y_tmp_i = y_tmp_i + a * mlist(k) * x(j)
            y(j) = y(j) + a * mlist(k) * x(i)
         end do
         y(i) = y(i) + y_tmp_i
      end do
      !$omp end parallel do

   end subroutine gemv_cmp_111

!====================================================================================
! GEMV 211 - Standard CSR-based SpMV based on CSR list, full matrix and vector array
!====================================================================================
   subroutine gemv_cmp_211_standard(list, matr, x, y, alpha, beta, symmetric, complete)
      type(csr_list), intent(in) :: list
      real(wp), intent(in)  :: matr(:, :)
      real(wp), intent(in)  :: x(:)
      real(wp), intent(inout) :: y(:)
      real(wp), intent(in)  :: alpha, beta
      logical, intent(in), optional :: symmetric
      logical, intent(in), optional :: complete

      integer :: i, k, j, n
      logical :: is_sym, is_cmp
      real(wp) :: y_tmp_i

      is_sym = .true.
      is_cmp = .true.
      if (present(symmetric)) is_sym = symmetric
      if (present(complete)) is_cmp = complete

      n = size(list%inl) - 1
      !$omp parallel do default(shared) private(i)
      do i = 1, size(y)
         if (beta == 0.0_wp) then
            y(i) = 0.0_wp
         else if (beta /= 1.0_wp) then
            y(i) = y(i) * beta
         end if
      end do
      !$omp end parallel do
      if (is_cmp .or. (.not. is_sym)) then
         !$omp parallel do default(shared) private(i, k, j, y_tmp_i)
         do i = 1, n
            y_tmp_i = 0.0_wp
            do k = list%inl(i), list%inl(i+1) - 1
               j = list%nlat(k)
               y_tmp_i = y_tmp_i + alpha * matr(j, i) * x(j)
            end do
            y(i) = y(i) + y_tmp_i
         end do
         !$omp end parallel do
      else
         !$omp parallel do default(shared) private(i, k, j, y_tmp_i) reduction(+:y)
         do i = 1, n
            y_tmp_i = 0.0_wp
            do k = list%inl(i), list%inl(i+1) - 1
               j = list%nlat(k)

               ! Contribution to row i
               y_tmp_i = y_tmp_i + alpha * matr(j, i) * x(j)

               ! Contribution to row j
               if (j /= i) then
                  y(j) = y(j) + alpha * matr(j, i) * x(i)
               end if
            end do

            y(i) = y(i) + y_tmp_i
         end do
         !$omp end parallel do
      end if

   end subroutine gemv_cmp_211_standard

end module mctc_csrlist_linal
