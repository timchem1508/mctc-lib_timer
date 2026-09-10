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

!> @file mctc/csrlist/linal.f90
!> Sparse matrix-vector routines for CSR compressed matrices.

module mctc_csrlist_linal
   use mctc_csrlist_type, only : csr_list
   use mctc_env, only : wp
   implicit none
   private

   public :: spmv_csr

   !> Performs the Compressed Sparse Row (CSR) matrix-vector operation
   !>
   !>    y := alpha*A*x + beta*y
   !>
   !> where alpha and beta are scalars, x and y are vectors and A is a matrix.
   !> The ranks of the A matrix, x and y vectors are denoted in the names
   !> of the procedures. Procedures support standard CSR format,
   !> both complete and upper triangular storage schemes.
   !> Also supports the case where the diagonal elements are stored separately.
   interface spmv_csr
      !> Standard CSR matrix-vector multiplication triangular/complete.
      module procedure spmv_csr_111_standard
      !> CSR matrix-vector multiplication with separate diagonal elements.
      module procedure spmv_csr_111
      !> Standard CSR matrix-vector multiplication for rectangular matrices.
      module procedure spmv_csr_211_standard
   end interface spmv_csr


contains


!> Multiply a CSR matrix by a vector with optional symmetry handling
subroutine spmv_csr_111_standard(list, mlist, x, y, alpha, beta, symmetric, complete)

   !> CSR neighbour-list structure
   type(csr_list), intent(in) :: list

   !> Matrix elements in CSR order
   real(wp), intent(in) :: mlist(:)

   !> Input vector
   real(wp), intent(in) :: x(:)

   !> Output vector, updated in place
   real(wp), intent(inout) :: y(:)

   !> Matrix scaling factor
   real(wp), intent(in) :: alpha

   !> Existing-vector scaling factor
   real(wp), intent(in) :: beta

   !> Whether the matrix has symmetric sparsity
   logical, intent(in), optional :: symmetric

   !> Whether the CSR list contains both matrix triangles
   logical, intent(in), optional :: complete

   integer :: i, k, j, n, ny
   logical :: is_sym, is_csr
   real(wp) :: y_tmp_i
   real(wp), allocatable :: y_priv(:)

   is_sym = .true.
   is_csr = .false.
   if (present(symmetric)) is_sym = symmetric
   if (present(complete)) is_csr = complete

   n = size(list%inl) - 1
   ny = size(y)
   if (size(mlist) /= size(list%nlat)) return

   if (is_csr .or. (.not. is_sym)) then
      ! Full CSR or non-symmetric
      !$omp parallel do private(i, k, j, y_tmp_i) shared(list, mlist, x, y, alpha, beta, n) schedule(static)
      do i = 1, n
         if (beta == 0.0_wp) then
            y_tmp_i = 0.0_wp
         else if (beta == 1.0_wp) then
            y_tmp_i = y(i)
         else
            y_tmp_i = y(i) * beta
         end if

         do k = list%inl(i), list%inl(i+1) - 1
            j = list%nlat(k)
            y_tmp_i = y_tmp_i + alpha * mlist(k) * x(j)
         end do
         y(i) = y_tmp_i
      end do
      !$omp end parallel do

      if (ny > n) then
         !$omp parallel do private(i) shared(y, beta, n, ny) schedule(static)
         do i = n + 1, ny
            if (beta == 0.0_wp) then
               y(i) = 0.0_wp
            else if (beta /= 1.0_wp) then
               y(i) = y(i) * beta
            end if
         end do
         !$omp end parallel do
      end if

   else
      ! Half-matrix symmetric CSR
      !$omp parallel private(y_priv, i, k, j, y_tmp_i) shared(list, mlist, x, y, alpha, beta, n, ny)
      allocate(y_priv(ny))
      y_priv = 0.0_wp

      !$omp do schedule(static)
      do i = 1, n
         y_tmp_i = 0.0_wp
         do k = list%inl(i), list%inl(i+1) - 1
            j = list%nlat(k)
            ! Contribution to row i
            y_tmp_i = y_tmp_i + alpha * mlist(k) * x(j)
            ! Contribution to row j
            if (j /= i) then
               y_priv(j) = y_priv(j) + alpha * mlist(k) * x(i)
            end if
         end do
         y_priv(i) = y_priv(i) + y_tmp_i
      end do
      !$omp end do

      !$omp do schedule(static)
      do i = 1, ny
         if (beta == 0.0_wp) then
            y(i) = 0.0_wp
         else if (beta /= 1.0_wp) then
            y(i) = y(i) * beta
         end if
      end do
      !$omp end do

      !$omp critical
      do i = 1, ny
         y(i) = y(i) + y_priv(i)
      end do
      !$omp end critical

      deallocate(y_priv)
      !$omp end parallel
   end if

end subroutine spmv_csr_111_standard


!> Multiply a symmetric CSR matrix with separate diagonal elements by a vector
subroutine spmv_csr_111(list, mlist, mdiag, x, y, alpha, beta, symmetric)

   !> CSR neighbour-list structure
   type(csr_list), intent(in) :: list

   !> Off-diagonal matrix elements in CSR order
   real(wp), intent(in) :: mlist(:)

   !> Diagonal matrix elements
   real(wp), intent(in) :: mdiag(:)

   !> Input vector
   real(wp), intent(in) :: x(:)

   !> Output vector, updated in place
   real(wp), intent(inout) :: y(:)

   !> Matrix scaling factor
   real(wp), intent(in) :: alpha

   !> Existing-vector scaling factor
   real(wp), intent(in) :: beta

   !> Whether the matrix has symmetric sparsity
   logical, intent(in), optional :: symmetric

   integer :: i, k, j, n, ny
   logical :: is_sym
   real(wp) :: y_tmp_i, a
   real(wp), allocatable :: y_priv(:)

   is_sym = .true.
   if (present(symmetric)) is_sym = symmetric

   n = size(list%inl) - 1
   ny = size(y)
   if (size(mlist) /= size(list%nlat)) return

   if (is_sym) then
      a = alpha
   else
      a = -alpha
   end if

   ! Single parallel region
   !$omp parallel private(y_priv, i, k, j, y_tmp_i) &
   !$omp& shared(list, mlist, mdiag, x, y, a, beta, n, ny)
   allocate(y_priv(ny))
   y_priv = 0.0_wp

   !$omp do schedule(static)
   do i = 1, n
      y_tmp_i = a * mdiag(i) * x(i)
      do k = list%inl(i) + 1, list%inl(i+1) - 1
         j = list%nlat(k)
         y_tmp_i = y_tmp_i + a * mlist(k) * x(j)
         y_priv(j) = y_priv(j) + a * mlist(k) * x(i)
      end do
      y_priv(i) = y_priv(i) + y_tmp_i
   end do
   !$omp end do

   ! Scale vector y in parallel
   !$omp do schedule(static)
   do i = 1, ny
      if (beta == 0.0_wp) then
         y(i) = 0.0_wp
      else if (beta /= 1.0_wp) then
         y(i) = y(i) * beta
      end if
   end do
   !$omp end do

   !$omp critical
   do i = 1, ny
      y(i) = y(i) + y_priv(i)
   end do
   !$omp end critical

   deallocate(y_priv)
   !$omp end parallel

end subroutine spmv_csr_111


!> Multiply a CSR-indexed full matrix by a vector with optional symmetry handling
subroutine spmv_csr_211_standard(list, matr, x, y, alpha, beta, symmetric, complete)

   !> CSR neighbour-list structure
   type(csr_list), intent(in) :: list

   !> Full matrix indexed by neighbouring and central atoms
   real(wp), intent(in) :: matr(:, :)

   !> Input vector
   real(wp), intent(in) :: x(:)

   !> Output vector, updated in place
   real(wp), intent(inout) :: y(:)

   !> Matrix scaling factor
   real(wp), intent(in) :: alpha

   !> Existing-vector scaling factor
   real(wp), intent(in) :: beta

   !> Whether the matrix has symmetric sparsity
   logical, intent(in), optional :: symmetric

   !> Whether the CSR list contains both matrix triangles
   logical, intent(in), optional :: complete

   integer :: i, k, j, n, ny
   logical :: is_sym, is_csr
   real(wp) :: y_tmp_i
   real(wp), allocatable :: y_priv(:)

   is_sym = .true.
   is_csr = .true.
   if (present(symmetric)) is_sym = symmetric
   if (present(complete)) is_csr = complete

   n = size(list%inl) - 1
   ny = size(y)

   if (is_csr .or. (.not. is_sym)) then
      ! Full CSR or non-symmetric
      !$omp parallel do private(i, k, j, y_tmp_i) &
      !$omp& shared(list, matr, x, y, alpha, beta, n) &
      !$omp& schedule(static)
      do i = 1, n
         if (beta == 0.0_wp) then
            y_tmp_i = 0.0_wp
         else if (beta == 1.0_wp) then
            y_tmp_i = y(i)
         else
            y_tmp_i = y(i) * beta
         end if

         do k = list%inl(i), list%inl(i+1) - 1
            j = list%nlat(k)
            y_tmp_i = y_tmp_i + alpha * matr(j, i) * x(j)
         end do
         y(i) = y_tmp_i
      end do
      !$omp end parallel do

      if (ny > n) then
         !$omp parallel do private(i) shared(y, beta, n, ny) schedule(static)
         do i = n + 1, ny
            if (beta == 0.0_wp) then
               y(i) = 0.0_wp
            else if (beta /= 1.0_wp) then
               y(i) = y(i) * beta
            end if
         end do
         !$omp end parallel do
      end if

   else
      ! Half-matrix symmetric CSR
      !$omp parallel private(y_priv, i, k, j, y_tmp_i) shared(list, matr, x, y, alpha, beta, n, ny)
      allocate(y_priv(ny))
      y_priv = 0.0_wp

      !$omp do schedule(static)
      do i = 1, n
         y_tmp_i = 0.0_wp
         do k = list%inl(i), list%inl(i+1) - 1
            j = list%nlat(k)
            ! Contribution to row i
            y_tmp_i = y_tmp_i + alpha * matr(j, i) * x(j)
            ! Contribution to row j
            if (j /= i) then
               y_priv(j) = y_priv(j) + alpha * matr(j, i) * x(i)
            end if
         end do
         y_priv(i) = y_priv(i) + y_tmp_i
      end do
      !$omp end do

      !$omp do schedule(static)
      do i = 1, ny
         if (beta == 0.0_wp) then
            y(i) = 0.0_wp
         else if (beta /= 1.0_wp) then
            y(i) = y(i) * beta
         end if
      end do
      !$omp end do

      ! Accumulate private thread contributions into y safely without ATOMIC
      !$omp critical
      do i = 1, ny
         y(i) = y(i) + y_priv(i)
      end do
      !$omp end critical

      deallocate(y_priv)
      !$omp end parallel
   end if

end subroutine spmv_csr_211_standard

end module mctc_csrlist_linal
