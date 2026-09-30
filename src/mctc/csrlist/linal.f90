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
!> Sparse matrix-vector and matrix-matrix routines for CSR compressed matrices.

module mctc_csrlist_linal
   use mctc_csrlist_type, only : csr_list
   use mctc_env, only : wp, i8
   implicit none
   private

   public :: spmv_csr, spmm_csr, spmspv_csr

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

   !> Performs the sparse matrix - sparse vector operation
   !>
   !>    y := alpha*A*x
   !>
   !> where alpha is a scalar, A is a matrix given by a complete CSR list and
   !> x is a sparse vector given as a pair of index/value arrays, holding the
   !> positions and values of its non-zero elements. The resulting vector y is
   !> returned in the same sparse representation, its index/value arrays are
   !> allocated by the routine.
   interface spmspv_csr
      !> Sparse matrix - sparse vector multiplication, complete CSR storage.
      module procedure spmspv_csr_111
   end interface spmspv_csr


   !> Performs the Compressed Sparse Row (CSR) matrix-matrix operation
   !>
   !>    C := alpha*A*B + beta*C
   !>
   !> where alpha and beta are scalars and A is a sparse matrix given by a
   !> complete CSR list. The second operand B and the result C are either both
   !> sparse, each carrying its own complete CSR list, or both dense.
   !> The ranks of the storage of A, B and C are denoted in the names of the
   !> procedures. All CSR lists are required to use the complete storage
   !> scheme, the sparsity patterns of A, B and C may differ.
   interface spmm_csr
      !> Sparse-sparse product projected onto a given output CSR pattern.
      module procedure spmm_csr_111
      !> Sparse-dense product for a dense right-hand side block.
      module procedure spmm_csr_122
   end interface spmm_csr

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

   integer :: i, j, n, ny
   integer(i8) :: k
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
      if (beta == 0.0_wp) then
         y(:) = 0.0_wp
      else if (beta /= 1.0_wp) then
         y(:) = beta * y
      end if

      !$omp parallel do default(none) schedule(guided)&
      !$omp& private(i, k, j, y_tmp_i) &
      !$omp& shared(list, mlist, x, y, alpha, n)
      do i = 1, n
         y_tmp_i = y(i)
         do k = list%inl(i), list%inl(i+1) - 1
            j = list%nlat(k)
            y_tmp_i = y_tmp_i + alpha * mlist(k) * x(j)
         end do
         y(i) = y_tmp_i
      end do
      !$omp end parallel do

   else
      ! Half-matrix symmetric CSR
      if (beta == 0.0_wp) then
         y(:) = 0.0_wp
      else if (beta /= 1.0_wp) then
         y(:) = beta * y
      end if

      !$omp parallel default(none) &
      !$omp& private(y_priv, i, k, j, y_tmp_i) &
      !$omp& shared(list, mlist, x, y, alpha, n, ny)
      allocate(y_priv(ny))
      y_priv = 0.0_wp

      !$omp do schedule(guided)
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

   integer :: i, j, n, ny
   integer(i8) :: k

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

   if (beta == 0.0_wp) then
      y(:) = 0.0_wp
   else if (beta /= 1.0_wp) then
      y(:) = beta * y
   end if

   ! Single parallel region
   !$omp parallel default(none) &
   !$omp& private(y_priv, i, k, j, y_tmp_i) &
   !$omp& shared(list, mlist, mdiag, x, y, a, n, ny)
   allocate(y_priv(ny))
   y_priv = 0.0_wp

   !$omp do schedule(guided)
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

   integer :: i, j, n, ny
   integer(i8) :: k
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
      if (beta == 0.0_wp) then
         y(:) = 0.0_wp
      else if (beta /= 1.0_wp) then
         y(:) = beta * y
      end if

      !$omp parallel do default(none) &
      !$omp& private(i, k, j, y_tmp_i) &
      !$omp& shared(list, matr, x, y, alpha, n) &
      !$omp& schedule(dynamic)
      do i = 1, n
         y_tmp_i = y(i)
         do k = list%inl(i), list%inl(i+1) - 1
            j = list%nlat(k)
            y_tmp_i = y_tmp_i + alpha * matr(j, i) * x(j)
         end do
         y(i) = y_tmp_i
      end do
      !$omp end parallel do

   else
      ! Half-matrix symmetric CSR
      if (beta == 0.0_wp) then
         y(:) = 0.0_wp
      else if (beta /= 1.0_wp) then
         y(:) = beta * y
      end if

      !$omp parallel default(none) &
      !$omp& private(y_priv, i, k, j, y_tmp_i) &
      !$omp& shared(list, matr, x, y, alpha, n, ny)
      allocate(y_priv(ny))
      y_priv = 0.0_wp

      !$omp do schedule(guided)
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

!> Multiply a CSR matrix given in complete storage by a sparse vector,
!> both the input and the resulting vector are held as index/value pairs
subroutine spmspv_csr_111(list, mlist, xptr, xval, yptr, yval, alpha)

   !> CSR neighbour-list structure
   type(csr_list), intent(in) :: list

   !> Matrix elements in CSR order
   real(wp), intent(in) :: mlist(:)

   !> Row indices of the non-zero elements of the input vector
   integer, intent(in) :: xptr(:)

   !> Values of the non-zero elements of the input vector
   real(wp), intent(in) :: xval(:)

   !> Row indices of the non-zero elements of the product vector, allocated here
   integer, allocatable, intent(out) :: yptr(:)

   !> Values of the non-zero elements of the product vector, allocated here
   real(wp), allocatable, intent(out) :: yval(:)

   !> Matrix scaling factor, defaults to one
   real(wp), intent(in), optional :: alpha

   integer :: i, j, n, m, nnz
   integer(i8) :: k
   real(wp) :: a, y_tmp_i
   real(wp), allocatable :: xdense(:), ydense(:)
   logical, allocatable :: yflag(:)

   a = 1.0_wp
   if (present(alpha)) a = alpha

   n = size(list%inl) - 1

   if (size(mlist) /= size(list%nlat) .or. n < 0) then
      allocate(yptr(0), yval(0))
      return
   end if

   ! Scatter the sparse input vector into a dense row-addressable buffer
   allocate(xdense(n), source=0.0_wp)
   m = min(size(xptr), size(xval))
   do i = 1, m
      j = xptr(i)
      if (j >= 1 .and. j <= n) xdense(j) = xdense(j) + xval(i)
   end do

   allocate(ydense(n))
   allocate(yflag(n), source=.false.)

   !$omp parallel do default(none) schedule(guided) &
   !$omp& private(i, k, j, y_tmp_i) &
   !$omp& shared(list, mlist, xdense, ydense, yflag, a, n)
   do i = 1, n
      y_tmp_i = 0.0_wp
      do k = list%inl(i), list%inl(i+1) - 1
         j = list%nlat(k)
         y_tmp_i = y_tmp_i + mlist(k) * xdense(j)
      end do
      ydense(i) = a * y_tmp_i
      yflag(i) = y_tmp_i /= 0.0_wp
   end do
   !$omp end parallel do

   ! Gather the non-zero rows of the dense product into the sparse output
   nnz = count(yflag)
   allocate(yptr(nnz))
   allocate(yval(nnz))

   nnz = 0
   do i = 1, n
      if (yflag(i)) then
         nnz = nnz + 1
         yptr(nnz) = i
         yval(nnz) = ydense(i)
      end if
   end do

end subroutine spmspv_csr_111

!> Multiply two CSR matrices given in complete storage, the product is
!> accumulated on the sparsity pattern of the output list.
subroutine spmm_csr_111(lista, alist, listb, blist, listc, clist, alpha, beta)

   !> CSR neighbour-list structure of the left operand
   type(csr_list), intent(in) :: lista

   !> Matrix elements of the left operand in CSR order
   real(wp), intent(in) :: alist(:)

   !> CSR neighbour-list structure of the right operand
   type(csr_list), intent(in) :: listb

   !> Matrix elements of the right operand in CSR order
   real(wp), intent(in) :: blist(:)

   !> CSR neighbour-list structure of the product
   type(csr_list), intent(in) :: listc

   !> Matrix elements of the product in CSR order, updated in place
   real(wp), intent(inout) :: clist(:)

   !> Matrix scaling factor
   real(wp), intent(in) :: alpha

   !> Existing-matrix scaling factor
   real(wp), intent(in) :: beta

   integer :: i, j, jc, n, ncol, ntouch, it
   integer(i8) :: ka, kb, kc
   real(wp) :: aij

   ! Thread-private sparse accumulator for a single row of the product
   real(wp), allocatable :: acc(:)
   logical, allocatable :: flag(:)
   integer, allocatable :: touch(:)

   n = size(lista%inl) - 1
   if (size(listc%inl) - 1 /= n) return
   if (size(alist) /= size(lista%nlat)) return
   if (size(blist) /= size(listb%nlat)) return
   if (size(clist) /= size(listc%nlat)) return

   ! Column space spanned by the right operand and by the output pattern
   ncol = size(listb%inl) - 1
   if (size(listb%nlat) > 0) ncol = max(ncol, maxval(listb%nlat))
   if (size(listc%nlat) > 0) ncol = max(ncol, maxval(listc%nlat))

   if (beta == 0.0_wp) then
      clist(:) = 0.0_wp
   else if (beta /= 1.0_wp) then
      clist(:) = beta * clist
   end if

   !$omp parallel default(none) &
   !$omp& private(i, j, jc, ka, kb, kc, aij, ntouch, it, acc, flag, touch) &
   !$omp& shared(lista, alist, listb, blist, listc, clist, alpha, n, ncol)
   allocate(acc(ncol), source=0.0_wp)
   allocate(flag(ncol), source=.false.)
   allocate(touch(ncol), source=0)

   !$omp do schedule(guided)
   do i = 1, n
      ! Gather the row i of the product in the dense accumulator
      ntouch = 0
      do ka = lista%inl(i), lista%inl(i+1) - 1
         j = lista%nlat(ka)
         aij = alist(ka)
         do kb = listb%inl(j), listb%inl(j+1) - 1
            jc = listb%nlat(kb)
            if (.not. flag(jc)) then
               flag(jc) = .true.
               ntouch = ntouch + 1
               touch(ntouch) = jc
               acc(jc) = 0.0_wp
            end if
            acc(jc) = acc(jc) + aij * blist(kb)
         end do
      end do

      ! Scatter the accumulator on the output pattern
      do kc = listc%inl(i), listc%inl(i+1) - 1
         jc = listc%nlat(kc)
         if (flag(jc)) clist(kc) = clist(kc) + alpha * acc(jc)
      end do

      ! Reset the accumulator for the next row
      do it = 1, ntouch
         flag(touch(it)) = .false.
      end do
   end do
   !$omp end do

   deallocate(acc, flag, touch)
   !$omp end parallel

end subroutine spmm_csr_111

!> Multiply a CSR matrix given in complete storage by a dense matrix
subroutine spmm_csr_122(list, mlist, bmat, cmat, alpha, beta)

   !> CSR neighbour-list structure
   type(csr_list), intent(in) :: list

   !> Matrix elements in CSR order
   real(wp), intent(in) :: mlist(:)

   !> Dense right-hand side block
   real(wp), intent(in) :: bmat(:, :)

   !> Dense product block, updated in place
   real(wp), intent(inout) :: cmat(:, :)

   !> Matrix scaling factor
   real(wp), intent(in) :: alpha

   !> Existing-matrix scaling factor
   real(wp), intent(in) :: beta

   integer :: i, j, n, nrow, ncol
   integer(i8) :: k

   real(wp), allocatable :: c_tmp(:)

   n = size(list%inl) - 1
   nrow = size(cmat, 1)
   ncol = size(cmat, 2)
   if (size(mlist) /= size(list%nlat)) return
   if (size(bmat, 2) /= ncol) return
   if (nrow < n) return

   ! Scale the full product block, including rows outside of the sparse matrix
   if (beta == 0.0_wp) then
      cmat(:, :) = 0.0_wp
   else if (beta /= 1.0_wp) then
      cmat(:, :) = beta * cmat
   end if

   !$omp parallel default(none) &
   !$omp& private(i, j, k, c_tmp) &
   !$omp& shared(list, mlist, bmat, cmat, alpha, n, ncol)
   allocate(c_tmp(ncol), source=0.0_wp)

   !$omp do schedule(runtime)
   do i = 1, n
      c_tmp(:) = cmat(i, :)
      do k = list%inl(i), list%inl(i+1) - 1
         j = list%nlat(k)
         c_tmp(:) = c_tmp(:) + alpha * mlist(k) * bmat(j, :)
      end do
      cmat(i, :) = c_tmp(:)
   end do
   !$omp end do

   deallocate(c_tmp)
   !$omp end parallel

end subroutine spmm_csr_122

end module mctc_csrlist_linal
