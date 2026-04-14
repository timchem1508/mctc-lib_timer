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

!> Declaration of base class for coordination number evalulations
module mctc_ncoord_type
   use mctc_env, only : wp
   use mctc_io, only : structure_type
   use mctc_cutoff, only : get_lattice_points
   use mctc_ncoord_adjlist_type, only : adjacency_list

   implicit none
   private

   !> Abstract base class for coordination number evaluator
   type, public, abstract :: ncoord_type
      !> Radial cutoff for the coordination number
      real(wp)  :: cutoff
      !> Steepness of counting function
      real(wp)  :: kcn
      !> Factor determining whether the CN is evaluated with direction
      !> if +1 the CN contribution is added equally to both partners
      !> if -1 (i.e. with the EN-dep.) it is added to one and subtracted from the other
      real(wp)  :: directed_factor
      !> Cutoff for the maximum coordination number (negative value, no cutoff)
      real(wp)  :: cut = -1.0_wp
   contains
      !> Obtains lattice information and calls get_coordination number
      procedure :: get_cn
      !> Decides whether the energy or gradient is calculated
      procedure :: get_coordination_number
      !> Evaluates the CN from the specific counting function
      procedure :: ncoord
      !> Evaluates derivative of the CN from the specific counting function
      procedure :: ncoord_d
      !> Evaluates pairwise electronegativity factor
      procedure :: get_en_factor
      !> Add CN derivative of an arbitrary function
      procedure :: add_coordination_number_derivs
      !> Add CN derivative of an arbitrary function using the neighbour list
      procedure :: add_coordination_number_derivs_list
      !> Add CN derivative of an arbitrary function using the neighbour list with support of the wsc
      procedure :: add_coordination_number_derivs_list_wsc
      !> Evaluates the counting function (exp, dexp, erf, ...)
      procedure(ncoord_count),  deferred :: ncoord_count
      !> Evaluates the derivative of the counting function (exp, dexp, erf, ...)
      procedure(ncoord_dcount), deferred :: ncoord_dcount
   end type ncoord_type

   abstract interface

      !> Abstract counting function
      elemental function ncoord_count(self, izp, jzp, r) result(count)
         import :: ncoord_type, wp
         !> Instance of coordination number container
         class(ncoord_type), intent(in) :: self
         !> Atom i index
         integer, intent(in)  :: izp
         !> Atom j index
         integer, intent(in)  :: jzp
         !> Current distance.
         real(wp), intent(in) :: r

         real(wp) :: count
      end function ncoord_count

      !> Abstract derivative of the counting function w.r.t. the distance.
      elemental function ncoord_dcount(self, izp, jzp, r) result(count)
         import :: ncoord_type, wp
         !> Instance of coordination number container
         class(ncoord_type), intent(in) :: self
         !> Atom i index
         integer, intent(in)  :: izp
         !> Atom j index
         integer, intent(in)  :: jzp
         !> Current distance.
         real(wp), intent(in) :: r

         real(wp) :: count
      end function ncoord_dcount

   end interface

contains

   !> Wrapper for CN using the CN cutoff for the lattice
   subroutine get_cn(self, mol, cn, dcndr, dcndL, list)
      !> Coordination number container
      class(ncoord_type), intent(in) :: self
      !> Molecular structure data
      type(structure_type), intent(in) :: mol
      !> Adjacency list for neighbourlist-based CN evaluation
      type(adjacency_list), intent(in), optional :: list
      !> Error function coordination number.
      real(wp), intent(out) :: cn(:)
      !> Derivative of the CN with respect to the Cartesian coordinates.
      real(wp), intent(out), optional :: dcndr(:, :, :)
      !> Derivative of the CN with respect to strain deformations.
      real(wp), intent(out), optional :: dcndL(:, :, :)

      real(wp), allocatable :: lattr(:, :)

      call get_lattice_points(mol%periodic, mol%lattice, self%cutoff, lattr)
      call get_coordination_number(self, mol, lattr, cn, dcndr, dcndL, list)
   end subroutine get_cn

   !> Geometric fractional coordination number
   subroutine get_coordination_number(self, mol, trans, cn, dcndr, dcndL, list, dcndrij, dcndrji, dcndrdiag)

      !> Coordination number container
      class(ncoord_type), intent(in) :: self

      !> Molecular structure data
      type(structure_type), intent(in) :: mol

      !> Lattice points
      real(wp), intent(in) :: trans(:, :)

      !> Error function coordination number.
      real(wp), intent(out) :: cn(:)

      !> Derivative of the CN with respect to the Cartesian coordinates.
      real(wp), intent(out), optional :: dcndr(:, :, :)


      !> Derivative of the CN with respect to strain deformations.
      real(wp), intent(out), optional :: dcndL(:, :, :)

      !> Adjacency list for neighbourlist-based CN evaluation
      type(adjacency_list), intent(in), optional :: list

      !> Derivative of the CN with respect to the Cartesian coordinates.
      real(wp), intent(out), optional :: dcndrij(:, :), dcndrji(:, :), dcndrdiag(:, :)



      if (present(list)) then
         if (present(dcndrij) .and. present(dcndrji) &
         & .and. present(dcndrdiag) .and. present(dcndL)) then
            call ncoord_d_list(self, mol, trans, cn, dcndrij, dcndrji, dcndrdiag, dcndL, list)
         else
            call ncoord_list(self, mol, trans, cn, list)
         end if
      else
         if (present(dcndr) .and. present(dcndL)) then
            call ncoord_d(self, mol, trans, cn, dcndr, dcndL)
         else
            call ncoord(self, mol, trans, cn)
         end if
      end if

      if (self%cut > 0.0_wp) then
         call cut_coordination_number(self%cut, cn, dcndr, dcndL)
      end if

   end subroutine get_coordination_number


   subroutine ncoord(self, mol, trans, cn)
      !> Coordination number container
      class(ncoord_type), intent(in) :: self
      !> Molecular structure data
      type(structure_type), intent(in) :: mol
      !> Lattice points
      real(wp), intent(in) :: trans(:, :)
      !> Error function coordination number.
      real(wp), intent(out) :: cn(:)

      integer :: iat, jat, izp, jzp, itr
      real(wp) :: r2, r1, rij(3), countf, cutoff2, den

      ! Thread-private array for reduction
      real(wp), allocatable :: cn_local(:)

      cn(:) = 0.0_wp
      cutoff2 = self%cutoff**2

      !$omp parallel default(none) &
      !$omp shared(self, mol, trans, cutoff2, cn) &
      !$omp private(jat, itr, izp, jzp, r2, rij, r1, den, countf) &
      !$omp private(cn_local)
      allocate(cn_local, source=cn)
      !$omp do schedule(runtime)
      do iat = 1, mol%nat
         izp = mol%id(iat)
         do jat = 1, iat
            jzp = mol%id(jat)
            den = self%get_en_factor(izp, jzp)

            do itr = 1, size(trans, dim=2)
               rij = mol%xyz(:, iat) - (mol%xyz(:, jat) + trans(:, itr))
               r2 = sum(rij**2)
               if (r2 > cutoff2 .or. r2 < 1.0e-12_wp) cycle
               r1 = sqrt(r2)

               countf = den * self%ncoord_count(izp, jzp, r1)

               cn_local(iat) = cn_local(iat) + countf
               if (iat /= jat) then
                  cn_local(jat) = cn_local(jat) + countf * self%directed_factor
               end if

            end do
         end do
      end do
      !$omp end do
      !$omp critical (ncoord_)
      cn(:) = cn(:) + cn_local(:)
      !$omp end critical (ncoord_)
      deallocate(cn_local)
      !$omp end parallel

   end subroutine ncoord

   subroutine ncoord_list(self, mol, trans, cn, list)
      !> Coordination number container
      class(ncoord_type), intent(in) :: self
      !> Molecular structure data
      type(structure_type), intent(in) :: mol
      !> Lattice points
      real(wp), intent(in) :: trans(:, :)
      !> Error function coordination number.
      real(wp), intent(out) :: cn(:)
      !> Adjacency list for neighbourlist-based CN evaluation
      type(adjacency_list), intent(in) :: list

      integer :: iat, jat, kat, izp, jzp, itr
      real(wp) :: r2, r1, rij(3), countf, cutoff2, den

      ! Thread-private array for reduction
      real(wp), allocatable :: cn_local(:)

      cn(:) = 0.0_wp
      cutoff2 = self%cutoff**2

      !$omp parallel default(none) &
      !$omp shared(self, mol, list, trans, cutoff2, cn) &
      !$omp private(jat, kat, itr, izp, jzp, r2, rij, r1, den, countf) &
      !$omp private(cn_local)
      allocate(cn_local, source=cn)
      !$omp do schedule(runtime)
      do iat = 1, mol%nat
         izp = mol%id(iat)
         do kat = list%inl(iat) + 1, list%inl(iat) + list%nnl(iat)
            jat = list%nlat(kat)
            jzp = mol%id(jat)
            den = self%get_en_factor(izp, jzp)

            do itr = 1, size(trans, dim=2)
               rij = mol%xyz(:, iat) - (mol%xyz(:, jat) + trans(:, itr))
               r2 = sum(rij**2)
               if (r2 > cutoff2 .or. r2 < 1.0e-12_wp) cycle
               r1 = sqrt(r2)

               countf = den * self%ncoord_count(izp, jzp, r1)

               cn_local(iat) = cn_local(iat) + countf
               if (iat /= jat) then
                  cn_local(jat) = cn_local(jat) + countf * self%directed_factor
               end if

            end do
         end do
      end do
      !$omp end do
      !$omp critical (ncoord_list_)
      cn(:) = cn(:) + cn_local(:)
      !$omp end critical (ncoord_list_)
      deallocate(cn_local)
      !$omp end parallel

   end subroutine ncoord_list

   subroutine ncoord_d(self, mol, trans, cn, dcndr, dcndL)
      !> Coordination number container
      class(ncoord_type), intent(in) :: self
      !> Molecular structure data
      type(structure_type), intent(in) :: mol
      !> Lattice points
      real(wp), intent(in) :: trans(:, :)
      !> Error function coordination number.
      real(wp), intent(out) :: cn(:)
      !> Derivative of the CN with respect to the Cartesian coordinates.
      real(wp), intent(out) :: dcndr(:, :, :)
      !> Derivative of the CN with respect to strain deformations.
      real(wp), intent(out) :: dcndL(:, :, :)

      integer :: iat, jat, izp, jzp, itr
      real(wp) :: r2, r1, rij(3), countf, countd(3), sigma(3, 3), cutoff2, den

      ! Thread-private arrays for reduction
      real(wp), allocatable :: cn_local(:)
      real(wp), allocatable :: dcndr_local(:, :, :), dcndL_local(:, :, :)

      cn(:) = 0.0_wp
      dcndr(:, :, :) = 0.0_wp
      dcndL(:, :, :) = 0.0_wp
      cutoff2 = self%cutoff**2

      !$omp parallel default(none) &
      !$omp shared(self, mol, trans, cutoff2, cn, dcndr, dcndL) &
      !$omp private(jat, itr, izp, jzp, r2, rij, r1, den, countf, countd) &
      !$omp private(sigma, cn_local, dcndr_local, dcndL_local)
      allocate(cn_local, source=cn)
      allocate(dcndr_local, source=dcndr)
      allocate(dcndL_local, source=dcndL)
      !$omp do schedule(runtime)
      do iat = 1, mol%nat
         izp = mol%id(iat)
         do jat = 1, iat
            jzp = mol%id(jat)
            den = self%get_en_factor(izp, jzp)

            do itr = 1, size(trans, dim=2)
               rij = mol%xyz(:, iat) - (mol%xyz(:, jat) + trans(:, itr))
               r2 = sum(rij**2)
               if (r2 > cutoff2 .or. r2 < 1.0e-12_wp) cycle
               r1 = sqrt(r2)

               countf = den * self%ncoord_count(izp, jzp, r1)
               countd = den * self%ncoord_dcount(izp, jzp, r1) * rij/r1

               cn_local(iat) = cn_local(iat) + countf
               if (iat /= jat) then
                  cn_local(jat) = cn_local(jat) + countf * self%directed_factor
               end if

               dcndr_local(:, iat, iat) = dcndr_local(:, iat, iat) + countd
               dcndr_local(:, jat, jat) = dcndr_local(:, jat, jat) - countd * self%directed_factor
               dcndr_local(:, iat, jat) = dcndr_local(:, iat, jat) + countd * self%directed_factor
               dcndr_local(:, jat, iat) = dcndr_local(:, jat, iat) - countd

               sigma = spread(countd, 1, 3) * spread(rij, 2, 3)

               dcndL_local(:, :, iat) = dcndL_local(:, :, iat) + sigma
               if (iat /= jat) then
                  dcndL_local(:, :, jat) = dcndL_local(:, :, jat) + sigma * self%directed_factor
               end if

            end do
         end do
      end do
      !$omp end do
      !$omp critical (ncoord_d_)
      cn(:) = cn(:) + cn_local(:)
      dcndr(:, :, :) = dcndr(:, :, :) + dcndr_local(:, :, :)
      dcndL(:, :, :) = dcndL(:, :, :) + dcndL_local(:, :, :)
      !$omp end critical (ncoord_d_)
      deallocate(cn_local, dcndr_local, dcndL_local)
      !$omp end parallel

   end subroutine ncoord_d

   subroutine ncoord_d_list(self, mol, trans, cn, dcndrij, dcndrji, dcndrdiag, dcndL, list)
      !> Coordination number container
      class(ncoord_type), intent(in) :: self
      !> Molecular structure data
      type(structure_type), intent(in) :: mol
      !> Lattice points
      real(wp), intent(in) :: trans(:, :)
      !> Error function coordination number.
      real(wp), intent(out) :: cn(:)
      !> Derivative of the CN with respect to the Cartesian coordinates.
      real(wp), intent(out) :: dcndrij(:, :), dcndrji(:, :), dcndrdiag(:, :)
      !> Derivative of the CN with respect to strain deformations.
      real(wp), intent(out) :: dcndL(:, :, :)
      !> Adjacency list for neighbourlist-based CN evaluation
      type(adjacency_list), intent(in) :: list

      integer :: iat, jat, kat, izp, jzp, itr
      real(wp) :: r2, r1, rij(3), countf, countd(3), sigma(3, 3), cutoff2, den

      ! Thread-private arrays for reduction
      real(wp), allocatable :: cn_local(:)
      real(wp), allocatable :: dcndrdiag_local(:, :),  dcndL_local(:, :, :)
      real(wp), allocatable :: dcndrlistij_local(:, :), dcndrlistji_local(:, :)

      cn(:) = 0.0_wp
      dcndrij(:, :)  = 0.0_wp
      dcndrji(:, :)  = 0.0_wp
      dcndrdiag(:, :)  = 0.0_wp
      dcndL(:, :, :) = 0.0_wp
      cutoff2 = self%cutoff**2

      !$omp parallel default(none) &
      !$omp shared(self, mol, list, trans, cutoff2, cn, dcndrij, dcndrji, dcndrdiag, dcndL) &
      !$omp private(jat, kat, itr, izp, jzp, r2, rij, r1, den, countf, countd) &
      !$omp private(sigma, cn_local, dcndrlistij_local, dcndrlistji_local, dcndrdiag_local, dcndL_local)
      allocate(cn_local, source=cn)
      allocate(dcndrlistij_local, source=dcndrij)
      allocate(dcndrlistji_local, source=dcndrji)
      allocate(dcndrdiag_local, source=dcndrdiag)
      allocate(dcndL_local, source=dcndL)
      !$omp do schedule(runtime)
      do iat = 1, mol%nat
         izp = mol%id(iat)
         do kat = list%inl(iat) + 1, list%inl(iat) + list%nnl(iat)
            jat = list%nlat(kat)
            jzp = mol%id(jat)
            den = self%get_en_factor(izp, jzp)

            do itr = 1, size(trans, dim=2)
               rij = mol%xyz(:, iat) - (mol%xyz(:, jat) + trans(:, itr))
               r2 = sum(rij**2)
               if (r2 > cutoff2 .or. r2 < 1.0e-12_wp) cycle
               r1 = sqrt(r2)

               countf = den * self%ncoord_count(izp, jzp, r1)
               countd = den * self%ncoord_dcount(izp, jzp, r1) * rij/r1

               cn_local(iat) = cn_local(iat) + countf
               if (iat /= jat) then
                  cn_local(jat) = cn_local(jat) + countf * self%directed_factor
               end if

               ! store off-diagonal block (iat,jat)
               dcndrlistij_local(:, kat) = dcndrlistij_local(:, kat) + countd * self%directed_factor
               dcndrlistji_local(:, kat) = dcndrlistji_local(:, kat) - countd

               ! accumulate diagonals
               dcndrdiag_local(:,iat) = dcndrdiag_local(:,iat) + countd
               dcndrdiag_local(:,jat) = dcndrdiag_local(:,jat) - countd * self%directed_factor

               sigma = spread(countd, 1, 3) * spread(rij, 2, 3)

               dcndL_local(:, :, iat) = dcndL_local(:, :, iat) + sigma
               if (iat /= jat) then
                  dcndL_local(:, :, jat) = dcndL_local(:, :, jat) + sigma * self%directed_factor
               end if

            end do
         end do
      end do
      !$omp end do
      !$omp critical (ncoord_d_list_)
      cn(:)            = cn(:)            + cn_local(:)
      dcndrij(:, :)  = dcndrij(:, :)  + dcndrlistij_local(:, :)
      dcndrji(:, :)  = dcndrji(:, :)  + dcndrlistji_local(:, :)
      dcndrdiag(:, :)  = dcndrdiag(:, :)  + dcndrdiag_local(:, :)
      dcndL(:, :, :)   = dcndL(:, :, :)   + dcndL_local(:, :, :)
      !$omp end critical (ncoord_d_list_)
      deallocate(cn_local, dcndrlistij_local, dcndrlistji_local, dcndrdiag_local, dcndL_local)
      !$omp end parallel

   end subroutine ncoord_d_list


   subroutine add_coordination_number_derivs(self, mol, trans, dEdcn, gradient, sigma)

      !> Coordination number container
      class(ncoord_type), intent(in) :: self

      !> Molecular structure data
      type(structure_type), intent(in) :: mol

      !> Lattice points
      real(wp), intent(in) :: trans(:, :)

      !> Derivative of expression with respect to the coordination number
      real(wp), intent(in) :: dEdcn(:)

      !> Derivative of the CN with respect to the Cartesian coordinates
      real(wp), intent(inout) :: gradient(:, :)

      !> Derivative of the CN with respect to strain deformations
      real(wp), intent(inout) :: sigma(:, :)

      integer :: iat, jat, izp, jzp, itr
      real(wp) :: r2, r1, rij(3), countd(3), ds(3, 3), cutoff2, den

      ! Thread-private arrays for reduction
      ! Set to zero explicitly as the shared variants are potentially non-zero (inout)
      real(wp), allocatable :: gradient_local(:, :), sigma_local(:, :)

      cutoff2 = self%cutoff**2

      !$omp parallel default(none) &
      !$omp shared(self, mol, trans, cutoff2, dEdcn, gradient, sigma) &
      !$omp private(iat, jat, itr, izp, jzp, r2, rij, r1, countd, ds, den) &
      !$omp private(gradient_local, sigma_local)
      allocate(gradient_local(size(gradient, 1), size(gradient, 2)), source=0.0_wp)
      allocate(sigma_local(size(sigma, 1), size(sigma, 2)), source=0.0_wp)
      !$omp do schedule(runtime)
      do iat = 1, mol%nat
         izp = mol%id(iat)
         do jat = 1, iat
            jzp = mol%id(jat)
            den = self%get_en_factor(izp, jzp)

            do itr = 1, size(trans, dim=2)
               rij = mol%xyz(:, iat) - (mol%xyz(:, jat) + trans(:, itr))
               r2 = sum(rij**2)
               if (r2 > cutoff2 .or. r2 < 1.0e-12_wp) cycle
               r1 = sqrt(r2)

               countd = den * self%ncoord_dcount(izp, jzp, r1) * rij/r1

               gradient_local(:, iat) = gradient_local(:, iat) + countd &
               & * (dEdcn(iat) + dEdcn(jat) * self%directed_factor)
               gradient_local(:, jat) = gradient_local(:, jat) - countd &
               & * (dEdcn(iat) + dEdcn(jat) * self%directed_factor)

               ds = spread(countd, 1, 3) * spread(rij, 2, 3)

               sigma_local(:, :) = sigma_local(:, :) &
               & + ds * (dEdcn(iat) + &
               & merge(dEdcn(jat) * self%directed_factor, 0.0_wp, jat /= iat))
            end do
         end do
      end do
      !$omp end do
      !$omp critical (add_coordination_number_derivs_)
      gradient(:, :) = gradient(:, :) + gradient_local(:, :)
      sigma(:, :) = sigma(:, :) + sigma_local(:, :)
      !$omp end critical (add_coordination_number_derivs_)
      deallocate(gradient_local, sigma_local)
      !$omp end parallel

   end subroutine add_coordination_number_derivs

   subroutine add_coordination_number_derivs_list(self, mol, trans, dEdcn, gradient, sigma, list)

      !> Coordination number container
      class(ncoord_type), intent(in) :: self

      !> Molecular structure data
      type(structure_type), intent(in) :: mol

      !> Lattice points
      real(wp), intent(in) :: trans(:, :)

      !> Derivative of expression with respect to the coordination number
      real(wp), intent(in) :: dEdcn(:)

      !> Derivative of the CN with respect to the Cartesian coordinates
      real(wp), intent(inout) :: gradient(:, :)

      !> Derivative of the CN with respect to strain deformations
      real(wp), intent(inout) :: sigma(:, :)

      !> Adjacency list for neighbourlist-based CN evaluation
      type(adjacency_list), intent(in) :: list

      integer :: iat, jat, kat, izp, jzp, itr
      real(wp) :: r2, r1, rij(3), countd(3), ds(3, 3), cutoff2, den, fac

      ! Thread-private arrays for reduction
      real(wp), allocatable :: gradient_local(:, :), sigma_local(:, :)

      cutoff2 = self%cutoff**2

      !$omp parallel default(none) &
      !$omp shared(self, mol, list, trans, cutoff2, dEdcn, gradient, sigma) &
      !$omp private(iat, jat, kat, itr, izp, jzp, r2, rij, r1, countd, ds, den, fac) &
      !$omp private(gradient_local, sigma_local)
      allocate(gradient_local(size(gradient, 1), size(gradient, 2)), source=0.0_wp)
      allocate(sigma_local(size(sigma, 1), size(sigma, 2)), source=0.0_wp)

      !$omp do schedule(runtime)
      do iat = 1, mol%nat
         izp = mol%id(iat)
         do kat = list%inl(iat) + 1, list%inl(iat) + list%nnl(iat)
            jat = list%nlat(kat)
            jzp = mol%id(jat)
            den = self%get_en_factor(izp, jzp)

            do itr = 1, size(trans, dim=2)
               rij = mol%xyz(:, iat) - (mol%xyz(:, jat) + trans(:, itr))
               r2 = sum(rij**2)
               if (r2 > cutoff2 .or. r2 < 1.0e-12_wp) cycle
               r1 = sqrt(r2)

               ! Derivative of distance w.r.t coordinates
               countd = den * self%ncoord_dcount(izp, jzp, r1) * rij/r1

               ! Based on ncoord_d_list: if iat == jat, only CN(iat) is updated.
               ! If iat /= jat, the neighbor CN(jat) is also updated (scaled by directed_factor).
               fac = dEdcn(iat) + merge(dEdcn(jat) * self%directed_factor, 0.0_wp, iat /= jat)

               ! Update atomic gradients (Chain rule: dE/dR = dE/dCN * dCN/dR)
               ! If iat == jat, these two updates cancel out, giving a zero gradient for
               ! self-images, which is physically correct for translation invariance.
               gradient_local(:, iat) = gradient_local(:, iat) + countd * fac
               gradient_local(:, jat) = gradient_local(:, jat) - countd * fac

               ! Update Virial/Stress (Chain rule: dE/deps = dE/dCN * dCN/deps)
               ds = spread(countd, 1, 3) * spread(rij, 2, 3)
               sigma_local(:, :) = sigma_local(:, :) + ds * fac

            end do
         end do
      end do
      !$omp end do

      !$omp critical (add_coordination_number_derivs_list_)
      gradient(:, :) = gradient(:, :) + gradient_local(:, :)
      sigma(:, :) = sigma(:, :) + sigma_local(:, :)
      !$omp end critical (add_coordination_number_derivs_list_)

      deallocate(gradient_local, sigma_local)
      !$omp end parallel

   end subroutine add_coordination_number_derivs_list

   subroutine add_coordination_number_derivs_list_wsc(self, mol, trans, dEdcn, gradient, sigma, list)

      class(ncoord_type), intent(in) :: self
      type(structure_type), intent(in) :: mol
      real(wp), intent(in) :: trans(:, :)
      real(wp), intent(in) :: dEdcn(:)
      real(wp), intent(inout) :: gradient(:, :)
      real(wp), intent(inout) :: sigma(:, :)
      type(adjacency_list), intent(in) :: list

      integer :: iat, jat, kat, izp, jzp, img_idx, itr
      real(wp) :: r2, r1, rij(3), countd(3), ds(3, 3), den, fac, wsw
      real(wp), allocatable :: gradient_local(:, :), sigma_local(:, :)

      !$omp parallel default(none) &
      !$omp shared(self, mol, list, dEdcn, gradient, sigma) &
      !$omp private(iat, jat, kat, img_idx, itr, izp, jzp, r2, r1, rij, countd, ds, den, fac, wsw) &
      !$omp private(gradient_local, sigma_local)

      allocate(gradient_local(size(gradient, 1), size(gradient, 2)), source=0.0_wp)
      allocate(sigma_local(size(sigma, 1), size(sigma, 2)), source=0.0_wp)

      !$omp do schedule(runtime)
      do iat = 1, mol%nat
         izp = mol%id(iat)

         ! 1. Loop over the neighbors found by generate_3d
         do kat = list%inl(iat) + 1, list%inl(iat) + list%nnl(iat)
            jat = list%nlat(kat)
            jzp = mol%id(jat)
            den = self%get_en_factor(izp, jzp)

            ! Weight for equivalent images (Wigner-Seitz boundary cases)
            wsw = 1.0_wp / real(list%nimg(kat), wp)

            ! 2. Loop ONLY over the translations stored in the list for this pair
            do img_idx = 1, list%nimg(kat)
               itr = list%tridx(img_idx, kat)

               ! rij = R_iat - (R_jat + T)
               ! Note: generate_3d uses: vec = R_iat - R_jat - trans
               rij = mol%xyz(:, iat) - mol%xyz(:, jat) - list%trans(:, itr)

               r2 = sum(rij**2)
               if (r2 < 1.0e-12_wp) cycle ! Safety for T=0 self-interaction
               r1 = sqrt(r2)

               ! Derivative of CN w.r.t distance
               countd = den * self%ncoord_dcount(izp, jzp, r1) * rij/r1

               ! Chain rule factor: dE/dCN
               fac = dEdcn(iat) + merge(dEdcn(jat) * self%directed_factor, 0.0_wp, iat /= jat)

               ! Apply weight and update local arrays
               ! Gradient: dE/dR
               gradient_local(:, iat) = gradient_local(:, iat) + (wsw * fac) * countd
               gradient_local(:, jat) = gradient_local(:, jat) - (wsw * fac) * countd

               ! Virial/Stress: rij (outer product) dE/drij
               ds = spread(countd, 1, 3) * spread(rij, 2, 3)
               sigma_local(:, :) = sigma_local(:, :) + (wsw * fac) * ds
            end do
         end do
      end do
      !$omp end do

      !$omp critical (reduction_add_cn)
      gradient(:, :) = gradient(:, :) + gradient_local(:, :)
      sigma(:, :) = sigma(:, :) + sigma_local(:, :)
      !$omp end critical (reduction_add_cn)

      deallocate(gradient_local, sigma_local)
      !$omp end parallel

   end subroutine add_coordination_number_derivs_list_wsc


   !> Evaluates pairwise electronegativity factor if non applies
   elemental function get_en_factor(self, izp, jzp) result(en_factor)
      !> Coordination number container
      class(ncoord_type), intent(in) :: self
      !> Atom i index
      integer, intent(in)  :: izp
      !> Atom j index
      integer, intent(in)  :: jzp

      real(wp) :: en_factor

      en_factor = 1.0_wp

   end function get_en_factor


   !> Cutoff function for large coordination numbers
   pure subroutine cut_coordination_number(cn_max, cn, dcndr, dcndL)

      !> Maximum CN (not strictly obeyed)
      real(wp), intent(in) :: cn_max

      !> On input coordination number, on output modified CN
      real(wp), intent(inout) :: cn(:)

      !> On input derivative of CN w.r.t. cartesian coordinates,
      !> on output derivative of modified CN
      real(wp), intent(inout), optional :: dcndr(:, :, :)

      !> On input derivative of CN w.r.t. strain deformation,
      !> on output derivative of modified CN
      real(wp), intent(inout), optional :: dcndL(:, :, :)

      real(wp) :: dcnpdcn
      integer  :: iat

      if (present(dcndL)) then
         do iat = 1, size(cn)
            dcnpdcn = dlog_cn_cut(cn(iat), cn_max)
            dcndL(:, :, iat) = dcnpdcn*dcndL(:, :, iat)
         enddo
      endif

      if (present(dcndr)) then
         do iat = 1, size(cn)
            dcnpdcn = dlog_cn_cut(cn(iat), cn_max)
            dcndr(:, :, iat) = dcnpdcn*dcndr(:, :, iat)
         enddo
      endif

      do iat = 1, size(cn)
         cn(iat) = log_cn_cut(cn(iat), cn_max)
      enddo

   end subroutine cut_coordination_number

   elemental function log_cn_cut(cn, cnmax) result(cnp)
      real(wp), intent(in) :: cn
      real(wp), intent(in) :: cnmax
      real(wp) :: cnp
      cnp = log(1.0_wp + exp(cnmax)) - log(1.0_wp + exp(cnmax - cn))
   end function log_cn_cut

   elemental function dlog_cn_cut(cn, cnmax) result(dcnpdcn)
      real(wp), intent(in) :: cn
      real(wp), intent(in) :: cnmax
      real(wp) :: dcnpdcn
      dcnpdcn = exp(cnmax)/(exp(cnmax) + exp(cn))
   end function dlog_cn_cut

end module mctc_ncoord_type
