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

!> Unit tests for compressed sparse row neighbor lists
module test_csrlist
   use mctc_csrlist, only : csr_list, new_csr_list, update_list, compute_grid, &
      & get_linked_cell, grid_type
   use mctc_cutoff, only : get_lattice_points
   use mctc_env, only : wp, i8, timer_type, format_time
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, &
      & test_failed, check
   use mctc_io_math, only : outerprod
   use mctc_io_resize, only : resize
   use mctc_io_structure, only : structure_type, new
   use mctc_wignerseitz, only : wignerseitz_cell, new_wignerseitz_cell
   use testsuite_structure, only : get_structure
   implicit none
   private

   public :: collect_csrlist

   !> Tolerance for floating-point comparisons
   real(wp), parameter :: thr = 100*epsilon(1.0_wp)


contains


!> Collect all exported unit tests
subroutine collect_csrlist(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("grid-methane", test_grid_methane), &
      & new_unittest("grid-water", test_grid_water), &
      & new_unittest("grid-fullerene-cut-1", test_grid_fullerene_cut_1), &
      & new_unittest("grid-water-cut-1", test_grid_water_cut_1), &
      & new_unittest("grid-nacl", test_grid_nacl), &
      & new_unittest("grid-feo2", test_grid_feo2), &
      & new_unittest("grid-x02-1x1x4", test_grid_x02_114), &
      & new_unittest("distance-fullerene-cut-5", test_distance_fullerene_cut_5), &
      & new_unittest("distance-feo2-cut-5", test_distance_feo2_cut_5), &
      & new_unittest("csr-vs-verlet-water", test_list_water), &
      & new_unittest("csr-vs-verlet-methane", test_list_methane), &
      & new_unittest("csr-vs-verlet-fullerene", test_list_fullerene), &
      & new_unittest("csr-vs-verlet-nacl", test_list_nacl), &
      & new_unittest("csr-vs-verlet-feo2", test_list_feo2), &
      & new_unittest("csr-vs-verlet-x01", test_list_x01), &
      & new_unittest("csr-vs-verlet-x02", test_list_x02), &
      & new_unittest("csr-vs-verlet-x02-1x1x4", test_list_x02_114), &
      & new_unittest("csr-vs-verlet-mb01", test_list_mb01), &
      & new_unittest("csr-vs-verlet-mb02", test_list_mb02), &
      & new_unittest("csr-vs-verlet-water-complete", test_list_water_complete), &
      & new_unittest("csr-vs-verlet-methane-complete", test_list_methane_complete), &
      & new_unittest("csr-vs-verlet-fullerene-complete", test_list_fullerene_complete), &
      & new_unittest("csr-vs-verlet-mb09-complete", test_list_mb09_complete), &
      & new_unittest("csr-vs-verlet-mb10-complete", test_list_mb10_complete), &
      & new_unittest("csr-vs-verlet-nacl-complete", test_list_nacl_complete), &
      & new_unittest("csr-vs-verlet-feo2-complete", test_list_feo2_complete), &
      & new_unittest("csr-vs-verlet-x04-complete", test_list_x04_complete), &
      & new_unittest("csr-vs-verlet-x05-complete", test_list_x05_complete), &
      & new_unittest("nacl-wsc", test_nacl_wsc), &
      & new_unittest("feo2-wsc", test_feo2_wsc), &
      & new_unittest("update-mindless01", test_update_mb01), &
      & new_unittest("update-mindless01-complete", test_update_mb01_complete), &
      & new_unittest("update-feo2", test_update_feo2), &
      & new_unittest("update-feo2-complete", test_update_feo2_complete), &
      & new_unittest("update-x02", test_update_x02), &
      & new_unittest("update-nacl-wsc", test_update_nacl_wsc), &
      & new_unittest("update-feo2-wsc-complete", test_update_feo2_wsc_complete), &
      & new_unittest("update-x02-wsc", test_update_x02_wsc), &
      & new_unittest("update-wrong-size", test_update_wrong_size, should_fail=.true.) &
      & ]

end subroutine collect_csrlist

!> Generate reference 1-based CSR list
subroutine gen_verlet(mol, trans, cutoff, row_ptr, col_ind, nltr, complete)

   !> Molecular structure data
   type(structure_type), intent(in) :: mol

   !> Translation vectors for all images
   real(wp), intent(in) :: trans(:, :)

   !> Realspace cutoff for neighborlist generation
   real(wp), intent(in) :: cutoff

   !> Standard 1-based row pointer array of size (nat + 1)
   integer, intent(inout) :: row_ptr(:)

   !> Column indices / neighboring atom indices (nlat)
   integer, allocatable, intent(out) :: col_ind(:)

   !> Cell index of the neighboring atom
   integer, allocatable, intent(out) :: nltr(:)

   !> Whether a complete or a symmetrical upper-triangular map should be generated
   logical, intent(in) :: complete

   integer :: iat, jat, itr, nnz
   real(wp) :: r2, vec(3), cutoff2

   nnz = 0
   cutoff2 = cutoff**2

   call resize(col_ind, 10*mol%nat)
   call resize(nltr, 10*mol%nat)
   row_ptr(1) = 1

   do iat = 1, mol%nat
      ! 1. Insert Diagonal Element at start of row
      nnz = nnz + 1
      if (size(col_ind) < nnz) call resize(col_ind)
      if (size(nltr) < nnz) call resize(nltr)

      col_ind(nnz) = iat
      nltr(nnz) = 1

      ! 2. Off-Diagonal neighbors (inverse to fit the linked-cell list ordering)
      do jat = 1, mol%nat
         ! Skip lower triangle if incomplete
         if (.not. complete .and. jat < iat) cycle

         do itr = 1, size(trans, 2)
            ! Skip identity self-interaction (already added as diagonal)
            if (iat == jat .and. itr == 1) cycle

            vec(:) = mol%xyz(:, iat) - mol%xyz(:, jat) - trans(:, itr)
            r2 = sum(vec**2)
            if (r2 < epsilon(cutoff2) .or. r2 > cutoff2) cycle

            nnz = nnz + 1
            if (size(col_ind) < nnz) call resize(col_ind)
            if (size(nltr) < nnz) call resize(nltr)
            col_ind(nnz) = jat
            nltr(nnz) = itr
         end do
      end do
      row_ptr(iat + 1) = nnz + 1
   end do

   call resize(col_ind, nnz)
   call resize(nltr, nnz)

end subroutine gen_verlet

!> Replicate a structure along each lattice-vector direction
subroutine make_supercell(mol, rep)

   !> Structure to replicate
   type(structure_type), intent(inout) :: mol

   !> Replication factors along the three lattice vectors
   integer, intent(in) :: rep(3)

   real(wp), allocatable :: xyz(:, :), lattice(:, :)
   integer, allocatable :: num(:)
   integer :: i, j, k, l, c

   num = [(mol%num(mol%id), i=1, product(rep))]
   lattice = reshape(&
      [rep(1)*mol%lattice(:, 1), rep(2)*mol%lattice(:, 2), rep(3)*mol%lattice(:, 3)], &
      shape(mol%lattice))
   allocate(xyz(3, product(rep)*mol%nat))
   c = 0
   do i = 0, rep(1)-1
      do j = 0, rep(2)-1
         do k = 0, rep(3)-1
            xyz(:, c+1:c+mol%nat) = mol%xyz &
               & + outerprod(matmul(mol%lattice, [real(wp):: i, j, k]), &
               & [(1.0_wp, l=1, mol%nat)])
            c = c + mol%nat
         end do
      end do
   end do

   call new(mol, num, xyz, lattice=lattice)
end subroutine make_supercell

subroutine test_grid_gen(error, mol, cutoff, ref_nxyz)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   !> Molecular structure data
   type(structure_type), intent(in) :: mol

   !> Cutoff radius
   real(wp), intent(in) :: cutoff

   !> Reference grid dimensions
   integer, intent(in) :: ref_nxyz(3)

   type(grid_type) :: grid

   integer :: n_xyz(3)
   real(wp) :: lat_inv(3, 3), cell_w(3), det

   if (any(mol%periodic)) then
      call compute_grid(mol=mol, cutoff=cutoff, grid=grid, det=det, lat_inv=lat_inv)
      if (any(grid%n_xyz /= ref_nxyz)) then
         call test_failed(error, "Grid dimensions do not match reference.")
         write(*,*) "Generated grid:", grid%n_xyz
         write(*,*) "Reference grid:", ref_nxyz
      end if
   else
      call compute_grid(mol=mol, cutoff=cutoff, grid=grid, det=det, cell_w=cell_w)
      if (any(grid%n_xyz /= ref_nxyz)) then
         call test_failed(error, "Grid dimensions do not match reference.")
         write(*,*) "Generated grid:", grid%n_xyz
         write(*,*) "Reference grid:", ref_nxyz
      end if

      if (any(cell_w <= cutoff .and. grid%n_xyz > 1)) then
         call test_failed(error, "Linked-cell width is smaller than cutoff radius.")
         write(*,*) "Generated cell_width:", cell_w
         write(*,*) "Cutoff:", cutoff
      end if

   end if

end subroutine test_grid_gen

subroutine test_distance(error, mol, cutoff, trans, cmp)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   !> Molecular structure data
   type(structure_type), intent(in) :: mol

   !> Cutoff radius
   real(wp), intent(in) :: cutoff

   !> Translation vectors for all images
   real(wp), intent(in) :: trans(:, :)

   !> Whether a complete or a symmetrical reduced map should be generated
   logical, intent(in) :: cmp

   type(csr_list), allocatable :: list

   real(wp) :: vec(3)
   integer :: iat, jat, itr
   integer(i8) :: kat

   allocate(list)
   call new_csr_list(list, mol, error, cutoff=cutoff, trans=trans, complete=cmp)

   do iat = 1, mol%nat
      do kat = list%inl(iat) + 1, list%inl(iat+1) - 1
         jat = list%nlat(kat)
         if (allocated(list%nltr)) then
            itr = list%nltr(kat)
         else
            itr = 1
         end if
         vec(:) = mol%xyz(:, iat) - mol%xyz(:, jat) - trans(:, itr)
         if (sum(vec**2) >= cutoff**2) then
            call test_failed(error, &
               & "The pair in the neighbor list is outside the cutoff radius.")
            exit
         end if
      end do
   end do


end subroutine test_distance

subroutine test_mol_list_gen(error, mol, cutoff, trans, cmp)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   !> Molecular structure data
   type(structure_type), intent(in) :: mol

   !> Cutoff radius
   real(wp), intent(in) :: cutoff

   !> Translation vectors for all images
   real(wp), intent(in) :: trans(:, :)

   !> Whether a complete or a symmetrical reduced map should be generated
   logical, intent(in) :: cmp

   type(csr_list), allocatable :: list

   integer, allocatable :: ref_ptr(:), ref_list(:), ref_nltr(:)
   integer :: iat, jat
   integer(i8) :: kat

   allocate(list)
   call new_csr_list(list, mol, error, cutoff=cutoff, trans=trans, complete=cmp)

   allocate(ref_ptr(mol%nat + 1))
   call gen_verlet(mol, trans, cutoff, ref_ptr, ref_list, ref_nltr, cmp)

   if (any(list%inl /= ref_ptr)) then
      call test_failed(error, "neighbor list pointer array does not match reference.")
      print"(20a)", "Generated pointer:"
      print"(10i6)", list%inl
      print"(20a)", "Reference pointer:"
      print"(10i6)", ref_ptr
   end if

   do iat = 1, mol%nat
      do kat = list%inl(iat), list%inl(iat+1) - 1
         jat = list%nlat(kat)
         if (.not. any(jat == ref_list(ref_ptr(iat):ref_ptr(iat+1)-1))) then
            call test_failed(error, "neighbor list array does not match reference.")
            print"(20a)", "Generated list:"
            print"(10i6)", list%nlat
            print"(20a)", "Reference list:"
            print"(10i6)", ref_list
            exit
         end if
         if (any(jat == list%nlat(kat+1:list%inl(iat+1)-1))) then
            call test_failed(error, "neighbors duplicates.")
            exit
         end if
      end do
   end do

end subroutine test_mol_list_gen

subroutine test_pbc_list_gen(error, mol, cutoff, trans, cmp)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   !> Molecular structure data
   type(structure_type), intent(in) :: mol

   !> Cutoff radius
   real(wp), intent(in) :: cutoff

   !> Translation vectors for all images
   real(wp), intent(in) :: trans(:, :)

   !> Whether a complete or a symmetrical reduced map should be generated
   logical, intent(in) :: cmp

   type(csr_list), allocatable   :: list

   ! Reference Lists
   integer, allocatable :: ref_ptr(:), ref_list(:), ref_nltr(:)
   integer :: iat, jat, tr
   integer(i8) :: kat

   allocate(list)
   call new_csr_list(list, mol, error, cutoff=cutoff, trans=trans, complete=cmp)

   allocate(ref_ptr(mol%nat + 1))
   call gen_verlet(mol, trans, cutoff, ref_ptr, ref_list, ref_nltr, cmp)

   if (any(list%inl /= ref_ptr)) then
      call test_failed(error, "neighbor list pointer array does not match reference.")
      write(*,*) "Generated pointer:", list%inl
      write(*,*) "Reference pointer:", ref_ptr
   end if

   do iat = 1, mol%nat
      do kat = list%inl(iat), list%inl(iat+1) - 1
         jat = list%nlat(kat)
         tr = list%nltr(kat)
         if (.not. any(jat == ref_list(ref_ptr(iat):ref_ptr(iat+1)-1))) then
            call test_failed(error, "neighbor list array does not match reference.")
            print"(20a)", "Generated list:"
            print"(10i6)", list%nlat
            print"(20a)", "Reference list:"
            print"(10i6)", ref_list
            exit
         end if
         if (.not. any(tr == ref_nltr(ref_ptr(iat):ref_ptr(iat+1)-1))) then
            call test_failed(error, &
               & "neighbor list translations array does not match reference.")
            print"(20a)", "Generated translations:"
            print"(10i6)", list%nltr
            print"(20a)", "Reference translations:"
            print"(10i6)", ref_nltr
            exit
         end if
      end do
   end do

end subroutine test_pbc_list_gen

subroutine test_wsc(error, mol, cutoff, cmp, ref_list, ref_nimg)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   !> Molecular structure data
   type(structure_type), intent(in) :: mol

   !> Cutoff radius
   real(wp), intent(in) :: cutoff

   !> Whether a complete or a symmetrical reduced map should be generated
   logical, intent(in) :: cmp

   !> Reference neighbor list
   integer, intent(in) :: ref_list(:)

   !> Reference array of the number of translation images
   integer, intent(in) :: ref_nimg(:)

   type(csr_list), allocatable :: list
   type(wignerseitz_cell) :: wsc
   integer :: iat, jat, ntr, tridx
   integer(i8) :: itr, kat
   real(wp) :: vec(3)

   allocate(list)
   call new_csr_list(list, mol, error, wsc, cutoff=cutoff, complete=cmp)

   do iat = 1, mol%nat
      do kat = list%inl(iat), list%inl(iat+1) - 1
         jat = list%nlat(kat)
         ntr = wsc%nimg_list(kat)
         if (.not. any(jat == ref_list(list%inl(iat): list%inl(iat+1) - 1))) then
            call test_failed(error, "neighbor list array does not match reference.")
            print"(20a)", "Generated list:"
            print"(10i6)", list%nlat
            print"(20a)", "Reference list:"
            print"(10i6)", ref_list
            exit
         end if
         if (.not. ntr == ref_nimg(kat)) then
            call test_failed(error, &
               & "neighbor list WSC translations number array does not match " // &
               & "reference.")
            print"(20a)", "Generated translations:"
            print"(10i6)", wsc%nimg_list
            print"(20a)", "Reference translations:"
            print"(10i6)", ref_nimg
            exit
         end if
         do itr = wsc%itr_list(kat), wsc%itr_list(kat+1) - 1
            tridx = wsc%tridx_list(itr)
            vec = mol%xyz(:, jat) - mol%xyz(:, iat) &
               & + wsc%trans(:, wsc%tridx_list(itr))
            if (sum(vec**2) >= cutoff**2) then
               call test_failed(error, &
                  & "The pair in the neighbor list is outside the cutoff radius.")
               exit
            end if
         end do
      end do
   end do

end subroutine test_wsc


!> Species dependent cutoff matrix for the neighbor list
subroutine get_cutmat(mol, scale, cutmat)

   !> Molecular structure data
   type(structure_type), intent(in) :: mol

   !> Scaling factor applied to all cutoffs
   real(wp), intent(in) :: scale

   !> Cutoff matrix for each pair of species
   real(wp), allocatable, intent(out) :: cutmat(:, :)

   integer :: isp, jsp

   allocate(cutmat(mol%nid, mol%nid))
   do isp = 1, mol%nid
      do jsp = 1, mol%nid
         cutmat(jsp, isp) = scale * (7.0_wp + 1.5_wp*real(isp + jsp, wp) &
            & + 2.0_wp*real(mod(isp*jsp, 3), wp))
      end do
   end do

end subroutine get_cutmat


!> Compare two neighbor lists, the order of the neighbors within a row may differ
subroutine compare_list(error, mol, list, ref)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   !> Molecular structure data
   type(structure_type), intent(in) :: mol

   !> Neighbor list to check
   type(csr_list), intent(in) :: list

   !> Reference neighbor list
   type(csr_list), intent(in) :: ref

   logical :: found
   integer :: iat, jat
   integer(i8) :: kat, kst, kfin

   if (any(list%inl /= ref%inl)) then
      call test_failed(error, "neighbor list pointer array does not match reference.")
      write(*,*) "Generated pointer:", list%inl
      write(*,*) "Reference pointer:", ref%inl
      return
   end if

   do iat = 1, mol%nat
      kst = ref%inl(iat)
      kfin = ref%inl(iat+1) - 1
      if (list%nlat(kst) /= iat) then
         call test_failed(error, "diagonal entry is not the first of its row.")
         return
      end if
      ! Each neighbor and translation occurs once per row
      do kat = kst + 1, kfin
         jat = list%nlat(kat)
         if (allocated(list%nltr)) then
            found = any(jat == ref%nlat(kst+1:kfin) &
               & .and. list%nltr(kat) == ref%nltr(kst+1:kfin))
         else
            found = any(jat == ref%nlat(kst+1:kfin))
         end if
         if (.not. found) then
            call test_failed(error, "neighbor list array does not match reference.")
            print"(20a)", "Generated row:"
            print"(10i6)", list%nlat(kst:kfin)
            print"(20a)", "Reference row:"
            print"(10i6)", ref%nlat(kst:kfin)
            return
         end if
      end do
   end do

end subroutine compare_list


!> Compare two Wigner-Seitz neighbor lists including the images of each pair
subroutine compare_wsc_list(error, mol, list, wsc, ref, ref_wsc)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   !> Molecular structure data
   type(structure_type), intent(in) :: mol

   !> Neighbor list to check
   type(csr_list), intent(in) :: list

   !> Wigner-Seitz cell of the neighbor list to check
   type(wignerseitz_cell), intent(in) :: wsc

   !> Reference neighbor list
   type(csr_list), intent(in) :: ref

   !> Wigner-Seitz cell of the reference neighbor list
   type(wignerseitz_cell), intent(in) :: ref_wsc

   integer :: iat, jat, nimg
   integer(i8) :: kat, kref, kst, kfin, itr, ref_itr

   if (any(list%inl /= ref%inl)) then
      call test_failed(error, "neighbor list pointer array does not match reference.")
      write(*,*) "Generated pointer:", list%inl
      write(*,*) "Reference pointer:", ref%inl
      return
   end if

   if (wsc%nimg_max /= ref_wsc%nimg_max) then
      call test_failed(error, "maximum number of WSC images does not match reference.")
      write(*,*) "Generated:", wsc%nimg_max, "Reference:", ref_wsc%nimg_max
      return
   end if

   do iat = 1, mol%nat
      kst = ref%inl(iat)
      kfin = ref%inl(iat+1) - 1
      ! Each neighbor occurs once per row, the diagonal entry included
      do kat = kst, kfin
         jat = list%nlat(kat)
         do kref = kst, kfin
            if (ref%nlat(kref) == jat) exit
         end do
         if (kref > kfin) then
            call test_failed(error, "neighbor list array does not match reference.")
            print"(20a)", "Generated row:"
            print"(10i6)", list%nlat(kst:kfin)
            print"(20a)", "Reference row:"
            print"(10i6)", ref%nlat(kst:kfin)
            return
         end if

         nimg = wsc%nimg_list(kat)
         itr = wsc%itr_list(kat)
         ref_itr = ref_wsc%itr_list(kref)
         if (nimg /= ref_wsc%nimg_list(kref)) then
            call test_failed(error, &
               & "neighbor list WSC translations number does not match reference.")
            write(*,*) "Pair:", iat, jat
            write(*,*) "Generated:", nimg, "Reference:", ref_wsc%nimg_list(kref)
            return
         end if
         if (any(wsc%tridx_list(itr:itr+nimg-1) &
            & /= ref_wsc%tridx_list(ref_itr:ref_itr+nimg-1))) then
            call test_failed(error, &
               & "neighbor list WSC translations do not match reference.")
            write(*,*) "Pair:", iat, jat
            write(*,*) "Generated:", wsc%tridx_list(itr:itr+nimg-1)
            write(*,*) "Reference:", ref_wsc%tridx_list(ref_itr:ref_itr+nimg-1)
            return
         end if
      end do
   end do

end subroutine compare_wsc_list


!> Check the update of a neighbor list for a new cutoff matrix against a new list
subroutine test_update_gen(error, mol, cmp)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   !> Molecular structure data
   type(structure_type), intent(in) :: mol

   !> Whether a complete or a symmetrical reduced map should be generated
   logical, intent(in) :: cmp

   type(csr_list), allocatable :: list, ref
   real(wp), allocatable :: cutmat(:, :), cutmat_small(:, :), trans(:, :)

   call get_cutmat(mol, 1.0_wp, cutmat)
   call get_cutmat(mol, 0.55_wp, cutmat_small)
   ! Shrink a single species pair even further to drop its periodic self-images
   cutmat_small(1, 1) = 0.3_wp * cutmat_small(1, 1)
   ! Common translations, such that the translation indices are comparable
   call get_lattice_points(mol%periodic, mol%lattice, maxval(cutmat), trans)

   allocate(list, ref)

   ! Smaller cutoffs remove the pairs beyond them from the list
   call new_csr_list(list, mol, error, cutmat=cutmat, trans=trans, complete=cmp)
   if (allocated(error)) return
   call update_list(list, mol, cutmat_small, error)
   if (allocated(error)) return
   call new_csr_list(ref, mol, error, cutmat=cutmat_small, trans=trans, complete=cmp)
   if (allocated(error)) return
   call compare_list(error, mol, list, ref)
   if (allocated(error)) return

   ! Larger cutoffs rebuild the list
   call new_csr_list(list, mol, error, cutmat=cutmat_small, complete=cmp)
   if (allocated(error)) return
   call update_list(list, mol, cutmat, error)
   if (allocated(error)) return
   call new_csr_list(ref, mol, error, cutmat=cutmat, complete=cmp)
   if (allocated(error)) return
   call compare_list(error, mol, list, ref)

end subroutine test_update_gen


!> Check the update of a Wigner-Seitz neighbor list for a new cutoff matrix
!> against a new list
subroutine test_update_wsc_gen(error, mol, cmp)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   !> Molecular structure data
   type(structure_type), intent(in) :: mol

   !> Whether a complete or a symmetrical reduced map should be generated
   logical, intent(in) :: cmp

   type(csr_list), allocatable :: list, ref
   type(wignerseitz_cell) :: wsc, ref_wsc
   real(wp), allocatable :: cutmat(:, :), cutmat_small(:, :)

   call get_cutmat(mol, 1.0_wp, cutmat)
   call get_cutmat(mol, 0.55_wp, cutmat_small)
   ! Shrink a single species pair even further to drop its periodic self-images
   cutmat_small(1, 1) = 0.3_wp * cutmat_small(1, 1)

   allocate(list, ref)

   ! Smaller cutoffs remove the pairs and their images from the list
   call new_csr_list(list, mol, error, wsc, cutmat=cutmat, complete=cmp)
   if (allocated(error)) return
   call update_list(list, mol, cutmat_small, error, wsc)
   if (allocated(error)) return
   call new_csr_list(ref, mol, error, ref_wsc, cutmat=cutmat_small, complete=cmp)
   if (allocated(error)) return
   call compare_wsc_list(error, mol, list, wsc, ref, ref_wsc)
   if (allocated(error)) return

   ! Larger cutoffs rebuild the list, reusing the Wigner-Seitz cells
   call new_csr_list(list, mol, error, wsc, cutmat=cutmat_small, complete=cmp)
   if (allocated(error)) return
   call update_list(list, mol, cutmat, error, wsc)
   if (allocated(error)) return
   call new_csr_list(ref, mol, error, ref_wsc, cutmat=cutmat, complete=cmp)
   if (allocated(error)) return
   call compare_wsc_list(error, mol, list, wsc, ref, ref_wsc)

end subroutine test_update_wsc_gen

subroutine test_grid_methane(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer, parameter :: ref_nxyz(3) = [ 1, 1, 1 ]

   type(structure_type) :: mol
   real(wp), parameter :: cutoff = 29.0_wp
   real(wp), allocatable :: trans(:, :)

   allocate(trans(3, 1))
   trans = 0.0_wp

   call get_structure(mol, "methane")

   call test_grid_gen(error, mol, cutoff, ref_nxyz)

end subroutine test_grid_methane

subroutine test_grid_water(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer, parameter :: ref_nxyz(3) = [ 1, 1, 1 ]

   type(structure_type) :: mol

   real(wp), parameter :: cutoff = 29.0_wp
   real(wp), allocatable :: trans(:, :)

   allocate(trans(3, 1))
   trans = 0.0_wp

   call get_structure(mol, "water")

   call test_grid_gen(error, mol, cutoff, ref_nxyz)

end subroutine test_grid_water

subroutine test_grid_fullerene_cut_1(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer, parameter :: ref_nxyz(3) = [ 13, 13, 13 ]

   type(structure_type) :: mol

   real(wp), parameter :: cutoff = 1.0_wp
   real(wp), allocatable :: trans(:, :)

   allocate(trans(3, 1))
   trans = 0.0_wp

   call get_structure(mol, "fullerene")

   call test_grid_gen(error, mol, cutoff, ref_nxyz)

end subroutine test_grid_fullerene_cut_1

subroutine test_grid_water_cut_1(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer, parameter :: ref_nxyz(3) = [ 1, 3, 1 ]

   type(structure_type) :: mol

   real(wp), parameter :: cutoff = 1.0_wp
   real(wp), allocatable :: trans(:, :)

   allocate(trans(3, 1))
   trans = 0.0_wp

   call get_structure(mol, "water")

   call test_grid_gen(error, mol, cutoff, ref_nxyz)

end subroutine test_grid_water_cut_1

subroutine test_grid_nacl(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer, parameter :: ref_nxyz(3) = [ 1, 1, 1 ]

   type(structure_type) :: mol

   real(wp), parameter :: cutoff = 29.0_wp
   real(wp), allocatable :: trans(:, :)

   call get_structure(mol, "nacl")
   call get_lattice_points(mol%periodic, mol%lattice, cutoff, trans)

   call test_grid_gen(error, mol, cutoff, ref_nxyz)

end subroutine test_grid_nacl

subroutine test_grid_feo2(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer, parameter :: ref_nxyz(3) = [ 1, 1, 1 ]

   type(structure_type) :: mol

   real(wp), parameter :: cutoff = 29.0_wp
   real(wp), allocatable :: trans(:, :)

   call get_structure(mol, "feo2")
   call get_lattice_points(mol%periodic, mol%lattice, cutoff, trans)

   call test_grid_gen(error, mol, cutoff, ref_nxyz)

end subroutine test_grid_feo2

subroutine test_grid_x02_114(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer, parameter :: ref_nxyz(3) = [ 1, 1, 3 ]

   type(structure_type) :: mol

   real(wp), parameter :: cutoff = 29.0_wp
   real(wp), allocatable :: trans(:, :)
   integer, parameter :: supercell(3) = [1, 1, 4]

   call get_structure(mol, "x02")
   call make_supercell(mol, supercell)
   call get_lattice_points(mol%periodic, mol%lattice, cutoff, trans)

   call test_grid_gen(error, mol, cutoff, ref_nxyz)

end subroutine test_grid_x02_114

subroutine test_distance_fullerene_cut_5(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   real(wp), parameter :: cutoff = 5.0_wp
   real(wp), allocatable :: trans(:, :)

   allocate(trans(3, 1))
   trans = 0.0_wp

   call get_structure(mol, "fullerene")

   call test_distance(error, mol, cutoff, trans=trans, cmp=.false.)

end subroutine test_distance_fullerene_cut_5

subroutine test_distance_feo2_cut_5(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   real(wp), parameter :: cutoff = 5.0_wp
   real(wp), allocatable :: trans(:, :)

   call get_structure(mol, "feo2")
   call get_lattice_points(mol%periodic, mol%lattice, cutoff, trans)

   call test_distance(error, mol, cutoff, trans=trans, cmp=.false.)

end subroutine test_distance_feo2_cut_5

subroutine test_list_water(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer, parameter :: ref_ptr(4) = [&
      & 1, 4, 6, &
      & 7]
   integer, parameter :: ref_list(6) = [&
      & 1, 2, 3, &
      & 2, 3, 3]

   type(structure_type) :: mol

   real(wp), parameter :: cutoff = 29.0_wp
   real(wp), allocatable :: trans(:, :)

   allocate(trans(3, 1))
   trans = 0.0_wp

   call get_structure(mol, "water")

   call test_mol_list_gen(error, mol, cutoff, trans=trans, cmp=.false.)

end subroutine test_list_water

subroutine test_list_methane(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer, parameter :: ref_ptr(6) = [&
      & 1, 6, 10, &
      & 13, 15, 16]
   integer, parameter :: ref_list(15) = [&
      & 1, 2, 3, &
      & 4, 5, 2, &
      & 3, 4, 5, &
      & 3, 4, 5, &
      & 4, 5, 5]

   type(structure_type) :: mol

   real(wp), parameter :: cutoff = 29.0_wp
   real(wp), allocatable :: trans(:, :)

   allocate(trans(3, 1))
   trans = 0.0_wp

   call get_structure(mol, "methane")

   call test_mol_list_gen(error, mol, cutoff, trans=trans, cmp=.false.)

end subroutine test_list_methane

subroutine test_list_fullerene(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   real(wp), parameter :: cutoff = 29.0_wp
   real(wp), allocatable :: trans(:, :)

   allocate(trans(3, 1))
   trans = 0.0_wp

   call get_structure(mol, "fullerene")

   call test_mol_list_gen(error, mol, cutoff, trans=trans, cmp=.false.)

end subroutine test_list_fullerene

subroutine test_list_nacl(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   real(wp), parameter :: cutoff = 29.0_wp
   real(wp), allocatable :: trans(:, :)

   call get_structure(mol, "nacl")
   call get_lattice_points(mol%periodic, mol%lattice, cutoff, trans)

   call test_pbc_list_gen(error, mol, cutoff, trans=trans, cmp=.false.)

end subroutine test_list_nacl

subroutine test_list_feo2(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   real(wp), parameter :: cutoff = 29.0_wp
   real(wp), allocatable :: trans(:, :)

   call get_structure(mol, "feo2")
   call get_lattice_points(mol%periodic, mol%lattice, cutoff, trans)

   call test_pbc_list_gen(error, mol, cutoff, trans=trans, cmp=.false.)

end subroutine test_list_feo2

subroutine test_list_x01(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   real(wp), parameter :: cutoff = 29.0_wp
   real(wp), allocatable :: trans(:, :)

   call get_structure(mol, "x01")
   call get_lattice_points(mol%periodic, mol%lattice, cutoff, trans)

   call test_pbc_list_gen(error, mol, cutoff, trans=trans, cmp=.false.)

end subroutine test_list_x01


subroutine test_list_x02(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   real(wp), parameter :: cutoff = 29.0_wp
   real(wp), allocatable :: trans(:, :)

   call get_structure(mol, "x02")
   call get_lattice_points(mol%periodic, mol%lattice, cutoff, trans)

   call test_pbc_list_gen(error, mol, cutoff, trans=trans, cmp=.false.)

end subroutine test_list_x02


subroutine test_list_x02_114(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   real(wp), parameter :: cutoff = 29.0_wp
   real(wp), allocatable :: trans(:, :)
   integer, parameter :: supercell(3) = [1, 1, 4]

   call get_structure(mol, "x02")
   call make_supercell(mol, supercell)
   call get_lattice_points(mol%periodic, mol%lattice, cutoff, trans)

   call test_pbc_list_gen(error, mol, cutoff, trans=trans, cmp=.false.)

end subroutine test_list_x02_114

subroutine test_list_mb01(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   real(wp), parameter :: cutoff = 29.0_wp
   real(wp), allocatable :: trans(:, :)

   allocate(trans(3, 1))
   trans = 0.0_wp

   call get_structure(mol, "mindless01")

   call test_mol_list_gen(error, mol, cutoff, trans=trans, cmp=.false.)

end subroutine test_list_mb01

subroutine test_list_mb02(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   real(wp), parameter :: cutoff = 29.0_wp
   real(wp), allocatable :: trans(:, :)

   allocate(trans(3, 1))
   trans = 0.0_wp

   call get_structure(mol, "mindless02")

   call test_mol_list_gen(error, mol, cutoff, trans=trans, cmp=.false.)

end subroutine test_list_mb02

subroutine test_list_water_complete(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   real(wp), parameter :: cutoff = 29.0_wp
   real(wp), allocatable :: trans(:, :)

   allocate(trans(3, 1))
   trans = 0.0_wp

   call get_structure(mol, "water")

   call test_mol_list_gen(error, mol, cutoff, trans=trans, cmp=.true.)

end subroutine test_list_water_complete

subroutine test_list_methane_complete(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   real(wp), parameter :: cutoff = 29.0_wp
   real(wp), allocatable :: trans(:, :)

   allocate(trans(3, 1))
   trans = 0.0_wp

   call get_structure(mol, "methane")

   call test_mol_list_gen(error, mol, cutoff, trans=trans, cmp=.true.)

end subroutine test_list_methane_complete

subroutine test_list_fullerene_complete(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   real(wp), parameter :: cutoff = 29.0_wp
   real(wp), allocatable :: trans(:, :)

   allocate(trans(3, 1))
   trans = 0.0_wp

   call get_structure(mol, "fullerene")

   call test_mol_list_gen(error, mol, cutoff, trans=trans, cmp=.true.)

end subroutine test_list_fullerene_complete

subroutine test_list_mb09_complete(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   real(wp), parameter :: cutoff = 29.0_wp
   real(wp), allocatable :: trans(:, :)

   allocate(trans(3, 1))
   trans = 0.0_wp

   call get_structure(mol, "mindless09")

   call test_mol_list_gen(error, mol, cutoff, trans=trans, cmp=.true.)

end subroutine test_list_mb09_complete

subroutine test_list_mb10_complete(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   real(wp), parameter :: cutoff = 29.0_wp
   real(wp), allocatable :: trans(:, :)

   allocate(trans(3, 1))
   trans = 0.0_wp

   call get_structure(mol, "mindless10")

   call test_mol_list_gen(error, mol, cutoff, trans=trans, cmp=.true.)

end subroutine test_list_mb10_complete

subroutine test_list_nacl_complete(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   real(wp), parameter :: cutoff = 29.0_wp
   real(wp), allocatable :: trans(:, :)

   call get_structure(mol, "nacl")
   call get_lattice_points(mol%periodic, mol%lattice, cutoff, trans)

   call test_pbc_list_gen(error, mol, cutoff, trans=trans, cmp=.true.)

end subroutine test_list_nacl_complete

subroutine test_list_feo2_complete(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   real(wp), parameter :: cutoff = 29.0_wp
   real(wp), allocatable :: trans(:, :)

   call get_structure(mol, "feo2")
   call get_lattice_points(mol%periodic, mol%lattice, cutoff, trans)

   call test_pbc_list_gen(error, mol, cutoff, trans=trans, cmp=.true.)

end subroutine test_list_feo2_complete

subroutine test_list_x04_complete(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   real(wp), parameter :: cutoff = 29.0_wp
   real(wp), allocatable :: trans(:, :)

   call get_structure(mol, "x04")
   call get_lattice_points(mol%periodic, mol%lattice, cutoff, trans)

   call test_pbc_list_gen(error, mol, cutoff, trans=trans, cmp=.true.)

end subroutine test_list_x04_complete

subroutine test_list_x05_complete(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   real(wp), parameter :: cutoff = 29.0_wp
   real(wp), allocatable :: trans(:, :)

   call get_structure(mol, "x05")
   call get_lattice_points(mol%periodic, mol%lattice, cutoff, trans)

   call test_pbc_list_gen(error, mol, cutoff, trans=trans, cmp=.true.)

end subroutine test_list_x05_complete

subroutine test_nacl_wsc(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   real(wp), parameter :: cutoff = 29.0_wp
   ! Reference neighbor list
   integer, parameter :: ref_list(3) = [1, 2, 2]
   ! Reference array of the number of translation images
   integer, parameter :: ref_nimg(3) = [12, 6, 12]

   call get_structure(mol, "nacl")
   call test_wsc(error, mol, cutoff, .false., ref_list, ref_nimg)

end subroutine test_nacl_wsc

subroutine test_feo2_wsc(error)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   real(wp), parameter :: cutoff = 29.0_wp

   ! Reference neighbor list
   integer, parameter :: ref_list(6) = [1, 3, 2, 2, 3, 3]
   ! Reference array of the number of translation images
   integer, parameter :: ref_nimg(6) = [6, 3, 3, 6, 3, 6]

   call get_structure(mol, "feo2")
   call test_wsc(error, mol, cutoff, .false., ref_list, ref_nimg)

end subroutine test_feo2_wsc


subroutine test_update_mb01(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol

   call get_structure(mol, "mindless01")
   call test_update_gen(error, mol, .false.)

end subroutine test_update_mb01


subroutine test_update_mb01_complete(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol

   call get_structure(mol, "mindless01")
   call test_update_gen(error, mol, .true.)

end subroutine test_update_mb01_complete


subroutine test_update_feo2(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol

   call get_structure(mol, "feo2")
   call test_update_gen(error, mol, .false.)

end subroutine test_update_feo2


subroutine test_update_feo2_complete(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol

   call get_structure(mol, "feo2")
   call test_update_gen(error, mol, .true.)

end subroutine test_update_feo2_complete


subroutine test_update_x02(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol

   call get_structure(mol, "x02")
   call test_update_gen(error, mol, .false.)

end subroutine test_update_x02


subroutine test_update_nacl_wsc(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol

   call get_structure(mol, "nacl")
   call test_update_wsc_gen(error, mol, .false.)

end subroutine test_update_nacl_wsc


subroutine test_update_feo2_wsc_complete(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol

   call get_structure(mol, "feo2")
   call test_update_wsc_gen(error, mol, .true.)

end subroutine test_update_feo2_wsc_complete


subroutine test_update_x02_wsc(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol

   call get_structure(mol, "x02")
   call test_update_wsc_gen(error, mol, .false.)

end subroutine test_update_x02_wsc


subroutine test_update_wrong_size(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   type(csr_list), allocatable :: list
   real(wp), allocatable :: cutmat(:, :)

   call get_structure(mol, "mindless01")
   call get_cutmat(mol, 1.0_wp, cutmat)

   allocate(list)
   call new_csr_list(list, mol, error, cutmat=cutmat)
   if (allocated(error)) return
   ! A cutoff matrix for a single species does not fit the structure
   call update_list(list, mol, cutmat(:1, :1), error)

end subroutine test_update_wrong_size

end module test_csrlist
