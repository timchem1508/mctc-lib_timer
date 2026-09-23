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

!> @file mctc/csrlist/type.f90
!> @brief Compressed Sparse Row neighbour list implementation.
!>
!> A symmetric neighbour map given in a dense format like:
!>
!>   |   | 1 | 2 | 3 | 4 | 5 | 6 |
!>   |---|---|---|---|---|---|---|
!>   | 1 |   | x |   | x | x |   |
!>   | 2 | x |   | x |   | x | x |
!>   | 3 |   | x |   | x |   | x |
!>   | 4 | x |   | x |   | x | x |
!>   | 5 | x | x |   | x |   |   |
!>   | 6 |   | x | x | x |   |   |
!>
!> is stored in two compressed arrays: `nlat` identifying the neighbouring atom
!> and `nltr` tracking its cell index. The index array `inl` maps the atomic
!> index to the offset of its row.
!>
!> ```
!> inl   =     1,       4,          8,      11,         15,      18,     21
!> nlat  =     2, 4, 5, 1, 3, 5, 6, 2, 4, 6, 1, 3, 5, 6, 1, 2, 4, 2, 3, 4
!> nltr  =     1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
!> ```
!>
!> The first element of a row (`nlat(inl(i))`, `nltr(inl(i))`) is the diagonal
!> entry of the atom itself, which is always present. Either the full matrix
!> (complete mode) or only its upper triangular part is stored. The sparsity
!> pattern is symmetric, so the format also carries asymmetric matrices.
!>
!> The slice `nlat(inl(i):inl(i+1)-1)` can be handed to standard CSR libraries
!> (MKL, cuSPARSE, ...) directly.
!>
!> Construction proceeds in three stages:
!>
!>  1. `sort_atoms` sorts the atoms into a linked cell grid,
!>  2. `build_stencil` fixes which cells are scanned around each cell,
!>  3. `neighbour_pass` walks the grid twice, once to size the CSR arrays and
!>     once to fill them.
!>
!> The grid follows the classical linked cell condition: a cell is at least one
!> cutoff wide, measured as the perpendicular thickness for a periodic system,
!> so an interacting partner can only sit in the cell itself or in one of its
!> 26 direct neighbours.
module mctc_csrlist_type
   use mctc_cutoff, only : get_lattice_points
   use mctc_env, only : wp, i8, error_type, fatal_error
   use mctc_io, only : structure_type
   use mctc_io_math, only : matdet_3x3, matinv_3x3, crossprod
   use mctc_wignerseitz, only : wignerseitz_cell, get_pairs
   implicit none
   private

   public :: csr_list, new_csr_list, compute_grid, get_linked_cell

   !> Neighbourlist in CSR format
   type :: csr_list

      !> Realspace cutoff for neighbourlist generation
      real(wp), allocatable :: cutoff

      !> Complete asymmetric neighbour list flag
      logical :: complete

      !> Offset index in the neighbour map
      integer(i8), allocatable :: inl(:)

      !> Number of neighbours for each atom
      integer, allocatable :: nnl(:)

      !> Index of the neighbouring atom
      integer, allocatable :: nlat(:)

      !> Cell index of the neighbouring atom
      integer, allocatable :: nltr(:)

      !> Lattice translation vector
      real(wp), allocatable :: trans(:, :)

      !> Wigner-Seitz cell type
      type(wignerseitz_cell), allocatable :: wsc
   end type csr_list


   !> Cell-sorted linked cell grid used during the neighbour search
   type :: grid_type

      !> Number of linked cells along the x, y and z direction
      integer :: n_xyz(3) = 1

      !> Total number of linked cells
      integer :: ncell = 0

      !> Periodic wrap-around of the grid
      logical :: periodic = .false.

      !> Every stencil cell is distinct and holds a single image of its atoms,
      !> so the translation of a pair follows from the wrap-around alone and
      !> the loop over all translations can be skipped
      logical :: single = .false.

      !> Stencil offsets of the cells scanned for each cell
      integer, allocatable :: off(:, :)

      !> Offset of each cell into the cell-ordered atom list, size ncell + 1
      integer, allocatable :: start(:)

      !> Atom index of each entry of the cell-ordered atom list
      integer, allocatable :: atom(:)

      !> Cell-ordered coordinates, kept as separate streams for vectorisation
      real(wp), allocatable :: x(:), y(:), z(:)

      !> Map of the lattice shift of a wrapped cell to its translation index
      integer :: trmap(-1:1, -1:1, -1:1) = 0
   end type grid_type


   !> Default real-space cutoff
   real(wp), parameter :: cutoff_def = 29.0_wp

   !> Default non-periodic translation vector
   real(wp), parameter :: trans_def(3, 1) = 0.0_wp

   !> Default incomplete neighbour-list mode
   logical, parameter :: complete_def = .false.

   !> Padding applied to non-periodic cell bounds
   real(wp), parameter :: buffer = 0.1_wp

   !> Relative tolerance used when snapping fractional coordinates into the cell
   real(wp), parameter :: wrap_snap = 1.0e-12_wp

   !> Tolerance for identifying a translation vector with an integer lattice shift
   real(wp), parameter :: shift_tol = 1.0e-8_wp


contains


!> Create a neighbour list for a geometry and cutoff
subroutine new_csr_list(self, mol, wsc, cutoff, trans, complete, error)

   !> Instance of the neighbourlist
   type(csr_list), intent(out) :: self

   !> Structure type
   type(structure_type), intent(in) :: mol

   !> Wigner-Seitz cell type, enables the Wigner-Seitz image search
   type(wignerseitz_cell), intent(inout), allocatable, optional :: wsc

   !> Realspace cutoff for neighbourlist generation
   real(wp), intent(in), optional :: cutoff

   !> Lattice translation vectors for periodic systems
   real(wp), intent(in), optional :: trans(:, :)

   !> Flag for complete neighbourlist generation
   logical, intent(in), optional :: complete

   !> Error status, zero on success
   type(error_type), allocatable, intent(out), optional :: error

   type(error_type), allocatable :: err

   allocate(self%cutoff)
   self%cutoff = cutoff_def
   if (present(cutoff)) self%cutoff = cutoff

   self%complete = complete_def
   if (present(complete)) self%complete = complete

   ! A set matching the cutoff is generated in build_list if none is given
   if (present(trans)) self%trans = trans

   if (any(mol%periodic) .and. present(wsc)) then
      call build_list(self, mol, err, wsc)
      call move_alloc(wsc, self%wsc)
   else
      call build_list(self, mol, err)
   end if

   if (allocated(err)) then
      call move_alloc(err, error)
   end if

end subroutine new_csr_list


!> Build the CSR neighbour list for a structure
subroutine build_list(self, mol, error, wsc)

   !> Instance of the neighbourlist
   type(csr_list), intent(inout) :: self

   !> Molecular structure data
   type(structure_type), intent(in) :: mol

   !> Error status
   type(error_type), allocatable, intent(out) :: error

   !> Wigner-Seitz cell type, enables the Wigner-Seitz image search
   type(wignerseitz_cell), intent(inout), optional :: wsc

   type(grid_type) :: grid
   logical :: periodic, use_wsc, lshift, lmulti, isok
   integer :: nat, iat, nself, nimg_max, nimg_count, n_xyz(3), self_tridx(27)
   integer(i8) :: npair, nimgs
   real(wp) :: cutoff2, lat_inv(3, 3), cell_w(3), min_xyz(3), zero_vec(3), r2_min, det

   integer, allocatable :: cnt(:), icnt(:)
   integer(i8), allocatable :: ioff(:)
   real(wp), allocatable :: trans(:, :)

   nat = mol%nat
   cutoff2 = self%cutoff**2
   periodic = any(mol%periodic)
   use_wsc = present(wsc)
   nimg_max = 0
   nself = 0
   self_tridx = 0
   min_xyz = 0.0_wp

   ! 1. Lattice translations. In Wigner-Seitz mode the jacket translations are
   ! generated here and shared with the Wigner-Seitz cell.
   if (use_wsc) then
      call get_lattice_points(mol%periodic, mol%lattice, sqrt(epsilon(0.0_wp)), trans)
      self%trans = trans
      wsc%trans = trans
      wsc%nimg_max = 0
   else if (.not. allocated(self%trans)) then
      if (periodic) then
         call get_lattice_points(mol%periodic, mol%lattice, self%cutoff, trans)
         call move_alloc(trans, self%trans)
      else
         self%trans = trans_def
      end if
   end if

   if (allocated(self%inl)) deallocate(self%inl)
   allocate(self%inl(nat + 1), source=1_i8)
   if (nat <= 0) return

   ! 2. Linked cell grid and cell-sorted copy of the atoms. Classical linked
   ! cell condition: every cell is at least one cutoff wide, measured as the
   ! perpendicular thickness, so an interacting partner can only sit in the
   ! cell itself or in one of its 26 direct neighbours.
   call compute_grid(mol, self%cutoff, det, n_xyz, lat_inv, cell_w)
   grid%n_xyz = n_xyz
   grid%ncell = n_xyz(1)*n_xyz(2)*n_xyz(3)
   grid%periodic = periodic
   if (.not. periodic) min_xyz = minval(mol%xyz, dim=2) - buffer
   call sort_atoms(mol, grid, lat_inv, min_xyz, cell_w, lshift)

   ! 3. Stencil of cells scanned around each cell
   call build_stencil(grid, lmulti)

   ! The single image shortcut needs a grid wide enough that every stencil cell
   ! is distinct, and coordinates that already live inside the primitive cell,
   ! so that the recorded translation index is valid for `mol%xyz` itself.
   grid%single = periodic .and. .not. use_wsc .and. .not. lmulti .and. .not. lshift
   if (grid%single) then
      call build_shift_map(grid, self%trans, lat_inv)
      grid%single = grid%trmap(0, 0, 0) > 0
   end if

   ! 4. Diagonal self-images are independent of the atom, resolve them once
   if (use_wsc) then
      zero_vec = 0.0_wp
      call get_pairs(self%trans, zero_vec, nimg_count, self_tridx, r2_min)
      if (nimg_count > 0 .and. r2_min <= cutoff2) nself = nimg_count
      nimg_max = nself
   end if

   ! 5. Counting pass, determines the exact size of every CSR array
   allocate(cnt(nat), source=0)
   allocate(icnt(nat), source=0)
   allocate(ioff(nat + 1), source=1_i8)
   isok = .true.
   call neighbour_pass(self, grid, .false., ioff, cnt, icnt, nself, self_tridx, &
      & nimg_max, isok, wsc)

   ! 6. CSR pointer array.
   do iat = 1, nat
      if (cnt(iat) < 0) then
         call fatal_error(error, "[Fatal] neighbour list of a single atom exceeds "// &
            & "the addressable range of the CSR index arrays")
         return
      end if
      self%inl(iat + 1) = self%inl(iat) + int(cnt(iat), i8) + 1_i8
   end do
   npair = self%inl(nat + 1) - 1_i8

   nimgs = 0_i8
   if (use_wsc) then
      do iat = 1, nat
         ioff(iat + 1) = ioff(iat) + int(icnt(iat), i8)
      end do
      nimgs = ioff(nat + 1_i8) - 1_i8
   end if

   ! 7. Exact allocation of the CSR arrays, no copies and no over-allocation
   allocate(self%nlat(npair))
   if (use_wsc) then
      allocate(wsc%nimg_list(npair))
      allocate(wsc%itr_list(npair + 1))
      allocate(wsc%tridx_list(max(nimgs, 1_i8)))
   else if (periodic) then
      allocate(self%nltr(npair))
   end if

   ! 8. Filling pass, every thread writes directly into its own rows
   call neighbour_pass(self, grid, .true., ioff, cnt, icnt, nself, self_tridx, &
      & nimg_max, isok, wsc)

   if (.not. isok) then
      call fatal_error(error, "[Fatal] counting and filling pass of the "// &
         & "neighbour list disagree")
      return
   end if

   if (use_wsc) then
      wsc%nimg_max = nimg_max
      wsc%itr_list(npair + 1_i8) = int(nimgs + 1_i8, i8)
   end if

end subroutine build_list


!> Counting and filling pass over all cell pairs
!>
!> Both passes share this traversal so that the counts of the first one match
!> the entries written by the second one exactly.
subroutine neighbour_pass(self, grid, lstore, ioff, cnt, icnt, nself, &
   & self_tridx, nimg_max, isok, wsc)

   !> Instance of the neighbourlist, filled if lstore is set
   type(csr_list), intent(inout) :: self

   !> Cell-sorted linked cell grid
   type(grid_type), intent(in) :: grid

   !> Store the entries instead of counting them
   logical, intent(in) :: lstore

   !> Offset of each atom into the image index array, only used when storing
   integer(i8), intent(in) :: ioff(:)

   !> Number of neighbours of each atom, excluding the diagonal entry
   integer, intent(inout) :: cnt(:)

   !> Number of images of each atom, including the diagonal entry
   integer, intent(inout) :: icnt(:)

   !> Number of images of the diagonal entry
   integer, intent(in) :: nself

   !> Image indices of the diagonal entry
   integer, intent(in) :: self_tridx(:)

   !> Largest number of images found for a single pair
   integer, intent(inout) :: nimg_max

   !> Consistency flag, cleared if the two passes disagree
   logical, intent(inout) :: isok

   !> Wigner-Seitz cell type, enables the Wigner-Seitz image search
   type(wignerseitz_cell), intent(inout), optional :: wsc

   integer :: nsten, ntr, nmax
   integer :: ic, s, ns, jc, p, p0, p1, q, q0, q1, t, t0, t1
   integer :: iat, jmin, nn, nimgs, nimg_count, tridx_arr(27)
   integer(i8) :: pos, itp
   real(wp) :: cutoff2, xi, yi, zi, xit, yit, zit, dx, dy, dz, r2, r2_min, vec(3)
   logical :: use_wsc, lnltr, complete
   real(wp), allocatable :: trans(:,:)

   integer, allocatable :: jcl(:), itrl(:)

   nsten = size(grid%off, 2)
   cutoff2 = self%cutoff**2
   complete = self%complete
   use_wsc = present(wsc)
   allocate(trans, source=self%trans)
   ! Only a periodic list without the Wigner-Seitz images records nltr
   lnltr = grid%periodic .and. .not. use_wsc
   ntr = 1
   if (grid%periodic) ntr = size(self%trans, 2)
   nmax = 0

   !$omp parallel default(none)&
   !$omp& private(ic, s, ns, jc, p, p0, p1, q, q0, q1, t, t0, t1) &
   !$omp& private(iat, jmin, nn, nimgs, nimg_count, tridx_arr, pos, itp) &
   !$omp& private(xi, yi, zi, xit, yit, zit, dx, dy, dz, r2, r2_min, vec) &
   !$omp& private(jcl, itrl) &
   !$omp& shared(grid, self, lstore, ioff, cnt, icnt, nself, self_tridx) &
   !$omp& shared(nimg_max, isok, wsc, cutoff2, complete, use_wsc, lnltr) &
   !$omp& shared(nsten, ntr, trans) &
   !$omp& reduction(max: nmax)
   allocate(jcl(nsten), itrl(nsten))

   !$omp do schedule(dynamic, 16)
   ! Loop over all cells in the grid instead of each individual atom
   do ic = 1, grid%ncell
      p0 = grid%start(ic)
      p1 = grid%start(ic + 1) - 1
      if (p1 < p0) cycle

      ! A. The stencil is resolved once per cell, not once per atom
      call resolve_stencil(grid, ic, jcl, itrl, ns)

      ! B. All atoms of the cell share the stencil resolved above
      do p = p0, p1
         iat = grid%atom(p)
         xi = grid%x(p)
         yi = grid%y(p)
         zi = grid%z(p)

         ! Upper triangular (jat > iat) versus complete mode (all jat). For a
         ! periodic list without Wigner-Seitz images the images of an atom
         ! with itself are separate entries of its own row, hence jat >= iat.
         if (complete) then
            jmin = 1
         else if (lnltr) then
            jmin = iat
         else
            jmin = iat + 1
         end if

         nn = 0
         nimgs = 0
         pos = 0_i8
         itp = 0_i8

         ! C. Inject the diagonal (self-interaction) at position one
         if (lstore) then
            pos = self%inl(iat)
            self%nlat(pos) = iat
            if (lnltr) self%nltr(pos) = 1
            if (use_wsc) then
               itp = ioff(iat)
               wsc%nimg_list(pos) = nself
               wsc%itr_list(pos) = int(itp)
               if (nself > 0) then
                  wsc%tridx_list(itp:itp + nself - 1) = self_tridx(1:nself)
                  itp = itp + nself
               end if
            end if
         end if

         ! D. Traverse the stencil cells
         do s = 1, ns
            jc = jcl(s)
            q0 = grid%start(jc)
            q1 = grid%start(jc + 1) - 1
            if (q1 < q0) cycle

            ! Atoms are stored in ascending order inside every cell, so the
            ! upper triangular condition is a range restriction rather than a
            ! test on each candidate. This removes the rejected half of the
            ! candidates from the distance evaluation entirely.
            if (jmin > 1) then
               q0 = lower_bound(grid%atom, q0, q1, jmin)
               if (q0 > q1) cycle
            end if

            if (use_wsc) then
               do q = q0, q1
                  vec(1) = xi - grid%x(q)
                  vec(2) = yi - grid%y(q)
                  vec(3) = zi - grid%z(q)
                  call get_pairs(trans, vec, nimg_count, tridx_arr, r2_min)
                  if (nimg_count <= 0 .or. r2_min > cutoff2) cycle
                  nn = nn + 1
                  nmax = max(nmax, nimg_count)
                  if (lstore) then
                     self%nlat(pos + nn) = grid%atom(q)
                     wsc%nimg_list(pos + nn) = nimg_count
                     wsc%itr_list(pos + nn) = int(itp)
                     wsc%tridx_list(itp:itp + nimg_count - 1) = &
                        & tridx_arr(1:nimg_count)
                     itp = itp + nimg_count
                  else
                     nimgs = nimgs + nimg_count
                  end if
               end do
               cycle
            end if

            ! One shifted reference position per translation. Where the grid
            ! resolves the image itself only that one translation is probed.
            if (itrl(s) > 0) then
               t0 = itrl(s)
               t1 = itrl(s)
            else
               t0 = 1
               t1 = ntr
            end if

            do t = t0, t1
               xit = xi - trans(1, t)
               yit = yi - trans(2, t)
               zit = zi - trans(3, t)
               do q = q0, q1
                  dx = xit - grid%x(q)
                  dy = yit - grid%y(q)
                  dz = zit - grid%z(q)
                  r2 = dx*dx + dy*dy + dz*dz
                  if (r2 > cutoff2 .or. r2 < epsilon(1.0_wp)) cycle
                  nn = nn + 1
                  if (lstore) then
                     self%nlat(pos + nn) = grid%atom(q)
                     if (lnltr) self%nltr(pos + nn) = t
                  end if
               end do
            end do
         end do

         if (lstore) then
            ! Guard against a divergence between the two passes rather than
            ! silently writing into the row of the next atom
            if (nn /= self%inl(iat + 1) - self%inl(iat) - 1_i8) isok = .false.
         else
            cnt(iat) = nn
            if (use_wsc) icnt(iat) = nimgs + nself
         end if
      end do
   end do
   !$omp end do

   deallocate(jcl, itrl)
   !$omp end parallel

   nimg_max = max(nimg_max, nmax)

end subroutine neighbour_pass


!> Resolve the stencil around one cell into the list of cells to scan
!>
!> Periodic directions wrap around, molecular ones are clipped at the boundary.
!> An entry of itrl is the translation index implied by the wrap-around, or
!> zero if the caller has to probe every translation.
pure subroutine resolve_stencil(grid, ic, jcl, itrl, ns)

   !> Cell-sorted linked cell grid
   type(grid_type), intent(in) :: grid

   !> Index of the cell the stencil is centred on
   integer, intent(in) :: ic

   !> Indices of the cells to scan
   integer, intent(out) :: jcl(:)

   !> Translation index of each cell, zero if unresolved
   integer, intent(out) :: itrl(:)

   !> Number of cells to scan
   integer, intent(out) :: ns

   integer :: nx, ny, nz, nxy, ix, iy, iz, jx, jy, jz, sx, sy, sz, s, itr

   nx = grid%n_xyz(1)
   ny = grid%n_xyz(2)
   nz = grid%n_xyz(3)
   nxy = nx*ny

   iz = (ic - 1)/nxy + 1
   iy = mod((ic - 1)/nx, ny) + 1
   ix = mod(ic - 1, nx) + 1

   ns = 0
   do s = 1, size(grid%off, 2)
      itr = 0
      if (grid%periodic) then
         call wrap_index(ix + grid%off(1, s), nx, jx, sx)
         call wrap_index(iy + grid%off(2, s), ny, jy, sy)
         call wrap_index(iz + grid%off(3, s), nz, jz, sz)
         if (grid%single) then
            itr = grid%trmap(sx, sy, sz)
            ! A shift without a matching translation cannot contribute, the
            ! general path rejects it on the distance criterion
            if (itr == 0) cycle
         end if
      else
         jx = ix + grid%off(1, s)
         jy = iy + grid%off(2, s)
         jz = iz + grid%off(3, s)
         if (jx < 1 .or. jx > nx) cycle
         if (jy < 1 .or. jy > ny) cycle
         if (jz < 1 .or. jz > nz) cycle
      end if

      ns = ns + 1
      jcl(ns) = jx + nx*(jy - 1) + nxy*(jz - 1)
      itrl(ns) = itr
   end do

end subroutine resolve_stencil


!> Wrap a cell index into the grid, reporting the lattice shift applied
pure subroutine wrap_index(i, n, j, s)

   !> Unwrapped cell index
   integer, intent(in) :: i

   !> Number of cells along this direction
   integer, intent(in) :: n

   !> Wrapped cell index
   integer, intent(out) :: j

   !> Lattice shift applied, one of -1, 0 and 1
   integer, intent(out) :: s

   j = i
   s = 0
   if (j < 1) then
      j = j + n
      s = -1
   else if (j > n) then
      j = j - n
      s = 1
   end if

end subroutine wrap_index

!> First position in the ascending range grid%atom(q0:q1) not below jmin
pure function lower_bound(gat, q0, q1, jmin) result(lo)

   !> Cell-ordered atom indices
   integer, intent(in) :: gat(:)

   !> Bounds of the range to search
   integer, intent(in) :: q0, q1

   !> Smallest acceptable atom index
   integer, intent(in) :: jmin

   !> Position of the first acceptable entry, q1 + 1 if there is none
   integer :: lo

   integer :: hi, mid

   lo = q0
   hi = q1 + 1
   do while (lo < hi)
      mid = (lo + hi)/2
      if (gat(mid) < jmin) then
         lo = mid + 1
      else
         hi = mid
      end if
   end do

end function lower_bound


!> Counting sort of all atoms into the linked cell grid
subroutine sort_atoms(mol, grid, lat_inv, min_xyz, cell_w, lshift)

   !> Structure type
   type(structure_type), intent(in) :: mol

   !> Cell-sorted linked cell grid
   type(grid_type), intent(inout) :: grid

   !> Inverse of the lattice matrix
   real(wp), intent(in) :: lat_inv(3, 3)

   !> Lower bound of the molecular grid
   real(wp), intent(in) :: min_xyz(3)

   !> Width of each grid cell
   real(wp), intent(in) :: cell_w(3)

   !> At least one atom lives outside the primitive cell
   logical, intent(out) :: lshift

   integer :: nat, ncell, nx, ny, nz, iat, ic, p, d, f, ix, iy, iz
   real(wp) :: fr(3), fw(3)
   integer, allocatable :: cidx(:), cursor(:)

   nat = mol%nat
   ncell = grid%ncell
   nx = grid%n_xyz(1)
   ny = grid%n_xyz(2)
   nz = grid%n_xyz(3)
   lshift = .false.

   allocate(cidx(nat))
   allocate(grid%start(ncell + 1), source=0)

   ! Populations are accumulated at start(ic+1) so that the prefix sum below
   ! turns the array directly into the offsets of the cell-ordered list.
   if (grid%periodic) then
      !$omp parallel do schedule(static) default(none) &
      !$omp& private(iat, fr, fw, f, d, ix, iy, iz, ic) reduction(.or.: lshift) &
      !$omp& shared(nat, nx, ny, nz, lat_inv, mol, grid, cidx)
      do iat = 1, nat
         fr = matmul(lat_inv, mol%xyz(:, iat))
         do d = 1, 3
            f = floor(fr(d))
            fw(d) = fr(d) - f
            ! Snap round-off at the upper cell face back to the lower one
            if (fw(d) >= 1.0_wp - wrap_snap) then
               fw(d) = 0.0_wp
               f = f + 1
            end if
            if (f /= 0) lshift = .true.
         end do
         ix = min(nx, max(1, int(fw(1)*nx) + 1))
         iy = min(ny, max(1, int(fw(2)*ny) + 1))
         iz = min(nz, max(1, int(fw(3)*nz) + 1))
         ic = ix + nx*(iy - 1) + nx*ny*(iz - 1)
         cidx(iat) = ic
         !$omp atomic
         grid%start(ic + 1) = grid%start(ic + 1) + 1
      end do
   else
      !$omp parallel do schedule(static) default(none) &
      !$omp& private(iat, ix, iy, iz, ic) &
      !$omp& shared(nat, nx, ny, nz, mol, min_xyz, cell_w, grid, cidx)
      do iat = 1, nat
         ix = min(nx, max(1, int((mol%xyz(1, iat) - min_xyz(1))/cell_w(1)) + 1))
         iy = min(ny, max(1, int((mol%xyz(2, iat) - min_xyz(2))/cell_w(2)) + 1))
         iz = min(nz, max(1, int((mol%xyz(3, iat) - min_xyz(3))/cell_w(3)) + 1))
         ic = ix + nx*(iy - 1) + nx*ny*(iz - 1)
         cidx(iat) = ic
         !$omp atomic
         grid%start(ic + 1) = grid%start(ic + 1) + 1
      end do
   end if

   grid%start(1) = 1
   do ic = 1, ncell
      grid%start(ic + 1) = grid%start(ic) + grid%start(ic + 1)
   end do

   ! Scatter in atom order, which keeps the neighbour list reproducible and the
   ! atoms of a cell sorted ascendingly
   allocate(cursor(ncell), source=grid%start(1:ncell))
   allocate(grid%atom(nat))
   do iat = 1, nat
      ic = cidx(iat)
      grid%atom(cursor(ic)) = iat
      cursor(ic) = cursor(ic) + 1
   end do

   ! Cell-ordered copy of the coordinates, split into three streams so that the
   ! inner loop reads three contiguous vectors instead of a strided one
   allocate(grid%x(nat), grid%y(nat), grid%z(nat))
   !$omp parallel do schedule(static) default(none) &
   !$omp& shared(nat, grid, mol) &
   !$omp& private(p, iat)
   do p = 1, nat
      iat = grid%atom(p)
      grid%x(p) = mol%xyz(1, iat)
      grid%y(p) = mol%xyz(2, iat)
      grid%z(p) = mol%xyz(3, iat)
   end do

end subroutine sort_atoms


!> Build the list of stencil offsets scanned around every cell
subroutine build_stencil(grid, lmulti)

   !> Cell-sorted linked cell grid
   type(grid_type), intent(inout) :: grid

   !> Cells are visited as several periodic images of each other
   logical, intent(out) :: lmulti

   integer :: n(3), lo(3), hi(3)
   integer :: d, di, dj, dk, nsten
   integer, allocatable :: off(:, :)

   n = grid%n_xyz
   lmulti = .false.

   do d = 1, 3
      if (grid%periodic .and. n(d) < 3) then
         lo(d) = 0
         hi(d) = n(d) - 1
         lmulti = .true.
      else
         lo(d) = -1
         hi(d) = 1
      end if
   end do

   allocate(off(3, product(hi - lo + 1)))
   nsten = 0
   do dk = lo(3), hi(3)
      do dj = lo(2), hi(2)
         do di = lo(1), hi(1)
            nsten = nsten + 1
            off(:, nsten) = [di, dj, dk]
         end do
      end do
   end do

   grid%off = off(:, :nsten)

end subroutine build_stencil


!> Map the integer lattice shift of a wrapped cell to its translation index
subroutine build_shift_map(grid, trans, lat_inv)

   !> Cell-sorted linked cell grid
   type(grid_type), intent(inout) :: grid

   !> Lattice translation vectors
   real(wp), intent(in) :: trans(:, :)

   !> Inverse of the lattice matrix
   real(wp), intent(in) :: lat_inv(3, 3)

   integer :: itr, ishift(3)
   real(wp) :: rshift(3)

   grid%trmap = 0
   do itr = 1, size(trans, 2)
      rshift = matmul(lat_inv, trans(:, itr))
      ishift = nint(rshift)
      if (any(abs(ishift) > 1)) cycle
      if (maxval(abs(rshift - ishift)) > shift_tol) cycle
      grid%trmap(ishift(1), ishift(2), ishift(3)) = itr
   end do

end subroutine build_shift_map


!> Computes linked cell grid sub-divisions for any crystal class
!>
!> Every cell is at least one cutoff wide. For a periodic system the width is
!> the perpendicular thickness of the cell slab, which is the only measure that
!> stays valid for a skewed lattice. A direction thinner than the cutoff is
!> covered by a single cell.
subroutine compute_grid(mol, cutoff, det, n_xyz, lat_inv, cell_w)
   !> Stucture type
   type(structure_type), intent(in) :: mol
   !> Interaction cutoff radius
   real(wp), intent(in) :: cutoff
   !> Determinant (Volume) of the lattice
   real(wp), intent(out) :: det
   !> Output: Number of grid cells along each axis
   integer, intent(out) :: n_xyz(3)
   !> Inverse of the lattice matrix, zero for a molecular system
   real(wp), intent(out), optional :: lat_inv(3, 3)
   !> Perpendicular width of each grid cell
   real(wp), intent(out), optional :: cell_w(3)

   real(wp) :: H(3), cross_ij(3), lattice(3, 3)
   real(wp) :: max_xyz(3), min_xyz(3)
   integer  :: i

   if (present(lat_inv)) lat_inv = 0.0_wp

   if (any(mol%periodic)) then
      ! Inverse Lattice Matrix
      lattice = mol%lattice
      det = matdet_3x3(lattice)

      if (present(lat_inv)) lat_inv = matinv_3x3(lattice)

      ! Perpendicular height H1 (normal to a2 x a3)
      cross_ij(:) = crossprod(lattice(:,2), lattice(:,3))
      H(1) = abs(det) / sqrt(sum(cross_ij**2))

      ! Perpendicular height H2 (normal to a3 x a1)
      cross_ij(:) = crossprod(lattice(:,3), lattice(:,1))
      H(2) = abs(det) / sqrt(sum(cross_ij**2))

      ! Perpendicular height H3 (normal to a1 x a2)
      cross_ij(:) = crossprod(lattice(:,1), lattice(:,2))
      H(3) = abs(det) / sqrt(sum(cross_ij**2))

      ! Map cells dynamically to the strict real-space thickness
      do i = 1, 3
         n_xyz(i) = max(1, floor(H(i) / (cutoff + tiny(1.0_wp))))
      end do

      if (present(cell_w)) cell_w = H / real(n_xyz, wp)
   else
      min_xyz = minval(mol%xyz, dim=2) - buffer
      max_xyz = maxval(mol%xyz, dim=2) + buffer

      ! Number of cells: must be at least 1, and cell width >= cutoff
      n_xyz = max(1, floor((max_xyz - min_xyz) / (cutoff + tiny(1.0_wp))))

      if (present(cell_w)) then
         cell_w = (max_xyz - min_xyz) / (real(n_xyz, wp) + tiny(1.0_wp)) &
            & + tiny(1.0_wp)
      end if
      det = product(max_xyz - min_xyz)
   end if

end subroutine compute_grid


!> Build a classical linked cell list
!>
!> Retained for compatibility, the neighbour list construction itself uses the
!> cell-sorted grid built by sort_atoms instead.
subroutine get_linked_cell(mol, n_xyz, head, nxt, ccount, lat_inv, cell_w)
   !> Stucture type
   type(structure_type), intent(in) :: mol

   !> Number of linked cells along the x, y, and z directions
   integer, intent(in) :: n_xyz(3)

   !> Flattened cell-to-atom chain heads using x + nx * (y - 1) + nx * ny * (z - 1)
   integer, intent(out) :: head(:)

   !> Next atom in each linked-cell chain, indexed by atom
   integer, intent(out) :: nxt(:)

   !> Population of each flattened linked cell
   integer, intent(out) :: ccount(:)
   !> Inverse of the lattice matrix
   real(wp), intent(in), optional :: lat_inv(3, 3)
   !> Width of each grid cell
   real(wp), intent(in), optional :: cell_w(3)

   integer :: iat, ix, iy, iz, ic
   real(wp) :: fract(3), min_xyz(3)

   head = 0
   ccount = 0

   if (any(mol%periodic) .and. present(lat_inv)) then
      !$omp parallel do default(shared) private(iat, fract, ix, iy, iz, ic)
      do iat = 1, mol%nat
         fract(:) = matmul(lat_inv, mol%xyz(:, iat))
         fract(:) = fract(:) - floor(fract(:))

         ix = min(n_xyz(1), max(1, int(fract(1) * n_xyz(1)) + 1))
         iy = min(n_xyz(2), max(1, int(fract(2) * n_xyz(2)) + 1))
         iz = min(n_xyz(3), max(1, int(fract(3) * n_xyz(3)) + 1))

         ic = ix + n_xyz(1)*(iy-1) + n_xyz(1)*n_xyz(2)*(iz-1)

         !$omp atomic capture
         nxt(iat) = head(ic)
         head(ic) = iat
         !$omp end atomic

         !$omp atomic
         ccount(ic) = ccount(ic) + 1
      end do
   else if (present(cell_w)) then
      min_xyz = minval(mol%xyz, dim=2) - buffer

      !$omp parallel do default(shared) private(iat, ix, iy, iz, ic)
      do iat = 1, mol%nat
         ix = min(n_xyz(1), &
            & max(1, int((mol%xyz(1, iat) - min_xyz(1))/cell_w(1)) + 1))
         iy = min(n_xyz(2), &
            & max(1, int((mol%xyz(2, iat) - min_xyz(2))/cell_w(2)) + 1))
         iz = min(n_xyz(3), &
            & max(1, int((mol%xyz(3, iat) - min_xyz(3))/cell_w(3)) + 1))

         ic = ix + n_xyz(1)*(iy-1) + n_xyz(1)*n_xyz(2)*(iz-1)

         !$omp atomic capture
         nxt(iat) = head(ic)
         head(ic) = iat
         !$omp end atomic

         !$omp atomic
         ccount(ic) = ccount(ic) + 1
      end do
   end if

end subroutine get_linked_cell

end module mctc_csrlist_type
