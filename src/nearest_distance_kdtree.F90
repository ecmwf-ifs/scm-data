! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

subroutine nearest_distance_kdtree(nb_nodes, ghost, lonlat, nb_locations, locations)

use, intrinsic :: iso_C_binding
use atlas_module
use fckit_c_interop_module
use fckit_mpi_module

use yomvar

implicit none

integer(jpim), intent(in) :: nb_nodes
integer(c_int), pointer, intent(in)  :: ghost(:)
real(c_double), pointer,  intent(in) :: lonlat(:,:)
integer(jpim), intent(in) :: nb_locations
type(tlocation), intent(inout) :: locations(nb_locations)

integer(jpim) :: jnode
integer(jpim) :: iloc
real(jprb) :: zlon
real(jprb) :: zlat
integer(atlas_kind_idx) :: nearest_idx

type(atlas_geometry) :: geometry
type(atlas_indexkdtree) :: kdtree
real(c_double) :: plonlat(2)

integer(atlas_kind_idx), allocatable :: tree_indices(:)
real(c_double), allocatable :: tree_lonlats(:,:), tree_distances(:)

type(fckit_mpi_comm) :: mpi_comm
integer :: mpi_size
integer :: mpi_rank
real(c_double) :: dist
real(c_double) :: nearest_dist
real(c_double), allocatable :: nearest_dist_all(:)
integer :: nearest_idx_rank
integer(jpim) :: nb_non_ghost_nodes

! MPI information
mpi_comm = fckit_mpi_comm()
mpi_rank = mpi_comm%rank()
mpi_size = mpi_comm%size()

! count non ghost nodes
nb_non_ghost_nodes = 0
do jnode = 1, nb_nodes
  if ( ghost(jnode) /= 1 ) then
    nb_non_ghost_nodes = nb_non_ghost_nodes + 1
  endif
enddo

! Allocation
allocate(tree_indices(nb_non_ghost_nodes))
allocate(tree_lonlats(nb_non_ghost_nodes,2))

do jnode = 1,nb_non_ghost_nodes
  zlon = lonlat(1,jnode)
  zlat = lonlat(2,jnode)

  if ( ghost(jnode) /= 1 ) then ! redundant check
    tree_indices(jnode) = jnode
    tree_lonlats(jnode,1) = zlon
    tree_lonlats(jnode,2) = zlat
  endif
enddo

! Geometry and KDTree setup
geometry = atlas_Geometry("Earth")
kdtree = atlas_IndexKDTree(geometry)
call kdtree%reserve(nb_non_ghost_nodes)

! insert nodes
do jnode = 1, nb_non_ghost_nodes
  call kdtree%insert(tree_lonlats(jnode,1), tree_lonlats(jnode,2), tree_indices(jnode))
end do

! Build the KDTree
call kdtree%build()

! Find the closest points for each location
allocate( nearest_dist_all( mpi_size ) )
do iloc=1, nb_locations
  plonlat(1) = locations(iloc)%rloni_user
  plonlat(2) = locations(iloc)%rlati_user
  
  call kdtree%closestPoint(plonlat, nearest_idx, dist)

  ! MPI reduction across processors
  call mpi_comm%allgather(dist, nearest_dist_all)

  ! proc owning the nearest idx (1-based)
  nearest_idx_rank = MINLOC(nearest_dist_all, 1)

  ! fill in the location details
  locations(iloc)%iproc = nearest_idx_rank
  locations(iloc)%iloc  = nearest_idx
  locations(iloc)%rloni = lonlat(1,nearest_idx)
  locations(iloc)%rlati = lonlat(2,nearest_idx)

enddo

call kdtree%final()
call geometry%final()

end subroutine nearest_distance_kdtree
