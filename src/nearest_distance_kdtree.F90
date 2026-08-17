! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

subroutine nearest_distance_kdtree(nb_nodes, ghost, lonlat, nb_locations, locations)

use, intrinsic :: iso_C_binding, only: c_int, c_double
use atlas_module, only: atlas_Geometry, atlas_IndexKDTree, atlas_kind_idx
use fckit_mpi_module, only: fckit_mpi_comm
use fckit_log_module, only : log

use yomvar, only: jpim, jprb, tlocation

#ifdef WITH_SCM_PLUME_PLUGIN_PROFILER
use plugin_profiler_mod
#endif

#include "profiler_macros.h"

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
real(c_double), allocatable :: tree_lonlats(:,:)

type(fckit_mpi_comm) :: mpi_comm
integer :: mpi_size
real(c_double) :: dist
integer(jpim) :: nb_non_ghost_nodes

real(c_double), allocatable :: nearest_dist_allpts_local(:)
real(c_double), allocatable :: nearest_dist_batchpts_local(:)
real(c_double), allocatable :: nearest_dist_batchpts_gathered(:)
integer :: nb_batches
integer :: ibatch
integer :: loc_index
integer :: batch_size
integer :: min_pidx_in_batch
integer :: max_pidx_in_batch
integer :: nb_batch_points
integer :: min_rank_for_batch_point


! MPI information
mpi_comm = fckit_mpi_comm()
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
START_PLUGIN_TIMER("scm_setup.nearest_distance.kdtree_build")
geometry = atlas_Geometry("UnitSphere")
kdtree = atlas_IndexKDTree(geometry)
call kdtree%reserve(nb_non_ghost_nodes)

! insert nodes
do jnode = 1, nb_non_ghost_nodes
  call kdtree%insert(tree_lonlats(jnode,1), tree_lonlats(jnode,2), tree_indices(jnode))
end do

! Build the KDTree
call kdtree%build()
STOP_PLUGIN_TIMER("scm_setup.nearest_distance.kdtree_build")


! Find the closest (local) point for each location
START_PLUGIN_TIMER("scm_setup.nearest_distance.kdtree_query")
allocate(nearest_dist_allpts_local(nb_locations))
do iloc=1, nb_locations
  plonlat(1) = locations(iloc)%rloni_user
  plonlat(2) = locations(iloc)%rlati_user
  
  call kdtree%closestPoint(plonlat, nearest_idx, dist)

  nearest_dist_allpts_local(iloc) = dist

  locations(iloc)%iloc  = nearest_idx
  locations(iloc)%rloni = lonlat(1,nearest_idx)
  locations(iloc)%rlati = lonlat(2,nearest_idx)
enddo
STOP_PLUGIN_TIMER("scm_setup.nearest_distance.kdtree_query")


! Split the locations in batches
START_PLUGIN_TIMER("scm_setup.nearest_distance.allgather")
batch_size = 100
nb_batches = nb_locations / batch_size
if (mod(nb_locations, batch_size) /= 0) then
  nb_batches = nb_batches + 1
endif

! gather in batches
allocate(nearest_dist_batchpts_local(batch_size))
allocate(nearest_dist_batchpts_gathered(mpi_size*batch_size))

loc_index = 1
do ibatch=1,nb_batches

  min_pidx_in_batch = loc_index
  max_pidx_in_batch = min(min_pidx_in_batch+batch_size-1, nb_locations)
  nb_batch_points = max_pidx_in_batch - min_pidx_in_batch + 1

  nearest_dist_batchpts_local = HUGE(1.0_jprb)
  nearest_dist_batchpts_local(1:nb_batch_points) = nearest_dist_allpts_local(min_pidx_in_batch:max_pidx_in_batch)

  ! all gather the distances for this batch
  call mpi_comm%allgather(nearest_dist_batchpts_local, nearest_dist_batchpts_gathered, batch_size)

  ! check min distance across all gathered distances
  do iloc=1,nb_batch_points
    min_rank_for_batch_point = minloc( nearest_dist_batchpts_gathered(iloc:mpi_size*batch_size:batch_size),1 )
    locations(min_pidx_in_batch+iloc-1)%iproc = min_rank_for_batch_point
  enddo

  loc_index = loc_index + batch_size
enddo
STOP_PLUGIN_TIMER("scm_setup.nearest_distance.allgather")
! ========================================================================

call kdtree%final()
call geometry%final()

end subroutine nearest_distance_kdtree
