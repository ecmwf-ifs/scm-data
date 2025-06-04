! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

INTERFACE
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

end subroutine nearest_distance_kdtree
END INTERFACE
