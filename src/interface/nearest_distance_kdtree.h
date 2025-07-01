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

use, intrinsic :: iso_C_binding, only: c_int, c_double
use yomvar, only: jpim, tlocation

implicit none

integer(jpim), intent(in) :: nb_nodes
integer(c_int), pointer, intent(in)  :: ghost(:)
real(c_double), pointer,  intent(in) :: lonlat(:,:)
integer(jpim), intent(in) :: nb_locations
type(tlocation), intent(inout) :: locations(nb_locations)

end subroutine nearest_distance_kdtree
END INTERFACE
