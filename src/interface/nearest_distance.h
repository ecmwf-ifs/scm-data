! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

INTERFACE
subroutine nearest_distance(nb_nodes, ghost, lonlat, myproc, zpdelta, nb_locations, locations)

use, intrinsic :: iso_C_binding
use yomvar

implicit none

INTEGER(KIND=JPIM), intent(IN) :: nb_nodes
INTEGER(KIND=c_int), POINTER, intent(IN)  :: ghost(:)
REAL(KIND=c_double), POINTER,  intent(IN) :: lonlat(:,:)
!LOGICAL, POINTER, intent(IN)  :: ghost(:)
INTEGER(KIND=JPIM), intent(IN) :: myproc
REAL(KIND=JPRB), intent(IN) :: zpdelta
INTEGER(KIND=JPIM), intent(IN) :: nb_locations
TYPE(TLOCATION), intent(INOUT) :: locations(nb_locations)

end subroutine nearest_distance
END INTERFACE
