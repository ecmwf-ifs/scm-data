! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

subroutine nearest_distance(nb_nodes, ghost, lonlat, myproc, zpdelta, nb_locations, locations)

use, intrinsic :: iso_C_binding
use yomvar

implicit none

INTEGER(KIND=JPIM), intent(IN) :: nb_nodes
INTEGER(KIND=c_int), POINTER, intent(IN)  :: ghost(:)
REAL(KIND=c_double), POINTER,  intent(IN) :: lonlat(:,:)
INTEGER(KIND=JPIM), intent(IN) :: myproc
REAL(KIND=JPRB), intent(IN) :: zpdelta
INTEGER(KIND=JPIM), intent(IN) :: nb_locations
TYPE(TLOCATION), intent(INOUT) :: locations(nb_locations)

INTEGER(KIND=JPIM) :: jnode
INTEGER(KIND=JPIM) :: iloc
INTEGER(KIND=JPIM) :: k
INTEGER(KIND=JPIM) :: jpoints
INTEGER(KIND=JPIM) :: ilat
INTEGER(KIND=JPIM) :: ilon

REAL(KIND=JPRB) :: zlonc
REAL(KIND=JPRB) :: zlatc
REAL(KIND=JPRB) :: zlon
REAL(KIND=JPRB) :: zlat
REAL(KIND=JPRB) :: zdist
REAL(KIND=JPRB) :: zrad
REAL(KIND=JPRB) :: zdeg2rad
REAL(KIND=JPRB) :: zpi
REAL(KIND=JPRB) :: zrad2deg

zpi = 2.0_jprb*asin(1.0_jprb)
zdeg2rad = zpi/180._jprb
zrad = zpdelta * zdeg2rad
zrad2deg = 180._jprb/zpi

!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(iloc, jnode,zlonc,zlatc,zlon,zlat,zdist)
do iloc=1, nb_locations
  zlonc = locations(iloc)%rloni * zdeg2rad
  zlatc = locations(iloc)%rlati * zdeg2rad
  
  do jnode=1, nb_nodes
    zlon = lonlat(1,jnode) * zdeg2rad
    zlat = lonlat(2,jnode) * zdeg2rad
    zdist = 2._jprb * sqrt((cos(zlat) * sin((zlon-zlonc)/2._jprb)) * ( cos(zlat) * sin((zlon-zlonc)/2._jprb)) + &
     & sin((zlat-zlatc)/2._jprb) * sin((zlat-zlatc)/2._jprb))
    ! need to make sure that point location is inside core region not halo
    if( ( abs(zdist) < zrad ) .and. ( ghost(jnode) /= 1 ) ) then
      locations(iloc)%iproc = myproc
      locations(iloc)%iloc = jnode
      locations(iloc)%rloni = zlon * zrad2deg
      locations(iloc)%rlati = zlat * zrad2deg
      !write(*,*) 'details:', myproc, iloc, nb_locations, locations(iloc)%rloni,  locations(iloc)%rlati, locations(iloc)%iloc
    endif
  enddo
enddo
!$OMP END PARALLEL DO

end subroutine nearest_distance
