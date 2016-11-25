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

INTEGER(KIND=JPIM) :: jnode, iloc, k, jpoints, ilat, ilon
REAL(KIND=JPRB) :: zlonc, zlatc, zlon, zlat, zdist, zrad, zdeg2rad, zpi, zrad2deg

zpi = 2.0_jprb*asin(1.0_jprb)
zdeg2rad = zpi/180._jprb
zrad = zpdelta * zdeg2rad
zrad2deg = 180._jprb/zpi

!! playing around ... 
!!$fieldg = fvm%create_global_field('sample_global',atlas_real(c_double),[2])
!!$fieldl  = fvm%create_field('sample_local',atlas_real(c_double),[2])
!!$
!!$call fieldg%data(fieldg_data)
!!$
!!$if( myproc == 1 ) then
!!$  lats => grid%lat()
!!$  nlon => grid%nlon()
!!$  jnode=0
!!$  do ilat=1, grid%nlat()
!!$    do ilon=1, nlon(ilat)
!!$      jnode=jnode+1
!!$      zlon = real(ilon-1, jprb)*360._jprb/real(nlon(ilat), jprb)
!!$      zlat = lats(ilat)
!!$!      write(*,*) ' diag: ', jnode, zlon, zlat
!!$      fieldg_data(1,jnode) = zlon
!!$      fieldg_data(2,jnode) = zlat
!!$    enddo
!!$  enddo
!!$endif
!!$call fvm%scatter(fieldg,fieldl)
!!$call fieldl%data(dat_ll)
!!$dat_ll(1,:) = lonlat(1,:)
!!$dat_ll(2,:) = lonlat(2,:)
!!$do jnode=1, nb_nodes
!!$  if(( dat_ll(1,jnode) /= lonlat(1,jnode) ) .or. (dat_ll(2,jnode) /=  lonlat(2,jnode) ) ) then
!!$    write(*,*) 'myproc jnode ll ', myproc, jnode, dat_ll(1,jnode),  lonlat(1,jnode), dat_ll(2,jnode),  lonlat(2,jnode)
!!$  endif
!!$enddo

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
