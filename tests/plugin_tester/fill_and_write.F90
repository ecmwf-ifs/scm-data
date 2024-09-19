subroutine fill_And_write(INFO, &
                          LOCATIONS, &
                          nb_locations, &
                          zlat, &
                          zlon, &
                          nb_nodes, &
                          ghost, &
                          lonlat, &
                          myproc, &
                          zdelta, &
                          LAREA, &
                          PVAH, &
                          PVBH, &
                          DATAID, &
                          nlev, &
                          nstep, &
                          sfcfields, &
                          nproc, &
                          fvm, &
                          nodepoints, &
                          windfield, &
                          gpfields_from_sp, &
                          gridpoints, &
                          gpfields)

use, intrinsic :: iso_C_binding
use fckit_log_module, only : log

use atlas_module, only: atlas_fvm_Method
use atlas_module, only: atlas_functionspace_NodeColumns
use atlas_module, only: atlas_functionspace_StructuredColumns
use atlas_module, only: atlas_Field
use atlas_module, only: atlas_FieldSet

use yomvar

implicit none

TYPE(TINFO), intent(inout) :: INFO
TYPE(TLOCATION), ALLOCATABLE:: LOCATIONS(:)
INTEGER(KIND=JPIM) :: nb_locations
REAL(KIND=JPRB), ALLOCATABLE :: zlat(:)
REAL(KIND=JPRB), ALLOCATABLE :: zlon(:)
INTEGER(KIND=JPIM), intent(IN) :: nb_nodes
INTEGER(KIND=c_int), POINTER, intent(IN)  :: ghost(:)
REAL(KIND=c_double), POINTER,  intent(IN) :: lonlat(:,:)
INTEGER(KIND=JPIM), intent(IN) :: myproc
REAL(KIND=JPRB), intent(IN) :: zdelta
logical :: LAREA
REAL(KIND=JPRB), ALLOCATABLE :: PVAH(:)
REAL(KIND=JPRB), ALLOCATABLE :: PVBH(:)
CHARACTER(LEN=30) :: DATAID
INTEGER(KIND=JPIM) :: nlev
INTEGER(KIND=JPIM) :: nstep

type(atlas_FieldSet) :: sfcfields
INTEGER(KIND=JPIM) :: nproc
type(atlas_fvm_Method) :: fvm
type(atlas_functionspace_NodeColumns) :: nodepoints
type(atlas_Field) :: windfield
type(atlas_FieldSet) :: gpfields_from_sp
type(atlas_functionspace_StructuredColumns) :: gridpoints
type(atlas_FieldSet) :: gpfields


! internal
INTEGER(KIND=JPIM) :: J
INTEGER(KIND=JPIM) :: iloc
CHARACTER*127 msg

#include "nearest_distance.h"
#include "compute_fields.h"
#include "fillvar.h"
#include "su_wrt_nc.h"
#include "wrt1c_nc.h"

INFO%NSTEP=nstep
allocate(locations(nb_locations))

do j=1, nb_locations
  if( zlon(j) < 0. ) then
    zlon(j) = 360.+zlon(j)
  endif
  locations(j)%RLONI = zlon(j)
  locations(j)%RLATI = zlat(j)
  locations(j)%ILOC = -1
  locations(j)%IFILE_ID = -1
  locations(j)%IPROC = -1
enddo

call nearest_distance(nb_nodes, ghost, lonlat, myproc, zdelta, nb_locations, locations)

do j=1, nb_locations
  if( myproc == locations(j)%iproc ) then
    write(msg,'(A,I0)') "nearest point proc ", locations(j)%IPROC ; call log%info(msg)
    write(msg,'(A,I0,2(1X,F8.4))') "nearest point lon ", j, locations(j)%RLONI, ZLON(j) ; call log%info(msg)
    write(msg,'(A,I0,2(1X,F8.4))') "nearest point lat ", j, locations(j)%RLATI, ZLAT(j) ; call log%info(msg)
    write(msg,'(A,I0,1X, I0)') "nearest point knode ", j, locations(j)%iloc ; call log%info(msg)
    write(*,*) 'test if unique proc: ', myproc, locations(j)%iloc, locations(j)%RLONI, locations(j)%RLATI
  endif
enddo

write(msg,'(A)') "finished nearest distances "; call log%info(msg) 

if (.not.larea) then
!!!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(iloc)
  do iloc=1, nb_locations
    if( myproc == locations(iloc)%iproc ) then
      ! fill sfc variables      
      call fillvar(myproc,locations(iloc),sfcfields)
    endif
  enddo
!!!$OMP END PARALLEL DO
endif

write(msg,'(A)') " compute fields "; call log%info(msg)
call compute_fields(nproc,myproc,nb_locations,locations(1:nb_locations),nlev,&
  &pvah,pvbh,fvm,nodepoints, windfield,gpfields_from_sp, gridpoints, gpfields)
do iloc=1, nb_locations
  if( myproc == locations(iloc)%iproc ) then
    ! netcdf write this location from this processor
    if (.not.larea) then
      write(msg,'(A)') " setting up output fields to netcdf  "; call log%info(msg)
      write(msg,'(A,I0)') " loc processor ", locations(iloc)%IPROC; call log%info(msg)
      write(msg,'(A,I0)') " loc knode ", locations(iloc)%ILOC; call log%info(msg)
      write(msg,'(A,F8.4)') " loc latitude ", locations(iloc)%RLATI; call log%info(msg)
      write(msg,'(A,F8.4)') " loc longitude ", locations(iloc)%RLONI; call log%info(msg)
      write(msg,'(A,F8.4)') " loc pressure ", locations(iloc)%PP%PLNSP; call log%info(msg)

      write(msg,'(A,I0,1X,I0)') " writing output fields to netcdf  ", INFO%ISTEP, INFO%IDATE ; 
      call log%info(msg)
      
      CALL SU_WRT_NC (myproc,PVAH,PVBH,dataid,iloc,locations(iloc)%IFILE_ID,nlev,999)
      CALL WRT1C_NC(locations(iloc),PVAH,PVBH,INFO,locations(iloc)%IFILE_ID,nlev)
    endif
  endif
enddo

write(msg,'(A)') " finished, cleaning up! "; call log%info(msg)

do iloc=1, nb_locations
  if( myproc == locations(iloc)%iproc ) then
    call DEALLOCATE_COLUMNS(locations(iloc)%PP)
  endif
enddo

deallocate(pvah)
deallocate(pvbh)
deallocate(locations)


end subroutine fill_and_write