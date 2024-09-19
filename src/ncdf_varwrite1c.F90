subroutine ncdf_varwrite1c (kncid, nlev, kstart, kcount, varname, pvar)
!-----------------------------------------------------------------------
! Purpose: Write 0-dim or 1-dim variable in NetCDF file.
!
! Variables: kncid   - NetCDF file ID
!            nlev    - size of written array (1 for 0-dim)
!            kstart  - starting index(ices) of NetCDF file variable
!            kcount  - # of values written
!            varname - variable name
!            pvar    - variable
!
! Attention: in SCM pvar is defined as REAL(KIND=JPRB) (from tsmbkind.h)
!
! Martin Koehler, 11-2000
!-----------------------------------------------------------------------
use yomvar
use fckit_module, only: log => fckit_log

implicit none

!---global variables
character (len = *) :: varname
INTEGER(KIND=JPIM)           :: kncid, nlev, kstart(min(nlev,2)), kcount(min(nlev,2))
REAL(KIND=JPRB)              :: pvar(nlev)

!---local variables
REAL*4                   :: zvar1d_temp(nlev) ! single precision
!REAL(KIND=JPRB)            :: zvar1d_temp(nlev)
INTEGER(KIND=JPIM)           :: ivarid, istatus
INTEGER(KIND=JPIM), PARAMETER :: JPKD = KIND(zvar1d_temp)
CHARACTER*127 msg

#include "netcdf.inc"

!-----------------------------------------------------------------------

! write(*,*) 'Writing to NetCDF (', nlev, ' levels):  ', varname, &
!  '         mn ', sum(pvar)/nlev

! write(msg,'(A,I0,A,A,A,E)') 'Writing to NetCDF (', nlev, ' levels):  ', varname,'         mn ', sum(pvar)/nlev; call log%debug(msg)

zvar1d_temp = REAL(pvar,JPKD)         !convert REAL(KIND=JPRB) to NetCDF REAL format

istatus = NF_INQ_VARID    (kncid, varname, ivarid)
call handle_err_nc(istatus)
istatus = NF_PUT_VARA_REAL(kncid,ivarid,kstart,kcount, zvar1d_temp)     
call handle_err_nc(istatus)

end subroutine ncdf_varwrite1c
