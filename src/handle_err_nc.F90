subroutine handle_err_nc(kstatus)

!**** *handle_err_nc*  - Handle NetCDF errors

!     Purpose.
!     --------
!     Handle NetCDF errors

!**   Interface.
!     ----------
!        *CALL* *handle_err_nc

!        Explicit arguments :
!        --------------------


!        Implicit arguments :
!        --------------------

!     Method.
!     -------
!        See documentation

!     Externals.
!     ----------
!        None

!     Reference.
!     ----------
!        Taken and adopted from NetCDF documentation Version 3.

!     Author.
!     -------
!        Martin Koehler

!     Modifications.
!     --------------
!        Original : 00-09-12

!     ------------------------------------------------------------------

use yomvar

IMPLICIT NONE

INTEGER(KIND=JPIM) :: kstatus

#include "netcdf.inc"

if (kstatus .ne. nf_noerr) then
  print *, 'NetCDF error:  ', NF_STRERROR(kstatus)
  kstatus = kstatus/0.0              !optional core file production
  stop 'Stopped with NetCDF error'
endif

end subroutine handle_err_nc
