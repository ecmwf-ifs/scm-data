subroutine ncdf_varsetup1c (kncid, xtype, nvdims, vdims, &
 varname, longname, unitsname)

use yomvar

implicit none

!---global variables
INTEGER(KIND=JPIM)             :: kncid, xtype, nvdims, vdims(nvdims)
character (len = *) :: varname, unitsname, longname

!---local variables
INTEGER(KIND=JPIM)             :: ivarid, istatus

#include "netcdf.inc"

!     ------------------------------------------------------------------

istatus = NF_DEF_VAR (kncid, varname, xtype, nvdims, vdims, ivarid)
call handle_err_nc(istatus)
istatus = NF_PUT_ATT_TEXT (kncid, ivarid, 'units',   len(unitsname), unitsname)
call handle_err_nc(istatus)
istatus = NF_PUT_ATT_TEXT (kncid, ivarid, 'long_name', len(longname), longname)
call handle_err_nc(istatus)

end subroutine ncdf_varsetup1c
