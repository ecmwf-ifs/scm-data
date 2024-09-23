! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

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
