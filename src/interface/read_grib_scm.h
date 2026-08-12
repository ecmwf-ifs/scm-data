! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

INTERFACE
SUBROUTINE READ_GRIB_SCM(LSINGLE, NPROC, MYPROC, FILE,INFO, &
		     & spectral, spfields, gridpoints, gpfields)

use, intrinsic :: iso_C_binding

use fckit_log_module, only : log

use atlas_module, only : atlas_Field
use atlas_module, only : atlas_FieldSet
use atlas_module, only : atlas_functionspace_StructuredColumns
use atlas_module, only : atlas_functionspace_Spectral
use atlas_module, only : atlas_Metadata
use atlas_module, only : atlas_real

USE GRIB_API, only : GRIB_READ_FROM_FILE
USE GRIB_API, only : GRIB_NEW_FROM_MESSAGE
USE GRIB_API, only : GRIB_GET
USE GRIB_API, only : GRIB_GET_SIZE
USE GRIB_API, only : GRIB_RELEASE
USE GRIB_API, only : GRIB_OPEN_FILE
USE GRIB_API, only : GRIB_COUNT_IN_FILE
USE GRIB_API, only : GRIB_CLOSE_FILE
USE GRIB_API, only : GRIB_SUCCESS

USE MPL_MODULE, only : mpl_init
USE MPL_MODULE, only : mpl_broadcast
USE MPL_MODULE, only : mpl_send
USE MPL_MODULE, only : mpl_recv
USE MPL_MODULE, only : mpl_barrier
USE MPL_MODULE, only : mpl_wait
USE MPL_MODULE, only : jp_non_blocking_standard
USE MPL_MODULE, only : jp_blocking_standard

use yomvar

implicit none

!INTEGER(KIND=JPIM), PARAMETER :: JPMAXGRID = 1280_JPIM*640_JPIM
INTEGER(KIND=JPIM), PARAMETER :: JPMAXGRID = 5120_JPIM*2560_JPIM

LOGICAL, intent(in) :: LSINGLE
INTEGER(KIND=JPIM),intent(in) :: NPROC
INTEGER(KIND=JPIM),intent(in) :: MYPROC
CHARACTER(len=*), intent(in) :: FILE

type(atlas_FieldSet), intent(inout) :: spfields
type(atlas_FieldSet), intent(inout) :: gpfields
type(atlas_functionspace_StructuredColumns), intent(in) :: gridpoints
type(atlas_functionspace_Spectral), intent(in)          :: spectral

TYPE(TINFO), intent(inout) :: INFO

end subroutine read_grib_scm
END INTERFACE
