INTERFACE
SUBROUTINE READ_GRIB_SCM(LSINGLE, MYPROC, FILE,LPROGNOSTIC,LAREA,INFO, &
		     & spectral, spfields, gridpoints, gpfields)

use, intrinsic :: iso_C_binding

use atlas_module, only : atlas_Field, atlas_FieldSet, atlas_functionspace_StructuredColumns, atlas_functionspace_Spectral, &
 & atlas_Metadata, atlas_real, atlas_mpi_size
USE GRIB_API, only : GRIB_READ_FROM_FILE, GRIB_NEW_FROM_MESSAGE, GRIB_GET, GRIB_RELEASE, & 
  & GRIB_OPEN_FILE, GRIB_COUNT_IN_FILE, GRIB_CLOSE_FILE, GRIB_SUCCESS
USE MPL_MODULE, only : mpl_init, mpl_broadcast, mpl_send, mpl_recv, &
  & mpl_barrier, mpl_wait,jp_non_blocking_standard, jp_blocking_standard
use yomvar

implicit none

INTEGER(KIND=JPIM), PARAMETER :: JPMAXGRID = 5120_JPIM*2560_JPIM
!INTEGER(KIND=JPIM), PARAMETER :: JPMAXGRID = 1280_JPIM*640_JPIM

LOGICAL, intent(in) :: LSINGLE
INTEGER(KIND=JPIM),intent(in) :: MYPROC
CHARACTER(len=30), intent(in) :: FILE


type(atlas_FieldSet), intent(inout) :: spfields
type(atlas_FieldSet), intent(inout) :: gpfields
type(atlas_functionspace_StructuredColumns), intent(in) :: gridpoints
type(atlas_functionspace_Spectral), intent(in)          :: spectral

TYPE(TINFO), intent(inout) :: INFO
LOGICAL, intent(in) :: lprognostic,larea

end subroutine read_grib_scm
END INTERFACE
