INTERFACE
subroutine compute_fields(myproc,nb_locations,locations,klev,pvah,pvbh,&
			  & fvm,nodepoints, windfield,gpfields_from_sp,gridpoints,gpfields)

use, intrinsic :: iso_C_binding
use atlas_module
use atlas_mpi_module
use yomvar

implicit none

INTEGER(KIND=JPIM), intent(in) :: myproc
INTEGER(KIND=JPIM), intent(in) :: nb_locations
TYPE(TLOCATION), target, intent(in) :: locations(nb_locations)
INTEGER(KIND=JPIM), intent(in) :: klev
REAL(KIND=JPRB), intent(in) :: pvah(0:klev),pvbh(0:klev)
type(atlas_FieldSet),intent(in) :: gpfields_from_sp, gpfields
type(atlas_Field), intent(in) :: windfield
type(atlas_fvm_Method), intent(in) :: fvm
type(atlas_functionspace_NodeColumns), intent(in) :: nodepoints
type(atlas_functionspace_StructuredColumns), intent(in) :: gridpoints

end subroutine compute_fields
END INTERFACE
