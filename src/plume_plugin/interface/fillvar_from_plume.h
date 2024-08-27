INTERFACE
SUBROUTINE FILLVAR_FROM_PLUME(myproc,ilocation, gpfields)

use atlas_module
use yomvar

implicit none

INTEGER(KIND=JPIM), intent(in) :: myproc
TYPE(TLOCATION), target, intent(inout) :: ilocation
type(atlas_FieldSet), intent(in) :: gpfields

end subroutine FILLVAR_FROM_PLUME
END INTERFACE
