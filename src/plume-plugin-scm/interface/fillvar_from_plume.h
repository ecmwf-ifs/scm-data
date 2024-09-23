! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

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
