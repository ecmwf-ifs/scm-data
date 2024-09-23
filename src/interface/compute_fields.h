! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

INTERFACE
subroutine compute_fields(nproc,myproc,nb_locations,locations,klev,pvah,pvbh,&
			  & fvm,nodepoints, windfield,gpfields_from_sp,gridpoints,gpfields)

use, intrinsic :: iso_C_binding
use fckit_mpi_module, only : fckit_mpi_comm
use fckit_log_module, only : log
use atlas_module
use yomvar

implicit none

INTEGER(KIND=JPIM), intent(in) :: nproc
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
