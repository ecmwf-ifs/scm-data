! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

INTERFACE
subroutine fill_And_write(INFO, &
                          LOCATIONS, &
                          nb_locations, &
                          zlat, &
                          zlon, &
                          nb_nodes, &
                          ghost, &
                          lonlat, &
                          myproc, &
                          zdelta, &
                          LAREA, &
                          PVAH, &
                          PVBH, &
                          DATAID, &
                          nlev, &
                          nstep, &
                          sfcfields, &
                          nproc, &
                          fvm, &
                          nodepoints, &
                          windfield, &
                          gpfields_from_sp, &
                          gridpoints, &
                          gpfields)

use, intrinsic :: iso_C_binding
use fckit_log_module, only : log

use atlas_module, only: atlas_fvm_Method
use atlas_module, only: atlas_functionspace_NodeColumns
use atlas_module, only: atlas_functionspace_StructuredColumns
use atlas_module, only: atlas_Field
use atlas_module, only: atlas_FieldSet

use yomvar

implicit none

TYPE(TINFO), intent(inout) :: INFO
TYPE(TLOCATION), ALLOCATABLE:: LOCATIONS(:)
INTEGER(KIND=JPIM) :: nb_locations
REAL(KIND=JPRB), ALLOCATABLE :: zlat(:)
REAL(KIND=JPRB), ALLOCATABLE :: zlon(:)
INTEGER(KIND=JPIM), intent(IN) :: nb_nodes
INTEGER(KIND=c_int), POINTER, intent(IN)  :: ghost(:)
REAL(KIND=c_double), POINTER,  intent(IN) :: lonlat(:,:)
INTEGER(KIND=JPIM), intent(IN) :: myproc
REAL(KIND=JPRB), intent(IN) :: zdelta
logical :: LAREA
REAL(KIND=JPRB), ALLOCATABLE :: PVAH(:)
REAL(KIND=JPRB), ALLOCATABLE :: PVBH(:)
CHARACTER(LEN=30) :: DATAID
INTEGER(KIND=JPIM) :: nlev
INTEGER(KIND=JPIM) :: nstep

type(atlas_FieldSet) :: sfcfields
INTEGER(KIND=JPIM) :: nproc
type(atlas_fvm_Method) :: fvm
type(atlas_functionspace_NodeColumns) :: nodepoints
type(atlas_Field) :: windfield
type(atlas_FieldSet) :: gpfields_from_sp
type(atlas_functionspace_StructuredColumns) :: gridpoints
type(atlas_FieldSet) :: gpfields


! internal
INTEGER(KIND=JPIM) :: J
INTEGER(KIND=JPIM) :: iloc
character(127) :: msg


end subroutine fill_And_write

END INTERFACE