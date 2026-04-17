! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

INTERFACE
subroutine getini1c_run(return_code, sfc_grib, cld_grib, spec_grib, namelist_file)

use, intrinsic :: iso_C_binding
use fckit_mpi_module, only : fckit_mpi_comm
use fckit_log_module, only : log
use atlas_module, only: atlas_Config, atlas_StructuredGrid, atlas_Mesh, atlas_mesh_Nodes, atlas_MeshGenerator, &
 & atlas_Trans, atlas_functionspace_Spectral, atlas_fvm_Method, atlas_functionspace_NodeColumns, atlas_Field, atlas_FieldSet, &
 & atlas_Metadata, atlas_real, atlas_Meshgenerator, atlas_functionspace_StructuredColumns, atlas_Partitioner
use yomvar

implicit none

INTEGER(KIND=JPIM), intent(out) :: return_code
character(len=*), intent(in), optional :: sfc_grib
character(len=*), intent(in), optional :: cld_grib
character(len=*), intent(in), optional :: spec_grib
character(len=*), intent(in), optional :: namelist_file

type(atlas_Config) :: config
type(atlas_StructuredGrid) :: grid
type(atlas_Mesh) :: mesh
type(atlas_mesh_Nodes) :: nodes
type(atlas_MeshGenerator) :: meshgenerator
type(atlas_Partitioner) :: partitioner
type(atlas_Trans)                           :: trans
type(atlas_functionspace_Spectral)          :: spectral
type(atlas_fvm_Method) :: fvm
type(atlas_functionspace_NodeColumns) :: nodepoints
type(atlas_functionspace_StructuredColumns) :: gridpoints, dummy
type(atlas_Field) :: field
type(atlas_FieldSet) :: sfcfields, gpfields, gpfields_from_sp, gpdummy
type(atlas_FieldSet) :: spfields
type(atlas_Metadata) :: metadata
type(atlas_Field) :: vorfield, divfield, windfield, ghostField, lonlatField

REAL(KIND=c_double),POINTER :: ffvalues(:)
REAL(KIND=c_double),POINTER :: vor(:,:), div(:,:)
INTEGER(KIND=c_int), POINTER :: ghost(:)
REAL(KIND=c_double), POINTER :: lonlat(:,:)

TYPE(TLOCATION), ALLOCATABLE:: LOCATIONS(:)
TYPE(TINFO) :: INFO

character(len=30) :: dataid, cgrid, file
character(len=10) :: fieldname

REAL(KIND=JPRB) :: zdelta
logical   :: LAREA, lprognostic, LSINGLE
INTEGER(KIND=JPIM) :: nsmax, nstep, I, J, jfld, ilev, iloc, isize, nb_locations, nb_nodes, nlev, iparam, nlocmax
INTEGER(KIND=JPIM) :: NPROC, MYPROC
REAL(KIND=JPRB), ALLOCATABLE :: PVAH(:), PVBH(:)
REAL(KIND=JPRB), ALLOCATABLE :: zlat(:), zlon(:)

character(127) :: msg
type(fckit_mpi_comm) :: mpi_comm

end subroutine getini1c_run
END INTERFACE