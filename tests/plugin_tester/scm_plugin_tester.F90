! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

module testing_model_mod

use, intrinsic :: iso_C_binding

use fckit_mpi_module, only : fckit_mpi_comm
use fckit_log_module, only : log
use fckit_configuration_module, only : fckit_configuration
use fckit_configuration_module, only : fckit_YAMLConfiguration
use fckit_pathname_module, only : fckit_pathname

use atlas_module, only: atlas_Config
use atlas_module, only: atlas_StructuredGrid
use atlas_module, only: atlas_Mesh
use atlas_module, only: atlas_mesh_Nodes
use atlas_module, only: atlas_MeshGenerator
use atlas_module, only: atlas_Trans
use atlas_module, only: atlas_functionspace_Spectral
use atlas_module, only: atlas_fvm_Method
use atlas_module, only: atlas_functionspace_NodeColumns
use atlas_module, only: atlas_Field
use atlas_module, only: atlas_FieldSet
use atlas_module, only: atlas_Metadata
use atlas_module, only: atlas_real
use atlas_module, only: atlas_Meshgenerator
use atlas_module, only: atlas_functionspace_StructuredColumns
use atlas_module, only: atlas_Partitioner

use yomvar

use plume_module

use grib_fields_provider_mod, only : grib_fields_provider
use plugin_utils_mod, only : n_fields
use plugin_utils_mod, only : field_names

implicit none

private

type(plume_manager) :: manager
type(plume_protocol) :: offers
type(plume_data) :: plume_data

! Provider of fields for Plume
type(grib_fields_provider) :: fld_provider

! plume-config and plugin-config 
! needed by the driver to setup the GRIB fields
character(256) :: plume_config_file
type(fckit_configuration) :: plume_config
type(fckit_configuration), allocatable :: plugin_configs(:)
type(fckit_configuration) :: scm_plugin_config

public :: setup
public :: run
public :: teardown


contains


subroutine setup()

  type(fckit_configuration) :: requested_data_catalogue
  integer :: ifield
  integer :: config_env_status
  CHARACTER*127 msg

  call plume_check(plume_initialise())
  call plume_check(manager%initialise())
  call plume_check(offers%initialise())
  call plume_check(plume_data%initialise())  

  ! Setup the field provider
  call fld_provider%initialise()

  ! get plume configuration from env variable
  call get_environment_variable("PLUME_CONFIG_FILE", plume_config_file, status=config_env_status)

  ! offer simulation step
  call plume_check(offers%offer_int("NSTEP", "always", "none"))

  ! Offer Plume all available fields (through fields provider)
  do ifield=1,n_fields
    write(msg,'(A,A)'), "offering: ", field_names(ifield); call log%info(msg)
    call plume_check(offers%offer_atlas_field(field_names(ifield), "on-request", "") )
  enddo

  requested_data_catalogue = fckit_configuration()

  call log%info("*** Plume is now negotiating... ")
  call plume_check(manager%configure(plume_config_file))
  call plume_check(manager%negotiate(offers))
          
  requested_data_catalogue = manager%active_fields_catalogue()

  call log%info("*** Plume negotiation finished! ")

end subroutine setup


! the main execution routine (program is at the end)
subroutine run( return_code )

  INTEGER(KIND=JPIM) :: return_code
  
  type(atlas_Config) :: config
  type(atlas_StructuredGrid) :: grid
  type(atlas_Mesh) :: mesh
  type(atlas_mesh_Nodes) :: nodes
  type(atlas_MeshGenerator) :: meshgenerator
  type(atlas_Partitioner) :: partitioner
  type(atlas_Trans) :: trans
  type(atlas_functionspace_Spectral) :: spectral
  type(atlas_fvm_Method) :: fvm
  type(atlas_functionspace_NodeColumns) :: nodepoints
  type(atlas_functionspace_StructuredColumns) :: gridpoints
  type(atlas_functionspace_StructuredColumns) :: dummy

  type(atlas_Field) :: field
  type(atlas_FieldSet) :: sfcfields
  type(atlas_FieldSet) :: gpfields
  type(atlas_FieldSet) :: gpfields_from_sp
  type(atlas_FieldSet) :: gpdummy
  type(atlas_FieldSet) :: spfields
  type(atlas_Metadata) :: metadata

  type(atlas_Field) :: vorfield
  type(atlas_Field) :: divfield
  type(atlas_Field) :: windfield
  type(atlas_Field) :: ghostField
  type(atlas_Field) :: lonlatField
  
  REAL(KIND=c_double),POINTER :: ffvalues(:)
  REAL(KIND=c_double),POINTER :: vor(:,:)
  REAL(KIND=c_double),POINTER :: div(:,:)
  INTEGER(KIND=c_int), POINTER :: ghost(:)
  REAL(KIND=c_double), POINTER :: lonlat(:,:)
  
  TYPE(TLOCATION), ALLOCATABLE:: LOCATIONS(:)
  TYPE(TINFO) :: INFO

  character(len=30) :: file
  character(len=10) :: fieldname
  
  REAL(KIND=JPRB) :: zdelta
  
  CHARACTER(LEN=30) :: DATAID
  CHARACTER(LEN=30) :: CGRID
  
  logical :: LSINGLE
  
  logical :: LAREA
  LOGICAL :: LAREA_INT

  logical :: LPROGNOSTIC
  INTEGER :: LPROGNOSTIC_INT

  INTEGER(KIND=JPIM) :: nsmax
  INTEGER(KIND=JPIM) :: nstep
  INTEGER(KIND=JPIM) :: I
  INTEGER(KIND=JPIM) :: J
  INTEGER(KIND=JPIM) :: jfld
  INTEGER(KIND=JPIM) :: ilev
  INTEGER(KIND=JPIM) :: iloc
  INTEGER(KIND=JPIM) :: isize
  INTEGER(KIND=JPIM) :: nb_locations
  INTEGER(KIND=JPIM) :: nb_nodes
  INTEGER(KIND=JPIM) :: nlev
  INTEGER(KIND=JPIM) :: iparam
  INTEGER(KIND=JPIM) :: nlocmax
  INTEGER(KIND=JPIM) :: NPROC, MYPROC

  REAL(KIND=JPRB), ALLOCATABLE :: PVAH(:)
  REAL(KIND=JPRB), ALLOCATABLE :: PVBH(:)
  REAL(KIND=JPRB), ALLOCATABLE :: zlat(:)
  REAL(KIND=JPRB), ALLOCATABLE :: zlon(:)
  
  CHARACTER*127 msg
  type(fckit_mpi_comm) :: mpi_comm

  integer :: iplugin
  integer :: ifield
  integer :: ipoint

  character (len=:), allocatable :: plugin_name
  type(fckit_pathname) :: plugin_config_path
  logical :: found
  type(fckit_configuration), allocatable :: plugin_config_points(:)

  REAL(KIND=JPRB) :: PT_LAT
  REAL(KIND=JPRB) :: PT_LON
  

#include "rdnam.h"
#include "read_grib_scm.h"
#include "fill_and_write.h"
    
  write(msg,'(A)')  "TESTING: start"; call log%info(msg)

  mpi_comm = fckit_mpi_comm()
  NPROC  = mpi_comm%size()
  MYPROC = mpi_comm%rank() + 1 


  CALL RDNAM(LAREA, &
             LPROGNOSTIC, &
             dataid, &
             zdelta, &
             nlev, &
             nsmax, &
             nstep, &
             cgrid, &
             pvah, &
             pvbh, &
             nb_locations, &
             zlat, &
             zlon)

  write(msg,'(A,I0)') "nb_locations = ",nb_locations; call log%info(msg)
  write(msg,'(A,F5.3)') "zdelta = ",zdelta; call log%info(msg)
  write(msg,'(A,I0)') "nlev = ",nlev; call log%info(msg)
  write(msg,'(A,I0)') "nsmax = ",nsmax; call log%info(msg)
  write(msg,'(A,A)') "cgrid = ",cgrid; call log%info(msg)  

  config = atlas_Config()
  call config%set("radius",6371229.0)
  
  grid = atlas_StructuredGrid(cgrid)
  meshgenerator = atlas_Meshgenerator(config)
  partitioner = atlas_Partitioner("ectrans")
  mesh = meshgenerator%generate(grid,partitioner)
  call partitioner%final()
  nodes = mesh%nodes()
  
  nb_nodes = nodes%size()
  
  lonlatField = nodes%lonlat()
  call lonlatField%data(lonlat)

  ghostField = nodes%ghost()
  call ghostField%data(ghost)
  
  ! this is the functionspace nodepoints
  fvm  = atlas_fvm_Method(mesh, config)
  nodepoints = fvm%node_columns()
  write(msg,'(A,A)') "finished Atlas fvm function space"; call log%info(msg)
  
  ! for reading we also create a functionspace gridpoints
  gridpoints = atlas_functionspace_StructuredColumns(grid)

  ! read surface fieldset
  file=' '
  write(file,'(A)') 'sfc_grib'
  sfcfields = atlas_FieldSet("gridpoints")
  LSINGLE=.FALSE.
  call read_grib_scm(LSINGLE,NPROC,MYPROC,file,LPROGNOSTIC,LAREA,INFO,spectral,spfields,gridpoints,sfcfields)
  
  ! read upper air fieldset (gridpoint)
  file=' '
  write(file,'(A)') 'cld_grib '
  gpfields = atlas_FieldSet("gridpoints")
  call read_grib_scm(LSINGLE,NPROC,MYPROC,file,LPROGNOSTIC,LAREA,INFO,spectral,spfields,gridpoints,gpfields)
  
  ! Setup spectral transforms
  trans = atlas_Trans(grid,nsmax)
  spectral   = atlas_functionspace_Spectral(trans%truncation())
  write(msg,'(A,I0)') "spectral%truncation() = ",spectral%truncation(); call log%info(msg)
  
  ! read upper air fieldset (spectral)
  file=' '
  write(file,'(A)') 'spec_grib '
  spfields = atlas_FieldSet("spectral")
  call read_grib_scm(LSINGLE,NPROC,MYPROC,file,LPROGNOSTIC,LAREA,INFO,spectral,spfields,dummy,gpdummy)
  
  write(msg,'(A)') " finished I/O"; call log%info(msg)
  
  gpfields_from_sp = atlas_FieldSet("nodepoints")
  isize = spfields%size()
  do jfld=1,isize
  
    field = spfields%field(jfld)
    metadata = field%metadata()
    call metadata%get("paramId", iparam)
    call metadata%get("level", ilev)
  
    write(fieldname, '(I0)') jfld
    field = nodepoints%create_field(name=fieldname,kind=atlas_real(JPRB))
    metadata = field%metadata()
    call metadata%set("paramId",iparam)
    call metadata%set("level",ilev)
    call gpfields_from_sp%add( field )
  enddo
  write(msg,'(A)') " inverse transform starting "; call log%info(msg)
  call trans%invtrans(spfields,gpfields_from_sp)
  write(msg,'(A)') " inverse transform finished "; call log%info(msg)
  
  ! copy vor/div into level structure before transform to u/v
  vorfield = spectral%create_field(name="vorticity",kind=atlas_real(JPRB),levels=nlev)
  divfield =  spectral%create_field(name="divergence",kind=atlas_real(JPRB),levels=nlev)
  windfield = nodepoints%create_field(name="wind",kind=atlas_real(JPRB),levels=nlev,variables=2)
  call vorfield%data(vor)
  call divfield%data(div)
  do jfld=1,isize
    field = spfields%field(JFLD)
    call field%data(ffvalues)
    metadata = field%metadata()
    call metadata%get('paramId',iparam)
    call metadata%get('level',ilev)
    if( iparam == 138 ) then
      vor(ilev,:) = ffvalues(:)
    endif
    if( iparam == 155 ) then
      div(ilev,:) = ffvalues(:)
    endif
  enddo
  write(msg,'(A)') " inverse wind transform starting "; call log%info(msg)
  call trans%invtrans_vordiv2wind(vorfield,divfield,windfield)
  write(msg,'(A)') " inverse wind transform finished "; call log%info(msg)


  ! --- At this point, we need to pass all the GRIB fields to the field provider, 
  ! --- which will construct the plume fields to be passed to the plugin     
  call fld_provider%setup(nlev, gridpoints, nodepoints, sfcfields, gpfields, gpfields_from_sp, windfield)
  call fld_provider%provide_fields(plume_data)

  ! Initialise parameter
  call plume_check( plume_data%create_int("NSTEP", 999) )


  ! Feed plugins with the data
  call plume_check(manager%feed_plugins(plume_data))

  ! run
  call plume_check(manager%run())

  ! *** this writes the netcdf files as in the original workflow
  call lonlatField%data(lonlat)
  call ghostField%data(ghost)

  write(*,*) "INFO%IDATE: ", INFO%IDATE
  write(*,*) "INFO%ITIME: ", INFO%ITIME
  write(*,*) "INFO%ISTEP: ", INFO%ISTEP
  write(*,*) "INFO%NSTEP: ", INFO%NSTEP

  ! call fill_and_write(INFO, &
  !                     LOCATIONS, &
  !                     nb_locations, &
  !                     zlat, &
  !                     zlon, &
  !                     nb_nodes, &
  !                     ghost, &
  !                     lonlat, &
  !                     myproc, &
  !                     zdelta, &
  !                     LAREA, &
  !                     PVAH, &
  !                     PVBH, &
  !                     DATAID, &
  !                     nlev, &
  !                     666, &
  !                     sfcfields, &
  !                     nproc, &
  !                     fvm, &
  !                     nodepoints, &
  !                     windfield, &
  !                     gpfields_from_sp, &
  !                     gridpoints, &
  !                     gpfields)

  
  ! Cleanup
  call config%final()
  call fvm%final()
  call nodes%final()
  call mesh%final()
  call grid%final()
  call meshgenerator%final()
  
  write(msg,'(A)')  "getini1c: end"; call log%info(msg)
  
  return_code = 0
  
end subroutine run


subroutine teardown()
end subroutine teardown

end module testing_model_mod




program testing_model

    use testing_model_mod
    use atlas_module, only: atlas_library
    USE MPL_MODULE, only : mpl_init
    USE MPL_MODULE, only : mpl_end

    use fckit_mpi_module, only : fckit_mpi_comm
    use yomvar
    
    implicit none
    
    type(fckit_mpi_comm) :: mpi_comm
    INTEGER(KIND=JPIM) :: NPROC
    INTEGER(KIND=JPIM) :: MYPROC

    integer :: return_code

    call atlas_library%initialise()

    mpi_comm = fckit_mpi_comm()
    NPROC  = mpi_comm%size()
    MYPROC = mpi_comm%rank() + 1     

    IF( NPROC > 1 ) CALL MPL_INIT()
    call setup()
    call run(return_code)
    call teardown()

    IF( NPROC > 1 ) call mpl_end()
    
    call atlas_library%finalise()
    
    if( return_code /= 0 ) then 
        STOP 1
    endif
    
end program testing_model