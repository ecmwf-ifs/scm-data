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
type(plume_data) :: data_from_plume

! Provider of fields for Plume
type(grib_fields_provider) :: fld_provider

! plume-config and plugin-config 
! needed by the driver to setup the GRIB fields
character(1024) :: plume_config_file
type(fckit_configuration) :: plume_config
type(fckit_configuration), allocatable :: plugin_configs(:)
type(fckit_configuration) :: scm_plugin_config

public :: setup
public :: run
public :: teardown


contains


function parse_step_values(value) result(steps)
  implicit none

  character(len=*), intent(in) :: value
  integer, allocatable         :: steps(:)

  integer :: value_length
  integer :: number_of_steps
  integer :: character_index
  integer :: step_index
  integer :: token_start
  integer :: token_end
  integer :: comma_position
  integer :: read_status

  character(len=:), allocatable :: trimmed_value
  character(len=:), allocatable :: token

  trimmed_value = trim(value)
  value_length = len(trimmed_value)

  if (value_length == 0) then
    error stop "SCM_PLUGIN_TESTER_STEPS must not be empty"
  endif

  ! Count the comma-separated entries.
  number_of_steps = 1

  do character_index = 1, value_length
    if (trimmed_value(character_index:character_index) == ",") then
      number_of_steps = number_of_steps + 1
    endif
  enddo

  allocate(steps(number_of_steps))

  token_start = 1

  do step_index = 1, number_of_steps
    comma_position = index(trimmed_value(token_start:), ",")

    if (comma_position == 0) then
      token_end = value_length
    else
      token_end = token_start + comma_position - 2
    endif

    if (token_end < token_start) then
      error stop "SCM_PLUGIN_TESTER_STEPS contains an empty entry"
    endif

    token = trim(adjustl(trimmed_value(token_start:token_end)))

    if (len(token) == 0) then
      error stop "SCM_PLUGIN_TESTER_STEPS contains an empty entry"
    endif

    read(token, *, iostat=read_status) steps(step_index)

    if (read_status /= 0) then
      error stop "SCM_PLUGIN_TESTER_STEPS contains an invalid integer"
    endif

    if (steps(step_index) < 1) then
      error stop "SCM_PLUGIN_TESTER_STEPS values must be greater than zero"
    endif

    token_start = token_end + 2
  enddo
end function parse_step_values


subroutine setup()

  type(fckit_configuration) :: requested_data_catalogue
  integer :: ifield
  integer :: config_env_status
  character(512) :: msg

  call plume_check(plume_initialise())
  call plume_check(manager%initialise())
  call plume_check(offers%initialise())
  call plume_check(data_from_plume%initialise())  

  ! Setup the field provider
  call fld_provider%initialise()

  ! get plume configuration from env variable
  call get_environment_variable("PLUME_CONFIG_FILE", plume_config_file, status=config_env_status)

  ! offer simulation step
  call plume_check(offers%offer_int("NSTEP", "always", "none"))
  call plume_check(offers%offer_double("TSTEP", "always", "none"))
  call plume_check(offers%offer_int("INIT_DATE", "always", "none"))
  call plume_check(offers%offer_int("INIT_TIME", "always", "none"))

  ! Offer Plume all available fields (through fields provider)
  do ifield=1,n_fields
    write(msg,'(A,A)') "offering: ", field_names(ifield); call log%info(msg)
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
  REAL(KIND=c_double) :: tstep
  INTEGER(KIND=JPIM) :: init_date
  INTEGER(KIND=JPIM) :: init_time

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
  
  character(512) :: msg
  type(fckit_mpi_comm) :: mpi_comm

  integer :: iplugin
  integer :: ifield
  integer :: ipoint

  ! multi-step loop
  integer :: loop_nstep_first
  integer :: loop_nstep_last

  character (len=:), allocatable :: plugin_name
  type(fckit_pathname) :: plugin_config_path
  logical :: found
  type(fckit_configuration), allocatable :: plugin_config_points(:)

  REAL(KIND=JPRB) :: PT_LAT
  REAL(KIND=JPRB) :: PT_LON

  ! stepping
  character(len=1024)  :: steps_env
  character(len=32)    :: n_steps_env
  integer              :: steps_env_status
  integer              :: n_steps_env_status
  integer              :: n_steps
  integer              :: i_step
  ! integer              :: nstep
  integer, allocatable :: step_values(:)

  

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
  call fld_provider%provide_fields(data_from_plume)


  ! Determine the NSTEP values.
  !
  ! Precedence:
  ! 1. SCM_PLUGIN_TESTER_STEPS="1,3,7" runs the explicitly listed steps.
  ! 2. SCM_PLUGIN_TESTER_N_STEPS=N runs sequential steps 1..N.
  ! 3. If neither variable is set, run NSTEP=999 for backward compatibility.

  call get_environment_variable( &
    "SCM_PLUGIN_TESTER_STEPS", &
    steps_env, &
    status=steps_env_status)

  call get_environment_variable( &
    "SCM_PLUGIN_TESTER_N_STEPS", &
    n_steps_env, &
    status=n_steps_env_status)

  if (steps_env_status == 0 .and. len_trim(steps_env) > 0) then

    ! Explicit step values override SCM_PLUGIN_TESTER_N_STEPS.
    step_values = parse_step_values(steps_env)

  else if (n_steps_env_status == 0 .and. len_trim(n_steps_env) > 0) then

    read(n_steps_env, *) n_steps

    if (n_steps < 1) then
      error stop "SCM_PLUGIN_TESTER_N_STEPS must be greater than zero"
    endif

    allocate(step_values(n_steps))
    step_values = [(i_step, i_step = 1, n_steps)]

  else

    allocate(step_values(1))
    step_values(1) = 999

  endif

  ! Dummy values for TSTEP/INIT_DATE/INIT_TIME (for testing only)
  tstep     = 0.0
  init_date = 20151015
  init_time = 12

  call plume_check(data_from_plume%provide_double("TSTEP", tstep))
  call plume_check(data_from_plume%provide_int("INIT_DATE", init_date))
  call plume_check(data_from_plume%provide_int("INIT_TIME", init_time))

  INFO%IDATE = INIT_DATE
  INFO%ITIME = INIT_TIME / 3600  ! Convert seconds to hours.

  ! Register NSTEP using the first configured step.
  nstep = step_values(1)

  call plume_check(data_from_plume%create_int("NSTEP", nstep))
  call plume_check(manager%feed_plugins(data_from_plume))

  ! Run all configured steps.
  do i_step = 1, size(step_values)
    nstep = step_values(i_step)

    INFO%DTIME = nstep * tstep

    write (msg,'(A,I0)') "Tester has set NSTEP = ", nstep; call log%info(msg)
    call plume_check(data_from_plume%update_int("NSTEP", nstep))
    call plume_check(manager%run())
  enddo

  deallocate(step_values)

  ! *** this writes the netcdf files as in the original workflow
  call lonlatField%data(lonlat)
  call ghostField%data(ghost)
  
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
