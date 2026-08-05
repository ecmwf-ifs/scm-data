! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

module plugin_impl_mod

  use, intrinsic :: iso_C_binding
  use fckit_mpi_module, only : fckit_mpi_comm
  use fckit_log_module, only : log
  
  use atlas_module, only: atlas_Config
  use atlas_module, only: atlas_StructuredGrid
  use atlas_module, only: atlas_Mesh
  use atlas_module, only: atlas_mesh_Nodes
  use atlas_module, only: atlas_MeshGenerator

  use atlas_module, only: atlas_fvm_Method
  use atlas_module, only: atlas_functionspace_NodeColumns
  use atlas_module, only: atlas_Field
  use atlas_module, only: atlas_FieldSet
  use atlas_module, only: atlas_Metadata
  use atlas_module, only: atlas_real
  use atlas_module, only: atlas_Meshgenerator
  use atlas_module, only: atlas_Functionspace
  use atlas_module, only: atlas_functionspace_StructuredColumns
  use atlas_module, only: atlas_Partitioner
  use atlas_module, only: atlas_MatchingPartitioner
  use atlas_module, only: atlas_Grid
  use atlas_module, only: atlas_Interpolation
  use atlas_module, only: atlas_Nabla

  

  use fckit_configuration_module, only : fckit_configuration

  use plume_module, only : plume_check
  use plume_data_module, only : plume_data
  
  use yomvar
  use vert_coord_tables_mod

  use scm_nc_output_mod,      only : output_writer
  use extraction_manager_mod, only : extraction_manager
  use process_plume_fields_mod, only : process_plume_fields

  ! Available fields from the plugin
  use plugin_utils_mod, only : n_fields_srf
  use plugin_utils_mod, only : n_fields_cld
  use plugin_utils_mod, only : n_fields_spc
  use plugin_utils_mod, only : n_fields_oth
#ifdef WITH_SCM_GRIB2_FIELDS
  use plugin_utils_mod, only : n_fields_sol
#endif

  use plugin_utils_mod, only : field_names_srf
  use plugin_utils_mod, only : field_names_cld
  use plugin_utils_mod, only : field_names_spc
  use plugin_utils_mod, only : field_names_oth
#ifdef WITH_SCM_GRIB2_FIELDS
  use plugin_utils_mod, only : field_names_sol
#endif

  use plugin_utils_mod, only : param_name2id, param_name2idx, get_vertical_tables_from_namelist

#ifdef WITH_SCM_PLUME_PLUGIN_PROFILER
  use plugin_profiler_mod
#endif

implicit none


private


! vectors of fields
type(atlas_Field) :: fields_srf(n_fields_srf)
type(atlas_Field) :: fields_cld(n_fields_cld)
type(atlas_Field) :: fields_cld_nodes(n_fields_cld)
type(atlas_Field) :: fields_spc(n_fields_spc)
type(atlas_Field) :: fields_spc_nodes(n_fields_spc)
type(atlas_Field) :: fields_oth(n_fields_oth)
#ifdef WITH_SCM_GRIB2_FIELDS
! Multi-level soil fields (sot/vsw/sit) - merged into fields_srf_set at setup
! time so they flow through the existing FILLVAR_FROM_PLUME dispatch by paramId.
type(atlas_Field) :: fields_sol(n_fields_sol)
#endif

! Atlas Fieldsets
type(atlas_FieldSet) :: fields_srf_set  ! surfc field
type(atlas_FieldSet) :: fields_cld_set  ! cloud fields
type(atlas_FieldSet) :: fields_spc_set  ! spctr fields
type(atlas_FieldSet) :: fields_oth_set  ! other fields

! SP fields on nodepoints
type(atlas_FieldSet) :: gpfields_from_sp

! CLD fields on nodepoints
type(atlas_FieldSet) :: gpfields_cld_nodes

! wind field on nodepoints
type(atlas_Field) :: windfield

type(atlas_Field) :: field_u
type(atlas_Field) :: field_v
type(atlas_Field) :: fields_uv(2)

! Nabla operator and gradient fields (created once in scm_setup, reused in scm_run)
type(atlas_Nabla) :: nabla
type(atlas_Field) :: grad
type(atlas_Field) :: grad_one_lev
type(atlas_Field) :: grad_wind

! Ghost mask (created once in scm_setup, reused in scm_run)
INTEGER(KIND=c_int), POINTER :: ghost_mask(:) => null()

! Cached paramIds for fields (computed once in scm_setup, read in scm_run) - PLUME-85
INTEGER(KIND=JPIM), ALLOCATABLE :: param_ids_spc(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: param_ids_cld(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: param_ids_srf(:)

type(atlas_fvm_Method) :: fvm
type(atlas_functionspace_NodeColumns) :: nodepoints
type(atlas_functionspace_StructuredColumns) :: gridpoints

LOGICAL :: LPROGNOSTIC
LOGICAL :: LAREA
INTEGER :: RUN_EVERY
INTEGER :: INIT_STEP
INTEGER :: FINAL_STEP

! variable to detect an actual change in NSTEP, so that 
! the plugin is not executed multiple times for the same step
INTEGER(KIND=JPIM) :: NSTEP_OLD = -1

TYPE(TLOCATION), ALLOCATABLE:: LOCATIONS(:)
INTEGER(KIND=JPIM) :: NB_LOCATIONS

TYPE(TINFO) :: INFO

REAL(KIND=JPRB), ALLOCATABLE :: PVAH(:) ! A coefficients for calculation of vertical levels
REAL(KIND=JPRB), ALLOCATABLE :: PVBH(:) ! B coefficients for calculation of vertical levels
REAL(KIND=JPRB), ALLOCATABLE :: ZLAT(:) ! point lats
REAL(KIND=JPRB), ALLOCATABLE :: ZLON(:) ! point lons

INTEGER(KIND=JPIM) :: nstep
REAL(KIND=c_double) :: tstep
INTEGER(KIND=JPIM) :: init_date
INTEGER(KIND=JPIM) :: init_time

INTEGER(KIND=JPIM) :: NLEV
INTEGER(KIND=JPIM) :: NB_NODES

! MPI INFO
type(fckit_mpi_comm) :: mpi_comm
INTEGER(KIND=JPIM) :: NPROC
INTEGER(KIND=JPIM) :: MYPROC


! NetCDF output writer (owns output-directory + append-mode state)
type(output_writer) :: nc_writer

! Extraction scheduler: maps each time step -> list of location indices to extract.
type(extraction_manager) :: extract_mgr

CHARACTER(len=:), allocatable :: dataid

public :: scm_setup
public :: scm_run
public :: scm_teardown

contains



subroutine scm_setup(plugin_config, model_data)
  type(fckit_configuration) :: plugin_config
  type(fckit_configuration), allocatable :: plugin_config_points(:)
  type(plume_data) :: model_data

  type(atlas_Config) :: config

  CHARACTER(LEN=30) :: FILE
  CHARACTER(LEN=10) :: FIELDNAME

  REAL(KIND=JPRB) :: ZDELTA
  INTEGER :: LAREA_INT
  INTEGER :: LPROGNOSTIC_INT
  LOGICAL :: LSINGLE
  
  INTEGER(KIND=JPIM) :: I
  INTEGER(KIND=JPIM) :: J
  INTEGER(KIND=JPIM) :: JFLD
  INTEGER(KIND=JPIM) :: ILEV
  INTEGER(KIND=JPIM) :: ILOC
  INTEGER(KIND=JPIM) :: ISIZE
  INTEGER(KIND=JPIM) :: IPARAM
  INTEGER(KIND=JPIM) :: NLOCMAX

  REAL(KIND=JPRB) :: PT_LAT
  REAL(KIND=JPRB) :: PT_LON
  
  character(512) :: msg
  
  integer :: ifield
  integer :: ipoint
  logical :: found

  ! if delta is not specified, use kdtree to find nearest point
  logical :: found_delta

  type(atlas_functionspace_StructuredColumns) :: input_fs
  class(atlas_Functionspace), allocatable :: input_fs_parent
  type(atlas_grid) :: input_grid

  type(atlas_StructuredGrid) :: grid
  type(atlas_Mesh) :: mesh
  type(atlas_mesh_Nodes) :: nodes
  type(atlas_MeshGenerator) :: meshgenerator
  type(atlas_Partitioner) :: partitioner

  INTEGER(KIND=c_int), POINTER :: ghost(:)
  REAL(KIND=c_double), POINTER :: lonlat(:,:)
  type(atlas_Field) :: lonlatField
  type(atlas_Field) :: ghostField

  character(1024) :: vtable_testing_namelist
  integer :: vtable_testing_namelist_status

  type(atlas_Field) :: fieldtemp

#ifdef WITH_SCM_SINGLE_PRECISION  
  REAL(KIND=c_float), POINTER :: dummy_data(:,:)
#else
  REAL(KIND=c_double), POINTER :: dummy_data(:,:)
#endif


#include "nearest_distance.h"
#include "nearest_distance_kdtree.h"
#include "profiler_macros.h"

  write(msg,'(A)')  "--> getini1c: start"; call log%debug(msg)

  ! start the profiler timer
  START_PLUGIN_TIMER("scm_setup")

  ! setup MPI info
  mpi_comm = fckit_mpi_comm()
  NPROC  = mpi_comm%size()
  MYPROC = mpi_comm%rank() + 1 

  ! fill-in array of fields (SRF)
  do ifield=1,size(field_names_srf)
    write(msg,'(A,A)') "getting field: ", trim(field_names_srf(ifield)); call log%info(msg)
    call plume_check(model_data%get_shared_atlas_field(trim(field_names_srf(ifield)), fields_srf(ifield)) );
  enddo

#ifdef WITH_SCM_GRIB2_FIELDS
  ! fill-in array of fields (SOL - multi-level soil, each with ncss=4 layers)
  do ifield=1,size(field_names_sol)
    write(msg,'(A,A)') "getting field: ", trim(field_names_sol(ifield)); call log%info(msg)
    call plume_check(model_data%get_shared_atlas_field(trim(field_names_sol(ifield)), fields_sol(ifield)) );
  enddo
#endif

  ! fill-in array of fields (CLD)
  do ifield=1,size(field_names_cld)
    write(msg,'(A,A)') "getting field: ", trim(field_names_cld(ifield)); call log%info(msg)
    call plume_check(model_data%get_shared_atlas_field(trim(field_names_cld(ifield)), fields_cld(ifield)) );
  enddo
  
  ! fill-in array of fields (SPC)
  do ifield=1,size(field_names_spc)
    write(msg,'(A,A)') "getting field: ", trim(field_names_spc(ifield)); call log%info(msg)
    call plume_check(model_data%get_shared_atlas_field(trim(field_names_spc(ifield)), fields_spc(ifield)) );
  enddo

  ! fill-in array of fields (OTHERS)
  do ifield=1,size(field_names_oth)
    write(msg,'(A,A)') "getting field: ", trim(field_names_oth(ifield)); call log%info(msg)
    call plume_check(model_data%get_shared_atlas_field(trim(field_names_oth(ifield)), fields_oth(ifield)) );
  enddo

  ! fields U and V
  call plume_check(model_data%get_shared_atlas_field("u", field_u))
  call plume_check(model_data%get_shared_atlas_field("v", field_v))


  call plume_check(model_data%get_int("NSTEP",NSTEP))
  call plume_check(model_data%get_double("TSTEP",TSTEP))
  call plume_check(model_data%get_int("INIT_DATE",INIT_DATE))
  call plume_check(model_data%get_int("INIT_TIME",INIT_TIME))

  ! get the functionspace from first field
  input_fs = fields_srf(1)%functionspace()
  nlev = input_fs%levels()

  ! read parameters from plugin-core configuration
  found = plugin_config%get("LPROGNOSTIC", LPROGNOSTIC_INT)
  if (.not.found) then
    LPROGNOSTIC_INT = 1
  endif
  if (LPROGNOSTIC_INT == 1) then
    LPROGNOSTIC = .true.
  else
    LPROGNOSTIC = .false.
  endif
  
  found = plugin_config%get("LAREA", LAREA_INT)
  if (.not.found) then
    LAREA_INT = 0
  endif
  if (LAREA_INT == 1) then
    LAREA = .true.
  else
    LAREA = .false.
  endif

  ! Plugin run frequency
  found = plugin_config%get("RUN_EVERY", RUN_EVERY)
  if (.not.found) then
    RUN_EVERY = 1
  endif
  if (RUN_EVERY < 1) then
    write(msg,'(A,I0)') "RUN_EVERY must be >= 1, but is ", RUN_EVERY; call log%error(msg)
    stop 1
  endif

  ! Plugin initial step
  found = plugin_config%get("INIT_STEP", INIT_STEP)
  if (.not.found) then
    INIT_STEP = 0
  endif

  ! Plugin final step
  found = plugin_config%get("FINAL_STEP", FINAL_STEP)
  if (.not.found) then
    FINAL_STEP = -1
  endif

  ! ID of data
  found = plugin_config%get("DATAID", dataid)
  if (.not.found) then
    dataid = "plume-plugin-scm"
  endif

  ! max radius of search for nearest grid point
  found_delta = plugin_config%get("DELTA", ZDELTA)

  ! vertical levels coefficients
  ! For testing only: read the vertical levels from namelist (for consistency)
  call get_environment_variable("PLUME_SCM_PLUGIN_VERT_TABLES_TEST_NAMELIST", vtable_testing_namelist, status=vtable_testing_namelist_status)
  if (vtable_testing_namelist_status == 0) then
    write(msg,'(A,A)') "Reading vertical tables from namelist: ", trim(vtable_testing_namelist); call log%info(msg)
    call get_vertical_tables_from_namelist(vtable_testing_namelist, NLEV, PVAH, PVBH)
  else
    call get_vertical_tables(NLEV, PVAH, PVBH)
  endif

  ! point coordinates
  found = plugin_config%get("points", plugin_config_points)
  ! if no points are specified, then the plugin will not run
  if (.not.found) then
    write(msg,'(A)') "No points specified in plugin configuration, plugin will not run"; call log%info(msg)
    nb_locations = 0
    stop 1
  endif
  nb_locations = size(plugin_config_points)

  allocate(locations(nb_locations))
  allocate(zlat(NB_LOCATIONS))
  allocate(zlon(NB_LOCATIONS))

  do ipoint=1,nb_locations
    found = plugin_config_points(ipoint)%get("lat", PT_LAT)
    found = plugin_config_points(ipoint)%get("lon", PT_LON)

    if( PT_LON < 0. ) then
      PT_LON = 360. + PT_LON
    endif

    locations(ipoint)%RLONI = PT_LON
    locations(ipoint)%RLATI = PT_LAT
    locations(ipoint)%RLONI_USER = PT_LON
    locations(ipoint)%RLATI_USER = PT_LAT
    ! extraction schedule is now owned by extract_mgr (see below);
    ! ITARGET_STEP is left at its default value from yomvar.
    locations(ipoint)%ILOC = -1
    locations(ipoint)%IFILE_ID = -1
    locations(ipoint)%IPROC = -1

    zlat(ipoint) = PT_LAT
    zlon(ipoint) = PT_LON
  enddo

  ! Build the step -> [iloc] extraction schedule from the per-point config.
  call extract_mgr%init(plugin_config_points, nb_locations)

  write(msg,'(A,I0)')   "nb_locations = ", nb_locations; call log%info(msg)
  write(msg,'(A,F5.3)') "zdelta = ", zdelta; call log%info(msg)
  write(msg,'(A,I0)')   "nlev = ", nlev; call log%info(msg)

  do ipoint=1,nb_locations
    write(msg,'(A,F10.3,A,F10.3)') "lat = ",zlat(ipoint), ", lon=", zlon(ipoint); call log%info(msg)
  enddo

  !        2.   set up necessary info on gg and sh fields
  !             --------------------------------------------------------------

  ! initialize config on the sphere
  config = atlas_Config()
  call config%set("radius",6371229.0)

  ! grid from model
  input_grid = input_fs%grid()
  allocate(input_fs_parent, source=input_fs)
  partitioner = atlas_MatchingPartitioner(input_fs_parent)

  ! mesh
  meshgenerator = atlas_Meshgenerator(config)
  mesh = meshgenerator%generate(input_grid,partitioner)

  nodes = mesh%nodes()
  
  ! find the processor and node location on the processor responsible for each user specified lat/lon location
  nb_nodes = nodes%size()
  lonlatField = nodes%lonlat()
  call lonlatField%data(lonlat)
  ghostField = nodes%ghost()
  call ghostField%data(ghost)

  write(msg,'(A,I0,A,I0)') "nodes: ", nb_nodes, ", lonlat%size(): ", lonlatField%size(); call log%info(msg)

  ! find the nearest grid point to each user specified lat/lon location
  if ( .not. found_delta ) then
    write(msg,'(A)') "No ZDELTA specified, using kdtree to find nearest point"; call log%info(msg)
    call nearest_distance_kdtree(nb_nodes, ghost, lonlat, nb_locations, locations)
  else
    write(msg,'(A,F8.4)') "ZDELTA = ", zdelta; call log%info(msg)
    call nearest_distance(nb_nodes, ghost, lonlat, myproc, zdelta, nb_locations, locations)
  endif

  do j=1, nb_locations
    write(msg,'(A,I0)') "iproc: ", locations(j)%iproc ; call log%info(msg)
    if( myproc == locations(j)%iproc ) then
      write(msg,'(A,I0)') "Nearest point proc ", locations(j)%IPROC ; call log%info(msg)
      write(msg,'(A,I0,2(1X,F8.4))') "Nearest point lon ", j, locations(j)%RLONI, ZLON(j) ; call log%info(msg)
      write(msg,'(A,I0,2(1X,F8.4))') "Nearest point lat ", j, locations(j)%RLATI, ZLAT(j) ; call log%info(msg)
      write(msg,'(A,I0,1X, I0)') "Nearest point knode ",   j, locations(j)%iloc ; call log%info(msg)
      ! write(*,*) 'test if unique proc: ', myproc, locations(j)%iloc, locations(j)%RLONI, locations(j)%RLATI
    endif
  enddo

  write(msg,'(A)') "finished nearest distances "; call log%info(msg)
  write(msg,'(A,I0)') "input_grid%size(): ", input_grid%size(); call log%info(msg)

  ! this is the functionspace nodepoints
  fvm = atlas_fvm_Method(mesh, config)
  nodepoints = fvm%node_columns()
  write(msg,'(A,A)') "finished Atlas fvm function space"; call log%info(msg)

  gridpoints = input_fs

! initialize config on the sphere
  config = atlas_Config()
  call config%set("radius",6371229.0)

  fields_srf_set = atlas_FieldSet("gridpoints")
  do ifield=1,size(fields_srf)
    call fields_srf_set%add( fields_srf(ifield) )
  enddo
#ifdef WITH_SCM_GRIB2_FIELDS
  ! Merge the multi-level soil fields into fields_srf_set so the existing
  ! FILLVAR_FROM_PLUME call in scm_run dispatches them by paramId alongside the
  ! surface fields. FILLVAR_FROM_PLUME already indexes values(:,:) as (nlev,node)
  ! so the extra vertical dimension is handled transparently.
  do ifield=1,size(fields_sol)
    call fields_srf_set%add( fields_sol(ifield) )
  enddo
#endif

  fields_cld_set = atlas_FieldSet("gridpoints")
  do ifield=1,size(fields_cld)
    call fields_cld_set%add( fields_cld(ifield) )
  enddo

  fields_spc_set = atlas_FieldSet("gridpoints")
  do ifield=1,size(fields_spc)
    call fields_spc_set%add( fields_spc(ifield) )
  enddo

  fields_oth_set = atlas_FieldSet("gridpoints")
  do ifield=1,size(fields_oth)
    call fields_oth_set%add( fields_oth(ifield) )
  enddo

  ! PLUME-85: Cache paramIds for fields to avoid string comparison lookups in scm_run
  allocate(param_ids_spc(size(fields_spc)))
  do ifield=1,size(fields_spc)
    param_ids_spc(ifield) = param_name2id(fields_spc(ifield)%name())
  enddo

  allocate(param_ids_cld(size(fields_cld)))
  do ifield=1,size(fields_cld)
    param_ids_cld(ifield) = param_name2id(fields_cld(ifield)%name())
  enddo

  allocate(param_ids_srf(fields_srf_set%size()))
  do ifield=1,fields_srf_set%size()
    fieldtemp = fields_srf_set%field(ifield)
    param_ids_srf(ifield) = param_name2id(fieldtemp%name())
  enddo

  ! allocate and initialise location columns
  do iloc=1, nb_locations
    if( myproc == locations(iloc)%iproc ) then
      call ALLOCATE_COLUMNS(locations(iloc)%PP,nlev)
      locations(iloc)%PP%PW(:)=0._jprb
      locations(iloc)%PP%PR(:)=0._jprb
      locations(iloc)%PP%PRL(:)=0._jprb
      locations(iloc)%PP%PRM(:)=0._jprb
      locations(iloc)%PP%PS(:)=0._jprb
      locations(iloc)%PP%PSL(:)=0._jprb
      locations(iloc)%PP%PSM(:)=0._jprb
    endif
  enddo


! initialise the NetCDF output writer (reads APPEND_OUTPUT + PLUME_PLUGINS_OUTPUT_DIR)
call nc_writer%init(plugin_config)

! initialise fieldset for CLD fields on nodepoints
gpfields_cld_nodes = atlas_FieldSet("nodepoints")

! initialise fieldset for SP fields on nodepoints
gpfields_from_sp = atlas_FieldSet("nodepoints")

! create CLD fields in nodes
do ifield=1,size(fields_cld)
  fields_cld_nodes(ifield) = nodepoints%create_field(name=trim(fields_cld(ifield)%name()), levels=fields_cld(ifield)%levels(), kind=atlas_real(JPRB))
  call gpfields_cld_nodes%add(fields_cld_nodes(ifield))
enddo

! create SP fields in nodes
do ifield=1,size(fields_spc)
  fields_spc_nodes(ifield) = nodepoints%create_field(name=trim(fields_spc(ifield)%name()), levels=fields_spc(ifield)%levels(), kind=atlas_real(JPRB))
  call gpfields_from_sp%add(fields_spc_nodes(ifield))
enddo

! create wind field in nodes
fields_uv(1) = field_u
fields_uv(2) = field_v
windfield = nodepoints%create_field(name="wind", levels=field_u%levels(), kind=atlas_real(JPRB), variables=2)

! create Nabla operator and gradient fields (PLUME-85: reuse across timesteps)
nabla = atlas_Nabla(fvm)
grad = nodepoints%create_field(name="grad", kind=atlas_real(JPRB), levels=nlev, variables=2)
grad_one_lev = nodepoints%create_field(name="grad_one_lev", kind=atlas_real(JPRB), levels=1, variables=2)
grad_wind = nodepoints%create_field(name="gradwind", kind=atlas_real(JPRB), levels=nlev, variables=4)

! cache the ghost mask (PLUME-85: avoid refetching per-field in scm_run)
! allocate(ghost_mask(nb_nodes))
ghostField = nodes%ghost()
call ghostField%data(ghost_mask)
call ghostField%final()

! finalisation
call input_fs%final()
call input_fs_parent%final()
call input_grid%final()
call grid%final()
call mesh%final()
call nodes%final()
call meshgenerator%final()
call partitioner%final()
call lonlatField%final()
call ghostField%final()

! stop the profiler timer
STOP_PLUGIN_TIMER("scm_setup")
  
end subroutine scm_setup



!!! use ldd <executable> to figure out which libraries have been used in the executable !!!
!....................................................................... 
!     GETS A PROFILE OF ATMOSPHERIC AND SURFACE VARIABLES
!     FOR LATER INPUT FOR THE SINGLE COLUMN MODEL
!
!     Nils Wedi,  March 2016
!     
!    The program is a rewrite based on Atlas data structures (Willem Deconinck)
!    previous contributions by Martin Koehler, Gisela Seuffert, Pedro Viterbo, Nils Wedi
!....................................................................... 

! the main execution routine (program is at the end)
subroutine scm_run( plugin_config, model_data )

type(fckit_configuration) :: plugin_config
type(plume_data) :: model_data

INTEGER(KIND=c_int), POINTER :: ghost(:)
REAL(KIND=c_double), POINTER :: lonlat(:,:)

character(len=30) :: file
character(len=10) :: fieldname
REAL(KIND=JPRB) :: zdelta
logical :: LSINGLE

INTEGER(KIND=JPIM) :: I
INTEGER(KIND=JPIM) :: J
INTEGER(KIND=JPIM) :: jfld
INTEGER(KIND=JPIM) :: ilev
INTEGER(KIND=JPIM) :: inode
INTEGER(KIND=JPIM) :: iloc
INTEGER(KIND=JPIM) :: isize
INTEGER(KIND=JPIM) :: iparam
INTEGER(KIND=JPIM) :: nlocmax

character(512) :: msg
integer :: ifield

#ifdef WITH_SCM_SINGLE_PRECISION  
  REAL(KIND=c_float), POINTER :: dummy_data(:,:)
#else
  REAL(KIND=c_double), POINTER :: dummy_data(:,:)
#endif


#include "fillvar_from_plume.h"
#include "update_nodefield.h"

! start the profiler timer
START_PLUGIN_TIMER("scm_run")

call plume_check(model_data%get_int("NSTEP",NSTEP))
call plume_check(model_data%get_double("TSTEP",TSTEP))
call plume_check(model_data%get_int("INIT_DATE",INIT_DATE))
call plume_check(model_data%get_int("INIT_TIME",INIT_TIME))

! write some info to the INIT_DATE and INIT_TIME
INFO%IDATE = INIT_DATE
INFO%ITIME = INIT_TIME/3600 ! convert seconds to hours
INFO%LCALC_PLUGIN = .true. ! times are from plugin
INFO%DTIME = NSTEP * TSTEP ! time step in seconds

! use double precision for the time step from plugin..


! Run the plugin only at every RUN_EVERY steps
if ( (MOD(NSTEP, RUN_EVERY) /= 0) .or. (NSTEP.lt.INIT_STEP) ) then
  return
endif

if ( (FINAL_STEP.ge.0) .and. (NSTEP.gt.FINAL_STEP) ) then
  return
endif

! check if NSTEP has changed
#ifdef WITH_SCM_PLUME_PLUGIN_UNIQUE_STEPS
if (NSTEP == NSTEP_OLD) then
  return
else
  NSTEP_OLD = NSTEP
endif
#endif

call log%info("SCM-PLUGIN executing step..")

if (.not.larea) then
    do iloc=1, nb_locations
      if( myproc == locations(iloc)%iproc .and. extract_mgr%should_extract(iloc, NSTEP) ) then
        call fillvar_from_plume(myproc, locations(iloc), fields_srf_set, param_ids_srf)
      endif
    enddo
endif

! update all the SP fields into nodepoints
START_PLUGIN_TIMER("scm_run.update_sp_fields")
do ifield=1,fields_spc_set%size()
  call update_nodefield_from_field(fields_spc(ifield), nodepoints, fields_spc_nodes(ifield), ghost_mask)
enddo
STOP_PLUGIN_TIMER("scm_run.update_sp_fields")

! update the windfield on nodepoints
START_PLUGIN_TIMER("scm_run.update_wind_field")
call update_nodefield_from_fields(fields_uv, nodepoints, windfield, ghost_mask)
STOP_PLUGIN_TIMER("scm_run.update_wind_field")

! update all the CLD fields into nodepoints
START_PLUGIN_TIMER("scm_run.update_cld_fields")
do ifield=1,fields_cld_set%size()
  call update_nodefield_from_field(fields_cld(ifield),nodepoints,fields_cld_nodes(ifield), ghost_mask)
enddo
STOP_PLUGIN_TIMER("scm_run.update_cld_fields")

START_PLUGIN_TIMER("scm_run.process_plume_fields")

call process_plume_fields(nproc, &
                          myproc, &
                          NSTEP, &
                          nb_locations, &
                          locations(1:nb_locations), &
                          NLEV, &
                          pvah, &
                          pvbh, &
                          fvm, &
                          nodepoints, &
                          windfield, &
                          gpfields_from_sp, &
                          gridpoints, &
                          gpfields_cld_nodes, &
                          extract_mgr, &
                          nabla, &
                          grad, &
                          grad_one_lev, &
                          grad_wind, &
                          param_ids_spc, &
                          param_ids_cld)
STOP_PLUGIN_TIMER("scm_run.process_plume_fields")


START_PLUGIN_TIMER("scm_run.write_netcdf")
if (.not.larea) then
  call nc_writer%write(myproc, NSTEP, locations, nb_locations, &
                     & PVAH, PVBH, dataid, nlev, INFO, extract_mgr)
endif
STOP_PLUGIN_TIMER("scm_run.write_netcdf")

call log%info("SCM-PLUGIN step completed !")

! stop the profiler timer
STOP_PLUGIN_TIMER("scm_run")

end subroutine scm_run




subroutine scm_teardown(plugin_config, model_data)
  type(fckit_configuration) :: plugin_config
  type(plume_data) :: model_data
  integer :: iloc, ifield
  character(127) :: msg

  deallocate(pvah)
  deallocate(pvbh)
  deallocate(zlat)
  deallocate(zlon)  

  write(msg,'(A)') " finished, cleaning up! "; call log%info(msg)
  do iloc=1, nb_locations
    if( myproc == locations(iloc)%iproc ) then
      call DEALLOCATE_COLUMNS(locations(iloc)%PP)
    endif
  enddo

  deallocate(locations)

  ! Cleanup
  call fvm%final()  

  do ifield=1,n_fields_srf
    call fields_srf(ifield)%final()
  enddo

  do ifield=1,n_fields_cld
    call fields_cld(ifield)%final()
  enddo

  do ifield=1,n_fields_cld
    call fields_cld_nodes(ifield)%final()
  enddo

  do ifield=1,n_fields_spc
    call fields_spc(ifield)%final()
  enddo

  do ifield=1,n_fields_spc
    call fields_spc_nodes(ifield)%final()
  enddo

  do ifield=1,n_fields_oth
    call fields_oth(ifield)%final()
  enddo

#ifdef WITH_SCM_GRIB2_FIELDS
  do ifield=1,n_fields_sol
    call fields_sol(ifield)%final()
  enddo
#endif


  call fields_srf_set%final()
  call fields_cld_set%final()
  call fields_spc_set%final()
  call fields_oth_set%final()
  call gpfields_from_sp%final()
  call gpfields_cld_nodes%final()

  call windfield%final()
  call field_u%final()
  call field_v%final()
  call fields_uv(1)%final()
  call fields_uv(2)%final()

  call nabla%final()
  call grad%final()
  call grad_one_lev%final()
  call grad_wind%final()

  ! if (allocated(ghost_mask)) deallocate(ghost_mask)

  if (allocated(param_ids_spc)) deallocate(param_ids_spc)
  if (allocated(param_ids_cld)) deallocate(param_ids_cld)
  if (allocated(param_ids_srf)) deallocate(param_ids_srf)

  call fvm%final()
  call nodepoints%final()
  call gridpoints%final()

  call nc_writer%finalize()
  call extract_mgr%finalize()

  PRINT_PLUGIN_TIMER(mpi_comm)

  call mpi_comm%final()


end subroutine scm_teardown



end module plugin_impl_mod
