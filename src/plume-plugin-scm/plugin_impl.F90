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

  use config_handler_mod,     only : config_handler
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

! Ghost mask (created once in scm_setup, reused in scm_run).
! ghost_mask points into ghostField's data, so the field handle has to stay alive
! for as long as the mask is read - it is released in scm_teardown, not in scm_setup.
type(atlas_Field) :: ghostField
INTEGER(KIND=c_int), POINTER :: ghost_mask(:) => null()

! Cached paramIds for fields (computed once in scm_setup, read in scm_run) - PLUME-85
INTEGER(KIND=JPIM), ALLOCATABLE :: param_ids_spc(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: param_ids_cld(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: param_ids_srf(:)

type(atlas_fvm_Method) :: fvm
type(atlas_functionspace_NodeColumns) :: nodepoints
type(atlas_functionspace_StructuredColumns) :: gridpoints

! Plugin configuration: parsed and validated once in scm_setup, then read
! through its getters. It owns every configuration key and every environment
! variable the plugin looks at.
type(config_handler) :: plugin_cfg

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

public :: scm_setup
public :: scm_run
public :: scm_teardown

contains



subroutine scm_setup(plugin_config, model_data)
  type(fckit_configuration) :: plugin_config
  type(plume_data) :: model_data

  type(atlas_Config) :: config

  CHARACTER(LEN=30) :: FILE
  CHARACTER(LEN=10) :: FIELDNAME

  REAL(KIND=JPRB) :: ZDELTA
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

  START_PLUGIN_TIMER("scm_setup.get_fields")

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

  STOP_PLUGIN_TIMER("scm_setup.get_fields")

  START_PLUGIN_TIMER("scm_setup.config")

  call plume_check(model_data%get_int("NSTEP",NSTEP))
  call plume_check(model_data%get_double("TSTEP",TSTEP))
  call plume_check(model_data%get_int("INIT_DATE",INIT_DATE))
  call plume_check(model_data%get_int("INIT_TIME",INIT_TIME))

  ! get the functionspace from first field
  input_fs = fields_srf(1)%functionspace()
  nlev = input_fs%levels()

  ! Read, validate and log the plugin configuration (plugin-core configuration
  ! keys plus the environment variables the plugin honours). Every option is
  ! read from the handler from here on.
  call plugin_cfg%init(plugin_config)

  ! max radius of search for nearest grid point: without it the nearest point
  ! is found with a kdtree search
  found_delta = plugin_cfg%has_delta()
  ZDELTA      = 0._JPRB
  if (found_delta) ZDELTA = plugin_cfg%get_delta()

  STOP_PLUGIN_TIMER("scm_setup.config")

  START_PLUGIN_TIMER("scm_setup.vert_tables")
  ! vertical levels coefficients
  ! For testing only: read the vertical levels from namelist (for consistency)
  if (plugin_cfg%has_vert_tables_namelist()) then
    write(msg,'(A,A)') "Reading vertical tables from namelist: ", &
      & trim(plugin_cfg%get_vert_tables_namelist()); call log%info(msg)
    call get_vertical_tables_from_namelist(plugin_cfg%get_vert_tables_namelist(), NLEV, PVAH, PVBH)
  else
    call get_vertical_tables(NLEV, PVAH, PVBH)
  endif
  STOP_PLUGIN_TIMER("scm_setup.vert_tables")

  START_PLUGIN_TIMER("scm_setup.points")
  ! point coordinates (the handler has already checked that there is at least
  ! one point and that each one has valid coordinates)
  nb_locations = plugin_cfg%get_nb_points()

  allocate(locations(nb_locations))
  allocate(zlat(NB_LOCATIONS))
  allocate(zlon(NB_LOCATIONS))

  do ipoint=1,nb_locations
    PT_LAT = plugin_cfg%get_point_lat(ipoint)
    PT_LON = plugin_cfg%get_point_lon(ipoint)

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
  call extract_mgr%init(plugin_cfg)
  STOP_PLUGIN_TIMER("scm_setup.points")

  ! the configuration itself (points included) has been logged by the handler
  write(msg,'(A,I0)')   "nlev = ", nlev; call log%info(msg)

  !        2.   set up necessary info on gg and sh fields
  !             --------------------------------------------------------------

  START_PLUGIN_TIMER("scm_setup.partitioner")
  ! initialize config on the sphere
  config = atlas_Config()
  call config%set("radius",6371229.0)

  ! grid from model
  input_grid = input_fs%grid()
  allocate(input_fs_parent, source=input_fs)
  partitioner = atlas_MatchingPartitioner(input_fs_parent)
  STOP_PLUGIN_TIMER("scm_setup.partitioner")

  START_PLUGIN_TIMER("scm_setup.meshgen")
  ! mesh
  meshgenerator = atlas_Meshgenerator(config)
  mesh = meshgenerator%generate(input_grid,partitioner)
  STOP_PLUGIN_TIMER("scm_setup.meshgen")

  nodes = mesh%nodes()

  ! find the processor and node location on the processor responsible for each user specified lat/lon location
  nb_nodes = nodes%size()
  lonlatField = nodes%lonlat()
  call lonlatField%data(lonlat)
  ghostField = nodes%ghost()
  call ghostField%data(ghost)

  write(msg,'(A,I0,A,I0)') "nodes: ", nb_nodes, ", lonlat%size(): ", lonlatField%size(); call log%info(msg)

  ! find the nearest grid point to each user specified lat/lon location
  START_PLUGIN_TIMER("scm_setup.nearest_distance")
  if ( .not. found_delta ) then
    write(msg,'(A)') "No delta specified, using kdtree to find nearest point"; call log%info(msg)
    call nearest_distance_kdtree(nb_nodes, ghost, lonlat, nb_locations, locations)
  else
    write(msg,'(A,F8.4)') "delta = ", zdelta; call log%info(msg)
    call nearest_distance(nb_nodes, ghost, lonlat, myproc, zdelta, nb_locations, locations)
  endif
  STOP_PLUGIN_TIMER("scm_setup.nearest_distance")

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
  START_PLUGIN_TIMER("scm_setup.fvm")
  fvm = atlas_fvm_Method(mesh, config)
  nodepoints = fvm%node_columns()
  STOP_PLUGIN_TIMER("scm_setup.fvm")
  write(msg,'(A,A)') "finished Atlas fvm function space"; call log%info(msg)

  gridpoints = input_fs

  START_PLUGIN_TIMER("scm_setup.fieldsets")
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

  STOP_PLUGIN_TIMER("scm_setup.fieldsets")

  START_PLUGIN_TIMER("scm_setup.paramids")
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

  STOP_PLUGIN_TIMER("scm_setup.paramids")

  START_PLUGIN_TIMER("scm_setup.alloc_columns")
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


STOP_PLUGIN_TIMER("scm_setup.alloc_columns")

! initialise the NetCDF output writer (append_output, append_output_nsteps,
! dataid and the output directory; run_every/init_step define the output
! batch windows)
START_PLUGIN_TIMER("scm_setup.nc_init")
call nc_writer%init(plugin_cfg)
STOP_PLUGIN_TIMER("scm_setup.nc_init")

START_PLUGIN_TIMER("scm_setup.create_node_fields")
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
! ghostField is module scope and deliberately NOT finalised here: ghost_mask aliases
! its data and is read on every step by update_nodefield_from_field.
ghostField = nodes%ghost()
call ghostField%data(ghost_mask)
STOP_PLUGIN_TIMER("scm_setup.create_node_fields")

! finalisation
START_PLUGIN_TIMER("scm_setup.finalise")
call input_fs%final()
call input_fs_parent%final()
call input_grid%final()
call grid%final()
call mesh%final()
call nodes%final()
call meshgenerator%final()
call partitioner%final()
call lonlatField%final()
STOP_PLUGIN_TIMER("scm_setup.finalise")

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

! start the profiler timer. scm_run is entered on every model step, including the
! ones skipped by the guards below, so its call count reports how often the host
! called us and the executed-step children report how often we actually ran.
START_PLUGIN_TIMER("scm_run")

START_PLUGIN_TIMER("scm_run.prologue")
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
STOP_PLUGIN_TIMER("scm_run.prologue")


! Run the plugin only at every run_every steps.
! Every early return below must stop "scm_run" first, otherwise the region is left
! open and the enclosing self-time accounting is wrong.
if ( (MOD(NSTEP, plugin_cfg%get_run_every()) /= 0) .or. (NSTEP.lt.plugin_cfg%get_init_step()) ) then
  STOP_PLUGIN_TIMER("scm_run")
  return
endif

if ( (plugin_cfg%get_final_step().ge.0) .and. (NSTEP.gt.plugin_cfg%get_final_step()) ) then
  STOP_PLUGIN_TIMER("scm_run")
  return
endif

! check if NSTEP has changed
#ifdef WITH_SCM_PLUME_PLUGIN_UNIQUE_STEPS
if (NSTEP == NSTEP_OLD) then
  STOP_PLUGIN_TIMER("scm_run")
  return
else
  NSTEP_OLD = NSTEP
endif
#endif

call log%info("SCM-PLUGIN executing step..")

START_PLUGIN_TIMER("scm_run.fillvar_from_plume")
do iloc=1, nb_locations
  if( myproc == locations(iloc)%iproc .and. extract_mgr%should_extract(iloc, NSTEP) ) then
    call fillvar_from_plume(myproc, locations(iloc), fields_srf_set, param_ids_srf)
  endif
enddo
STOP_PLUGIN_TIMER("scm_run.fillvar_from_plume")

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
call nc_writer%write(myproc, NSTEP, locations, nb_locations, &
                   & PVAH, PVBH, nlev, INFO, extract_mgr)
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

  START_PLUGIN_TIMER("scm_teardown")

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
  START_PLUGIN_TIMER("scm_teardown.atlas_final")
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

  ! ghost_mask is not owned here - it aliases ghostField's data. Drop the alias
  ! first, then release the field handle that kept it valid since scm_setup.
  ghost_mask => null()
  call ghostField%final()

  if (allocated(param_ids_spc)) deallocate(param_ids_spc)
  if (allocated(param_ids_cld)) deallocate(param_ids_cld)
  if (allocated(param_ids_srf)) deallocate(param_ids_srf)

  call fvm%final()
  call nodepoints%final()
  call gridpoints%final()
  STOP_PLUGIN_TIMER("scm_teardown.atlas_final")

  START_PLUGIN_TIMER("scm_teardown.nc_finalize")
  call nc_writer%finalize()
  STOP_PLUGIN_TIMER("scm_teardown.nc_finalize")

  START_PLUGIN_TIMER("scm_teardown.extract_mgr_finalize")
  call extract_mgr%finalize()
  STOP_PLUGIN_TIMER("scm_teardown.extract_mgr_finalize")

  START_PLUGIN_TIMER("scm_teardown.config_finalize")
  call plugin_cfg%finalize()
  STOP_PLUGIN_TIMER("scm_teardown.config_finalize")

  ! Close scm_teardown before reporting, so that no region is still open when the
  ! table is built (the report itself is not part of the measured work).
  STOP_PLUGIN_TIMER("scm_teardown")

  ! Collective on mpi_comm: every rank must reach this call.
  PRINT_PLUGIN_TIMER(mpi_comm)

  call mpi_comm%final()


end subroutine scm_teardown



end module plugin_impl_mod
