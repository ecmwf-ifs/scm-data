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
  

  use fckit_configuration_module, only : fckit_configuration

  use plume_module, only : plume_check
  use plume_data_module, only : plume_data
  
  use yomvar
  use vert_coord_tables_mod

  ! Available fields from the plugin
  use plugin_utils_mod, only : n_fields_srf
  use plugin_utils_mod, only : n_fields_cld
  use plugin_utils_mod, only : n_fields_spc
  use plugin_utils_mod, only : n_fields_oth

  use plugin_utils_mod, only : field_names_srf
  use plugin_utils_mod, only : field_names_cld
  use plugin_utils_mod, only : field_names_spc
  use plugin_utils_mod, only : field_names_oth

  use plugin_utils_mod, only : param_name2idx, get_vertical_tables_from_namelist

implicit none


private


! vectors of fields
type(atlas_Field) :: fields_srf(n_fields_srf)
type(atlas_Field) :: fields_cld(n_fields_cld)
type(atlas_Field) :: fields_spc(n_fields_spc)
type(atlas_Field) :: fields_oth(n_fields_oth)

! Atlas Fieldsets
type(atlas_FieldSet) :: fields_srf_set  ! surfc field
type(atlas_FieldSet) :: fields_cld_set  ! cloud fields
type(atlas_FieldSet) :: fields_spc_set  ! spctr fields
type(atlas_FieldSet) :: fields_oth_set  ! other fields


type(atlas_Field) :: field_u
type(atlas_Field) :: field_v
type(atlas_Field) :: field_uv ! field that contains 2 variables: U and V


LOGICAL :: LPROGNOSTIC
LOGICAL :: LAREA
INTEGER :: RUN_EVERY

TYPE(TLOCATION), ALLOCATABLE:: LOCATIONS(:)
INTEGER(KIND=JPIM) :: NB_LOCATIONS

TYPE(TINFO) :: INFO

type(atlas_fvm_Method) :: fvm
type(atlas_functionspace_NodeColumns) :: nodepoints
type(atlas_functionspace_StructuredColumns) :: gridpoints

REAL(KIND=JPRB), ALLOCATABLE :: PVAH(:) ! A coefficients for calculation of vertical levels
REAL(KIND=JPRB), ALLOCATABLE :: PVBH(:) ! B coefficients for calculation of vertical levels
REAL(KIND=JPRB), ALLOCATABLE :: ZLAT(:) ! point lats
REAL(KIND=JPRB), ALLOCATABLE :: ZLON(:) ! point lons

INTEGER(KIND=JPIM) :: nstep
INTEGER(KIND=JPIM) :: NLEV
INTEGER(KIND=JPIM) :: NB_NODES

! MPI INFO
type(fckit_mpi_comm) :: mpi_comm
INTEGER(KIND=JPIM) :: NPROC
INTEGER(KIND=JPIM) :: MYPROC


! Output directory (from env variable)
character(1024) :: scm_data_output_dir
integer :: config_env_status


public :: scm_setup
public :: scm_run
public :: scm_teardown

contains



subroutine scm_setup(plugin_config, model_data)
  type(fckit_configuration) :: plugin_config
  type(fckit_configuration), allocatable :: plugin_config_points(:)
  type(plume_data) :: model_data

  type(atlas_Config) :: config

  CHARACTER(LEN=:), allocatable :: DATAID
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
  
  CHARACTER*127 msg
  
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

  character(256) :: vtable_testing_namelist
  integer :: vtable_testing_namelist_status

#ifdef WITH_SCM_SINGLE_PRECISION  
  REAL(KIND=c_float), POINTER :: dummy_data(:,:)
#else
  REAL(KIND=c_double), POINTER :: dummy_data(:,:)
#endif


#include "nearest_distance.h"
#include "nearest_distance_kdtree.h"

  write(msg,'(A)')  "--> getini1c: start"; call log%debug(msg)

  ! setup MPI info
  mpi_comm = fckit_mpi_comm()
  NPROC  = mpi_comm%size()
  MYPROC = mpi_comm%rank() + 1 

  ! fill-in array of fields (SRF)
  do ifield=1,size(field_names_srf)
    write(msg,'(A,A)') "getting field: ", trim(field_names_srf(ifield)); call log%info(msg)
    call plume_check(model_data%get_shared_atlas_field(trim(field_names_srf(ifield)), fields_srf(ifield)) );
  enddo

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

  ! get the functionspace from first field
  input_fs = fields_srf(1)%functionspace()
  nlev = input_fs%levels()

  ! read parameters from plugin-core configuration
  found = plugin_config%get("LPROGNOSTIC", LPROGNOSTIC_INT)
  if (LPROGNOSTIC_INT == 1) then
    LPROGNOSTIC = .true.
  else
    LPROGNOSTIC = .false.
  endif
  
  found = plugin_config%get("LAREA", LAREA_INT)
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

  ! ID of data
  found = plugin_config%get("DATAID", DATAID)

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
    locations(ipoint)%ILOC = -1
    locations(ipoint)%IFILE_ID = -1
    locations(ipoint)%IPROC = -1

    zlat(ipoint) = PT_LAT
    zlon(ipoint) = PT_LON
  enddo

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


! check if the environment variable PLUME_PLUGINS_OUTPUT_DIR is set, if so the plugin will write output files there
call get_environment_variable("PLUME_PLUGINS_OUTPUT_DIR", scm_data_output_dir, status=config_env_status)


! finalisation
call config%final()
call nodes%final()
call mesh%final()
call input_grid%final()
call meshgenerator%final()
call partitioner%final()

  
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

type(atlas_Field) :: field

type(atlas_FieldSet) :: sfcfields
type(atlas_FieldSet) :: gpfields
type(atlas_FieldSet) :: gpfields_from_sp
type(atlas_FieldSet) :: gpdummy

type(atlas_FieldSet) :: spfields
type(atlas_Metadata) :: metadata

type(atlas_Field) :: windfield

type(atlas_Field) :: ghostField
type(atlas_Field) :: lonlatField

type(atlas_Field) :: field_tmp
type(atlas_Field) :: fields_uv(2)

INTEGER(KIND=c_int), POINTER :: ghost(:)
REAL(KIND=c_double), POINTER :: lonlat(:,:)

character(len=30) :: dataid, file
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

CHARACTER*127 msg
integer :: ifield

#ifdef WITH_SCM_SINGLE_PRECISION  
  REAL(KIND=c_float), POINTER :: dummy_data(:,:)
#else
  REAL(KIND=c_double), POINTER :: dummy_data(:,:)
#endif

type(atlas_FieldSet) :: gpfields_cld_nodes  ! CLD fields on nodes

! name of output NetCDF file
character (len=40) :: nc_filename

! full path of output NetCDF file
character(len=:), allocatable :: nc_fullpath


#include "fillvar_from_plume.h"
#include "su_wrt_nc.h"
#include "wrt1c_nc.h"
#include "create_nodefield.h"

call plume_check(model_data%get_int("NSTEP",NSTEP))

! Run the plugin only at every RUN_EVERY steps
if (MOD(NSTEP, RUN_EVERY) /= 0) then
  return
endif

call log%info("SCM-PLUGIN executing step..")

if (.not.larea) then
  !$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(iloc)
    do iloc=1, nb_locations
      if( myproc == locations(iloc)%iproc ) then
        call fillvar_from_plume(myproc, locations(iloc), fields_srf_set)
      endif
    enddo
  !$OMP END PARALLEL DO
endif

! write(*,*) "field_u%halo() = ", field_u%halo()
gpfields_from_sp = atlas_FieldSet("nodepoints")
do ifield=1,fields_spc_set%size()
  field_tmp = create_nodefield_from_field(fields_spc(ifield),nodepoints)
  call gpfields_from_sp%add(field_tmp)
enddo

fields_uv(1) = field_u
fields_uv(2) = field_v
windfield = create_nodefield_from_fields("wind", fields_uv, nodepoints)


! interpolate all the CLD fields into nodepoints 
! write(*,*) "interpolating CLD.."
gpfields_cld_nodes = atlas_FieldSet("nodepoints")
do ifield=1,fields_cld_set%size()
  field_tmp = create_nodefield_from_field(fields_cld(ifield),nodepoints)
  call gpfields_cld_nodes%add(field_tmp)
enddo

call process_plume_fields(nproc, &
                          myproc, &
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
                          gpfields_cld_nodes)


do iloc=1, nb_locations
  if( myproc == locations(iloc)%iproc ) then
    ! netcdf write this location from this processor
    if (.not.larea) then
      write(msg,'(A)') " setting up output fields to netcdf  "; call log%debug(msg)
      write(msg,'(A,I0)') " loc processor ", locations(iloc)%IPROC; call log%debug(msg)
      write(msg,'(A,I0)') " loc knode ", locations(iloc)%ILOC; call log%debug(msg)
      write(msg,'(A,F8.4)') " loc latitude ", locations(iloc)%RLATI; call log%debug(msg)
      write(msg,'(A,F8.4)') " loc longitude ", locations(iloc)%RLONI; call log%debug(msg)
      write(msg,'(A,F8.4)') " loc pressure ", locations(iloc)%PP%PLNSP; call log%debug(msg)

      ! assemble the filename
      write(nc_filename,"(A,I5.5,A,I5.5,A,I5.5,A)") 'scm_in_proc_',myproc,'_pt_',iloc,'_step_',NSTEP,'.nc'

      ! if the env variable PLUME_PLUGINS_OUTPUT_DIR is set, write the output files there
      ! otherwise write them in the current directory
      if (config_env_status .eq. 0) then
        nc_fullpath = trim(scm_data_output_dir)//'/'//trim(nc_filename)
      else
        nc_fullpath = trim(nc_filename)
      end if

      ! setup the output NetCDF file
      CALL SU_WRT_NC (nc_fullpath,PVAH,PVBH,dataid,locations(iloc)%IFILE_ID,nlev)

      ! write data to the NetCDF file
      write(msg,'(A,I0,1X,I0)') " writing output fields to netcdf  ", INFO%ISTEP, INFO%IDATE; call log%debug(msg)
      CALL WRT1C_NC(locations(iloc),PVAH,PVBH,INFO,locations(iloc)%IFILE_ID,nlev)

    endif
  endif
enddo

call log%info("SCM-PLUGIN step completed !")

end subroutine scm_run




subroutine scm_teardown(plugin_config, model_data)
  type(fckit_configuration) :: plugin_config
  type(plume_data) :: model_data
  integer :: iloc
  CHARACTER*127 msg

  deallocate(pvah)
  deallocate(pvbh)
  deallocate(locations)

  write(msg,'(A)') " finished, cleaning up! "; call log%info(msg)
  do iloc=1, nb_locations
    if( myproc == locations(iloc)%iproc ) then
      call DEALLOCATE_COLUMNS(locations(iloc)%PP)
    endif
  enddo

  ! Cleanup  
  call fvm%final()
  call mpi_comm%final()
 
end subroutine scm_teardown



end module plugin_impl_mod
