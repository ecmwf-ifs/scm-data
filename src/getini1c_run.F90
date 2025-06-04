! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.


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

character(len=30) :: dataid, cgrid
character(len=10) :: fieldname

REAL(KIND=JPRB) :: zdelta
logical   :: LAREA, lprognostic, LSINGLE
INTEGER(KIND=JPIM) :: nsmax, nstep, I, J, jfld, ilev, iloc, isize, nb_locations, nb_nodes, nlev, iparam, nlocmax
INTEGER(KIND=JPIM) :: NPROC, MYPROC
REAL(KIND=JPRB), ALLOCATABLE :: PVAH(:), PVBH(:)
REAL(KIND=JPRB), ALLOCATABLE :: zlat(:), zlon(:)

CHARACTER*127 msg
type(fckit_mpi_comm) :: mpi_comm

! name of output NetCDF file
character (len=40) :: nc_filename

#include "rdnam.h"
#include "nearest_distance.h"
#include "read_grib_scm.h"
#include "fillvar.h"
#include "compute_fields.h"
#include "su_wrt_nc.h"
#include "wrt1c_nc.h"

write(msg,'(A)')  "getini1c: start"; call log%info(msg)

mpi_comm = fckit_mpi_comm()
! processor
NPROC  = mpi_comm%size()
MYPROC = mpi_comm%rank() + 1 

!        1.   read namelist on points list, grid info, netcdf info
!             ---------------------------------------------------------------------------

! need to add: 
! nsmax == spectral truncation
! cgrid == gridname N/O/G + number of latitudes (on one hemisphere)
! locations(i) list of the points (lat,lon)
! zdelta == minimum distance in degrees  to chosen point (needed or just 0.5 delta ?)

! need to initialize A,Bs from vtable added to namelist

! at the moment we support only points, i.e. larea=false

! read namelist
if (present(namelist_file)) then
  call RDNAM(LAREA,LPROGNOSTIC,dataid, zdelta, nlev, nsmax, nstep, cgrid, &
 & pvah, pvbh, nb_locations, zlat, zlon, namelist_file)
else
  call RDNAM(LAREA,LPROGNOSTIC,dataid, zdelta, nlev, nsmax, nstep, cgrid, &
 & pvah, pvbh, nb_locations, zlat, zlon)
endif

INFO%NSTEP=nstep
allocate(locations(nb_locations))
do j=1, nb_locations
  if( zlon(j) < 0. ) then
    zlon(j) = 360.+zlon(j)
  endif
  locations(j)%RLONI = zlon(j)
  locations(j)%RLATI = zlat(j)
  locations(j)%RLONI_USER = zlon(j)
  locations(j)%RLATI_USER = zlat(j)
  locations(j)%ILOC = -1
  locations(j)%IFILE_ID = -1
  locations(j)%IPROC = -1
enddo

write(msg,'(A,I0)') "nb_locations = ",nb_locations; call log%info(msg)
write(msg,'(A,F5.3)') "zdelta = ",zdelta; call log%info(msg)
write(msg,'(A,I0)') "nlev = ",nlev; call log%info(msg)
write(msg,'(A,I0)') "nsmax = ",nsmax; call log%info(msg)
write(msg,'(A,A)') "cgrid = ",cgrid; call log%info(msg)

!        2.   set up necessary info on gg and sh fields
!             --------------------------------------------------------------

! initialize config on the sphere
config = atlas_Config()
call config%set("radius",6371229.0)

! Setup local derivatives using FVM, use string of pre-defined grids "O" == octahedral, "N" == standard, "G" == full Gaussian
! cgrid = "N24"

!grid = atlas_grid_ReducedGaussian(cgrid)
grid = atlas_StructuredGrid(cgrid)
meshgenerator = atlas_Meshgenerator(config)
partitioner = atlas_Partitioner("ectrans")
mesh = meshgenerator%generate(grid,partitioner)
call partitioner%final()
nodes = mesh%nodes()

! find the processor and node location on the processor responsible for each user specified lat/lon location
nb_nodes = nodes%size()
lonlatField = nodes%lonlat()
call lonlatField%data(lonlat)
ghostField = nodes%ghost()
call ghostField%data(ghost)
write(msg,'(A,I0,1X, I0)') "ndes , lonlat%size", nb_nodes, lonlatField%size()/2; call log%info(msg)

call nearest_distance(nb_nodes, ghost, lonlat, myproc, zdelta, nb_locations, locations)
do j=1, nb_locations
  if( myproc == locations(j)%iproc ) then
    write(msg,'(A,I0)') "nearest point proc ", locations(j)%IPROC ; call log%info(msg)
!    write(msg,'(A,I0,2(1X,F8.4))') "nearest point lon ", j,locations(j)%RLONI, ZLON(j) ; call log%info(msg)
!    write(msg,'(A,I0,2(1X,F8.4))') "nearest point lat ", j,locations(j)%RLATI, ZLAT(j) ; call log%info(msg)
!    write(msg,'(A,I0,1X, I0)') "nearest point knode ", j,locations(j)%iloc ; call log%info(msg)
!    write(*,*) 'test if unique proc: ', myproc, locations(j)%iloc, locations(j)%RLONI, locations(j)%RLATI
  endif
enddo
write(msg,'(A)') "finished nearest distances "; call log%info(msg)

write(msg,'(A,I0)') "grid%size()   = ",grid%size();   call log%info(msg)

! this is the functionspace nodepoints
fvm  = atlas_fvm_Method(mesh, config)
nodepoints = fvm%node_columns()
write(msg,'(A,A)') "finished Atlas fvm function space"; call log%info(msg)

! for reading we also create a functionspace gridpoints
gridpoints = atlas_functionspace_StructuredColumns(grid)

! read surface fieldset
sfcfields = atlas_FieldSet("gridpoints")
LSINGLE=.FALSE.
if (present(sfc_grib)) then
  call read_grib_scm(LSINGLE,NPROC,MYPROC,sfc_grib,LPROGNOSTIC,LAREA,INFO,spectral,spfields,gridpoints,sfcfields)
else
  call read_grib_scm(LSINGLE,NPROC,MYPROC,'sfc_grib',LPROGNOSTIC,LAREA,INFO,spectral,spfields,gridpoints,sfcfields)
endif

if (.not.larea) then
!!!$OMP PARALLEL DO SCHEDULE(STATIC) PRIVATE(iloc)
  do iloc=1, nb_locations
    if( myproc == locations(iloc)%iproc ) then
      ! fill sfc variables      
      call fillvar(myproc,locations(iloc),sfcfields)
    endif
  enddo
!!!$OMP END PARALLEL DO
endif

! read upper air fieldset (gridpoint)
gpfields = atlas_FieldSet("gridpoints")
if (present(cld_grib)) then
  call read_grib_scm(LSINGLE,NPROC,MYPROC,cld_grib,LPROGNOSTIC,LAREA,INFO,spectral,spfields,gridpoints,gpfields)
else
  call read_grib_scm(LSINGLE,NPROC,MYPROC,'cld_grib',LPROGNOSTIC,LAREA,INFO,spectral,spfields,gridpoints,gpfields)
endif


! Setup spectral transforms
trans = atlas_Trans(grid,nsmax)
spectral   = atlas_functionspace_Spectral(trans%truncation())
write(msg,'(A,I0)') "spectral%truncation() = ",spectral%truncation(); call log%info(msg)

! read upper air fieldset (spectral)
spfields = atlas_FieldSet("spectral")
if (present(spec_grib)) then
  call read_grib_scm(LSINGLE,NPROC,MYPROC,spec_grib,LPROGNOSTIC,LAREA,INFO,spectral,spfields,dummy,gpdummy)
else
  call read_grib_scm(LSINGLE,NPROC,MYPROC,'spec_grib',LPROGNOSTIC,LAREA,INFO,spectral,spfields,dummy,gpdummy)
endif


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


! here we need a computation at given locations directly from spectral coefficients of the scalars vor/div/omega/z/lnsp/T

! VOR/DIV/omega
! Z/LNSP/T as scalars (and their derivatives as these would no longer be computed from atlas)

! and derived
! U/V wind (and their derivatives as these would no longer be computed from atlas)

! needs then modification to compute_fields to remove gradient calls ... 

write(msg,'(A)') " compute fields "; call log%info(msg)
call compute_fields(nproc,myproc,nb_locations,locations(1:nb_locations),nlev,&
 &pvah,pvbh,fvm,nodepoints, windfield,gpfields_from_sp, gridpoints, gpfields)
do iloc=1, nb_locations
  if( myproc == locations(iloc)%iproc ) then
    ! netcdf write this location from this processor
    if (.not.larea) then
      write(msg,'(A)') " setting up output fields to netcdf  "; call log%info(msg)
!      write(msg,'(A,I0)') " loc processor ", locations(iloc)%IPROC; call log%info(msg)
!      write(msg,'(A,I0)') " loc knode ", locations(iloc)%ILOC; call log%info(msg)
!      write(msg,'(A,F8.4)') " loc latitude ", locations(iloc)%RLATI; call log%info(msg)
!      write(msg,'(A,F8.4)') " loc longitude ", locations(iloc)%RLONI; call log%info(msg)
!      write(msg,'(A,F8.4)') " loc pressure ", locations(iloc)%PP%PLNSP; call log%info(msg)
      
      ! setup output netcdf file
      write(nc_filename,"(A,I5.5,A,I5.5,A,I5.5,A)") 'scm_in_proc_',myproc,'_pt_',iloc,'_step_',0,'.nc'
      CALL SU_WRT_NC (nc_filename,PVAH,PVBH,dataid,locations(iloc)%IFILE_ID,nlev)

      ! write output fields to netcdf
      write(msg,'(A,I0,1X,I0)') " writing output fields to netcdf  ", INFO%ISTEP, INFO%IDATE ; call log%info(msg)
      CALL WRT1C_NC(locations(iloc),PVAH,PVBH,INFO,locations(iloc)%IFILE_ID,nlev)
      
    endif
  endif
enddo
write(msg,'(A)') " finished, cleaning up! "; call log%info(msg)
do iloc=1, nb_locations
  if( myproc == locations(iloc)%iproc ) then
    call DEALLOCATE_COLUMNS(locations(iloc)%PP)
  endif
enddo
deallocate(pvah)
deallocate(pvbh)
deallocate(locations)

! Cleanup
call config%final()
call fvm%final()
call nodes%final()
call mesh%final()
call grid%final()
call meshgenerator%final()

write(msg,'(A)')  "getini1c: end"
call log%info(msg)

return_code = 0

end subroutine getini1c_run