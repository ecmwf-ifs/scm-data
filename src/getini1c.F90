!!! use ldd <executable> to figure out which libraries have been used in the executable !!!
!....................................................................... 
!     GETS A PROFILE OF ATMOSPHERIC AND SURFACE VARIABLES
!     FOR LATER INPUT FOR THE SINGLE COLUMN MODEL
!
!     Nils Wedi,  March 2016
!     
!    The program is a rewrite based on Atlas data structures (Willem Deconinck)
!    and previous contributions by Martin Koehler, Gisela Seuffert, Pedro Viterbo, Nils Wedi
!....................................................................... 

! the main execution routine (program is at the end)
subroutine run( return_code )

use, intrinsic :: iso_C_binding
use atlas_module, only: atlas_Config, atlas_grid_Structured, atlas_Mesh, atlas_mesh_Nodes, atlas_MeshGenerator, &
 & atlas_Trans, atlas_functionspace_Spectral, atlas_fvm_Method, atlas_functionspace_NodeColumns, atlas_Field, atlas_FieldSet, &
 & atlas_Metadata, atlas_log, atlas_mpi_size,  atlas_mpi_proc, atlas_real, atlas_meshgenerator_Structured, atlas_mpi_comm, &
 & atlas_functionspace_StructuredColumns
use yomvar

implicit none

INTEGER(KIND=JPIM) :: return_code
type(atlas_Config) :: config
type(atlas_grid_Structured) :: grid
type(atlas_Mesh) :: mesh
type(atlas_mesh_Nodes) :: nodes
type(atlas_MeshGenerator) :: meshgenerator
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

#include "rdnam.h"
#include "nearest_distance.h"
#include "read_grib_scm.h"
#include "fillvar.h"
#include "compute_fields.h"
#include "su_wrt_nc.h"
#include "wrt1c_nc.h"

call atlas_log%info("getini1c: start")

! processor
NPROC  = atlas_mpi_size()
MYPROC = atlas_mpi_proc()

!        1.   read namelist on points list, grid info, netcdf info
!             ---------------------------------------------------------------------------

! need to add: 
! nsmax == spectral truncation
! cgrid == gridname N/O/G + number of latitudes (on one hemisphere)
! locations(i) list of the points (lat,lon)
! zdelta == minimum distance in degrees  to chosen point (needed or just 0.5 delta ?)

! need to initialize A,Bs from vtable added to namelist

! at the moment we support only points, i.e. larea=false

CALL RDNAM(LAREA,LPROGNOSTIC,dataid, zdelta, nlev, nsmax, nstep, cgrid, &
 & pvah, pvbh, nb_locations, zlat, zlon)
INFO%NSTEP=nstep
allocate(locations(nb_locations))
do j=1, nb_locations
  if( zlon(j) < 0. ) then
    zlon(j) = 360.+zlon(j)
  endif
  locations(j)%RLONI = zlon(j)
  locations(j)%RLATI = zlat(j)
  locations(j)%ILOC = -1
  locations(j)%IFILE_ID = -1
  locations(j)%IPROC = -1
enddo

write(atlas_log%msg,'(A,I0)') "nb_locations = ",nb_locations; call atlas_log%info(atlas_log%msg)
write(atlas_log%msg,'(A,F5.3)') "zdelta = ",zdelta; call atlas_log%info(atlas_log%msg)
write(atlas_log%msg,'(A,I0)') "nlev = ",nlev; call atlas_log%info(atlas_log%msg)
write(atlas_log%msg,'(A,I0)') "nsmax = ",nsmax; call atlas_log%info(atlas_log%msg)
write(atlas_log%msg,'(A,A)') "cgrid = ",cgrid; call atlas_log%info(atlas_log%msg)

!        2.   set up necessary info on gg and sh fields
!             --------------------------------------------------------------

! initialize config on the sphere
config = atlas_Config()
call config%set("radius",6371229.0)

! Setup local derivatives using FVM, use string of pre-defined grids "O" == octahedral, "N" == standard, "G" == full Gaussian
! cgrid = "N24"

!grid = atlas_grid_ReducedGaussian(cgrid)
grid = atlas_grid_Structured(cgrid)
meshgenerator = atlas_meshgenerator_Structured(config)
mesh = meshgenerator%generate(grid) ! second optional argument for atlas_GridDistribution
nodes = mesh%nodes()

! find the processor and node location on the processor responsible for each user specified lat/lon location
nb_nodes = nodes%size()
lonlatField = nodes%lonlat()
call lonlatField%data(lonlat)
ghostField = nodes%ghost()
call ghostField%data(ghost)
write(atlas_log%msg,'(A,I0,1X, I0)') "ndes , lonlat%size", nb_nodes, lonlatField%size()/2; call atlas_log%info(atlas_log%msg)

call nearest_distance(nb_nodes, ghost, lonlat, myproc, zdelta, nb_locations, locations)
do j=1, nb_locations
  if( myproc == locations(j)%iproc ) then
    write(atlas_log%msg,'(A,I0)') "nearest point proc ", locations(j)%IPROC ; call atlas_log%info(atlas_log%msg)
!    write(atlas_log%msg,'(A,I0,2(1X,F8.4))') "nearest point lon ", j,locations(j)%RLONI, ZLON(j) ; call atlas_log%info(atlas_log%msg)
!    write(atlas_log%msg,'(A,I0,2(1X,F8.4))') "nearest point lat ", j,locations(j)%RLATI, ZLAT(j) ; call atlas_log%info(atlas_log%msg)
!    write(atlas_log%msg,'(A,I0,1X, I0)') "nearest point knode ", j,locations(j)%iloc ; call atlas_log%info(atlas_log%msg)
!    write(*,*) 'test if unique proc: ', myproc, locations(j)%iloc, locations(j)%RLONI, locations(j)%RLATI
  endif
enddo
write(atlas_log%msg,'(A)') "finished nearest distances "; call atlas_log%info(atlas_log%msg)

write(atlas_log%msg,'(A,I0)') "grid%N()      = ",grid%N();      call atlas_log%info(atlas_log%msg)
write(atlas_log%msg,'(A,I0)') "grid%npts()   = ",grid%npts();   call atlas_log%info(atlas_log%msg)

! this is the functionspace nodepoints
fvm  = atlas_fvm_Method(mesh, config)
nodepoints = fvm%node_columns()
write(atlas_log%msg,'(A,A)') "finished Atlas fvm function space"; call atlas_log%info(atlas_log%msg)

! for reading we also create a functionspace gridpoints
gridpoints = atlas_functionspace_StructuredColumns(grid)

! read surface fieldset
file=' '
write(file,'(A)') 'sfc_grib '
sfcfields = atlas_FieldSet("gridpoints")
LSINGLE=.FALSE.
call read_grib_scm(LSINGLE,MYPROC,file,LPROGNOSTIC,LAREA,INFO,spectral,spfields,gridpoints,sfcfields)
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
file=' '
write(file,'(A)') 'cld_grib '
gpfields = atlas_FieldSet("gridpoints")
call read_grib_scm(LSINGLE,MYPROC,file,LPROGNOSTIC,LAREA,INFO,spectral,spfields,gridpoints,gpfields)

! Setup spectral transforms
trans = atlas_Trans(grid,nsmax)
spectral   = atlas_functionspace_Spectral(trans)
write(atlas_log%msg,'(A,I0)') "trans%nsmax() = ",trans%nsmax(); call atlas_log%info(atlas_log%msg)

! read upper air fieldset (spectral)
file=' '
write(file,'(A)') 'spec_grib '
spfields = atlas_FieldSet("spectral")
call read_grib_scm(LSINGLE,MYPROC,file,LPROGNOSTIC,LAREA,INFO,spectral,spfields,dummy,gpdummy)

write(atlas_log%msg,'(A)') " finished I/O"; call atlas_log%info(atlas_log%msg)

gpfields_from_sp = atlas_FieldSet("nodepoints")
isize = spfields%size()
do jfld=1,isize

  field = spfields%field(jfld)
  metadata = field%metadata()
  call metadata%get("paramId", iparam)
  call metadata%get("level", ilev)

  write(fieldname, '(I0)') jfld
  field = nodepoints%create_field(fieldname,atlas_real(JPRB))
  metadata = field%metadata()
  call metadata%set("paramId",iparam)
  call metadata%set("level",ilev)
  call gpfields_from_sp%add( field )
enddo
write(atlas_log%msg,'(A)') " inverse transform starting "; call atlas_log%info(atlas_log%msg)
!call trans%invtrans(spfields,gpfields_from_sp)
call trans%invtrans(spectral,spfields,nodepoints,gpfields_from_sp)
write(atlas_log%msg,'(A)') " inverse transform finished "; call atlas_log%info(atlas_log%msg)

! copy vor/div into level structure before transform to u/v
vorfield = spectral%create_field("vorticity",atlas_real(JPRB),nlev)
divfield =  spectral%create_field("divergence",atlas_real(JPRB),nlev)
windfield = nodepoints%create_field("wind",atlas_real(JPRB),nlev,[2])
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
write(atlas_log%msg,'(A)') " inverse wind transform starting "; call atlas_log%info(atlas_log%msg)
call trans%invtrans_vordiv2wind(spectral,vorfield,divfield,nodepoints,windfield)
write(atlas_log%msg,'(A)') " inverse wind transform finished "; call atlas_log%info(atlas_log%msg)


! here we need a computation at given locations directly from spectral coefficients of the scalars vor/div/omega/z/lnsp/T

! VOR/DIV/omega
! Z/LNSP/T as scalars (and their derivatives as these would no longer be computed from atlas)

! and derived
! U/V wind (and their derivatives as these would no longer be computed from atlas)

! needs then modification to compute_fields to remove gradient calls ... 

write(atlas_log%msg,'(A)') " compute fields "; call atlas_log%info(atlas_log%msg)
call compute_fields(myproc,nb_locations,locations(1:nb_locations),nlev,&
 &pvah,pvbh,fvm,nodepoints, windfield,gpfields_from_sp, gridpoints, gpfields)
do iloc=1, nb_locations
  if( myproc == locations(iloc)%iproc ) then
    ! netcdf write this location from this processor
    if (.not.larea) then
      write(atlas_log%msg,'(A)') " setting up output fields to netcdf  "; call atlas_log%info(atlas_log%msg)
!      write(atlas_log%msg,'(A,I0)') " loc processor ", locations(iloc)%IPROC; call atlas_log%info(atlas_log%msg)
!      write(atlas_log%msg,'(A,I0)') " loc knode ", locations(iloc)%ILOC; call atlas_log%info(atlas_log%msg)
!      write(atlas_log%msg,'(A,F8.4)') " loc latitude ", locations(iloc)%RLATI; call atlas_log%info(atlas_log%msg)
!      write(atlas_log%msg,'(A,F8.4)') " loc longitude ", locations(iloc)%RLONI; call atlas_log%info(atlas_log%msg)
!      write(atlas_log%msg,'(A,F8.4)') " loc pressure ", locations(iloc)%PP%PLNSP; call atlas_log%info(atlas_log%msg)
      CALL SU_WRT_NC (myproc,PVAH,PVBH,dataid,iloc,locations(iloc)%IFILE_ID,nlev)
      write(atlas_log%msg,'(A,I0,1X,I0)') " writing output fields to netcdf  ", INFO%ISTEP, INFO%IDATE 
      call atlas_log%info(atlas_log%msg)
      CALL WRT1C_NC(locations(iloc),PVAH,PVBH,INFO,locations(iloc)%IFILE_ID,nlev)
    endif
  endif
enddo
write(atlas_log%msg,'(A)') " finished, cleaning up! "; call atlas_log%info(atlas_log%msg)
!call atlas_mpi_barrier(atlas_mpi_comm())
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

call atlas_log%info("getini1c: end")

end subroutine run

! =============================================================================
! MAIN PROGRAM
program getini1c
use atlas_module, only: &
  & atlas_init         ,&
  & atlas_finalize
USE MPL_MODULE, only : mpl_end
implicit none
integer :: return_code
call atlas_init()
call run(return_code)
call mpl_end()
call atlas_finalize()
if( return_code /= 0 ) then 
  STOP 1
endif
end program getini1c
