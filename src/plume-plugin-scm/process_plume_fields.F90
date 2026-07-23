! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

subroutine process_plume_fields(nproc, &
                                myproc, &
                                kstep, &
                                nb_locations, &
                                locations, &
                                klev, &
                                pvah, &
                                pvbh, &
                                fvm, &
                                nodepoints, &
                                windfield, &
                                gpfields_from_sp, &
                                gridpoints, &
                                gpfields, &
                                extract_mgr)

! * calculates horizontal gradients of z, T and q
!    are used in the pressure gradient force to get the geostrophic wind
! * calculates horizontal gradient of p for d(eta)/dt*dp/d(eta) calculation
! * calculates qadv,tadv,uadv,vadv (subroutine calcadv.F90)
! * calculates Ug,Vg 
! * allocates and stores q, a, l, i, u, v,  t, w, z, lnsp, qadv,tadv,uadv,vadv, Ug,Vg at point locations (yomvar)

!    klev         I   Number of vertical levels
!    pvah         I   Vertical coordinate table
!    pvbh         I   Vertical coordinate table

!-------------------------------------------------------------------------
use, intrinsic :: iso_C_binding
use fckit_mpi_module, only : fckit_mpi_comm
use fckit_log_module, only : log
use atlas_module

use yomvar
use plugin_utils_mod, only : param_name2id
use extraction_manager_mod, only : extraction_manager

#ifdef WITH_SCM_PLUME_PLUGIN_PROFILER
  use plugin_profiler_mod
#endif

implicit none

INTEGER(KIND=JPIM), intent(in) :: nproc
INTEGER(KIND=JPIM), intent(in) :: myproc
INTEGER(KIND=JPIM), intent(in) :: kstep
INTEGER(KIND=JPIM), intent(in) :: nb_locations
TYPE(TLOCATION), target, intent(inout) :: locations(nb_locations)
INTEGER(KIND=JPIM), intent(in) :: klev
REAL(KIND=JPRB), intent(in) :: pvah(0:klev)
REAL(KIND=JPRB), intent(in) :: pvbh(0:klev)
type(atlas_fvm_Method), intent(in)  :: fvm
type(atlas_functionspace_NodeColumns), intent(in) :: nodepoints
type(atlas_Field), intent(inout) :: windfield
type(atlas_FieldSet),intent(in) :: gpfields_from_sp
type(atlas_functionspace_StructuredColumns), intent(in) :: gridpoints
type(atlas_FieldSet),intent(in) :: gpfields
type(extraction_manager), intent(in) :: extract_mgr

INTEGER(KIND=JPIM) :: jlev
INTEGER(KIND=JPIM) :: n_field_levels
INTEGER(KIND=JPIM) :: jfld
INTEGER(KIND=JPIM) :: iparam
INTEGER(KIND=JPIM) :: inode
INTEGER(KIND=JPIM) :: ilev
INTEGER(KIND=JPIM) :: isize
INTEGER(KIND=JPIM) :: iloc

type(atlas_Nabla) :: nabla
type(atlas_Field) :: grad
type(atlas_Field) :: grad_one_lev ! special case (1 lvl only)
type(atlas_Field) :: grad_wind
type(atlas_Field) :: field
type(atlas_Metadata) :: metadata

character(len=10) :: fieldname
INTEGER(KIND=JPIM) :: nb_nodes
INTEGER(KIND=JPIM) :: jnode


#ifdef WITH_SCM_SINGLE_PRECISION
REAL(KIND=c_float), POINTER :: nodedata(:,:)
REAL(KIND=c_float), POINTER :: values(:)
REAL(KIND=c_float), POINTER :: values_plume_fields(:,:)
REAL(KIND=c_float), POINTER :: zu(:,:)
REAL(KIND=c_float), POINTER :: zv(:,:)
REAL(KIND=c_float), POINTER :: grad_data(:,:,:)
REAL(KIND=c_float), POINTER :: gradu_data(:,:,:)
REAL(KIND=c_float), POINTER :: gradv_data(:,:,:) 
REAL(KIND=c_float), POINTER :: grad_wind_data(:,:,:)
REAL(KIND=c_float), POINTER :: wind(:,:,:)
#else
REAL(KIND=c_double), POINTER :: nodedata(:,:)
REAL(KIND=c_double), POINTER :: values(:)
REAL(KIND=c_double), POINTER :: values_plume_fields(:,:)
REAL(KIND=c_double), POINTER :: zu(:,:)
REAL(KIND=c_double), POINTER :: zv(:,:)
REAL(KIND=c_double), POINTER :: grad_data(:,:,:)
REAL(KIND=c_double), POINTER :: gradu_data(:,:,:)
REAL(KIND=c_double), POINTER :: gradv_data(:,:,:) 
REAL(KIND=c_double), POINTER :: grad_wind_data(:,:,:)
REAL(KIND=c_double), POINTER :: wind(:,:,:)
#endif

REAL(KIND=JPRB) :: zslat_p

REAL(KIND=JPRB) :: zdudx(klev)
REAL(KIND=JPRB) :: zdudy(klev)
REAL(KIND=JPRB) :: zdvdx(klev)
REAL(KIND=JPRB) :: zdvdy(klev)

REAL(KIND=JPRB)    :: zdeg2rad
REAL(KIND=JPRB)    :: zzaux
REAL(KIND=JPRB)    :: zdir
REAL(KIND=JPRB)    :: zpi

character(512) :: msg
type(fckit_mpi_comm) :: mpi_comm

TYPE(TPARAM), POINTER :: PX


integer :: i,j

#include "calcgeost.h"
#include "profiler_macros.h"
!-------------------------------------------------------------------------

IF( NPROC > 1 ) mpi_comm = fckit_mpi_comm()

! need minus as advective forcing on rhs
zdir = -1.0_jprb
zpi = 2.0_jprb*asin(1.0_jprb)
zdeg2rad= zpi/180._jprb

!! calculate wind + derivatives
START_PLUGIN_TIMER("scm_run.process_plume_fields.gradient_wind")
nabla = atlas_Nabla(fvm)
grad_wind = nodepoints%create_field(name="gradwind",kind=atlas_real(JPRB),levels=klev,variables=4) ! 4 vars = 2x2 matrix
call nodepoints%halo_exchange(windfield)
call nabla%gradient(windfield,grad_wind)
call grad_wind%data(grad_wind_data)
call windfield%data(wind)

grad = nodepoints%create_field(name="grad", kind=atlas_real(JPRB),levels=klev, variables=2)
grad_one_lev = nodepoints%create_field(name="grad_one_lev", kind=atlas_real(JPRB),levels=1, variables=2)
STOP_PLUGIN_TIMER("scm_run.process_plume_fields.gradient_wind")

! scalar gp from sp
START_PLUGIN_TIMER("scm_run.process_plume_fields.gradients_sp")
isize = gpfields_from_sp%size()
do jfld=1,isize
  field = gpfields_from_sp%field(jfld)
  metadata = field%metadata()
  
  ! call field%data(values)
  call field%data(values_plume_fields)

  ! loop over field levels
  n_field_levels = field%levels()
  iparam = param_name2id(field%name())

  ! derivatives
  if ( iparam == 130 ) then
    call nodepoints%halo_exchange(field)
    call nabla%gradient(field,grad)
    call grad%data(grad_data)
  endif

  ! special case for sp and geopotential
  if ( iparam == 152 .or. iparam == 129) then

    call nodepoints%halo_exchange(field)

    if ( field%levels() == 1 ) then
      call nabla%gradient(field,grad_one_lev)
      call grad_one_lev%data(grad_data)
    else
      write(msg,'(A,A,A)') " ERROR: field ", field%name(), " has more than 1 level, but should have only 1 level!" ; call log%error(msg)
    endif
    ! write(*,*) "--- size(grad_data,1): ", size(grad_data,1)
    ! write(*,*) "--- size(grad_data,2): ", size(grad_data,2)
    ! write(*,*) "--- size(grad_data,3): ", size(grad_data,3)
  endif

  ! fill values
  do iloc=1, nb_locations
    if( myproc == locations(iloc)%iproc .and. extract_mgr%should_extract(iloc, kstep) ) then
      do ilev=1,n_field_levels
        inode = locations(iloc)%ILOC
        PX => locations(iloc)%PP
        SELECT CASE (iparam)
        CASE (152)
          PX%PLNSP = values_plume_fields(1,inode) ! pressure only at level 1
          PX%PSP = exp(PX%PLNSP)
          PX%PSPL = PX%PSP * grad_data(1,1,inode)
          PX%PSPM = PX%PSP * grad_data(2,1,inode)
        CASE (129)
          PX%PZ = values_plume_fields(1,inode) ! Z only at level 1
          PX%PZL = grad_data(1,1,inode)
          PX%PZM = grad_data(2,1,inode)
        CASE (130)
          PX%PT(ilev) = values_plume_fields(ilev,inode) ! store into yomvar
          PX%PTL(ilev) = grad_data(1,ilev,inode)
          PX%PTM(ilev) = grad_data(2,ilev,inode)
        CASE (135)
          PX%PW(ilev) = values_plume_fields(ilev,inode) ! store into yomvar
        CASE (155)
          PX%PDIV(ilev) = values_plume_fields(ilev,inode) ! store locally
        CASE (138)
          PX%PROT(ilev) = values_plume_fields(ilev,inode) ! store locally
        CASE DEFAULT
          write(msg,'(A,I0,A,I0)') " GP_FROM_SP WARNING: UNKNOWN FIELD (PARAMETER , LEVEL) ", iparam, " ", ilev ; call log%info(msg)
        END SELECT
      enddo ! ilev
    endif
  enddo ! iloc

enddo ! ifield
STOP_PLUGIN_TIMER("scm_run.process_plume_fields.gradients_sp")


! loop over gp fields
START_PLUGIN_TIMER("scm_run.process_plume_fields.gradients_gp")
isize = gpfields%size()
do jfld=1,isize

  field = gpfields%field(JFLD)

  metadata = field%metadata()
  call field%data(values_plume_fields)

  ! loop over field levels
  n_field_levels = field%levels()
  iparam = param_name2id(field%name())

  ! derivatives
  if ( iparam == 133 .or. iparam ==  75 .or. iparam == 76 .or. iparam == 246 .or. iparam == 247 .or. iparam == 248 ) then
    call nodepoints%halo_exchange(field)
    call nabla%gradient(field, grad)    
    call grad%data(grad_data)
  endif

  ! fill values
  do iloc=1, nb_locations
    if( myproc == locations(iloc)%iproc .and. extract_mgr%should_extract(iloc, kstep) ) then
      do ilev=1,n_field_levels
        inode = locations(iloc)%ILOC
        PX => locations(iloc)%PP
        SELECT CASE (iparam)
        CASE (133)
          PX%PQ(ilev)  = values_plume_fields(ilev,inode) ! store into yomvar
          PX%PQL(ilev) = grad_data(1,ilev,inode)
          PX%PQM(ilev) = grad_data(2,ilev,inode)
        CASE (75)
          PX%PR(ilev)  = values_plume_fields(ilev,inode) ! store into yomvar
          PX%PRL(ilev) = grad_data(1,ilev,inode)
          PX%PRM(ilev) = grad_data(2,ilev,inode)
        CASE (76)
          PX%PS(ilev)  = values_plume_fields(ilev,inode) ! store into yomvar
          PX%PSL(ilev) = grad_data(1,ilev,inode)
          PX%PSM(ilev) = grad_data(2,ilev,inode)
        CASE (246)
          PX%PL(ilev)  = values_plume_fields(ilev,inode) ! store into yomvar
          PX%PLL(ilev) = grad_data(1,ilev,inode)
          PX%PLM(ilev) = grad_data(2,ilev,inode)
        CASE (247)
          PX%PI(ilev)  = values_plume_fields(ilev,inode) ! store into yomvar
          PX%PIL(ilev) = grad_data(1,ilev,inode)
          PX%PIM(ilev) = grad_data(2,ilev,inode)
        CASE (248)
          PX%PA(ilev)   = values_plume_fields(ilev,inode) ! store into yomvar
          PX%PCAL(ilev) = grad_data(1,ilev,inode)
          PX%PCAM(ilev) = grad_data(2,ilev,inode)
        CASE DEFAULT
          write(msg,'(A,I0,A,I0)') " GPFIELDS WARNING, NOT USED (PARAMETER , LEVEL)", iparam, " ", ilev ; call log%info(msg)
        END SELECT
      enddo ! ilev
    endif
  enddo ! iloc
enddo ! field
STOP_PLUGIN_TIMER("scm_run.process_plume_fields.gradients_gp")

START_PLUGIN_TIMER("scm_run.process_plume_fields.mpi_barrier")
if( NPROC > 1 ) call mpi_comm%barrier()
STOP_PLUGIN_TIMER("scm_run.process_plume_fields.mpi_barrier")

START_PLUGIN_TIMER("scm_run.process_plume_fields.fill_locations")
do iloc=1, nb_locations
  if( myproc == locations(iloc)%iproc .and. extract_mgr%should_extract(iloc, kstep) ) then
    inode = locations(iloc)%ILOC
    PX => locations(iloc)%PP

    PX%PU(:) = wind(1,:,inode)
    PX%PV(:) = wind(2,:,inode)

#define INDEX_DUDX 1
#define INDEX_DUDY 2
#define INDEX_DVDX 3
#define INDEX_DVDY 4

    zdudx(:) = grad_wind_data( INDEX_DUDX,  :, inode )
    zdudy(:) = grad_wind_data( INDEX_DUDY,  :, inode )
    zdvdx(:) = grad_wind_data( INDEX_DVDX,  :, inode )
    zdvdy(:) = grad_wind_data( INDEX_DVDY,  :, inode )
    
    ! Compute petadotdpdeta -----------------------------------------------------
    PX%petadotdpdeta(0)=0._jprb
    do jlev=1,klev
      zzaux=pvah(jlev)-pvah(jlev-1)+PX%PSP*(pvbh(jlev)-pvbh(jlev-1))
      PX%petadotdpdeta(jlev)=PX%petadotdpdeta(jlev-1)-PX%PDIV(jlev)*zzaux
      PX%petadotdpdeta(jlev)=PX%petadotdpdeta(jlev)- &
       &       (PX%PU(jlev)*PX%PSPL+PX%PV(jlev)*PX%PSPM)*(pvbh(jlev)-pvbh(jlev-1))
    enddo

    ! fix by adding partial ps / partial t
    do jlev=1,klev-1
      PX%petadotdpdeta(jlev) = -pvbh(jlev) * PX%petadotdpdeta(klev) + PX%petadotdpdeta(jlev)
    enddo

    ! compute advection terms -------------------------------------------------------
    ! lower resol advection terms ?!    

    do jlev=1,klev
      PX%PUADV(jlev) = zdir * ( PX%PU(jlev) * zdudx(jlev)   + PX%PV(jlev) * zdudy(jlev)   )
      PX%PVADV(jlev) = zdir * ( PX%PU(jlev) * zdvdx(jlev)   + PX%PV(jlev) * zdvdy(jlev)   )
      PX%PTADV(jlev) = zdir * ( PX%PU(jlev) * PX%PTL(jlev)  + PX%PV(jlev) * PX%PTM(jlev)  )
      PX%PQADV(jlev) = zdir * ( PX%PU(jlev) * PX%PQL(jlev)  + PX%PV(jlev) * PX%PQM(jlev)  )
      PX%PLADV(jlev) = zdir * ( PX%PU(jlev) * PX%PLL(jlev)  + PX%PV(jlev) * PX%PLM(jlev)  )
      PX%PIADV(jlev) = zdir * ( PX%PU(jlev) * PX%PIL(jlev)  + PX%PV(jlev) * PX%PIM(jlev)  )
      PX%PAADV(jlev) = zdir * ( PX%PU(jlev) * PX%PCAL(jlev) + PX%PV(jlev) * PX%PCAM(jlev) )
      PX%PSADV(jlev) = zdir * ( PX%PU(jlev) * PX%PSL(jlev)  + PX%PV(jlev) * PX%PSM(jlev)  )
      PX%PRADV(jlev) = zdir * ( PX%PU(jlev) * PX%PRL(jlev)  + PX%PV(jlev) * PX%PRM(jlev)  )
    enddo
    
    ! calculate ug,vg geostrophic winds ----------------------------------------------

    zslat_p =sin(zdeg2rad*locations(iloc)%RLATI)
    call calcgeost(klev, &
                   pvah, &
                   pvbh, &
                   zslat_p, &
                   PX%PT, &
                   PX%PQ, &
                   PX%PSP, &
                   PX%PTL, &
                   PX%PQL, &
                   PX%PSPL, &
                   PX%PZL, &
                   PX%PTM, &
                   PX%PQM, &
                   PX%PSPM, &
                   PX%PZM, &
                   PX%PUG, &
                   PX%PVG )
    
  endif
enddo
STOP_PLUGIN_TIMER("scm_run.process_plume_fields.fill_locations")

START_PLUGIN_TIMER("scm_run.process_plume_fields.finalise_fields")
call nabla%final()
call grad%final()
call grad_one_lev%final()
call grad_wind%final()
call field%final()
call metadata%final()

nullify(PX)
STOP_PLUGIN_TIMER("scm_run.process_plume_fields.finalise_fields")

end subroutine process_plume_fields
