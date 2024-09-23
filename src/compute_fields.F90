! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

subroutine compute_fields(nproc,myproc,nb_locations,locations,klev,pvah,pvbh,&
& fvm,nodepoints, windfield,gpfields_from_sp,gridpoints, gpfields)

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
implicit none

INTEGER(KIND=JPIM), intent(in) :: nproc
INTEGER(KIND=JPIM), intent(in) :: myproc
INTEGER(KIND=JPIM), intent(in) :: nb_locations
TYPE(TLOCATION), target, intent(inout) :: locations(nb_locations)
INTEGER(KIND=JPIM), intent(in) :: klev
REAL(KIND=JPRB), intent(in) :: pvah(0:klev),pvbh(0:klev)
type(atlas_FieldSet),intent(in) :: gpfields_from_sp, gpfields
type(atlas_Field), intent(inout) :: windfield
type(atlas_fvm_Method), intent(in)  :: fvm
type(atlas_functionspace_StructuredColumns), intent(in) :: gridpoints
type(atlas_functionspace_NodeColumns), intent(in) :: nodepoints

INTEGER(KIND=JPIM) :: jlev, jfld, iparam, inode, ilev, isize, iloc
type(atlas_Nabla) :: nabla
type(atlas_Field) :: grad, gradu, gradv, wind_u, wind_v, gradq, grad_wind
type(atlas_Field) :: field
type(atlas_Metadata) :: metadata
character(len=10) :: fieldname

type(atlas_Field) :: field_nodes, ghostField
INTEGER(KIND=c_int), POINTER :: ghost(:)
type(atlas_mesh_Nodes) :: nodes
INTEGER(KIND=JPIM) :: nb_nodes, jnode
REAL(KIND=c_double),POINTER :: nodedata(:)

REAL(KIND=c_double),POINTER :: values(:), zu(:,:), zv(:,:), &
 & grad_data(:,:), gradu_data(:,:,:), gradv_data(:,:,:) 

REAL(KIND=c_double),POINTER :: grad_wind_data(:,:,:), wind(:,:,:)

!REAL(KIND=JPRB),allocatable :: zu(:,:), zv(:,:)

REAL(KIND=JPRB) :: zslat_p
REAL(KIND=JPRB) :: zdudx(klev), zdudy(klev), zdvdx(klev), zdvdy(klev)
REAL(KIND=JPRB)    :: zdeg2rad,zzaux, zdir, zpi

CHARACTER*127 msg
type(fckit_mpi_comm) :: mpi_comm

TYPE(TPARAM), POINTER :: PX

#include "calcgeost.h"
!-------------------------------------------------------------------------

IF( NPROC > 1 ) mpi_comm = fckit_mpi_comm()

! need minus as advective forcing on rhs
zdir = -1.0_jprb
zpi = 2.0_jprb*asin(1.0_jprb)
zdeg2rad= zpi/180._jprb

do iloc=1, nb_locations
  if( myproc == locations(iloc)%iproc ) then
    PX => locations(iloc)%PP
    call ALLOCATE_COLUMNS(PX,klev)
    PX%PW(:)=0._jprb
    PX%PR(:)=0._jprb
    PX%PRL(:)=0._jprb
    PX%PRM(:)=0._jprb
    PX%PS(:)=0._jprb
    PX%PSL(:)=0._jprb
   PX%PSM(:)=0._jprb
  endif
enddo

nabla = atlas_Nabla(fvm)

!! calculate wind + derivatives
grad_wind = nodepoints%create_field(name="gradwind",kind=atlas_real(JPRB),levels=klev,variables=4) ! 4 vars = 2x2 matrix
call nodepoints%halo_exchange(windfield)
call nabla%gradient(windfield,grad_wind)
!! grad_wind_data == 2(u,v), 2(dx,dy), nodes, levels, nodes
call grad_wind%data(grad_wind_data)
!! wind == 2(u,v), levels, nodes
call windfield%data(wind)

!!$gradu = gridpoints%create_field("gradu",atlas_real(JPRB),klev,[2])
!!$gradv = gridpoints%create_field("gradv",atlas_real(JPRB),klev,[2])
!!$
!!$wind_u = gridpoints%create_field("u",atlas_real(JPRB),klev)
!!$
!!$zu => wind(1,:,:)
!!$call wind_u%data(zu)
!!$call gridpoints%halo_exchange(wind_u)
!!$call nabla%gradient(wind_u,gradu)
!!$wind_v = gridpoints%create_field("v",atlas_real(JPRB),klev)
!!$zv => wind(2,:,:)
!!$call wind_v%data(zv)
!!$call gridpoints%halo_exchange(wind_v)
!!$call nabla%gradient(wind_v,gradv)
!!$
!!$call gradu%data(gradu_data)
!!$call gradv%data(gradv_data)

! end wind

grad = nodepoints%create_field(name="grad", kind=atlas_real(JPRB),variables=2)
! scalar gp from sp
isize = gpfields_from_sp%size()
do jfld=1,isize
  field = gpfields_from_sp%field(JFLD)
  metadata = field%metadata()
  call field%data(values)
  call metadata%get("paramId",iparam)
  call metadata%get("level",ilev)
  
  ! derivatives
  if ( iparam == 152 .or. iparam == 129 .or. iparam == 130 ) then
    call nodepoints%halo_exchange(field)
    call nabla%gradient(field,grad)
    call grad%data(grad_data)
  endif
  
  ! fill values
  do iloc=1, nb_locations
    if( myproc == locations(iloc)%iproc ) then
      inode = locations(iloc)%ILOC
      PX => locations(iloc)%PP
      SELECT CASE (iparam)
      CASE (152)
        PX%PLNSP = values(inode) ! store into yomvar
        PX%PSP = exp(PX%PLNSP)
        PX%PSPL = PX%PSP * grad_data(1,inode)
        PX%PSPM = PX%PSP * grad_data(2,inode)
!        write(*,*) 'check sfc press ',  iloc, inode, values(inode),    PX%PSP,  PX%PSPL ,  PX%PSPM 
       CASE (129)
        PX%PZ = values(inode) ! store into yomvar
        PX%PZL =  grad_data(1,inode)
        PX%PZM = grad_data(2,inode)
      CASE (130)
        PX%PT(ilev) = values(inode) ! store into yomvar
        PX%PTL(ilev)  = grad_data(1,inode)
        PX%PTM(ilev) =grad_data(2,inode)
        !  CASE (133)
        !    PQ(ilev) = values(inode) ! store into yomvar
        !    zq_p(ilev)   = PQ(ilev)
        !    zql_p(ilev)  = grad_data(1,inode)
        !    zqm_p(ilev) =grad_data(2,inode)
      CASE (135)
        PX%PW(ilev) = values(inode) ! store into yomvar
      CASE (155)
        PX%PDIV(ilev) = values(inode) ! store locally
      CASE (138)
        PX%PROT(ilev) = values(inode) ! store locally
      CASE DEFAULT
        write(msg,'(A, I0, I0)') " GP_FROM_SP WARNING: UNKNOWN FIELD (PARAMETER , LEVEL) ", iparam, ilev ; call log%info(msg)
      END SELECT
    endif
  enddo
enddo

! loop over gp fields 

isize = gpfields%size()
do jfld=1,isize

  field = gpfields%field(JFLD)
  metadata = field%metadata()
  call field%data(values)
  call metadata%get('paramId',iparam)
  call metadata%get('level',ilev)

  ! derivatives
  if ( iparam == 133 .or. iparam ==  75 .or. iparam == 76 .or. iparam == 246 .or. iparam == 247 .or. iparam == 248 ) then
    write(fieldname, '(I0)') jfld+iparam
    field_nodes = nodepoints%create_field(name=fieldname,kind=atlas_real(JPRB))
    call field_nodes%data(nodedata)
    ! begin pack
    nodes = nodepoints%nodes()
    ghostField = nodes%ghost()
    call ghostField%data(ghost)
    nb_nodes =  nodes%size()
    inode=0
    do jnode=1, nb_nodes
      if ( ghost(jnode) /= 1 ) then
        inode=inode+1
        nodedata(jnode) = values(inode)
      endif
    enddo
    ! end pack
    call nodepoints%halo_exchange(field_nodes)
    call nabla%gradient(field_nodes,grad)
    call grad%data(grad_data)
  endif

  ! fill values
  do iloc=1, nb_locations
    if( myproc == locations(iloc)%iproc ) then
      inode = locations(iloc)%ILOC
      PX => locations(iloc)%PP
      SELECT CASE (iparam)
      CASE (133)
        PX%PQ(ilev) = values(inode) ! store into yomvar
        PX%PQL(ilev)  = grad_data(1,inode)
        PX%PQM(ilev) = grad_data(2,inode)
      CASE (75)
        PX%PR(ilev) = values(inode) ! store into yomvar
        PX%PRL(ilev)  = grad_data(1,inode)
        PX%PRM(ilev) = grad_data(2,inode)
      CASE (76)
        PX%PS(ilev) = values(inode) ! store into yomvar
        PX%PSL(ilev)  = grad_data(1,inode)
        PX%PSM(ilev) = grad_data(2,inode)
      CASE (246)
        PX%PL(ilev) = values(inode) ! store into yomvar
        PX%PLL(ilev)  = grad_data(1,inode)
        PX%PLM(ilev) = grad_data(2,inode)
      CASE (247)
        PX%PI(ilev) = values(inode) ! store into yomvar
        PX%PIL(ilev)  = grad_data(1,inode)
        PX%PIM(ilev) = grad_data(2,inode)
      CASE (248)
        PX%PA(ilev) = values(inode) ! store into yomvar
        PX%PCAL(ilev)  = grad_data(1,inode)
        PX%PCAM(ilev) = grad_data(2,inode)
      CASE DEFAULT
!        write(msg,'(A, I0,' ', I0)') " GPFIELDS WARNING, NOT USED (PARAMETER , LEVEL)", iparam,ilev ; call log%info(msg)
      END SELECT
    endif
  enddo
enddo

if( NPROC > 1 ) call mpi_comm%barrier()

do iloc=1, nb_locations
  if( myproc == locations(iloc)%iproc ) then
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
    
!    zdudx(:) = gradu_data(1,:,inode)
!    zdudy(:) = gradu_data(2,:,inode)
!    zdvdx(:) = gradv_data(1,:,inode)
!    zdvdy(:) = gradv_data(2,:,inode)
    
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
      PX%PUADV(jlev) = zdir * ( PX%PU(jlev) * zdudx(jlev) + PX%PV(jlev) * zdudy(jlev) )
      PX%PVADV(jlev) =  zdir * ( PX%PU(jlev) * zdvdx(jlev) + PX%PV(jlev) * zdvdy(jlev) )
      PX%PTADV(jlev) =  zdir * ( PX%PU(jlev) * PX%PTL(jlev) + PX%PV(jlev) * PX%PTM(jlev) )
      PX%PQADV(jlev) =  zdir * ( PX%PU(jlev) * PX%PQL(jlev) + PX%PV(jlev) * PX%PQM(jlev) )
      PX%PLADV(jlev) =  zdir * ( PX%PU(jlev) * PX%PLL(jlev) + PX%PV(jlev) * PX%PLM(jlev) )
      PX%PIADV(jlev) =  zdir * ( PX%PU(jlev) * PX%PIL(jlev) + PX%PV(jlev) * PX%PIM(jlev) )
      PX%PAADV(jlev) =  zdir * ( PX%PU(jlev) * PX%PCAL(jlev) + PX%PV(jlev) * PX%PCAM(jlev) )
      PX%PSADV(jlev) =  zdir * ( PX%PU(jlev) * PX%PSL(jlev) + PX%PV(jlev) * PX%PSM(jlev) )
      PX%PRADV(jlev) =  zdir * ( PX%PU(jlev) * PX%PRL(jlev) + PX%PV(jlev) * PX%PRM(jlev) )
    enddo
    
    ! calculate ug,vg geostrophic winds ----------------------------------------------

    zslat_p =sin(zdeg2rad*locations(iloc)%RLATI)
    call calcgeost( &
     &   klev, pvah, pvbh, zslat_p  &
     & , PX%PT    , PX%PQ  , PX%PSP  &
     & , PX%PTL   , PX%PQL , PX%PSPL  ,  PX%PZL &
     & , PX%PTM , PX%PQM , PX%PSPM  ,  PX%PZM &
     & , PX%PUG   , PX%PVG )
    
  endif
enddo

call nabla%final()
nullify(PX)

end subroutine compute_fields
