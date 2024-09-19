SUBROUTINE WRT1C_NC (ILOCATION,PVAH, PVBH, INFO, incid,klev)
!     ------------------------------------------------------------------
!     write netCDF output
!     ------------------------------------------------------------------

!   PVAH, PVBH A,Bs for the vertical levels
!   INFO contains
!       IDATE    I  Initial date (YYYYMMDD)
!       ITIME    I  Initial time (HH)
!       ISTEP    I  FC step (HH)
!   ILOCATION contains
!       RLAT, RLON of  point  
!   incid    I  Logical unit of netcdf files
!   klev   I number of levels

use yomvar 

implicit none

REAL(KIND=JPRB),    intent(in) :: pvah(0:klev), pvbh(0:klev)
TYPE(TINFO), intent(in) :: INFO
TYPE(TLOCATION), intent(in) :: ILOCATION
INTEGER(KIND=JPIM), intent(in) :: incid, klev

!...ECMWF IFS 21r4 physical constants (SI units)
REAL(KIND=JPRB), parameter :: rd   =  287.060    ! gas constant for dry air   
REAL(KIND=JPRB), parameter :: rg   =    9.80665  ! gravitational acceleration
REAL(KIND=JPRB), parameter :: rcpd = 1004.703    ! specific heat at constant pressure
REAL(KIND=JPRB), parameter :: RV   =  461.52     ! Rgas,vapor
REAL(KIND=JPRB), parameter :: RLVTT=    2.5008e6 ! L,cond
REAL(KIND=JPRB), parameter :: RETV = RV/RD-1     ! conversion factor for Rgas,moist (.608)
REAL(KIND=JPRB), parameter :: eps  = 18.0153 / 28.9644 ! mass,wv / mass,dry
REAL(KIND=JPRB), parameter :: rtt     = 273.16   !freezing temperature
REAL(KIND=JPRB), parameter :: rtwat   = rtt      !       -  "  -
REAL(KIND=JPRB), parameter :: rtice   = rtt - 23.0 !complete ice temperature
REAL(KIND=JPRB), parameter :: r2es = 611.21 * eps!vapor pressure,liq. at 0K * eps
REAL(KIND=JPRB), parameter :: r3les= 17.502      !linear physics coefficients
REAL(KIND=JPRB), parameter :: r3ies= 22.587      !       -  "  -
REAL(KIND=JPRB), parameter :: r4les= 32.19       !       -  "  -
REAL(KIND=JPRB), parameter :: r4ies= -0.7        !       -  "  -

!...vertical coodinate parameters for ECMWF model
REAL(KIND=JPRB) :: zpresh(0:klev), zpresf(klev)     ! half and full level pressures
REAL(KIND=JPRB) :: zh(0:klev),    zf(klev)        ! half and full level heights

REAL(KIND=JPRB)    :: zqs(klev), zrh(klev), ztheta(klev), ztheta_e(klev), &
            &zdry_st(klev), zmoist_st(klev)

INTEGER(KIND=JPIM) :: j, jk, &
            &idimid, idimlen, ivarid, istatus, &
            &istart1, istart2(2), &
            &icount1, icount2(2), icount3(2), icount4(2)

#include "netcdf.inc"

!#include "fcttre.h" ...1 functions from fcttre explicitly included:
!     Pressure of water vapour at saturation
!        INPUT : PTARE = TEMPERATURE
REAL(KIND=JPRB) :: PTARE
REAL(KIND=JPRB) :: FOEALFA
FOEALFA (PTARE) = MIN(1._jprb,((MAX(RTICE,MIN(RTWAT,PTARE))-RTICE)&
 &/(RTWAT-RTICE))**2) 
REAL(KIND=JPRB) :: FOEEWM,FOEDEM,FOELDCPM,FOELHM
FOEEWM ( PTARE ) = R2ES *&
     &(FOEALFA(PTARE) *EXP(R3LES*(PTARE-RTT)/(PTARE-R4LES))+&
  &(1._jprb-FOEALFA(PTARE))*EXP(R3IES*(PTARE-RTT)/(PTARE-R4IES)))

!     ------------------------------------------------------------------


!        1.   set-up

istatus = NF_INQ_DIMID   (incid, 'time', idimid)
call handle_err_nc(istatus)
istatus = NF_INQ_DIMLEN  (incid, idimid, idimlen)    !get current time index
call handle_err_nc(istatus)

istart1    = idimlen+1  ! 1-d variables: starting index
icount1    = 1          !      -"-       written indices
istart2(1) = 1          ! 2-d variables - dim 1: starting index
icount2(1) = klev      !      -"-               written indices
icount3(1) = klev+1    !      -"-               written indices
icount4(1) = ncss       !      -"-               written indices
istart2(2) = idimlen+1  ! 2-d variables - dim 2: starting index
icount2(2) = 1          !      -"-               written indices
icount3(2) = 1          !      -"-               written indices
icount4(2) = 1          !      -"-               written indices

!        2.   diagnostic variable definitions

!...pressure levels (full levels = prognostic variables; see gppreh/gppref.F90)

! write(*,*) 'pressure ', ILOCATION%PP%plnsp, exp(ILOCATION%PP%plnsp)

zpresh(klev) = exp(ILOCATION%PP%plnsp)
do j = 0, klev-1
  zpresh(J) = PVAH(J) + PVBH(J) * zpresh(klev)  !half levels
enddo
do J = 1,KLEV
  zpresf(J) = ( zpresh(J-1) + zpresh(J) ) * 0.5   !full levels
enddo

!...height levels (full levels = prognostic variables; see gpgeo.F90)

zh(klev) = 0.0
do j = klev,2,-1
  zh(j-1) = zh(j) + rd/rg * ILOCATION%PP%pt(j) * log( zpresh(j) / zpresh(j-1) )
enddo                 !...attention: zh(0) = inf because presh(0) = 0
zh(0) = zh(1)+10000.0 !...large number representing infinity (savely)
do j = klev,1,-1
  zf(j)   = zh(j) + rd/rg * ILOCATION%PP%pt(j) * log( zpresh(j) / zpresf(j)   )
enddo

!...relative humidity

DO JK=1,KLEV
  ZQS(JK)=FOEEWM(ILOCATION%PP%PT(JK))/ZPRESF(JK)
  ZQS(JK)=MIN(0.5_JPRB,ZQS(JK))
  ZQS(JK)=ZQS(JK)/(1.0-RETV*ZQS(JK))
  ZRH(JK)=ILOCATION%PP%PQ(JK)/ZQS(JK)
  ZRH(JK)=MAX(0.0_JPRB,ZRH(JK))
!  write(*,*) 'rel hum: ', JK, ZRH(JK), PQ(JK), PT(JK), ZQS(JK)
ENDDO

!...thermodynamic conserved variables.
!   caution:  cpd_moist = cp_dry, R=RD

! potential temperature
ztheta    = ILOCATION%PP%pt(1:klev) * ( 1.0e5 / zpresf(1:klev) ) ** ( rd / rcpd )

! equivalent potential temperature (Emanuel, 1994, p120)
ztheta_e  = ILOCATION%PP%pt(1:klev) * ( 1.0e5 / zpresf(1:klev) ) ** ( rd  / rcpd )  &  
 *  zrh(1:klev) ** ( - ILOCATION%PP%pq(1:klev) * rv / rcpd )                        &
 *  exp( rlvtt * ILOCATION%PP%pq(1:klev) / ILOCATION%PP%pt(1:klev) / rcpd )

! dry static energy
zdry_st   = zpresf(1:klev) + ILOCATION%PP%pt(1:klev) * rcpd

! moist static energy
zmoist_st = zdry_st + rlvtt * ILOCATION%PP%pq(1:klev)

!        3.   write time and location

!call ncdf_varwrite1c (incid, 1, istart1,icount1, 'time', INFO%ITIME)
! this is a provided counter in hours or the step in hours from the grib-file 
if ( INFO%NSTEP /= 0 ) then
  call ncdf_varwrite1c (incid, 1, istart1,icount1, 'time', REAL(INFO%NSTEP*3600,JPRB))
else
  call ncdf_varwrite1c (incid, 1, istart1,icount1, 'time', REAL(INFO%ISTEP*3600,JPRB))
endif

istatus = NF_INQ_VARID   (incid, 'date', ivarid)
call handle_err_nc(istatus)
istatus = NF_PUT_VAR1_INT(incid,ivarid,idimlen+1, INFO%IDATE)
call handle_err_nc(istatus)

istatus = NF_INQ_VARID   (incid, 'hour', ivarid)
call handle_err_nc(istatus)
istatus = NF_PUT_VAR1_INT(incid,ivarid,idimlen+1, INFO%ITIME*3600)
call handle_err_nc(istatus)

! this parameter is for IFS to tell the starting hour in seconds
istatus = NF_INQ_VARID   (incid, 'second', ivarid)
call handle_err_nc(istatus)
istatus = NF_PUT_VAR1_INT(incid,ivarid,idimlen+1, INFO%ITIME*3600)
call handle_err_nc(istatus)

!istatus = NF_INQ_VARID   (incid, 'timestp', ivarid)
!call handle_err_nc(istatus)
!istatus = NF_PUT_VAR1_INT(incid,ivarid,idimlen+1, nstep)     
!call handle_err_nc(istatus)

call ncdf_varwrite1c (incid, 1, istart1,icount1, 'lat', ILOCATION%RLATI)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'lon',  ILOCATION%RLONI)


!        4.   write scalar variables.

call ncdf_varwrite1c (incid, 1, istart1,icount1, 'ps',             exp(ILOCATION%PP%plnsp))
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'lsm',            ILOCATION%PP%plsm)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'mom_rough',      ILOCATION%PP%psr)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'heat_rough',     exp(ILOCATION%PP%plsrh))
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'albedo',         ILOCATION%PP%pal)
!call ncdf_varwrite1c (incid, 1, istart1,icount1, 'emiss',          plwe)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'orog',           ILOCATION%PP%pz/rg)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'sdfor',          ILOCATION%PP%psdfor)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'sdor',           ILOCATION%PP%psdor)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'isor',           ILOCATION%PP%pisor)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'anor',           ILOCATION%PP%panor)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'slor',           ILOCATION%PP%pslor)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'snow',           ILOCATION%PP%psd)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 't_skin',         ILOCATION%PP%pskt)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'q_skin',         ILOCATION%PP%psrc)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'sea_ice_frct',   ILOCATION%PP%pci)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'open_sst',       ILOCATION%PP%psst)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 't_snow',         ILOCATION%PP%ptsn)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'albedo_snow',    ILOCATION%PP%pasn)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'density_snow',   ILOCATION%PP%prsn)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'low_veg_cover',  ILOCATION%PP%pcvl)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'high_veg_cover', ILOCATION%PP%pcvh)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'low_veg_type',   ILOCATION%PP%ptvl)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'high_veg_type', ILOCATION%PP%ptvh)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'sfc_sens_flx',   ILOCATION%PP%psshf)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'sfc_lat_flx',    ILOCATION%PP%pslhf)
  
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'aluvp',          ILOCATION%PP%paluvp)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'aluvd',          ILOCATION%PP%paluvd)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'alnip',          ILOCATION%PP%palnip)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'alnid',          ILOCATION%PP%palnid)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'soty',           ILOCATION%PP%psoty)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'lail',           ILOCATION%PP%plailc)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'laih',           ILOCATION%PP%plaihc)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'cl',             ILOCATION%PP%plakefr)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'dl',             ILOCATION%PP%plakedl)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'mlt',             ILOCATION%PP%plakemlt)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'mld',             ILOCATION%PP%plakemld)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'blt',             ILOCATION%PP%plakeblt)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'tlt',             ILOCATION%PP%plaketlt)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'shf',             ILOCATION%PP%plakeshf)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'ict',             ILOCATION%PP%plakeict)
call ncdf_varwrite1c (incid, 1, istart1,icount1, 'icd',             ILOCATION%PP%plakeicd)

!        5.   write column variables.

call ncdf_varwrite1c (incid, klev,   istart2,icount2, 'pressure_f', zpresf)
call ncdf_varwrite1c (incid, klev+1, istart2,icount3, 'pressure_h', zpresh)
call ncdf_varwrite1c (incid, klev,   istart2,icount2, 'height_f', zf)
call ncdf_varwrite1c (incid, klev+1, istart2,icount3, 'height_h', zh)
call ncdf_varwrite1c (incid, klev,   istart2,icount2, 't',        ILOCATION%PP%pt(1))
call ncdf_varwrite1c (incid, klev,   istart2,icount2, 'u',        ILOCATION%PP%pu(1))
call ncdf_varwrite1c (incid, klev,   istart2,icount2, 'v',        ILOCATION%PP%pv(1))
call ncdf_varwrite1c (incid, klev,   istart2,icount2, 'q',        ILOCATION%PP%pq(1))
call ncdf_varwrite1c (incid, klev,   istart2,icount2, 'ql',       ILOCATION%PP%pl(1))
call ncdf_varwrite1c (incid, klev,   istart2,icount2, 'qi',       ILOCATION%PP%pi(1))
call ncdf_varwrite1c (incid, klev,   istart2,icount2, 'qr',       ILOCATION%PP%pr(1))
call ncdf_varwrite1c (incid, klev,   istart2,icount2, 'qsn',       ILOCATION%PP%ps(1))
call ncdf_varwrite1c (incid, klev,   istart2,icount2, 'cloud_fraction',   ILOCATION%PP%pa(1))
call ncdf_varwrite1c (incid, klev,   istart2,icount2, 'relative_humidity',zrh)
call ncdf_varwrite1c (incid, klev,   istart2,icount2, 'dry_st_energy',    zdry_st)
call ncdf_varwrite1c (incid, klev,   istart2,icount2, 'moist_st_energy',  zmoist_st)
call ncdf_varwrite1c (incid, klev,   istart2,icount2, 'q_sat',            zqs)
call ncdf_varwrite1c (incid, klev,   istart2,icount2, 'pot_temperature',  ztheta)
call ncdf_varwrite1c (incid, klev,   istart2,icount2, 'pot_temp_e',       ztheta_e)
call ncdf_varwrite1c (incid, klev,   istart2,icount2, 'ug',       ILOCATION%PP%pug(1))
call ncdf_varwrite1c (incid, klev,   istart2,icount2, 'vg',       ILOCATION%PP%pvg(1))
call ncdf_varwrite1c (incid, klev,   istart2,icount2, 'omega',    ILOCATION%PP%pw(1))
call ncdf_varwrite1c (incid, klev,   istart2,icount2, 'uadv',     ILOCATION%PP%puadv(1))
call ncdf_varwrite1c (incid, klev,   istart2,icount2, 'vadv',     ILOCATION%PP%pvadv(1))
call ncdf_varwrite1c (incid, klev,   istart2,icount2, 'tadv',     ILOCATION%PP%ptadv(1))
call ncdf_varwrite1c (incid, klev,   istart2,icount2, 'qadv',     ILOCATION%PP%pqadv(1))
call ncdf_varwrite1c (incid, klev,   istart2,icount2, 'cladv',     ILOCATION%PP%pladv(1))
call ncdf_varwrite1c (incid, klev,   istart2,icount2, 'ciadv',     ILOCATION%PP%piadv(1))
call ncdf_varwrite1c (incid, klev,   istart2,icount2, 'ccadv',     ILOCATION%PP%paadv(1))
call ncdf_varwrite1c (incid, klev,   istart2,icount2, 'csadv',     ILOCATION%PP%psadv(1))
call ncdf_varwrite1c (incid, klev,   istart2,icount2, 'cradv',     ILOCATION%PP%pradv(1))
call ncdf_varwrite1c (incid, klev+1, istart2,icount3, 'etadotdpdeta',  ILOCATION%PP%petadotdpdeta(0))

call ncdf_varwrite1c (incid, ncss,    istart2,icount4, 't_soil',   ILOCATION%PP%pstl(1))
call ncdf_varwrite1c (incid, ncss,    istart2,icount4, 'q_soil',   ILOCATION%PP%pswl(1))
call ncdf_varwrite1c (incid, ncss,    istart2,icount4, 't_sea_ice',ILOCATION%PP%ptia(1))

!        6.  close NetCDF file.

  istatus = NF_CLOSE(incid)
  call handle_err_nc(istatus)

! ENDDO

END SUBROUTINE WRT1C_NC
