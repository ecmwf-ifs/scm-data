subroutine calcgeost( &
 &   klev, pah, pbh, pslat  &
 & , pt    , pq  , psp                     &
 & , ptl   , pql , pspl  , pzl                    &
 & , ptm   , pqm , pspm  , pzm                    &
 & , pug   , pvg)

implicit none

INTEGER, PARAMETER :: JPIM = SELECTED_INT_KIND(9)

#ifdef WITH_SINGLE_PRECISION
INTEGER, PARAMETER :: JPRB = SELECTED_REAL_KIND(6,37)   ! SINGLE_PRECISION
#else
INTEGER, PARAMETER :: JPRB = SELECTED_REAL_KIND(13,300) ! DOUBLE_PRECISION
#endif

! Computes the geostrophic wind for a set of levels

REAL(KIND=JPRB), intent(in) :: &
 &   pt(klev) , pq(klev) , psp, &
 &   ptl(klev), pql(klev), pspl, pzl, &
 &   ptm(klev), pqm(klev), pspm, pzm, &
 &   pslat
REAL(KIND=JPRB), intent(in) :: pah(0:klev) , pbh(0:klev)
REAL(KIND=JPRB), intent(out) :: pug(klev), pvg(klev)
INTEGER(KIND=JPIM), intent(in) :: klev

! Local arrays

REAL(KIND=JPRB) :: zvc(klev), zdelb(klev)
REAL(KIND=JPRB) :: ZTOPPRES

REAL(KIND=JPRB) zpreh(0:klev), zdelp(klev), zrdelp, zcoefapl, zcoefa & 
 &   , zxybder_lnprl(klev), zxybder_lnprm(klev), zxybder_m_alphll(klev), zxybder_m_alphlm(klev) &
 &   , zlnpr(klev)   , zalph(klev), zrtgr  &
 &   , zrpres  , zrpp &
 &   , zr(klev), zprehydspre(klev) &
 &   , zphihl(0:klev)  , zphihm(0:klev) &
 &   , zrtl(klev)    , zrtm(klev) , zcori &
 &   ,  zphifl(klev)  , zphifm(klev)

REAL(KIND=JPRB) RKBOL,RNAVO,R,RMD,RMV,RMO3,RD,RV,RCPD,RCVD,RCPV,RCVV,RKAPPA,RETV
REAL(KIND=JPRB) rpi, rday, rsiyea, rsiday, romega

INTEGER(KIND=JPIM) jlev

! Initialise miscellaneous variables
RKBOL=1.380658E-23
RNAVO=6.0221367E+23
R=RNAVO*RKBOL
RMD=28.9644
RMV=18.0153
RMO3=47.9942
RD=1000.*R/RMD
RV=1000.*R/RMV
RCPD=3.5*RD
RCVD=RCPD-RD
RCPV=4. *RV
RCVV=RCPV-RV
RKAPPA=RD/RCPD
RETV=RV/RD-1.

rpi=2.*asin(1.)
rday=86400.
rsiyea=365.25*rday*2.*rpi/6.283076
rsiday=rday/(1.+rday/rsiyea)
romega=2.*rpi/rsiday

do jlev=1,klev
  zvc(jlev)=pah(jlev)*pbh(jlev-1)-pah(jlev-1)*pbh(jlev)
  zdelb(jlev)=pbh(jlev)-pbh(jlev-1)
end do

!emulate call gphpre.F90
! Compute pressure at half-levels pressure
zpreh(klev)=psp
do jlev=0,klev-1
  zpreh(jlev) = pah(jlev)+pbh(jlev)*zpreh(klev)
enddo
! computes auxiliary arrays related to the vertical and full-level pressure
! need only zlnpr, zalph, zxybder_lnprl, zxybder_lnprm, zxybder_m_alphll, zxybder_m_alphlm
ZTOPPRES=0.1
zalph(1) = LOG(2.)
zlnpr(1) = LOG(zpreh(1)/ZTOPPRES)
zxybder_lnprl(1)=0.
zxybder_lnprm(1)=0.

do jlev=2,klev
    zrpres = 1.0/zpreh(jlev)
    zdelp(jlev)=zpreh(jlev)-zpreh(jlev-1)
    zrdelp=1./zdelp(jlev)
    zlnpr(jlev)=LOG(zpreh(jlev)/zpreh(jlev-1))
    zalph(jlev)=1.-zpreh(jlev-1)*zrdelp*zlnpr(jlev)

    zrpp = 1.0/(zpreh(jlev)*zpreh(jlev-1))
    zxybder_lnprl(jlev) = -zvc(jlev)*zrpp*pspl
    zxybder_lnprm(jlev) = -zvc(jlev)*zrpp*pspm

!    zrtgr = zrdelp * (zdelb(jlev) + zvc(jlev)*zlnpr(jlev)*zrdelp)
!    ZCOEFAPL = pbh(jlev)*zrpres
!    ZCOEFA = ZCOEFAPL - zrtgr

enddo

do jlev=1,klev
  zrpres = 1.0/zpreh(jlev)
  ZCOEFAPL = pbh(jlev)*zrpres
  zxybder_m_alphll(jlev) =  ZCOEFAPL * pspl
  zxybder_m_alphlm(jlev) =  ZCOEFAPL * pspm
enddo

! emulate call gprcp(kproma,kstart,kend,klev,pq,zcp,zr,zkap)
!    compute R, Cp AND kappa
! need only zr 
do jlev=1,klev
  zr(jlev) = RD*(1.-pq(JLEV))+ RV*pq(JLEV)
enddo

!    computation of rt and its derivatives

do jlev=1,klev
  zrtl(jlev)=(rv-rd)*pt(jlev)*pql(jlev) + zr(jlev)*ptl(jlev)
  zrtm(jlev)=(rv-rd)*pt(jlev)*pqm(jlev) + zr(jlev)*ptm(jlev)
enddo

! emulating call gpgrgeo()
! calculate "grad (gz)" at half levels

zphihl(klev) = pzl
zphihm(klev) = pzm
! hydrostatic
zprehydspre(:)=1.

! 2.1 "delta" and "grad delta" terms contributions.
do jlev=klev,1,-1
  zphihl(jlev-1) = zphihl(jlev) + zlnpr(jlev)*zprehydspre(jlev)*zrtl(jlev) + zxybder_lnprl(jlev)*zprehydspre(jlev)*zr(jlev)*pt(jlev)
  zphihm(jlev-1) = zphihm(jlev) + zlnpr(jlev)*zprehydspre(jlev)*zrtm(jlev) + zxybder_lnprm(jlev)*zprehydspre(jlev)*zr(jlev)*pt(jlev)
enddo

! 2.2 compute [ grad(Phi) + RT grad(log(p)) == grad_p (phi) ] on full levels

! start with half level grad(phi)
do jlev=1,klev
  zphifl(jlev) = zphihl(jlev)
  zphifm(jlev) = zphihm(jlev)
enddo

do jlev=1,klev
  zphifl(jlev) = zphifl(jlev) + zalph(jlev)*zrtl(jlev)+zxybder_m_alphll(jlev)*zr(jlev)*pt(jlev)
  zphifm(jlev) = zphifm(jlev) + zalph(jlev)*zrtm(jlev)+zxybder_m_alphlm(jlev)*zr(jlev)*pt(jlev)
enddo

! final calculation of geostrophic winds
zcori = 2.*romega*pslat
do jlev=1,klev
  pug(jlev) = - zphifm(jlev)/zcori
  pvg(jlev) = + zphifl(jlev)/zcori
enddo

end subroutine calcgeost
