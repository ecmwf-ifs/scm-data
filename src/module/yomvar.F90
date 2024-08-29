MODULE YOMVAR

IMPLICIT NONE

SAVE

INTEGER, PARAMETER :: JPIM = SELECTED_INT_KIND(9)

#ifdef WITH_SCM_SINGLE_PRECISION
INTEGER, PARAMETER :: JPRB = SELECTED_REAL_KIND(6,37)   ! SINGLE_PRECISION
#else
INTEGER, PARAMETER :: JPRB = SELECTED_REAL_KIND(13,300) ! DOUBLE_PRECISION
#endif

INTEGER, PARAMETER :: JPRD = SELECTED_REAL_KIND(13,300)

!... definition of variables

INTEGER(KIND=JPIM), parameter :: ncss    = 4

type TPARAM
!  sequence
  REAL(KIND=JPRB), allocatable :: PU(:)   ,PV(:)   ,PT(:)   ,PQ(:)   ,PW(:)  , &
   & PA(:)   ,PL(:)   ,PI(:)   ,PR(:), PS(:)

  REAL(KIND=JPRB) :: PSTL(ncss) ,PSWL(ncss) ,PTIA(ncss)

  REAL(KIND=JPRB) :: PSD    ,PSRC   ,PSKT   ,PCI    ,PSST  ,PLNSP, PSP, PSPL, PSPM, &
   & PTSN   ,PASN   ,PRSN   ,&
   & PCVL   ,PCVH   ,PTVL   ,PTVH   ,PZ    , PZL, PZM, &
   & PLSM   ,PSR    ,PLSRH  ,PAL    ,&
   & PSDFOR ,PSDOR  ,PISOR  ,PANOR  ,PSLOR ,&
   & PSSHF  ,PSLHF,&
   & PALUVP, PALUVD, PALNIP, PALNID, PLAILC, PLAIHC, PSOTY,&
   & PLAKEFR, PLAKEDL, PLAKEMLT, PLAKEMLD, PLAKEBLT, &
   & PLAKETLT, PLAKESHF, PLAKEICT, PLAKEICD
  
  REAL(KIND=JPRB), allocatable :: PUG(:)  ,PVG(:)
  REAL(KIND=JPRB), allocatable :: PTADV(:),PQADV(:),PUADV(:),PVADV(:), &
   & PAADV(:),PLADV(:),PIADV(:),PRADV(:),PSADV(:),PETADOTDPDETA(:), PDIV(:), PROT(:)
  REAL(KIND=JPRB), allocatable :: PTL(:), PTM(:), PQL(:), PQM(:), &
   & PLL(:), PLM(:), PIL(:), PIM(:), PCAL(:), PCAM(:), PSL(:), PSM(:), PRL(:), PRM(:) 

end type TPARAM

type TINFO
  INTEGER(KIND=JPIM) :: IDATE
  INTEGER(KIND=JPIM) :: ITIME
  INTEGER(KIND=JPIM) :: ISTEP ! in hours
  INTEGER(KIND=JPIM) :: NSTEP ! index in hours (not from grib-file)
end type TINFO

type TLOCATION
!  sequence
  REAL(KIND=JPRB) :: RLONI ! nearest location actually used in degrees
   REAL(KIND=JPRB) :: RLATI ! nearest location actually used in degrees
   INTEGER(KIND=JPIM) :: IPROC
   INTEGER(KIND=JPIM) :: ILOC
   INTEGER(KIND=JPIM) :: IFILE_ID
   TYPE(TPARAM) :: PP
end type TLOCATION

CONTAINS

SUBROUTINE DEALLOCATE_COLUMNS(PP)

implicit none

TYPE(TPARAM), intent(inout) :: PP

deallocate(PP%PU)
deallocate(PP%PV)
deallocate(PP%PW)
deallocate(PP%PT)
deallocate(PP%PQ)
deallocate(PP%PA)
deallocate(PP%PL)
deallocate(PP%PI)
deallocate(PP%PR)
deallocate(PP%PS)
deallocate(PP%PETADOTDPDETA)
deallocate(PP%PTADV)
deallocate(PP%PQADV)
deallocate(PP%PLADV)
deallocate(PP%PIADV)
deallocate(PP%PAADV)
deallocate(PP%PRADV)
deallocate(PP%PSADV)
deallocate(PP%PUADV)
deallocate(PP%PVADV)
deallocate(PP%PUG)
deallocate(PP%PVG)
deallocate(PP%PQL)
deallocate(PP%PQM)
deallocate(PP%PLL)
deallocate(PP%PLM)
deallocate(PP%PIL)
deallocate(PP%PIM)
deallocate(PP%PCAL)
deallocate(PP%PCAM)
deallocate(PP%PSL)
deallocate(PP%PSM)
deallocate(PP%PRL)
deallocate(PP%PRM)
deallocate(PP%PTL)
deallocate(PP%PTM)
deallocate(PP%PDIV)
deallocate(PP%PROT)

END SUBROUTINE DEALLOCATE_COLUMNS

SUBROUTINE ALLOCATE_COLUMNS(PP, klev)

implicit none

TYPE(TPARAM), intent(inout) :: PP
INTEGER(KIND=JPIM), intent(in) :: klev

allocate(PP%PU(klev))
allocate(PP%PV(klev))
allocate(PP%PW(klev))
allocate(PP%PT(klev))
allocate(PP%PQ(klev))
allocate(PP%PA(klev))
allocate(PP%PL(klev))
allocate(PP%PI(klev))
allocate(PP%PR(klev))
allocate(PP%PS(klev))
allocate(PP%PETADOTDPDETA(0:klev))
allocate(PP%PTADV(klev))
allocate(PP%PQADV(klev))
allocate(PP%PLADV(klev))
allocate(PP%PIADV(klev))
allocate(PP%PAADV(klev))
allocate(PP%PSADV(klev))
allocate(PP%PRADV(klev))
allocate(PP%PUADV(klev))
allocate(PP%PVADV(klev))
allocate(PP%PUG(klev))
allocate(PP%PVG(klev))
! OPTIONAL (need to create gradients for these)
allocate(PP%PTL(klev))
allocate(PP%PTM(klev))
allocate(PP%PQL(klev))
allocate(PP%PQM(klev))
allocate(PP%PLL(klev))
allocate(PP%PLM(klev))
allocate(PP%PIL(klev))
allocate(PP%PIM(klev))
allocate(PP%PCAL(klev))
allocate(PP%PCAM(klev))
allocate(PP%PSL(klev))
allocate(PP%PSM(klev))
allocate(PP%PRL(klev))
allocate(PP%PRM(klev))
allocate(PP%PDIV(klev))
allocate(PP%PROT(klev))

END SUBROUTINE ALLOCATE_COLUMNS

END MODULE YOMVAR
