! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

SUBROUTINE RDNAM(DATAID,DELTA, NLEV, NSMAX, NSTEP, CGRID, &
 & PVAH, PVBH, KLOCMAX, PLAT, PLON, namelist_path)
!-----------------------------------------------------------------------
!     reads the namelist:
!         DATAID      O    Text identifying the dataset
!
!     Locations are always selected as one or more points, via their
!     coordinates (LAT/LON).
!-----------------------------------------------------------------------
use yomvar

implicit none

INTEGER, PARAMETER :: JMAXPTS = 100
INTEGER, PARAMETER :: JMAXLEV = 200

INTEGER(KIND=JPIM), intent(out) :: KLOCMAX
INTEGER(KIND=JPIM), intent(out) :: NLEV, NSMAX, NSTEP

character(len=30), intent(out) :: dataid, cgrid
REAL(KIND=JPRB), allocatable, intent(out) :: PVAH(:), PVBH(:)
REAL(KIND=JPRB), intent(out) :: DELTA
REAL(KIND=JPRB), allocatable, intent(out) :: PLAT(:), PLON(:)
character(len=*), intent(in), optional :: namelist_path

REAL(KIND=JPRB) :: LAT(JMAXPTS), LON(JMAXPTS)
REAL(KIND=JPRB) :: DVALH(0:JMAXLEV), DVBH(0:JMAXLEV)  

! LPROGNOSTIC and the lat-lon rectangle bounds are still accepted in the
! namelist (so that existing namelists keep reading), but they are ignored:
! all variables found in the input are processed, and locations are always
! selected as points.
LOGICAL :: LPROGNOSTIC
REAL(KIND=JPRB) :: LATN,LATS,LONW, LONE
REAL(KIND=JPRB) :: TSTEP

INTEGER(KIND=JPIM) :: j, k

NAMELIST /NAMUS/ LAT, LON, LATN, LATS, LONW, LONE, LPROGNOSTIC, &
 &               DATAID,TSTEP, DELTA, &
 &               NSTEP, NLEV, NSMAX, CGRID, DVALH, DVBH

!     open file
!     ---------

DVALH(:) = -999.
DVBH(:) = -999.
LATN=-999.
LATS=-999.
LONW=-999.
LONE=-999.
LAT(:)=-999.
LON(:)=-999.
NLEV=-1
! current step in hours
NSTEP=0
NSMAX = -1
CGRID = ' '
! distance from nearest
DELTA=-999.
LPROGNOSTIC=.TRUE.
DATAID='nocomments'

! read namelist from file
if (present(namelist_path)) then
  open(51,FILE=trim(namelist_path))
else
  open(51,FILE='namelist_1c')
endif

!     read namelist
!     -------------

READ(51,NAMUS)

IF( NLEV == -1 .or. NSMAX == -1 .or. CGRID == ' ' ) THEN
  write(6,*) 'NLEV=137 + NSMAX=95 + CGRID=\"N48\" or similar must be given'
  stop "RDNAM"
endif
write(*,*) 'NSMAX, NLEV, CGRID, dataid', NSMAX, NLEV, CGRID, dataid

IF( DVALH(1) == -999. .or. DVBH(1) == -999. ) THEN
  write(6,*) 'As and Bs must be provided'
  stop "RDNAM"
endif

if( NLEV > -1 ) THEN
  allocate(pvah(0:nlev))
  allocate(pvbh(0:nlev))
  pvah(0:nlev) = DVALH(0:nlev)
  pvbh(0:nlev) = DVBH(0:nlev)
endif

IF (LATN.NE.-999. .OR. LATS.NE.-999. .OR. LONW.NE.-999. .OR. LONE.NE.-999.) then
  write(6,*) 'LATN/LATS/LONW/LONE are ignored: only point selection is supported'
ENDIF

IF (.NOT.LPROGNOSTIC) then
  write(6,*) 'LPROGNOSTIC=.FALSE. is ignored: all input variables are processed'
ENDIF

write(6,*)'POINT'
do j=1, JMAXPTS
  if (lat(j) == -999.) exit
  if (lon(j) == -999.) exit
  write(6,*) 'LAT,LON= : ', lat(j),lon(j)
enddo
klocmax=j-1
write(6,*) j-1,' points'
if (klocmax == 0) then
  write(6,*) 'One or more points need to be given'
  stop "RDNAM"
endif
allocate(plon(klocmax))
allocate(plat(klocmax))
do k=1,klocmax
  plon(k) = lon(k)
  plat(k) = lat(k)
enddo

END SUBROUTINE RDNAM
