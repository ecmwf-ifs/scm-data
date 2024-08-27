INTERFACE
SUBROUTINE RDNAM(LAREA,LPROGNOSTIC,DATAID,DELTA, NLEV, NSMAX, NSTEP, CGRID, &
		 & PVAH, PVBH, KLOCMAX, PLAT, PLON)

use yomvar

implicit none

INTEGER, PARAMETER :: JMAXPOINTS = 100
INTEGER, PARAMETER :: JMAXLEV = 200

LOGICAL, intent(out)   :: larea, lprognostic
INTEGER(KIND=JPIM), intent(out) :: KLOCMAX
INTEGER(KIND=JPIM), intent(out) :: NLEV, NSMAX, NSTEP

character(len=30), intent(out) :: dataid, cgrid
REAL(KIND=JPRB), allocatable, intent(out) :: PVAH(:), PVBH(:)
REAL(KIND=JPRB), intent(out) :: DELTA
REAL(KIND=JPRB), allocatable, intent(out) :: PLAT(:), PLON(:)

END SUBROUTINE RDNAM
END INTERFACE
