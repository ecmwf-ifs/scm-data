INTERFACE
SUBROUTINE SU_WRT_NC (myproc,PVAH,PVBH,dataid,inum,incid,klev,nstep)

use atlas_module
use yomvar

implicit none
INTEGER(KIND=JPIM),        intent(in) :: myproc
REAL(KIND=JPRB),           intent(in) :: PVAH(0:KLEV), PVBH(0:KLEV)
character(len=*), intent(in) :: dataid
INTEGER(KIND=JPIM),        intent(out):: incid
INTEGER(KIND=JPIM),        intent(in) :: inum
INTEGER(KIND=JPIM),        intent(in) :: klev
INTEGER(KIND=JPIM),        intent(in) :: nstep

end subroutine SU_WRT_NC
END INTERFACE
