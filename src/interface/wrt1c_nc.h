INTERFACE
SUBROUTINE WRT1C_NC(ILOCATION,PVAH, PVBH, INFO, incid,klev)

use yomvar 

implicit none

REAL(KIND=JPRB),    intent(in) :: pvah(0:klev), pvbh(0:klev)
TYPE(TINFO), intent(in) :: INFO
TYPE(TLOCATION), intent(in) :: ILOCATION
INTEGER(KIND=JPIM), intent(in) :: incid, klev

end subroutine WRT1C_NC
END INTERFACE
