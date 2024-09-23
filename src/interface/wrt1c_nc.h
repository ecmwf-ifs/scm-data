! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

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
