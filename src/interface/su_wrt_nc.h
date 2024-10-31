! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

INTERFACE
SUBROUTINE SU_WRT_NC (filename,PVAH,PVBH,dataid,incid,klev)

use atlas_module
use yomvar

implicit none

character(len=*),   intent(in)  :: filename
REAL(KIND=JPRB),    intent(in)  :: PVAH(0:KLEV)
REAL(KIND=JPRB),    intent(in)  :: PVBH(0:KLEV)
character(len=*),   intent(in)  :: dataid
INTEGER(KIND=JPIM), intent(out) :: incid
INTEGER(KIND=JPIM), intent(in)  :: klev

end subroutine SU_WRT_NC
END INTERFACE
