! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

INTERFACE
SUBROUTINE RDNAM(DATAID,DELTA, NLEV, NSMAX, NSTEP, CGRID, &
		 & PVAH, PVBH, KLOCMAX, PLAT, PLON, namelist_path)

use yomvar

implicit none

INTEGER, PARAMETER :: JMAXPOINTS = 100
INTEGER, PARAMETER :: JMAXLEV = 200

INTEGER(KIND=JPIM), intent(out) :: KLOCMAX
INTEGER(KIND=JPIM), intent(out) :: NLEV, NSMAX, NSTEP

character(len=30), intent(out) :: dataid, cgrid
REAL(KIND=JPRB), allocatable, intent(out) :: PVAH(:), PVBH(:)
REAL(KIND=JPRB), intent(out) :: DELTA
REAL(KIND=JPRB), allocatable, intent(out) :: PLAT(:), PLON(:)
character(len=*), intent(in), optional :: namelist_path

END SUBROUTINE RDNAM
END INTERFACE
