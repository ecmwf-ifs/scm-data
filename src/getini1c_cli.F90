! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

program getini1c

use atlas_module, only: atlas_library
USE MPL_MODULE, only : mpl_end
implicit none

integer :: return_code
integer :: num_args
character(len=1024) :: sfc_grib_file
character(len=1024) :: cld_grib_file
character(len=1024) :: spec_grib_file
character(len=1024) :: namelist_file

#include "getini1c_run.h"

! initialise Atlas
call atlas_library%initialise()

! get command line arguments
num_args = command_argument_count()

if (num_args /= 4) then
  print *, "Usage: getini1c <sfc_grib_file> <cld_grib_file> <spec_grib_file> <namelist_file>"
  STOP 1
else  
  call get_command_argument(1, sfc_grib_file)
  call get_command_argument(2, cld_grib_file)
  call get_command_argument(3, spec_grib_file)
  call get_command_argument(4, namelist_file)
endif


! run getini1c program
call getini1c_run(return_code, sfc_grib_file, cld_grib_file, spec_grib_file, namelist_file)

! finalise mpl
call mpl_end()

! finalise Atlas
call atlas_library%finalise()

if( return_code /= 0 ) then 
  STOP 1
endif

end program getini1c
