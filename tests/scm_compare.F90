module scm_compare_mod

use fckit_log_module, only: log => fckit_log

IMPLICIT NONE

include 'netcdf.inc'

contains


subroutine compare_attributes(ncid1, ncid2)

integer, intent(in) :: ncid1, ncid2
integer :: nvars1, nvars2
integer :: ndims1, ndims2
integer :: natt1, natt2
integer :: unlimdimid1, unlimdimid2
character*256 :: attname1, attname2

integer :: i, status
character*1024 :: msg

! Inquire variables
status = nf_inq(ncid1, ndims1, nvars1, natt1, unlimdimid1); call handle_err_nc(status)
status = nf_inq(ncid2, ndims2, nvars2, natt2, unlimdimid2); call handle_err_nc(status)

write(msg,'(A,I0,A,I0,A,I0)') 'File1: vars: ', nvars1, ", dims: ", ndims1, ", atts: ", natt1; call log%info(msg)
write(msg,'(A,I0,A,I0,A,I0)') 'File2: vars: ', nvars2, ", dims: ", ndims2, ", atts: ", natt2; call log%info(msg)


! Compare number of attributes
write(msg,'(A)') 'Checking number of attributes...'; call log%info(msg)
if (natt1 .ne. natt2) then
    write(msg,'(A,I0,A,I0)') 'Number of attributes differs between files: ', natt1, ' vs ', natt2; call log%error(msg)
    call exit(1)
else
    write(msg,'(A,I0)') 'Number of attributes is the same in both files: ', natt1; call log%info(msg)

    ! Compare each attribute name
    do i = 1, natt1
        write(msg,'(A,I0)') ' --> Checking attribute: ', i; call log%info(msg)
        status = nf_inq_attname(ncid1, nf_global, i, attname1); call handle_err_nc(status)
        status = nf_inq_attname(ncid2, nf_global, i, attname2); call handle_err_nc(status)

        if (attname1 .ne. attname2) then
            write(msg,'(A,A,A,A)') ' ------> Attribute names differ: ', attname1, ' vs ', attname2; call log%error(msg)
            call exit(1)
        else
            write(msg,'(A,A)') ' ------> Attribute name matches. ', attname1; call log%info(msg)
        endif
    end do
endif
end subroutine compare_attributes


subroutine compare_var_names(ncid1, ncid2)

    integer, intent(in) :: ncid1, ncid2
    integer :: nvars1, nvars2
    integer :: ndims1, ndims2
    integer :: natt1, natt2
    integer :: unlimdimid1, unlimdimid2
    character*256 :: varname1, varname2

    integer :: i, status
    character*1024 :: msg

    ! Inquire variables
    status = nf_inq(ncid1, ndims1, nvars1, natt1, unlimdimid1); call handle_err_nc(status)
    status = nf_inq(ncid2, ndims2, nvars2, natt2, unlimdimid2); call handle_err_nc(status)

    write(msg,'(A,I0,A,I0,A,I0)') 'File1: vars: ', nvars1, ", dims: ", ndims1, ", atts: ", natt1; call log%info(msg)
    write(msg,'(A,I0,A,I0,A,I0)') 'File2: vars: ', nvars2, ", dims: ", ndims2, ", atts: ", natt2; call log%info(msg)

    ! Compare number of variables
    write(msg,'(A)') 'Checking number of variables...'; call log%info(msg)
    if (nvars1 .ne. nvars2) then
        write(msg,'(A,I0,A,I0)') ' --> Number of variables differs between files: ', nvars1, ' vs ', nvars2; call log%error(msg)
        call exit(1)
    else
        write(msg,'(A,I0)') ' --> Number of variables is the same in both files: ', nvars1; call log%info(msg)
    endif

    ! Compare variable names
    write(msg,'(A)') 'Checking variable names..'; call log%info(msg)
    do i = 1, nvars1

        write(msg,'(A,I0)') ' --> Checking variable: ', i; call log%info(msg)
        status = nf_inq_varname(ncid1, i, varname1); call handle_err_nc(status)
        status = nf_inq_varname(ncid2, i, varname2); call handle_err_nc(status)

        if (varname1 .ne. varname2) then
            write(msg,'(A,A,A,A)') ' ----> Variable names differ: ', varname1, ' vs ', varname2; call log%error(msg)
            call exit(1)
        else
            write(msg,'(A,A)') ' ----> Variable name matches: ', varname1; call log%info(msg)
        endif
    end do
end subroutine compare_var_names






subroutine compare(ncid1, ncid2)

    integer, intent(in) :: ncid1, ncid2

    integer :: nvars1, nvars2
    integer :: ndims1, ndims2
    integer :: natt1, natt2
    integer :: unlimdimid1, unlimdimid2
    integer :: dimids1(127), dimids2(127)
    integer :: i, j, status
    integer :: varid1, varid2, dimid1, dimid2, attid1, attid2
    integer :: dimlen1, dimlen2
    integer :: datalen1, datalen2
    integer :: xtype1, xtype2

    real(8), allocatable :: data1_double(:), data2_double(:)
    integer, allocatable :: data1_int(:), data2_int(:)

    character*256 :: varname1, varname2, dimname1, dimname2, attname1, attname2
    character*1024 :: msg

    real(8) :: eps = 1.0e-8


    ! Inquire about variables in the first file
    status = nf_inq(ncid1, ndims1, nvars1, natt1, unlimdimid1); call handle_err_nc(status)
    status = nf_inq(ncid2, ndims2, nvars2, natt2, unlimdimid2); call handle_err_nc(status)


    ! Compare number/names of dimensions for each variable
    write(msg,'(A)') 'Checking number/names of dimensions for each variable...'; call log%info(msg)
    do i = 1, nvars1

        status = nf_inq_varname(ncid1, i, varname1); call handle_err_nc(status)
        write(msg,'(A,I0,A,A)') ' --> Checking variable: ', i, ' named: ', varname1; call log%info(msg)

        status = nf_inq_varid(ncid1, varname1, varid1); call handle_err_nc(status)
        status = nf_inq_varid(ncid2, varname1, varid2); call handle_err_nc(status)

        status = nf_inq_varndims(ncid1, varid1, ndims1); call handle_err_nc(status)
        status = nf_inq_varndims(ncid2, varid2, ndims2); call handle_err_nc(status)

        if (ndims1 .ne. ndims2) then
            write(msg,'(A,I0,A,I0)') ' ----> Number of dimensions differs between files: ', ndims1, ' vs ', ndims2; call log%error(msg)
            call exit(1)
        else
            write(msg,'(A,I0)') ' ----> Number of dimensions is the same in both files: ', ndims1; call log%info(msg)

            write(msg,'(A)') ' ----> Checking dimension names...'; call log%info(msg)
            status = nf_inq_vardimid(ncid1, varid1, dimids1); call handle_err_nc(status)
            status = nf_inq_vardimid(ncid2, varid2, dimids2); call handle_err_nc(status)

            ! Compare each dimension name
            do j = 1, ndims1

                status = nf_inq_dimname(ncid1, dimids1(j), dimname1); call handle_err_nc(status)
                status = nf_inq_dimname(ncid2, dimids2(j), dimname2); call handle_err_nc(status)

                if (dimname1 .ne. dimname2) then
                    write(msg,'(A,A,A,A)') ' ------> Dimension names differ: ', dimname1, ' vs ', dimname2; call log%error(msg)
                    call exit(1)
                else
                    write(msg,'(A,A)') ' ------> Dimension name matches: ', dimname1; call log%info(msg)
                endif
            end do
        endif
    end do


    ! compare values of variables from the 2 files
    write(msg,'(A,I0)') 'Checking values of variables... ', nvars1; call log%info(msg)
    do i=1,nvars1

        status = nf_inq_varname(ncid1, i, varname1); call handle_err_nc(status)
        write(msg,'(A,A)') ' --> Checking variable: ', varname1; call log%info(msg)
        
        ! Get the variable ID
        status = nf_inq_varid(ncid1, varname1, varid1); call handle_err_nc(status)
        status = nf_inq_varid(ncid2, varname1, varid2); call handle_err_nc(status)
        write(msg,'(A,I0)') ' ----> with ID: ', varid1; call log%info(msg)

        ! Get the number of dimensions
        status = nf_inq_varndims(ncid1, varid1, ndims1); call handle_err_nc(status)
        status = nf_inq_varndims(ncid2, varid2, ndims2); call handle_err_nc(status)
        write(msg,'(A,I0)') ' ----> with number of dimensions: ', ndims1; call log%info(msg)    

        ! Get the dimension IDs
        status = nf_inq_vardimid(ncid1, varid1, dimids1); call handle_err_nc(status)
        status = nf_inq_vardimid(ncid2, varid2, dimids2); call handle_err_nc(status)

        datalen1=1
        do j=1,ndims1
            status = nf_inq_dimlen(ncid1, dimids1(j), dimlen1); call handle_err_nc(status)
            datalen1 = datalen1 * dimlen1
            write(*,*) "dimids1: ", dimids1(j), ", len = ", dimlen1
        end do

        datalen2=1
        do j=1,ndims2
            status = nf_inq_dimlen(ncid2, dimids2(j), dimlen2); call handle_err_nc(status)
            datalen2 = datalen2 * dimlen2
            write(*,*) "dimids2: ", dimids2(j), ", len = ", dimlen2
        end do

        ! var type
        status = nf_inq_vartype(ncid1, varid1, xtype1); call handle_err_nc(status)
        status = nf_inq_vartype(ncid2, varid2, xtype2); call handle_err_nc(status)

        if (xtype1 .ne. xtype2) then
            write(msg,'(A,A,A,A)') ' ----> Variable types differ: ', xtype1, ' vs ', xtype2; call log%error(msg)
            call exit(1)
        else
            write(msg,'(A)') ' ----> Variable types match. '; call log%info(msg)
        endif


        if (xtype1 .eq. NF_INT) then
            
            allocate(data1_int(datalen1))
            allocate(data2_int(datalen2))

            status = nf_get_var(ncid1, varid1, data1_int); call handle_err_nc(status)
            status = nf_get_var(ncid2, varid2, data2_int); call handle_err_nc(status)

            do j=1,datalen1
                if (data1_int(j) .ne. data2_int(j)) then
                    write(msg,'(A,I0,A,I0,A,I0)') ' ----> Data values differ at index: ', j, ' between files: ', data1_int(j), ' vs ', data2_int(j); call log%error(msg)
                    call exit(1)
                else
                    write(msg,'(A,I0,A,I0,A,I0)') ' ----> Data values match at index: ', j, ' between files: ', data1_int(j), ' vs ', data2_int(j); call log%info(msg)
                endif
            end do
            deallocate(data1_int)
            deallocate(data2_int)

        else if (xtype1 .eq. NF_DOUBLE) then

            allocate(data1_double(datalen1))
            allocate(data2_double(datalen2))

            status = nf_get_var(ncid1, varid1, data1_double); call handle_err_nc(status)
            status = nf_get_var(ncid2, varid2, data2_double); call handle_err_nc(status)

            do j=1,datalen1
                if (abs(data1_double(j) - data2_double(j)) > eps) then
                    write(msg,'(A,I0,A,F,A,F)') ' ----> Data values differ at index: ', j, ' between files: ', data1_double(j), ' vs ', data2_double(j); call log%error(msg)
                    call exit(1)
                else
                    write(msg,'(A,I0,A,F,A,F)') ' ----> Data values match at index: ', j, ' between files: ', data1_double(j), ' vs ', data2_double(j); call log%info(msg)
                endif
            end do
            deallocate(data1_double)
            deallocate(data2_double)
            
        else
            write(*,*) "Unsupported type: ", xtype1
        endif

    end do
end subroutine compare

end module scm_compare_mod



! ********************************
! ***** Compare NetCDF files *****
! ********************************
program compare_netcdf_keys

use fckit_log_module, only: log => fckit_log
use scm_compare_mod

IMPLICIT NONE

integer :: arg_count
character*256 :: filename1, filename2
integer :: ncid1, ncid2
character*1024 :: msg
integer :: status


! Get Command line arguments
arg_count = command_argument_count()
if ((arg_count<2).or.(arg_count>2)) then
    write(msg,'(A)') "Error: This tool must be invoked as: scm_compare <file1.nc> <file2.nc>"; call log%error(msg)
    call exit(1)
endif

CALL get_command_argument(1, filename1)
CALL get_command_argument(2, filename2)

write(*,*) '---'
write(*,'(A)') "Comparing files: "
write(*,'(A,A)') "  ", trim(filename1)
write(*,'(A,A)') "  ", trim(filename2)
write(*,*) '---'


! Open the first NetCDF file
status = nf_open(trim(filename1), nf_nowrite, ncid1)
call handle_err_nc(status)
write(msg,'(A,A,A,I0)') 'Opened file: ', trim(filename1), " with ID: ", ncid1

! Open the second NetCDF file
status = nf_open(trim(filename2), nf_nowrite, ncid2)
call handle_err_nc(status)
write(msg,'(A,A,A,I0)') 'Opened file: ', trim(filename2), " with ID: ", ncid2

! compare attributes
call compare_attributes(ncid1, ncid2)

! compare variable names
call compare_var_names(ncid1, ncid2)

! compare variable values
call compare(ncid1, ncid2)


! Close the NetCDF files
status = nf_close(ncid1)
call handle_err_nc(status)

status = nf_close(ncid2)
call handle_err_nc(status)

end program compare_netcdf_keys
! ********************************