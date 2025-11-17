module plugin_profiler_mod
#ifdef WITH_SCM_PLUME_PLUGIN_PROFILER
  use yomvar

  implicit none

  ! Define a timer entry type
  type :: timer_entry
     character(len=64) :: name
     real(kind=jprd) :: start = 0.0_jprd
     real(kind=jprd) :: total = 0.0_jprd
  end type

  ! all timers
  type(timer_entry), allocatable :: timers(:)
  
  ! total number of timers
  integer :: nTimers = 0

contains

  subroutine start_timer(name)
    character(len=*), intent(in) :: name
    real(kind=jprd), external :: timef
    integer :: idx
    idx = find_or_add(name)
    timers(idx)%start = timef()/1000.0_jprd
  end subroutine

  subroutine stop_timer(name)
    character(len=*), intent(in) :: name
    real(kind=jprd), external :: timef
    integer :: idx
    idx = find_or_add(name)
    timers(idx)%total = timers(idx)%total + timef()/1000.0_jprd - timers(idx)%start
  end subroutine

  function find_or_add(name) result(idx)
    character(len=*), intent(in) :: name
    integer :: idx, i

    do i = 1, nTimers
       if (trim(timers(i)%name) == trim(name)) then
          idx = i
          return
       end if
    end do

    ! Add new timer
    nTimers = nTimers + 1
    call extend_array(nTimers)
    timers(nTimers)%name = trim(name)
    timers(nTimers)%total = 0.0_jprd
    timers(nTimers)%start = 0.0_jprd
    idx = nTimers
  end function find_or_add

  subroutine extend_array(newsize)
    integer, intent(in) :: newsize
    type(timer_entry), allocatable :: tmp(:)

    if (.not.allocated(timers)) then
       allocate(timers(newsize))
    else
       allocate(tmp(size(timers)))
       tmp = timers
       deallocate(timers)
       allocate(timers(newsize))
       timers(1:size(tmp)) = tmp
       deallocate(tmp)
    end if
  end subroutine extend_array

  subroutine print_timers(mpi_comm)
    use yomvar
    use fckit_mpi_module, only : fckit_mpi_comm
    use fckit_mpi_module, only : fckit_mpi_min
    use fckit_mpi_module, only : fckit_mpi_max
    use fckit_mpi_module, only : fckit_mpi_sum
    use fckit_log_module, only : log
    implicit none

    type(fckit_mpi_comm) :: mpi_comm
    integer(kind=jpim) :: nproc
    integer(kind=jpim) :: myproc
    real(kind=jprd) :: local, gmin, gmax, gsum
    integer :: i_timer
    CHARACTER*127 msg

    nproc  = mpi_comm%size()
    myproc = mpi_comm%rank()

    if (myproc == 0) then
      write(msg,'(A)') "[SCM-TIMER] ================================ SCM Timer Report ================================"; call log%info(msg)
      write(msg,'(A12, A45, 3X, A10, A12, A12)') "[SCM-TIMER] ", "Timer Name", "Min", "Max", "Mean"; call log%info(msg)
    end if

    do i_timer = 1, nTimers
      local = timers(i_timer)%total

      call mpi_comm%allreduce(local, gmin, fckit_mpi_min())
      call mpi_comm%allreduce(local, gmax, fckit_mpi_max())
      call mpi_comm%allreduce(local, gsum, fckit_mpi_sum())

      if (myproc == 0) then
          write(msg,'(A12, A45, 3X, F10.6, 2X, F10.6, 2X, F10.6)') "[SCM-TIMER] ", timers(i_timer)%name, gmin, gmax, gsum/nproc; call log%info(msg)
      end if
    end do

    if (myproc == 0) then
      write(msg,'(A)') "[SCM-TIMER] =================================================================================="; call log%info(msg)
    end if

  end subroutine print_timers

#endif
end module plugin_profiler_mod
