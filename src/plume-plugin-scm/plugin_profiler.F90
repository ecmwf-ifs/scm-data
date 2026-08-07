module plugin_profiler_mod
#ifdef WITH_SCM_PLUME_PLUGIN_PROFILER

  ! Lightweight hierarchical profiler for the SCM plume plugin.
  !
  ! Regions are identified by dot-separated names ("scm_run.process_plume_fields.gradients_sp")
  ! which are interpreted as a tree: a region's *self* time is its total minus the time spent
  ! in the nested regions opened while it was running.
  !
  ! Timing source is the intrinsic SYSTEM_CLOCK (portable, no fiat/timef dependency). In
  ! addition, every region opens an atlas_Trace with the same name, so that when atlas is run
  ! with ATLAS_TRACE_REPORT=1 the atlas-internal costs (halo_exchange, nabla, mesh generation)
  ! nest underneath our regions in atlas' own report. The table produced by print_timers never
  ! reads back from atlas, so it stays correct even if atlas was built without trace support.

  use yomvar
  use, intrinsic :: iso_fortran_env, only : int64
  use, intrinsic :: iso_c_binding, only : c_int32_t
  use atlas_module, only : atlas_Trace

  implicit none
  private

  public :: register_timer
  public :: start_timer, stop_timer
  public :: start_timer_h, stop_timer_h
  public :: print_timers
  public :: reset_timers

  ! Maximum length of a (fully qualified) timer name.
  integer, parameter :: TIMER_NAME_LEN = 64
  ! Maximum region nesting depth.
  integer, parameter :: MAX_DEPTH = 32
  ! Initial capacity of the timer table; grows geometrically.
  integer, parameter :: INITIAL_CAPACITY = 32

  type :: timer_entry
     character(len=TIMER_NAME_LEN) :: name = ''
     integer(kind=int64) :: tick0 = 0_int64     ! clock count at the last start
     real(kind=jprd) :: total = 0.0_jprd        ! inclusive time
     real(kind=jprd) :: child_total = 0.0_jprd  ! time attributed to nested regions
     integer :: ncalls = 0
     integer :: depth = 0                       ! number of '.' in name
     logical :: running = .false.
  end type

  ! all timers, in registration (= first-touch) order, which mirrors the region tree
  type(timer_entry), allocatable :: timers(:)
  integer :: nTimers = 0
  integer :: nCapacity = 0

  ! stack of currently open timers
  integer :: stack(MAX_DEPTH) = 0
  integer :: nstack = 0
  type(atlas_Trace) :: trace_stack(MAX_DEPTH)
  logical :: trace_open(MAX_DEPTH) = .false.

  ! SYSTEM_CLOCK characteristics, queried once
  integer(kind=int64) :: clock_rate = 0_int64
  integer(kind=int64) :: clock_max = 0_int64
  logical :: clock_ready = .false.

  ! guard against printing the report twice
  logical :: report_done = .false.

contains

  !---------------------------------------------------------------------------
  ! Clock helpers
  !---------------------------------------------------------------------------

  subroutine init_clock()
    if (clock_ready) return
    call system_clock(count_rate=clock_rate, count_max=clock_max)
    clock_ready = .true.
  end subroutine init_clock

  function elapsed_since(tick0) result(dt)
    integer(kind=int64), intent(in) :: tick0
    real(kind=jprd) :: dt
    integer(kind=int64) :: tick, dticks

    call system_clock(tick)
    dticks = tick - tick0
    ! SYSTEM_CLOCK wraps at count_max; with int64 counts this is practically
    ! unreachable, but handle it so a wrap can never produce a negative time.
    if (dticks < 0_int64) dticks = dticks + clock_max + 1_int64

    if (clock_rate > 0_int64) then
      dt = real(dticks, jprd) / real(clock_rate, jprd)
    else
      dt = 0.0_jprd
    endif
  end function elapsed_since

  !---------------------------------------------------------------------------
  ! Timer table
  !---------------------------------------------------------------------------

  subroutine warn(text)
    use fckit_log_module, only : log
    character(len=*), intent(in) :: text
    character(512) :: msg
    write(msg,'(A,A)') "[SCM-TIMER] ", trim(text)
    call log%warning(msg)
  end subroutine warn

  ! Index of an existing timer, or 0 when the name is unknown. Never registers.
  function find_timer(name) result(idx)
    character(len=*), intent(in) :: name
    integer :: idx, i

    idx = 0
    do i = 1, nTimers
      if (timers(i)%name == name) then
        idx = i
        return
      endif
    enddo
  end function find_timer

  ! Register a name (idempotent) and return its handle. Callers in hot code should
  ! call this once during setup and then use start_timer_h/stop_timer_h.
  function register_timer(name) result(idx)
    character(len=*), intent(in) :: name
    integer :: idx, i

    call init_clock()

    idx = find_timer(name)
    if (idx > 0) return

    call grow_table()
    nTimers = nTimers + 1
    idx = nTimers

    timers(idx)%name        = name
    timers(idx)%tick0       = 0_int64
    timers(idx)%total       = 0.0_jprd
    timers(idx)%child_total = 0.0_jprd
    timers(idx)%ncalls      = 0
    timers(idx)%running     = .false.

    ! nesting depth for the report = number of '.' separators
    timers(idx)%depth = 0
    do i = 1, len_trim(name)
      if (name(i:i) == '.') timers(idx)%depth = timers(idx)%depth + 1
    enddo

    if (len_trim(name) > TIMER_NAME_LEN) then
      call warn("timer name is longer than the name field and was truncated: "//trim(name))
    endif
  end function register_timer

  ! Ensure there is room for one more entry; capacity doubles rather than +1.
  subroutine grow_table()
    type(timer_entry), allocatable :: tmp(:)
    integer :: newcap

    if (.not.allocated(timers)) then
      nCapacity = INITIAL_CAPACITY
      allocate(timers(nCapacity))
      return
    endif

    if (nTimers < nCapacity) return

    newcap = 2 * nCapacity
    allocate(tmp(newcap))
    tmp(1:nTimers) = timers(1:nTimers)
    call move_alloc(tmp, timers)
    nCapacity = newcap
  end subroutine grow_table

  !---------------------------------------------------------------------------
  ! Start / stop
  !---------------------------------------------------------------------------

  subroutine start_timer(name, file, line)
    character(len=*), intent(in) :: name
    character(len=*), intent(in), optional :: file
    integer, intent(in), optional :: line

    call start_timer_h(register_timer(name), file, line)
  end subroutine start_timer

  subroutine start_timer_h(handle, file, line)
    integer, intent(in) :: handle
    character(len=*), intent(in), optional :: file
    integer, intent(in), optional :: line

    character(len=256) :: tfile
    integer :: tline
    
    if (handle < 1 .or. handle > nTimers) then
      call warn("start on invalid timer handle, ignored")
      return
    endif

    if (timers(handle)%running) then
      ! Recursive / overlapping entry of the same region: the single start slot
      ! cannot represent it, so ignore the inner start rather than corrupt the total.
      call warn("timer already running, nested start ignored: "//trim(timers(handle)%name))
      return
    endif

    if (nstack >= MAX_DEPTH) then
      call warn("maximum nesting depth reached, not timing: "//trim(timers(handle)%name))
      return
    endif

    timers(handle)%running = .true.
    timers(handle)%ncalls  = timers(handle)%ncalls + 1

    nstack = nstack + 1
    stack(nstack) = handle

    ! Mirror the region into atlas' trace tree so atlas internals attribute correctly.
    tfile = "plugin_profiler.F90"
    tline = 0
    if (present(file)) tfile = file
    if (present(line)) tline = line
    trace_stack(nstack) = atlas_Trace(trim(tfile), tline, trim(timers(handle)%name))
    trace_open(nstack) = .true.

    ! Take the timestamp last so the bookkeeping above is not charged to the region.
    call system_clock(timers(handle)%tick0)
  end subroutine start_timer_h

  subroutine stop_timer(name)
    character(len=*), intent(in) :: name
    integer :: handle

    ! Deliberately does NOT register the name: a stop without a start is a bug in
    ! the instrumentation and must not silently create an entry (which used to be
    ! charged the full absolute wall clock).
    handle = find_timer(name)
    if (handle == 0) then
      call warn("stop without matching start, ignored: "//trim(name))
      return
    endif
    call stop_timer_h(handle)
  end subroutine stop_timer

  subroutine stop_timer_h(handle)
    integer, intent(in) :: handle
    real(kind=jprd) :: dt
    integer :: k, j

    if (handle < 1 .or. handle > nTimers) then
      call warn("stop on invalid timer handle, ignored")
      return
    endif

    if (.not.timers(handle)%running) then
      call warn("stop without matching start, ignored: "//trim(timers(handle)%name))
      return
    endif

    dt = elapsed_since(timers(handle)%tick0)
    timers(handle)%total   = timers(handle)%total + dt
    timers(handle)%running = .false.

    ! Locate this region on the stack. Normally it is the top entry.
    k = 0
    do j = nstack, 1, -1
      if (stack(j) == handle) then
        k = j
        exit
      endif
    enddo

    if (k == 0) then
      call warn("timer not found on the open-region stack: "//trim(timers(handle)%name))
      return
    endif

    ! Force-close any regions left open inside this one (crossed start/stop pairs).
    do j = nstack, k+1, -1
      call warn("region left open inside "//trim(timers(handle)%name)//", closing: " &
        &       //trim(timers(stack(j))%name))
      timers(stack(j))%running = .false.
      call close_trace(j)
    enddo

    call close_trace(k)
    nstack = k - 1

    ! Charge the elapsed time to the enclosing region so that its self time excludes it.
    if (nstack >= 1) then
      timers(stack(nstack))%child_total = timers(stack(nstack))%child_total + dt
    endif
  end subroutine stop_timer_h

  subroutine close_trace(islot)
    integer, intent(in) :: islot
    if (.not.trace_open(islot)) return
    call trace_stack(islot)%stop()
    call trace_stack(islot)%final()
    trace_open(islot) = .false.
  end subroutine close_trace

  !---------------------------------------------------------------------------
  ! Reset
  !---------------------------------------------------------------------------

  subroutine reset_timers()
    integer :: j

    do j = nstack, 1, -1
      call close_trace(j)
    enddo
    nstack = 0
    stack(:) = 0

    if (allocated(timers)) deallocate(timers)
    nTimers = 0
    nCapacity = 0
    report_done = .false.
  end subroutine reset_timers

  !---------------------------------------------------------------------------
  ! Report
  !---------------------------------------------------------------------------

  subroutine print_timers(mpi_comm)
    use fckit_mpi_module, only : fckit_mpi_comm
    use fckit_mpi_module, only : fckit_mpi_min
    use fckit_mpi_module, only : fckit_mpi_max
    use fckit_mpi_module, only : fckit_mpi_sum
    use fckit_log_module, only : log

    type(fckit_mpi_comm) :: mpi_comm

    integer(kind=c_int32_t) :: nproc, myproc
    integer(kind=c_int32_t) :: ngtimers
    integer(kind=c_int32_t) :: icand

    character(len=:), allocatable :: namebuf
    character(len=TIMER_NAME_LEN) :: gname
    character(len=56) :: h_region

    real(kind=jprd), allocatable :: gmin(:), gmax(:), gsum(:), gself(:)
    integer(kind=c_int32_t), allocatable :: gcalls(:), gmaxrank(:)
    character(len=TIMER_NAME_LEN), allocatable :: gnames(:)

    real(kind=jprd) :: local, lself, dnproc, grand, mean, self_mean, imbal, pct
    integer :: i, j, ib
    integer(kind=c_int32_t) :: ilocal
    character(512) :: msg
    character(len=TIMER_NAME_LEN+2*MAX_DEPTH) :: label

    nproc  = mpi_comm%size()
    myproc = mpi_comm%rank()
    dnproc = real(nproc, jprd)    

    if (report_done) then
      if (myproc == 0) call warn("print_timers called more than once; totals are cumulative")
    endif
    report_done = .true.

    ! Warn about regions still open (unbalanced instrumentation) before reporting.
    do j = 1, nstack
      call warn("region still open at report time: "//trim(timers(stack(j))%name))
    enddo

    !-------------------------------------------------------------------------
    ! Agree on a single, globally identical list of timer names.
    !
    ! The reductions below are collective, so every rank must iterate over the
    ! same list in the same order. Ranks can legitimately register different
    ! timers (a rank owning no locations, a conditionally instrumented branch),
    ! so the list is defined by rank 0 and broadcast rather than assumed equal.
    ! Both the count and the names come from rank 0, which makes the loop bound
    ! below identical on every rank by construction.
    !-------------------------------------------------------------------------
    ngtimers = nTimers
    call mpi_comm%broadcast(ngtimers, 0_c_int32_t)

    if (ngtimers <= 0) then
      if (myproc == 0) then
        write(msg,'(A)') "[SCM-TIMER] no timers recorded"; call log%info(msg)
      endif
      return
    endif

    allocate(character(len=TIMER_NAME_LEN*ngtimers) :: namebuf)
    namebuf = ''
    if (myproc == 0) then
      do i = 1, nTimers
        ib = (i-1)*TIMER_NAME_LEN
        namebuf(ib+1:ib+TIMER_NAME_LEN) = timers(i)%name
      enddo
    endif
    call mpi_comm%broadcast(namebuf, 0_c_int32_t)

    allocate(gnames(ngtimers))
    allocate(gmin(ngtimers), gmax(ngtimers), gsum(ngtimers), gself(ngtimers))
    allocate(gcalls(ngtimers), gmaxrank(ngtimers))

    !-------------------------------------------------------------------------
    ! Reduce over the agreed list. A rank that does not have a given timer
    ! contributes zeros; the loop bound is identical everywhere by construction.
    !-------------------------------------------------------------------------
    do i = 1, ngtimers
      ib = (i-1)*TIMER_NAME_LEN
      gname = namebuf(ib+1:ib+TIMER_NAME_LEN)
      gnames(i) = gname

      j = find_timer(gname)
      if (j > 0) then
        local  = timers(j)%total
        lself  = timers(j)%total - timers(j)%child_total
        ilocal = timers(j)%ncalls
      else
        local  = 0.0_jprd
        lself  = 0.0_jprd
        ilocal = 0
      endif

      call mpi_comm%allreduce(local,  gmin(i), fckit_mpi_min())
      call mpi_comm%allreduce(local,  gmax(i), fckit_mpi_max())
      call mpi_comm%allreduce(local,  gsum(i), fckit_mpi_sum())
      call mpi_comm%allreduce(lself,  gself(i), fckit_mpi_sum())
      call mpi_comm%allreduce(ilocal, gcalls(i), fckit_mpi_sum())

      ! Rank holding the maximum (lowest such rank if several) -> straggler.
      if (local >= gmax(i)) then
        icand = myproc
      else
        icand = nproc
      endif
      call mpi_comm%allreduce(icand, gmaxrank(i), fckit_mpi_min())
    enddo

    ! Flag any local timer missing from the agreed list, so it is not silently dropped.
    do j = 1, nTimers
      if (.not.any(gnames(:) == timers(j)%name)) then
        call warn("timer not present on rank 0, omitted from the report: "//trim(timers(j)%name))
      endif
    enddo

    !-------------------------------------------------------------------------
    ! Print (rank 0 only)
    !-------------------------------------------------------------------------
    if (myproc == 0) then

      ! Grand total = sum of the root regions (those with no '.' in their name).
      grand = 0.0_jprd
      do i = 1, ngtimers
        if (index(trim(gnames(i)), '.') == 0) grand = grand + gsum(i)/dnproc
      enddo

      write(msg,'(A)') "[SCM-TIMER] =========================== SCM plugin timer report ==========================="
      call log%info(msg)
      write(msg,'(A,I0,A)') "[SCM-TIMER] ranks: ", nproc, "   times in seconds"; call log%info(msg)
      ! Region header is left-justified over the (left-justified) names; the numeric
      ! headers are right-justified by the A edit descriptor, matching their columns.
      h_region = "Region"
      write(msg,'(A12,A56,A10,A13,A13,A8,A13,A13,A8,A8)') "[SCM-TIMER] ", &
        & h_region, "Calls", "Self", "Total", "%Tot", "Min", "Max", "Imbal", "MaxRk"
      call log%info(msg)

      do i = 1, ngtimers
        mean      = gsum(i)/dnproc
        self_mean = gself(i)/dnproc

        if (mean > 0.0_jprd) then
          imbal = gmax(i)/mean
        else
          imbal = 0.0_jprd
        endif

        if (grand > 0.0_jprd) then
          pct = 100.0_jprd * mean / grand
        else
          pct = 0.0_jprd
        endif

        label = indent_label(gnames(i))

        write(msg,'(A12,A56,I10,F13.3,F13.3,F8.2,F13.3,F13.3,F8.2,I8)') "[SCM-TIMER] ", &
          & label, gcalls(i), self_mean, mean, pct, gmin(i), gmax(i), imbal, gmaxrank(i)
        call log%info(msg)
      enddo

      write(msg,'(A)') "[SCM-TIMER] ------------------------------------------------------------------------------"
      call log%info(msg)
      write(msg,'(A)') "[SCM-TIMER] Regions are shown as a tree (leaf name, indented by nesting depth)."
      call log%info(msg)
      write(msg,'(A)') "[SCM-TIMER] Self = Total minus time spent in nested regions, i.e. unaccounted time."
      call log%info(msg)
      write(msg,'(A)') "[SCM-TIMER] Calls is summed over ranks; Self and Total are per-rank means."
      call log%info(msg)
      write(msg,'(A)') "[SCM-TIMER] Min/Max are taken across ranks per region independently, so they do"
      call log%info(msg)
      write(msg,'(A)') "[SCM-TIMER] NOT add up across the tree. Imbal = Max/Total; MaxRk = slowest rank."
      call log%info(msg)
      write(msg,'(A)') "[SCM-TIMER] =============================================================================="
      call log%info(msg)
    endif

    deallocate(gnames, gmin, gmax, gsum, gself, gcalls, gmaxrank, namebuf)

    write(*,*) "SCM plugin timer report complete on rank ", myproc, " of ", nproc

  end subroutine print_timers

  ! Render a dotted name as an indented leaf, e.g.
  !   "scm_run.process_plume_fields.gradients_sp" -> "    gradients_sp"
  function indent_label(name) result(label)
    character(len=*), intent(in) :: name
    character(len=TIMER_NAME_LEN+2*MAX_DEPTH) :: label
    integer :: i, idepth, ilast

    idepth = 0
    ilast  = 0
    do i = 1, len_trim(name)
      if (name(i:i) == '.') then
        idepth = idepth + 1
        ilast  = i
      endif
    enddo

    if (idepth > MAX_DEPTH) idepth = MAX_DEPTH

    label = ''
    label(2*idepth+1:) = name(ilast+1:len_trim(name))
  end function indent_label

#endif
end module plugin_profiler_mod
