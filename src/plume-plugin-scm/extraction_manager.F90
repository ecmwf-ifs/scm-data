! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

module extraction_manager_mod

  use, intrinsic :: iso_c_binding, only : c_int32_t

  use fckit_log_module,           only : log
  use fckit_configuration_module, only : fckit_configuration

  use yomvar, only : JPIM

  implicit none

  private

  public :: extraction_manager

  ! Bucket holding the indices of the locations that should be extracted at a
  ! particular time step.
  type :: iloc_bucket
    integer(kind=JPIM), allocatable :: ilocs(:)
  end type iloc_bucket

  ! Extraction scheduler for the SCM plugin.
  !
  ! At setup time the manager parses, for each configured point:
  !   * "timesteps"           : list of ints  (new, preferred)
  !   * "timestep" or "nstep" : single int    (kept for backward compatibility)
  ! and builds a step -> [location index] map plus a list of always-extract
  ! locations (any point whose schedule contains a negative value or is left
  ! unspecified is considered "always extract").
  !
  ! At run time consumers query:
  !   * get_points_at_step(step) -> compact array of ilocs to extract now
  !   * should_extract(iloc,step) -> per-iloc predicate (equivalent to the
  !                                  previous single-target-step test)
  type :: extraction_manager
    private
    integer(kind=JPIM)              :: nb_locations = 0
    integer(kind=JPIM)              :: max_step     = -1
    type(iloc_bucket), allocatable  :: buckets(:)          ! 0:max_step
    integer(kind=JPIM), allocatable :: always_extract(:)   ! ilocs that fire every step
    logical, allocatable            :: is_always(:)        ! per-iloc convenience flag
  contains
    procedure, pass(self) :: init                 => extraction_manager_init
    procedure, pass(self) :: should_extract       => extraction_manager_should_extract
    procedure, pass(self) :: get_points_at_step   => extraction_manager_get_points_at_step
    procedure, pass(self) :: finalize             => extraction_manager_finalize
  end type extraction_manager

contains


  ! ------------------------------------------------------------------------
  ! init: parse per-point timesteps and build the step -> [iloc] map.
  ! ------------------------------------------------------------------------
  subroutine extraction_manager_init(self, plugin_config_points, nb_locations)
    class(extraction_manager),               intent(inout) :: self
    type(fckit_configuration),               intent(in)    :: plugin_config_points(:)
    integer(kind=JPIM),                      intent(in)    :: nb_locations

    integer(kind=JPIM) :: ipoint, istep, ii, count_at
    integer(kind=JPIM) :: max_step_local
    logical            :: found

    ! per-point parsed schedule
    type(iloc_bucket), allocatable :: point_steps(:)
    logical,           allocatable :: point_always(:)

    integer(c_int32_t), allocatable :: steps_int32(:)
    integer(c_int32_t)              :: scalar_int32

    integer(kind=JPIM) :: n_always
    character(len=512) :: msg

    ! -- clean any prior state ------------------------------------------------
    call self%finalize()

    self%nb_locations = nb_locations

    allocate(point_steps(nb_locations))
    allocate(point_always(nb_locations))
    allocate(self%is_always(nb_locations))
    point_always(:)    = .false.
    self%is_always(:)  = .false.

    max_step_local = -1
    n_always       = 0

    ! -- pass 1: parse each point's schedule ---------------------------------
    do ipoint = 1, nb_locations

      ! Try the new list-of-ints form first.
      found = plugin_config_points(ipoint)%get("timesteps", steps_int32)

      if (.not.found) then
        ! Fall back to the legacy scalar forms.
        found = plugin_config_points(ipoint)%get("timestep", scalar_int32)
        if (.not.found) then
          found = plugin_config_points(ipoint)%get("nstep", scalar_int32)
        endif
        if (found) then
          allocate(steps_int32(1))
          steps_int32(1) = scalar_int32
        endif
      endif

      if (.not.found) then
        ! No schedule specified -> extract at every step.
        point_always(ipoint) = .true.
        n_always = n_always + 1
      else
        ! If any entry is negative, treat this point as "always extract"
        ! and ignore the specific step entries.
        if (any(steps_int32 < 0)) then
          point_always(ipoint) = .true.
          n_always = n_always + 1
        else
          ! copy into per-point bucket, dropping duplicates
          call unique_sorted(steps_int32, point_steps(ipoint)%ilocs)
          if (size(point_steps(ipoint)%ilocs) > 0) then
            max_step_local = max(max_step_local, maxval(point_steps(ipoint)%ilocs))
          endif
        endif
        deallocate(steps_int32)
      endif

    enddo

    self%max_step = max_step_local

    ! -- pass 2: build always_extract list -----------------------------------
    allocate(self%always_extract(n_always))
    ii = 0
    do ipoint = 1, nb_locations
      if (point_always(ipoint)) then
        ii = ii + 1
        self%always_extract(ii) = ipoint
        self%is_always(ipoint)  = .true.
      endif
    enddo

    ! -- pass 3: build step buckets ------------------------------------------
    if (max_step_local >= 0) then
      allocate(self%buckets(0:max_step_local))
      ! For each step, count matching points, then allocate + fill.
      do istep = 0, max_step_local
        count_at = 0
        do ipoint = 1, nb_locations
          if (point_always(ipoint)) cycle
          if (.not.allocated(point_steps(ipoint)%ilocs)) cycle
          if (any(point_steps(ipoint)%ilocs == istep)) count_at = count_at + 1
        enddo
        allocate(self%buckets(istep)%ilocs(count_at))
        ii = 0
        do ipoint = 1, nb_locations
          if (point_always(ipoint)) cycle
          if (.not.allocated(point_steps(ipoint)%ilocs)) cycle
          if (any(point_steps(ipoint)%ilocs == istep)) then
            ii = ii + 1
            self%buckets(istep)%ilocs(ii) = ipoint
          endif
        enddo
      enddo
    endif

    ! -- log a summary -------------------------------------------------------
    write(msg,'(A,I0,A,I0,A,I0)') "extraction_manager: nb_locations=", nb_locations, &
      & ", always_extract=", n_always, ", max_scheduled_step=", max_step_local
    call log%info(msg)

    do ipoint = 1, nb_locations
      if (point_always(ipoint)) then
        write(msg,'(A,I0,A)') "extraction_manager:   point ", ipoint, " -> every step"
      else if (allocated(point_steps(ipoint)%ilocs)) then
        call format_int_list(point_steps(ipoint)%ilocs, msg, &
          &                  "extraction_manager:   point ", ipoint)
      else
        write(msg,'(A,I0,A)') "extraction_manager:   point ", ipoint, " -> (no steps requested)"
      endif
      call log%info(msg)
    enddo

    ! per-point buffer no longer needed
    do ipoint = 1, nb_locations
      if (allocated(point_steps(ipoint)%ilocs)) deallocate(point_steps(ipoint)%ilocs)
    enddo
    deallocate(point_steps)
    deallocate(point_always)

  end subroutine extraction_manager_init


  ! ------------------------------------------------------------------------
  ! should_extract: predicate used by loops that iterate over all locations
  ! (kept for callers that don't want to rebuild the loop structure).
  ! ------------------------------------------------------------------------
  function extraction_manager_should_extract(self, iloc, nstep) result(res)
    class(extraction_manager), intent(in) :: self
    integer(kind=JPIM),        intent(in) :: iloc
    integer(kind=JPIM),        intent(in) :: nstep
    logical                               :: res

    res = .false.
    if (iloc < 1 .or. iloc > self%nb_locations) return

    if (allocated(self%is_always)) then
      if (self%is_always(iloc)) then
        res = .true.
        return
      endif
    endif

    if (nstep >= 0 .and. nstep <= self%max_step) then
      if (allocated(self%buckets)) then
        if (allocated(self%buckets(nstep)%ilocs)) then
          res = any(self%buckets(nstep)%ilocs == iloc)
        endif
      endif
    endif

  end function extraction_manager_should_extract


  ! ------------------------------------------------------------------------
  ! get_points_at_step: return a compact list of ilocs to extract at nstep.
  ! Combines always_extract with the step-specific bucket (if any).
  ! ------------------------------------------------------------------------
  function extraction_manager_get_points_at_step(self, nstep) result(ilocs)
    class(extraction_manager), intent(in)   :: self
    integer(kind=JPIM),        intent(in)   :: nstep
    integer(kind=JPIM), allocatable         :: ilocs(:)

    integer(kind=JPIM) :: n_always, n_step

    n_always = 0
    if (allocated(self%always_extract)) n_always = size(self%always_extract)

    n_step = 0
    if (nstep >= 0 .and. nstep <= self%max_step .and. allocated(self%buckets)) then
      if (allocated(self%buckets(nstep)%ilocs)) n_step = size(self%buckets(nstep)%ilocs)
    endif

    allocate(ilocs(n_always + n_step))

    if (n_always > 0) ilocs(1:n_always) = self%always_extract(1:n_always)
    if (n_step   > 0) ilocs(n_always+1:n_always+n_step) = self%buckets(nstep)%ilocs(1:n_step)

  end function extraction_manager_get_points_at_step


  ! ------------------------------------------------------------------------
  ! finalize: release all dynamic state.
  ! ------------------------------------------------------------------------
  subroutine extraction_manager_finalize(self)
    class(extraction_manager), intent(inout) :: self
    integer(kind=JPIM) :: i

    if (allocated(self%buckets)) then
      do i = lbound(self%buckets,1), ubound(self%buckets,1)
        if (allocated(self%buckets(i)%ilocs)) deallocate(self%buckets(i)%ilocs)
      enddo
      deallocate(self%buckets)
    endif

    if (allocated(self%always_extract)) deallocate(self%always_extract)
    if (allocated(self%is_always))      deallocate(self%is_always)

    self%nb_locations = 0
    self%max_step     = -1

  end subroutine extraction_manager_finalize


  ! ------------------------------------------------------------------------
  ! Helpers (module-private)
  ! ------------------------------------------------------------------------

  ! Return a sorted, duplicate-free copy of `input` into the allocatable
  ! `output`.  Uses an O(n^2) insertion approach; n is typically <= a handful.
  subroutine unique_sorted(input, output)
    integer(c_int32_t),              intent(in)    :: input(:)
    integer(kind=JPIM), allocatable, intent(inout) :: output(:)

    integer(kind=JPIM) :: n_in, i, j, count, tmp
    integer(kind=JPIM), allocatable :: work(:)

    n_in = size(input)
    if (n_in == 0) then
      allocate(output(0))
      return
    endif

    allocate(work(n_in))
    count = 0
    do i = 1, n_in
      ! skip duplicates already present
      if (any(work(1:count) == int(input(i), kind=JPIM))) cycle
      count = count + 1
      work(count) = int(input(i), kind=JPIM)
    enddo

    ! simple insertion sort
    do i = 2, count
      tmp = work(i)
      j = i - 1
      do while (j >= 1)
        if (work(j) <= tmp) exit
        work(j+1) = work(j)
        j = j - 1
      enddo
      work(j+1) = tmp
    enddo

    allocate(output(count))
    output(1:count) = work(1:count)
    deallocate(work)

  end subroutine unique_sorted


  ! Format a small integer list into a log message string.
  subroutine format_int_list(values, msg, prefix, ipoint)
    integer(kind=JPIM), intent(in)  :: values(:)
    character(len=*),   intent(out) :: msg
    character(len=*),   intent(in)  :: prefix
    integer(kind=JPIM), intent(in)  :: ipoint

    integer(kind=JPIM) :: i
    character(len=32)  :: num

    write(msg,'(A,I0,A)') prefix, ipoint, " -> steps ["
    do i = 1, size(values)
      write(num,'(I0)') values(i)
      msg = trim(msg) // trim(num)
      if (i < size(values)) msg = trim(msg) // ", "
    enddo
    msg = trim(msg) // "]"

  end subroutine format_int_list


end module extraction_manager_mod
