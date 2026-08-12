! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

module extraction_manager_mod

  use fckit_log_module, only : log

  use config_handler_mod, only : config_handler

  use yomvar, only : JPIM

  implicit none

  private

  public :: extraction_manager

  ! Bucket holding the indices of the locations that should be extracted at a
  ! particular time step.
  type :: iloc_bucket
    integer(kind=JPIM), allocatable :: ilocs(:)
  end type iloc_bucket

  ! List of time steps at which a particular point should be extracted
  ! (used only during setup, when parsing the per-point config).
  type :: step_list
    integer(kind=JPIM), allocatable :: steps(:)
  end type step_list

  ! Extraction scheduler for the SCM plugin.
  !
  ! At setup time the manager reads the per-point extraction schedule from the
  ! configuration handler (which owns the parsing of "timesteps" and of the
  ! legacy "timestep"/"nstep" spellings) and builds a step -> [location index]
  ! map plus a list of always-extract locations (any point whose schedule
  ! contains a negative value or is left unspecified is "always extract").
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
  ! init: read the per-point schedules and build the step -> [iloc] map.
  ! ------------------------------------------------------------------------
  subroutine extraction_manager_init(self, cfg)
    class(extraction_manager), intent(inout) :: self
    type(config_handler),      intent(in)    :: cfg

    integer(kind=JPIM) :: ipoint, istep, ii, count_at
    integer(kind=JPIM) :: nb_locations
    integer(kind=JPIM) :: max_step_local

    ! per-point parsed schedule
    type(step_list), allocatable :: point_steps(:)
    logical,         allocatable :: point_always(:)

    integer(kind=JPIM) :: n_always
    character(len=512) :: msg

    ! -- clean any prior state ------------------------------------------------
    call self%finalize()

    nb_locations      = cfg%get_nb_points()
    self%nb_locations = nb_locations

    allocate(point_steps(nb_locations))
    allocate(point_always(nb_locations))
    allocate(self%is_always(nb_locations))
    point_always(:)    = .false.
    self%is_always(:)  = .false.

    max_step_local = -1
    n_always       = 0

    ! -- pass 1: read each point's schedule ----------------------------------
    do ipoint = 1, nb_locations

      if (cfg%get_point_extract_always(ipoint)) then
        point_always(ipoint) = .true.
        n_always = n_always + 1
      else
        ! duplicates are harmless because bucket membership is tested with
        ! `any(... == step)`.
        point_steps(ipoint)%steps = cfg%get_point_timesteps(ipoint)
        if (size(point_steps(ipoint)%steps) > 0) then
          max_step_local = max(max_step_local, maxval(point_steps(ipoint)%steps))
        endif
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
          if (.not.allocated(point_steps(ipoint)%steps)) cycle
          if (any(point_steps(ipoint)%steps == istep)) count_at = count_at + 1
        enddo
        allocate(self%buckets(istep)%ilocs(count_at))
        ii = 0
        do ipoint = 1, nb_locations
          if (point_always(ipoint)) cycle
          if (.not.allocated(point_steps(ipoint)%steps)) cycle
          if (any(point_steps(ipoint)%steps == istep)) then
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
      else if (size(point_steps(ipoint)%steps) > 0) then
        call format_int_list(point_steps(ipoint)%steps, msg, &
          &                  "extraction_manager:   point ", ipoint)
      else
        write(msg,'(A,I0,A)') "extraction_manager:   point ", ipoint, " -> (no steps requested)"
      endif
      call log%info(msg)
    enddo

    ! per-point buffer no longer needed
    do ipoint = 1, nb_locations
      if (allocated(point_steps(ipoint)%steps)) deallocate(point_steps(ipoint)%steps)
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
