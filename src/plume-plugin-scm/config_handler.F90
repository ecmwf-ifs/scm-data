! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

module config_handler_mod

  use, intrinsic :: iso_c_binding, only : c_int32_t

  use fckit_log_module,           only : log
  use fckit_configuration_module, only : fckit_configuration

  use yomvar, only : JPIM, JPRB

  implicit none

  private

  public :: config_handler

  ! Environment accessors, usable without a configuration (see below).
  public :: config_env_output_dir
  public :: config_env_vert_tables_namelist
  public :: config_env_plume_config_file

  ! ------------------------------------------------------------------------
  ! Canonical configuration keys.
  !
  ! All keys are declared here in lower case and are matched
  ! case-insensitively, so "run_every", "RUN_EVERY" and "Run_Every" in a
  ! configuration file all select the same option.  Lower case is the
  ! documented spelling and the one used in every example / test config.
  ! ------------------------------------------------------------------------
  integer, parameter :: MAX_KEY_LEN = 128

  ! core-config keys
  character(len=*), parameter :: KEY_RUN_EVERY            = "run_every"
  character(len=*), parameter :: KEY_INIT_STEP            = "init_step"
  character(len=*), parameter :: KEY_FINAL_STEP           = "final_step"
  character(len=*), parameter :: KEY_DATAID               = "dataid"
  character(len=*), parameter :: KEY_DELTA                = "delta"
  character(len=*), parameter :: KEY_APPEND_OUTPUT        = "append_output"
  character(len=*), parameter :: KEY_APPEND_OUTPUT_NSTEPS = "append_output_nsteps"
  character(len=*), parameter :: KEY_POINTS               = "points"

  ! points entry keys
  character(len=*), parameter :: KEY_ID                   = "id"
  character(len=*), parameter :: KEY_NAME                 = "name"
  character(len=*), parameter :: KEY_LAT                  = "lat"
  character(len=*), parameter :: KEY_LON                  = "lon"
  character(len=*), parameter :: KEY_TIMESTEPS            = "timesteps"
  character(len=*), parameter :: KEY_TIMESTEP             = "timestep"
  character(len=*), parameter :: KEY_NSTEP                = "nstep"

  ! Keys understood by the plugin: anything else is reported as unknown so
  ! that typos in a configuration file do not pass silently.
  character(len=MAX_KEY_LEN), parameter :: CORE_KEYS(8) = [ character(len=MAX_KEY_LEN) :: &
    & KEY_RUN_EVERY, KEY_INIT_STEP, KEY_FINAL_STEP, &
    & KEY_DATAID, KEY_DELTA, KEY_APPEND_OUTPUT, KEY_APPEND_OUTPUT_NSTEPS, KEY_POINTS ]

  character(len=MAX_KEY_LEN), parameter :: POINT_KEYS(7) = [ character(len=MAX_KEY_LEN) :: &
    & KEY_ID, KEY_NAME, KEY_LAT, KEY_LON, KEY_TIMESTEPS, KEY_TIMESTEP, KEY_NSTEP ]

  ! ------------------------------------------------------------------------
  ! Environment variables read by the plugin.
  ! ------------------------------------------------------------------------
  ! Directory the NetCDF output is written to (default: current directory).
  character(len=*), parameter :: ENV_OUTPUT_DIR   = "PLUME_PLUGINS_OUTPUT_DIR"
  ! TESTING ONLY: namelist the vertical coefficient tables are read from,
  ! instead of the tables compiled into the plugin.
  character(len=*), parameter :: ENV_VERT_TABLES  = "PLUME_SCM_PLUGIN_VERT_TABLES_TEST_NAMELIST"
  ! Plume configuration file (read by the plume driver, i.e. the host model or
  ! the test driver, and reported here for traceability).
  character(len=*), parameter :: ENV_CONFIG_FILE  = "PLUME_CONFIG_FILE"

  ! Defaults for the core-config options.
  integer(kind=JPIM), parameter :: DEF_RUN_EVERY            = 1
  integer(kind=JPIM), parameter :: DEF_INIT_STEP            = 0
  integer(kind=JPIM), parameter :: DEF_FINAL_STEP           = -1
  character(len=*),   parameter :: DEF_DATAID               = "plume-plugin-scm"
  logical,            parameter :: DEF_APPEND_OUTPUT        = .true.
  integer(kind=JPIM), parameter :: DEF_APPEND_OUTPUT_NSTEPS = 0


  ! ------------------------------------------------------------------------
  ! Case-insensitive index over the keys of one fckit_configuration object.
  !
  ! fckit only offers a case-sensitive lookup, so the keys of a configuration
  ! are collected once and every lookup is resolved against the lower-cased
  ! spellings.  Resolving returns the key as spelled in the file, which is
  ! what has to be handed back to fckit.
  ! ------------------------------------------------------------------------
  type :: key_index
    private
    character(len=MAX_KEY_LEN), allocatable :: keys(:)
  contains
    procedure, pass(self) :: build        => key_index_build
    procedure, pass(self) :: resolve      => key_index_resolve
    procedure, pass(self) :: warn_unknown => key_index_warn_unknown
  end type key_index


  ! ------------------------------------------------------------------------
  ! One entry of the "points" list, after parsing and validation.
  ! ------------------------------------------------------------------------
  type :: scm_point_config
    private
    integer(kind=JPIM)              :: id  = -1
    character(len=:), allocatable   :: name
    real(kind=JPRB)                 :: lat = 0.0_JPRB
    ! Longitude wrapped to [0, 360) - the model grid convention.
    real(kind=JPRB)                 :: lon = 0.0_JPRB
    ! Extraction schedule: either "every step the plugin runs" or the
    ! explicit list of steps requested for this point.
    logical                         :: extract_always = .false.
    integer(kind=JPIM), allocatable :: timesteps(:)
  end type scm_point_config


  ! ------------------------------------------------------------------------
  ! Handler for the plugin configuration.
  !
  ! One instance is held at module level by plugin_impl and initialised at
  ! setup time from the plume plugin-core configuration.  It is the only place
  ! that reads configuration keys and environment variables: it parses them,
  ! applies the defaults, performs the consistency checks and then serves the
  ! values through the getters below.
  ! ------------------------------------------------------------------------
  type :: config_handler
    private
    ! -- core-config values -------------------------------------------------
    integer(kind=JPIM)            :: run_every            = DEF_RUN_EVERY
    integer(kind=JPIM)            :: init_step            = DEF_INIT_STEP
    integer(kind=JPIM)            :: final_step           = DEF_FINAL_STEP
    character(len=:), allocatable :: dataid
    ! delta is optional: without it the nearest grid point is found with a
    ! kdtree search instead of a radius search.
    real(kind=JPRB)               :: delta                = 0.0_JPRB
    logical                       :: delta_is_set         = .false.
    logical                       :: append_output        = DEF_APPEND_OUTPUT
    integer(kind=JPIM)            :: append_output_nsteps = DEF_APPEND_OUTPUT_NSTEPS

    ! -- points -------------------------------------------------------------
    integer(kind=JPIM)                   :: nb_points = 0
    type(scm_point_config), allocatable  :: points(:)

    ! -- environment --------------------------------------------------------
    character(len=:), allocatable :: output_dir
    logical                       :: output_dir_is_set = .false.
    character(len=:), allocatable :: vert_tables_namelist
    logical                       :: vert_tables_namelist_is_set = .false.
    character(len=:), allocatable :: config_file
    logical                       :: config_file_is_set = .false.
  contains
    procedure, pass(self) :: init     => config_handler_init
    procedure, pass(self) :: finalize => config_handler_finalize

    ! getters: core-config
    procedure, pass(self) :: get_run_every
    procedure, pass(self) :: get_init_step
    procedure, pass(self) :: get_final_step
    procedure, pass(self) :: get_dataid
    procedure, pass(self) :: get_delta
    procedure, pass(self) :: has_delta
    procedure, pass(self) :: get_append_output
    procedure, pass(self) :: get_append_output_nsteps

    ! getters: points
    procedure, pass(self) :: get_nb_points
    procedure, pass(self) :: get_point_id
    procedure, pass(self) :: get_point_name
    procedure, pass(self) :: get_point_lat
    procedure, pass(self) :: get_point_lon
    procedure, pass(self) :: get_point_timesteps
    procedure, pass(self) :: get_point_extract_always

    ! getters: environment
    procedure, pass(self) :: get_output_dir
    procedure, pass(self) :: has_output_dir
    procedure, pass(self) :: get_vert_tables_namelist
    procedure, pass(self) :: has_vert_tables_namelist
    procedure, pass(self) :: get_config_file
    procedure, pass(self) :: has_config_file

    procedure, pass(self) :: log_summary
  end type config_handler


  ! Case-insensitive typed lookups on an fckit configuration.
  interface config_get
    module procedure config_get_int
    module procedure config_get_real
    module procedure config_get_string
    module procedure config_get_int_array
    module procedure config_get_config_array
  end interface config_get


contains


  ! ========================================================================
  ! config_handler
  ! ========================================================================

  ! Parse, validate and log the whole plugin configuration.  Any error that
  ! makes the configuration unusable (no points, a point without coordinates,
  ! an out-of-range value) is fatal: continuing would either crash later or
  ! silently produce no output.
  subroutine config_handler_init(self, plugin_config)
    class(config_handler),     intent(inout) :: self
    type(fckit_configuration), intent(in)    :: plugin_config

    type(key_index) :: kidx
    type(fckit_configuration), allocatable :: points_config(:)

    integer(kind=JPIM) :: ipoint
    logical            :: found
    character(len=512) :: msg

    ! -- clean any prior state ----------------------------------------------
    call self%finalize()

    call kidx%build(plugin_config)
    call kidx%warn_unknown(CORE_KEYS, "core-config")

    ! -- scalar options -----------------------------------------------------
    self%run_every  = get_int_key(plugin_config, kidx, KEY_RUN_EVERY,  DEF_RUN_EVERY)
    self%init_step  = get_int_key(plugin_config, kidx, KEY_INIT_STEP,  DEF_INIT_STEP)
    self%final_step = get_int_key(plugin_config, kidx, KEY_FINAL_STEP, DEF_FINAL_STEP)

    self%append_output        = get_flag_key(plugin_config, kidx, KEY_APPEND_OUTPUT, DEF_APPEND_OUTPUT)
    self%append_output_nsteps = get_int_key(plugin_config, kidx, KEY_APPEND_OUTPUT_NSTEPS, &
      &                                     DEF_APPEND_OUTPUT_NSTEPS)

    found = config_get(plugin_config, kidx, KEY_DATAID, self%dataid)
    if (.not.found) self%dataid = DEF_DATAID

    self%delta        = 0.0_JPRB
    self%delta_is_set = config_get(plugin_config, kidx, KEY_DELTA, self%delta)

    ! -- checks on the scalar options ---------------------------------------
    if (self%run_every < 1) then
      write(msg,'(A,I0)') KEY_RUN_EVERY//" must be >= 1, but is ", self%run_every
      call fatal(msg)
    endif

    if (self%init_step < 0) then
      write(msg,'(A,I0)') KEY_INIT_STEP//" must be >= 0, but is ", self%init_step
      call fatal(msg)
    endif

    if (self%final_step >= 0 .and. self%final_step < self%init_step) then
      write(msg,'(A,I0,A,I0,A)') KEY_FINAL_STEP//" (", self%final_step, &
        & ") is smaller than "//KEY_INIT_STEP//" (", self%init_step, &
        & "): the plugin would never run"
      call fatal(msg)
    endif

    if (self%delta_is_set .and. self%delta <= 0.0_JPRB) then
      write(msg,'(A,E12.5)') KEY_DELTA//" must be > 0, but is ", self%delta
      call fatal(msg)
    endif

    if (self%append_output_nsteps < 0) then
      write(msg,'(A,I0,A)') "config_handler: "//KEY_APPEND_OUTPUT_NSTEPS//"=", &
        & self%append_output_nsteps, " is negative - treated as 0 (no batching)"
      call log%warning(msg)
      self%append_output_nsteps = 0
    endif

    if (.not.self%append_output .and. self%append_output_nsteps > 0) then
      call log%warning("config_handler: "//KEY_APPEND_OUTPUT_NSTEPS//" is ignored because " &
        & //KEY_APPEND_OUTPUT//"=0")
    endif

    ! -- points -------------------------------------------------------------
    found = config_get(plugin_config, kidx, KEY_POINTS, points_config)
    if (.not.found) then
      call fatal("no '"//KEY_POINTS//"' specified in the plugin configuration")
    endif

    self%nb_points = int(size(points_config), kind=JPIM)
    if (self%nb_points < 1) then
      call fatal("'"//KEY_POINTS//"' is empty: there is nothing to extract")
    endif

    allocate(self%points(self%nb_points))
    do ipoint = 1, self%nb_points
      call parse_point(self%points(ipoint), points_config(ipoint), ipoint)
    enddo

    ! -- environment --------------------------------------------------------
    call config_env_output_dir(self%output_dir, self%output_dir_is_set)
    call config_env_vert_tables_namelist(self%vert_tables_namelist, &
      &                                  self%vert_tables_namelist_is_set)
    call config_env_plume_config_file(self%config_file, self%config_file_is_set)

    call self%log_summary()

  end subroutine config_handler_init


  ! Parse and validate one entry of the "points" list.
  subroutine parse_point(point, point_config, ipoint)
    type(scm_point_config),    intent(inout) :: point
    type(fckit_configuration), intent(in)    :: point_config
    integer(kind=JPIM),        intent(in)    :: ipoint

    type(key_index) :: kidx

    integer(kind=c_int32_t)              :: scalar_step
    integer(kind=c_int32_t), allocatable :: steps(:)
    integer(kind=JPIM)                   :: id
    logical                              :: found
    character(len=64)                    :: context
    character(len=512)                   :: msg

    write(context,'(A,I0)') "points entry ", ipoint

    call kidx%build(point_config)
    call kidx%warn_unknown(POINT_KEYS, trim(context))

    ! -- identification (optional, used for logging only) -------------------
    id = get_int_key(point_config, kidx, KEY_ID, -1_JPIM)
    point%id = id

    found = config_get(point_config, kidx, KEY_NAME, point%name)
    if (.not.found) point%name = ""

    ! -- coordinates (required) ---------------------------------------------
    if (.not. config_get(point_config, kidx, KEY_LAT, point%lat)) then
      call fatal(trim(context)//" has no '"//KEY_LAT//"'")
    endif

    if (.not. config_get(point_config, kidx, KEY_LON, point%lon)) then
      call fatal(trim(context)//" has no '"//KEY_LON//"'")
    endif

    if (point%lat < -90.0_JPRB .or. point%lat > 90.0_JPRB) then
      write(msg,'(A,F12.5)') trim(context)//": '"//KEY_LAT//"' must be within [-90, 90], but is ", &
        & point%lat
      call fatal(msg)
    endif

    if (point%lon < -360.0_JPRB .or. point%lon > 360.0_JPRB) then
      write(msg,'(A,F12.5)') trim(context)//": '"//KEY_LON//"' must be within [-360, 360], but is ", &
        & point%lon
      call fatal(msg)
    endif

    ! negative longitudes are wrapped to the [0, 360) model convention
    if (point%lon < 0.0_JPRB) point%lon = 360.0_JPRB + point%lon

    ! -- extraction schedule ------------------------------------------------
    ! "timesteps" (list of ints) is the preferred form; "timestep" / "nstep"
    ! are the legacy scalar spellings.  No schedule at all, or any negative
    ! entry, means "extract at every step the plugin runs".
    found = config_get(point_config, kidx, KEY_TIMESTEPS, steps)

    if (.not.found) then
      scalar_step = 0
      found = config_get(point_config, kidx, KEY_TIMESTEP, scalar_step)
      if (.not.found) found = config_get(point_config, kidx, KEY_NSTEP, scalar_step)
      if (found) then
        allocate(steps(1))
        steps(1) = scalar_step
      endif
    endif

    if (.not.found) then
      point%extract_always = .true.
    else if (any(steps < 0)) then
      point%extract_always = .true.
    else
      ! duplicates are harmless: the schedule is queried by membership
      allocate(point%timesteps(size(steps)))
      point%timesteps(:) = int(steps, kind=JPIM)
    endif

    if (allocated(steps)) deallocate(steps)

  end subroutine parse_point


  ! Report the configuration as it is actually used, defaults included.
  subroutine log_summary(self)
    class(config_handler), intent(in) :: self

    integer(kind=JPIM) :: ipoint
    character(len=512) :: msg

    call log%info("config_handler: plugin configuration:")

    write(msg,'(A,I0)')  "config_handler:   "//KEY_RUN_EVERY//" = ", self%run_every
    call log%info(msg)
    write(msg,'(A,I0)')  "config_handler:   "//KEY_INIT_STEP//" = ", self%init_step
    call log%info(msg)
    if (self%final_step < 0) then
      write(msg,'(A,I0,A)') "config_handler:   "//KEY_FINAL_STEP//" = ", self%final_step, &
        & " (no limit)"
    else
      write(msg,'(A,I0)')   "config_handler:   "//KEY_FINAL_STEP//" = ", self%final_step
    endif
    call log%info(msg)
    call log%info("config_handler:   "//KEY_DATAID//" = "//trim(self%dataid))
    if (self%delta_is_set) then
      write(msg,'(A,F12.5)') "config_handler:   "//KEY_DELTA//" = ", self%delta
    else
      write(msg,'(A)') "config_handler:   "//KEY_DELTA//" not set - nearest point found with a kdtree search"
    endif
    call log%info(msg)
    write(msg,'(A,L1)')  "config_handler:   "//KEY_APPEND_OUTPUT//" = ", self%append_output
    call log%info(msg)
    write(msg,'(A,I0)')  "config_handler:   "//KEY_APPEND_OUTPUT_NSTEPS//" = ", self%append_output_nsteps
    call log%info(msg)

    write(msg,'(A,I0)')  "config_handler:   "//KEY_POINTS//" = ", self%nb_points
    call log%info(msg)
    do ipoint = 1, self%nb_points
      write(msg,'(A,I0,A,I0,2(A,F10.3),A)') "config_handler:     point ", ipoint, &
        & " ("//KEY_ID//"=", self%points(ipoint)%id, &
        & ", "//KEY_LAT//"=", self%points(ipoint)%lat, &
        & ", "//KEY_LON//"=", self%points(ipoint)%lon, &
        & ") "//trim(self%points(ipoint)%name)
      call log%info(msg)
    enddo

    if (self%output_dir_is_set) then
      call log%info("config_handler:   "//ENV_OUTPUT_DIR//" = "//trim(self%output_dir))
    else
      call log%info("config_handler:   "//ENV_OUTPUT_DIR//" not set - writing to the current directory")
    endif

    if (self%vert_tables_namelist_is_set) then
      call log%info("config_handler:   "//ENV_VERT_TABLES//" = "//trim(self%vert_tables_namelist))
    endif

    if (self%config_file_is_set) then
      call log%info("config_handler:   "//ENV_CONFIG_FILE//" = "//trim(self%config_file))
    endif

  end subroutine log_summary


  subroutine config_handler_finalize(self)
    class(config_handler), intent(inout) :: self

    integer(kind=JPIM) :: ipoint

    if (allocated(self%points)) then
      do ipoint = 1, size(self%points)
        if (allocated(self%points(ipoint)%timesteps)) deallocate(self%points(ipoint)%timesteps)
        if (allocated(self%points(ipoint)%name))      deallocate(self%points(ipoint)%name)
      enddo
      deallocate(self%points)
    endif
    self%nb_points = 0

    if (allocated(self%dataid))               deallocate(self%dataid)
    if (allocated(self%output_dir))           deallocate(self%output_dir)
    if (allocated(self%vert_tables_namelist)) deallocate(self%vert_tables_namelist)
    if (allocated(self%config_file))          deallocate(self%config_file)

    self%run_every            = DEF_RUN_EVERY
    self%init_step            = DEF_INIT_STEP
    self%final_step           = DEF_FINAL_STEP
    self%delta                = 0.0_JPRB
    self%delta_is_set         = .false.
    self%append_output        = DEF_APPEND_OUTPUT
    self%append_output_nsteps = DEF_APPEND_OUTPUT_NSTEPS

    self%output_dir_is_set           = .false.
    self%vert_tables_namelist_is_set = .false.
    self%config_file_is_set          = .false.

  end subroutine config_handler_finalize


  ! ========================================================================
  ! Getters: core-config
  ! ========================================================================

  function get_run_every(self) result(value)
    class(config_handler), intent(in) :: self
    integer(kind=JPIM) :: value
    value = self%run_every
  end function get_run_every


  function get_init_step(self) result(value)
    class(config_handler), intent(in) :: self
    integer(kind=JPIM) :: value
    value = self%init_step
  end function get_init_step


  function get_final_step(self) result(value)
    class(config_handler), intent(in) :: self
    integer(kind=JPIM) :: value
    value = self%final_step
  end function get_final_step


  function get_dataid(self) result(value)
    class(config_handler), intent(in) :: self
    character(len=:), allocatable :: value
    if (allocated(self%dataid)) then
      value = self%dataid
    else
      value = DEF_DATAID
    endif
  end function get_dataid


  ! Only meaningful when has_delta() is true.
  function get_delta(self) result(value)
    class(config_handler), intent(in) :: self
    real(kind=JPRB) :: value
    value = self%delta
  end function get_delta


  logical function has_delta(self)
    class(config_handler), intent(in) :: self
    has_delta = self%delta_is_set
  end function has_delta


  logical function get_append_output(self)
    class(config_handler), intent(in) :: self
    get_append_output = self%append_output
  end function get_append_output


  function get_append_output_nsteps(self) result(value)
    class(config_handler), intent(in) :: self
    integer(kind=JPIM) :: value
    value = self%append_output_nsteps
  end function get_append_output_nsteps


  ! ========================================================================
  ! Getters: points
  ! ========================================================================

  function get_nb_points(self) result(value)
    class(config_handler), intent(in) :: self
    integer(kind=JPIM) :: value
    value = self%nb_points
  end function get_nb_points


  function get_point_id(self, ipoint) result(value)
    class(config_handler), intent(in) :: self
    integer(kind=JPIM),    intent(in) :: ipoint
    integer(kind=JPIM) :: value
    call check_point_index(self, ipoint, "get_point_id")
    value = self%points(ipoint)%id
  end function get_point_id


  function get_point_name(self, ipoint) result(value)
    class(config_handler), intent(in) :: self
    integer(kind=JPIM),    intent(in) :: ipoint
    character(len=:), allocatable :: value
    call check_point_index(self, ipoint, "get_point_name")
    if (allocated(self%points(ipoint)%name)) then
      value = self%points(ipoint)%name
    else
      value = ""
    endif
  end function get_point_name


  function get_point_lat(self, ipoint) result(value)
    class(config_handler), intent(in) :: self
    integer(kind=JPIM),    intent(in) :: ipoint
    real(kind=JPRB) :: value
    call check_point_index(self, ipoint, "get_point_lat")
    value = self%points(ipoint)%lat
  end function get_point_lat


  ! Longitude wrapped to [0, 360), as expected by the grid search.
  function get_point_lon(self, ipoint) result(value)
    class(config_handler), intent(in) :: self
    integer(kind=JPIM),    intent(in) :: ipoint
    real(kind=JPRB) :: value
    call check_point_index(self, ipoint, "get_point_lon")
    value = self%points(ipoint)%lon
  end function get_point_lon


  ! The steps requested for this point.  A zero-sized result means either
  ! "extract at every step" (get_point_extract_always() is then true) or an
  ! explicitly empty schedule.
  function get_point_timesteps(self, ipoint) result(value)
    class(config_handler), intent(in) :: self
    integer(kind=JPIM),    intent(in) :: ipoint
    integer(kind=JPIM), allocatable :: value(:)
    call check_point_index(self, ipoint, "get_point_timesteps")
    if (allocated(self%points(ipoint)%timesteps)) then
      allocate(value(size(self%points(ipoint)%timesteps)))
      value(:) = self%points(ipoint)%timesteps(:)
    else
      allocate(value(0))
    endif
  end function get_point_timesteps


  logical function get_point_extract_always(self, ipoint)
    class(config_handler), intent(in) :: self
    integer(kind=JPIM),    intent(in) :: ipoint
    call check_point_index(self, ipoint, "get_point_extract_always")
    get_point_extract_always = self%points(ipoint)%extract_always
  end function get_point_extract_always


  ! ========================================================================
  ! Getters: environment
  ! ========================================================================

  function get_output_dir(self) result(value)
    class(config_handler), intent(in) :: self
    character(len=:), allocatable :: value
    if (allocated(self%output_dir)) then
      value = self%output_dir
    else
      value = ""
    endif
  end function get_output_dir


  logical function has_output_dir(self)
    class(config_handler), intent(in) :: self
    has_output_dir = self%output_dir_is_set
  end function has_output_dir


  function get_vert_tables_namelist(self) result(value)
    class(config_handler), intent(in) :: self
    character(len=:), allocatable :: value
    if (allocated(self%vert_tables_namelist)) then
      value = self%vert_tables_namelist
    else
      value = ""
    endif
  end function get_vert_tables_namelist


  logical function has_vert_tables_namelist(self)
    class(config_handler), intent(in) :: self
    has_vert_tables_namelist = self%vert_tables_namelist_is_set
  end function has_vert_tables_namelist


  function get_config_file(self) result(value)
    class(config_handler), intent(in) :: self
    character(len=:), allocatable :: value
    if (allocated(self%config_file)) then
      value = self%config_file
    else
      value = ""
    endif
  end function get_config_file


  logical function has_config_file(self)
    class(config_handler), intent(in) :: self
    has_config_file = self%config_file_is_set
  end function has_config_file


  ! ========================================================================
  ! Environment accessors
  !
  ! Also usable without a config_handler instance, for the drivers that need
  ! an environment variable before any configuration exists (the plume
  ! configuration file is read before the plugin is even loaded).
  ! ========================================================================

  subroutine config_env_output_dir(value, found)
    character(len=:), allocatable, intent(out) :: value
    logical,                       intent(out) :: found
    call get_env(ENV_OUTPUT_DIR, value, found)
  end subroutine config_env_output_dir


  subroutine config_env_vert_tables_namelist(value, found)
    character(len=:), allocatable, intent(out) :: value
    logical,                       intent(out) :: found
    call get_env(ENV_VERT_TABLES, value, found)
  end subroutine config_env_vert_tables_namelist


  subroutine config_env_plume_config_file(value, found)
    character(len=:), allocatable, intent(out) :: value
    logical,                       intent(out) :: found
    call get_env(ENV_CONFIG_FILE, value, found)
  end subroutine config_env_plume_config_file


  ! Read an environment variable into a right-sized string.  A variable that
  ! is unset or set to an empty value is reported as not found.
  subroutine get_env(name, value, found)
    character(len=*),              intent(in)  :: name
    character(len=:), allocatable, intent(out) :: value
    logical,                       intent(out) :: found

    integer :: value_len, env_status

    found = .false.
    call get_environment_variable(name, length=value_len, status=env_status)

    if (env_status /= 0 .or. value_len <= 0) then
      value = ""
      return
    endif

    allocate(character(len=value_len) :: value)
    call get_environment_variable(name, value, status=env_status)

    if (env_status /= 0) then
      deallocate(value)
      value = ""
      return
    endif

    found = .true.

  end subroutine get_env


  ! ========================================================================
  ! key_index
  ! ========================================================================

  ! Collect the keys of a configuration.  fckit's size() is the number of
  ! entries and key(i) their spelling in the file.
  subroutine key_index_build(self, config)
    class(key_index),          intent(inout) :: self
    type(fckit_configuration), intent(in)    :: config

    integer(kind=c_int32_t) :: nkeys, ikey
    character(len=:), allocatable :: key_str

    if (allocated(self%keys)) deallocate(self%keys)

    nkeys = config%size()
    if (nkeys < 0) nkeys = 0

    allocate(self%keys(nkeys))

    do ikey = 1, nkeys
      key_str = config%key(ikey)
      ! keys longer than MAX_KEY_LEN are truncated: they cannot match any of
      ! the keys the plugin knows about anyway
      self%keys(ikey) = adjustl(key_str)
    enddo

  end subroutine key_index_build


  ! Return the key as spelled in the configuration that matches name
  ! case-insensitively, or a zero-length string when there is no match.
  function key_index_resolve(self, name) result(actual)
    class(key_index), intent(in) :: self
    character(len=*), intent(in) :: name
    character(len=:), allocatable :: actual

    integer :: ikey

    actual = ""
    if (.not.allocated(self%keys)) return

    do ikey = 1, size(self%keys)
      if (to_lower(trim(self%keys(ikey))) == to_lower(trim(name))) then
        actual = trim(self%keys(ikey))
        return
      endif
    enddo

  end function key_index_resolve


  ! Warn about keys the plugin does not know: a mistyped key would otherwise
  ! be silently replaced by its default.
  subroutine key_index_warn_unknown(self, known_keys, context)
    class(key_index),           intent(in) :: self
    character(len=MAX_KEY_LEN), intent(in) :: known_keys(:)
    character(len=*),           intent(in) :: context

    integer :: ikey, iknown
    logical :: known

    if (.not.allocated(self%keys)) return

    do ikey = 1, size(self%keys)
      known = .false.
      do iknown = 1, size(known_keys)
        if (to_lower(trim(self%keys(ikey))) == trim(known_keys(iknown))) then
          known = .true.
          exit
        endif
      enddo
      if (.not.known) then
        call log%warning("config_handler: unknown key '"//trim(self%keys(ikey))// &
          & "' in "//trim(context)//" - ignored")
      endif
    enddo

  end subroutine key_index_warn_unknown


  ! ========================================================================
  ! Case-insensitive typed lookups
  ! ========================================================================

  function config_get_int(config, kidx, name, value) result(found)
    type(fckit_configuration), intent(in)    :: config
    type(key_index),           intent(in)    :: kidx
    character(len=*),          intent(in)    :: name
    integer(kind=c_int32_t),   intent(inout) :: value
    logical :: found

    character(len=:), allocatable :: actual

    found  = .false.
    actual = kidx%resolve(name)
    if (len(actual) == 0) return
    found = config%get(actual, value)

  end function config_get_int


  function config_get_real(config, kidx, name, value) result(found)
    type(fckit_configuration), intent(in)    :: config
    type(key_index),           intent(in)    :: kidx
    character(len=*),          intent(in)    :: name
    real(kind=JPRB),           intent(inout) :: value
    logical :: found

    character(len=:), allocatable :: actual

    found  = .false.
    actual = kidx%resolve(name)
    if (len(actual) == 0) return
    found = config%get(actual, value)

  end function config_get_real


  function config_get_string(config, kidx, name, value) result(found)
    type(fckit_configuration),     intent(in)    :: config
    type(key_index),               intent(in)    :: kidx
    character(len=*),              intent(in)    :: name
    character(len=:), allocatable, intent(inout) :: value
    logical :: found

    character(len=:), allocatable :: actual

    found  = .false.
    actual = kidx%resolve(name)
    if (len(actual) == 0) return
    found = config%get(actual, value)

  end function config_get_string


  function config_get_int_array(config, kidx, name, value) result(found)
    type(fckit_configuration),            intent(in)    :: config
    type(key_index),                      intent(in)    :: kidx
    character(len=*),                     intent(in)    :: name
    integer(kind=c_int32_t), allocatable, intent(inout) :: value(:)
    logical :: found

    character(len=:), allocatable :: actual

    found  = .false.
    actual = kidx%resolve(name)
    if (len(actual) == 0) return
    found = config%get(actual, value)

  end function config_get_int_array


  function config_get_config_array(config, kidx, name, value) result(found)
    type(fckit_configuration),              intent(in)    :: config
    type(key_index),                        intent(in)    :: kidx
    character(len=*),                       intent(in)    :: name
    type(fckit_configuration), allocatable, intent(inout) :: value(:)
    logical :: found

    character(len=:), allocatable :: actual

    found  = .false.
    actual = kidx%resolve(name)
    if (len(actual) == 0) return
    found = config%get(actual, value)

  end function config_get_config_array


  ! ========================================================================
  ! Helpers
  ! ========================================================================

  ! Integer option with a default.
  function get_int_key(config, kidx, name, default_value) result(value)
    type(fckit_configuration), intent(in) :: config
    type(key_index),           intent(in) :: kidx
    character(len=*),          intent(in) :: name
    integer(kind=JPIM),        intent(in) :: default_value
    integer(kind=JPIM) :: value

    integer(kind=c_int32_t) :: ivalue

    ivalue = int(default_value, kind=c_int32_t)
    if (config_get(config, kidx, name, ivalue)) then
      value = int(ivalue, kind=JPIM)
    else
      value = default_value
    endif

  end function get_int_key


  ! Boolean option written as an integer in the configuration (0 / 1).
  function get_flag_key(config, kidx, name, default_value) result(value)
    type(fckit_configuration), intent(in) :: config
    type(key_index),           intent(in) :: kidx
    character(len=*),          intent(in) :: name
    logical,                   intent(in) :: default_value
    logical :: value

    integer(kind=c_int32_t) :: ivalue
    character(len=512)      :: msg

    value  = default_value
    ivalue = 0

    if (.not. config_get(config, kidx, name, ivalue)) return

    if (ivalue /= 0 .and. ivalue /= 1) then
      write(msg,'(A,I0,A,L1)') "config_handler: "//trim(name)//" should be 0 or 1, but is ", &
        & ivalue, " - treated as ", (ivalue /= 0)
      call log%warning(msg)
    endif

    value = (ivalue /= 0)

  end function get_flag_key


  ! Guard against a point index that no longer matches the configuration:
  ! every caller loops up to get_nb_points(), so this can only trigger on a
  ! programming error.
  subroutine check_point_index(self, ipoint, caller)
    class(config_handler), intent(in) :: self
    integer(kind=JPIM),    intent(in) :: ipoint
    character(len=*),      intent(in) :: caller

    character(len=512) :: msg

    if (.not.allocated(self%points) .or. ipoint < 1 .or. ipoint > self%nb_points) then
      write(msg,'(A,I0,A,I0)') trim(caller)//": point index out of range: ", ipoint, &
        & ", nb_points = ", self%nb_points
      call fatal(msg)
    endif

  end subroutine check_point_index


  ! Configuration errors are not recoverable: without a usable configuration
  ! the plugin would either produce no output at all or crash later on.
  subroutine fatal(msg)
    character(len=*), intent(in) :: msg
    call log%error("config_handler: "//trim(msg))
    stop 1
  end subroutine fatal


  function to_lower(str) result(lower)
    character(len=*), intent(in) :: str
    character(len=len(str))      :: lower

    integer :: i, ic

    lower = str
    do i = 1, len(str)
      ic = iachar(str(i:i))
      if (ic >= iachar('A') .and. ic <= iachar('Z')) lower(i:i) = achar(ic + 32)
    enddo

  end function to_lower


end module config_handler_mod
