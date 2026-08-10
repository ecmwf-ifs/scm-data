! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

module scm_nc_output_mod

  use fckit_log_module,           only : log
  use fckit_configuration_module, only : fckit_configuration

  use yomvar, only : JPIM, JPRB, TLOCATION, TINFO

  use extraction_manager_mod, only : extraction_manager

#ifdef WITH_SCM_PLUME_PLUGIN_PROFILER
  use plugin_profiler_mod
#endif

#include "profiler_macros.h"

  implicit none

  private

  public :: output_writer

  ! Encapsulates state and behavior for writing SCM plugin extraction-point
  ! NetCDF output files.
  !
  ! Filename schemes:
  !   append_output = .true., nsteps_per_file <= 0 (default):
  !     scm_in_proc_<myproc>_pt_<iloc>.nc
  !     Each call opens the existing file (if any) and appends a new time
  !     record at the next unlimited-time index.
  !   append_output = .true., nsteps_per_file > 0:
  !     scm_in_proc_<myproc>_pt_<iloc>_step_<first>_to_<last>.nc
  !     Steps are batched into files covering consecutive windows of
  !     nsteps_per_file executed steps (see batch_window below), so a file
  !     holds at most nsteps_per_file time records.
  !   append_output = .false.:
  !     scm_in_proc_<myproc>_pt_<iloc>_step_<nstep>.nc
  !     One file per (proc, location, step) - legacy behavior.
  type :: output_writer
    private
    character(len=1024) :: output_dir     = ''
    logical             :: has_output_dir = .false.
    logical             :: append_output  = .true.
    ! Maximum number of steps (time records) batched into one file.
    ! <= 0 means "no limit": a single file per (proc, location).
    integer(kind=JPIM)  :: nsteps_per_file = 0
    ! Step spacing between two consecutive executed steps (RUN_EVERY) and
    ! first executed step: together they define the batch windows.
    integer(kind=JPIM)  :: step_stride     = 1
    integer(kind=JPIM)  :: anchor_step     = 0
  contains
    procedure, pass(self) :: init     => output_writer_init
    procedure, pass(self) :: write    => output_writer_write
    procedure, pass(self) :: finalize => output_writer_finalize
  end type output_writer

contains


  ! run_every / init_step are the plugin RUN_EVERY and INIT_STEP settings: they
  ! determine at which steps this writer is called and hence how the batch
  ! windows are laid out when APPEND_OUTPUT_NSTEPS is used.
  subroutine output_writer_init(self, plugin_config, run_every, init_step)
    class(output_writer),        intent(inout) :: self
    type(fckit_configuration),   intent(in)    :: plugin_config
    integer,                     intent(in)    :: run_every
    integer,                     intent(in)    :: init_step

    integer :: append_int
    integer :: nsteps_int
    integer :: env_status
    logical :: found
    character(len=512) :: msg

    ! APPEND_OUTPUT: 0 -> one file per step (legacy), 1 -> append to a single
    ! file per (proc, location).  Default: append (1).
    found = plugin_config%get("APPEND_OUTPUT", append_int)
    if (found) then
      self%append_output = (append_int /= 0)
    else
      self%append_output = .true.
    endif

    ! APPEND_OUTPUT_NSTEPS: maximum number of steps batched into one file when
    ! appending.  Absent or <= 0 -> no limit (a single file per location).
    found = plugin_config%get("APPEND_OUTPUT_NSTEPS", nsteps_int)
    if (.not.found) nsteps_int = 0

    ! The plugin runs at the multiples of RUN_EVERY that are >= INIT_STEP: the
    ! windows are anchored at the first of those steps and are step_stride
    ! apart, so all points share the same file boundaries.
    self%step_stride = max(1, int(run_every, kind=JPIM))
    self%anchor_step = max(0, int(init_step, kind=JPIM))
    if (mod(self%anchor_step, self%step_stride) /= 0) then
      self%anchor_step = (self%anchor_step / self%step_stride + 1) * self%step_stride
    endif

    if (self%append_output) then
      self%nsteps_per_file = max(0, int(nsteps_int, kind=JPIM))
      ! A window spans nsteps_per_file*step_stride steps: fall back to a single
      ! file rather than overflowing on absurdly large batch sizes.
      if (self%nsteps_per_file > huge(self%nsteps_per_file) / self%step_stride) then
        write(msg,'(A,I0,A)') "scm_nc_output: APPEND_OUTPUT_NSTEPS=", nsteps_int, &
          & " is too large - batching disabled (single file per location)"
        call log%warning(msg)
        self%nsteps_per_file = 0
      endif
    else
      if (nsteps_int > 0) then
        write(msg,'(A)') "scm_nc_output: APPEND_OUTPUT_NSTEPS ignored because APPEND_OUTPUT=0"
        call log%warning(msg)
      endif
      self%nsteps_per_file = 0
    endif

    if (self%append_output) then
      if (self%nsteps_per_file > 0) then
        write(msg,'(A,I0,A,I0,A,I0,A)') &
          & "scm_nc_output: APPEND_OUTPUT=1, APPEND_OUTPUT_NSTEPS=", self%nsteps_per_file, &
          & " - up to ", self%nsteps_per_file, " steps per file per (proc, location), windows of ", &
          & self%nsteps_per_file * self%step_stride, " steps"
      else
        write(msg,'(A)') "scm_nc_output: APPEND_OUTPUT=1 - one file per (proc, location)"
      endif
    else
      write(msg,'(A)') "scm_nc_output: APPEND_OUTPUT=0 - one file per (proc, location, step)"
    endif
    call log%info(msg)

    ! Output directory taken from the PLUME_PLUGINS_OUTPUT_DIR env variable.
    call get_environment_variable("PLUME_PLUGINS_OUTPUT_DIR", self%output_dir, status=env_status)
    self%has_output_dir = (env_status == 0)

    if (self%has_output_dir) then
      write(msg,'(A,A)') "scm_nc_output: output directory = ", trim(self%output_dir)
      call log%info(msg)
    else
      write(msg,'(A)') "scm_nc_output: PLUME_PLUGINS_OUTPUT_DIR not set, writing to current directory"
      call log%info(msg)
    endif

  end subroutine output_writer_init


  subroutine output_writer_write(self, myproc, nstep, locations, nb_locations, &
                                 pvah, pvbh, dataid, nlev, info, extract_mgr)
    class(output_writer),      intent(inout) :: self
    integer(kind=JPIM),        intent(in)    :: myproc
    integer(kind=JPIM),        intent(in)    :: nstep
    integer(kind=JPIM),        intent(in)    :: nb_locations
    type(TLOCATION),           intent(inout) :: locations(nb_locations)
    real(kind=JPRB),           intent(in)    :: pvah(0:nlev)
    real(kind=JPRB),           intent(in)    :: pvbh(0:nlev)
    character(len=*),          intent(in)    :: dataid
    integer(kind=JPIM),        intent(in)    :: nlev
    type(TINFO),               intent(in)    :: info
    type(extraction_manager),  intent(in)    :: extract_mgr

    integer(kind=JPIM)              :: ii, iloc
    integer(kind=JPIM), allocatable :: ilocs(:)
    character(len=:), allocatable   :: nc_fullpath
    character(len=512)              :: msg

    DECLARE_PLUGIN_TIMER(ih_nc_open)
    DECLARE_PLUGIN_TIMER(ih_nc_append)

#include "su_wrt_nc.h"
#include "wrt1c_nc.h"

    ! Registered outside the loop below: the loop body is skipped entirely on ranks
    ! that own none of the extraction points, and every rank must end up with the
    ! same set of timer names for the collective report to line up.
    REGISTER_PLUGIN_TIMER(ih_nc_open,   "scm_run.write_netcdf.open")
    REGISTER_PLUGIN_TIMER(ih_nc_append, "scm_run.write_netcdf.append")

    ilocs = extract_mgr%get_points_at_step(nstep)

    do ii = 1, size(ilocs)
      iloc = ilocs(ii)
      if ( myproc /= locations(iloc)%iproc ) cycle

      write(msg,'(A)')       " setting up output fields to netcdf  ";               call log%debug(msg)
      write(msg,'(A,I0)')    " loc processor ", locations(iloc)%IPROC;              call log%debug(msg)
      write(msg,'(A,I0)')    " loc knode ",     locations(iloc)%ILOC;               call log%debug(msg)
      write(msg,'(A,F8.4)')  " loc latitude ",  locations(iloc)%RLATI;              call log%debug(msg)
      write(msg,'(A,F8.4)')  " loc longitude ", locations(iloc)%RLONI;              call log%debug(msg)
      write(msg,'(A,F8.4)')  " loc pressure ",  locations(iloc)%PP%PLNSP;           call log%debug(msg)

      ! assemble the (possibly path-qualified) filename
      nc_fullpath = build_filename(self, myproc, iloc, nstep)

      ! setup the output NetCDF file (creates if missing, opens if present)
      START_PLUGIN_TIMER_H(ih_nc_open)
      call SU_WRT_NC(nc_fullpath, pvah, pvbh, dataid, locations(iloc)%IFILE_ID, nlev)
      STOP_PLUGIN_TIMER_H(ih_nc_open)

      ! write data to the NetCDF file
      write(msg,'(A,I0,1X,I0)') " writing output fields to netcdf  ", nstep, info%IDATE
      call log%debug(msg)
      START_PLUGIN_TIMER_H(ih_nc_append)
      call WRT1C_NC(locations(iloc), pvah, pvbh, info, locations(iloc)%IFILE_ID, nlev)
      STOP_PLUGIN_TIMER_H(ih_nc_append)

      deallocate(nc_fullpath)
    enddo

    if (allocated(ilocs)) deallocate(ilocs)

  end subroutine output_writer_write


  subroutine output_writer_finalize(self)
    class(output_writer), intent(inout) :: self

    ! Nothing to release at present; kept for symmetry / future cleanup.
    self%has_output_dir  = .false.
    self%output_dir      = ''
    self%nsteps_per_file = 0
    self%step_stride     = 1
    self%anchor_step     = 0

  end subroutine output_writer_finalize


  ! Build the filename (with optional output-directory prefix) for a given
  ! (proc, location, step), honouring the append_output flag.
  function build_filename(self, myproc, iloc, nstep) result(nc_fullpath)
    class(output_writer),          intent(in) :: self
    integer(kind=JPIM),            intent(in) :: myproc
    integer(kind=JPIM),            intent(in) :: iloc
    integer(kind=JPIM),            intent(in) :: nstep
    character(len=:), allocatable             :: nc_fullpath

    character(len=64) :: nc_filename
    integer(kind=JPIM) :: step_first, step_last

    if (self%append_output .and. self%nsteps_per_file > 0) then
      ! One file per (proc, location, batch of steps); the name carries the
      ! window of steps the file can hold.
      call batch_window(self, nstep, step_first, step_last)
      write(nc_filename, "(A,I5.5,A,I5.5,A,I5.5,A,I5.5,A)") &
        & 'scm_in_proc_', myproc, '_pt_', iloc, '_step_', step_first, '_to_', step_last, '.nc'
    else if (self%append_output) then
      ! One file per (proc, location); new time records are appended.
      write(nc_filename, "(A,I5.5,A,I5.5,A)") &
        & 'scm_in_proc_', myproc, '_pt_', iloc, '.nc'
    else
      ! Legacy: one file per (proc, location, step).
      write(nc_filename, "(A,I5.5,A,I5.5,A,I5.5,A)") &
        & 'scm_in_proc_', myproc, '_pt_', iloc, '_step_', nstep, '.nc'
    endif

    if (self%has_output_dir) then
      nc_fullpath = trim(self%output_dir) // '/' // trim(nc_filename)
    else
      nc_fullpath = trim(nc_filename)
    endif

  end function build_filename


  ! Batch window containing nstep: files cover consecutive windows of
  ! nsteps_per_file executed steps, i.e. nsteps_per_file*step_stride model
  ! steps, anchored at anchor_step (the first step the plugin runs at):
  !
  !   [anchor, anchor+W), [anchor+W, anchor+2W), ...   W = nsteps*stride
  !
  ! step_first / step_last are the first and last step the window can hold.
  ! A file holds fewer records than that when the point is only extracted at
  ! some of the steps of the window, or when the run stops mid-window.
  subroutine batch_window(self, nstep, step_first, step_last)
    class(output_writer), intent(in)  :: self
    integer(kind=JPIM),   intent(in)  :: nstep
    integer(kind=JPIM),   intent(out) :: step_first
    integer(kind=JPIM),   intent(out) :: step_last

    integer(kind=JPIM) :: window, offset, ibatch

    window = self%nsteps_per_file * self%step_stride
    offset = nstep - self%anchor_step

    ! Floor division, so that steps before the anchor (points explicitly
    ! scheduled below INIT_STEP never reach here, but be safe) still map to a
    ! well-defined window.
    if (offset >= 0) then
      ibatch = offset / window
    else
      ibatch = -((-offset + window - 1) / window)
    endif

    step_first = self%anchor_step + ibatch * window
    step_last  = step_first + window - self%step_stride

  end subroutine batch_window


end module scm_nc_output_mod
