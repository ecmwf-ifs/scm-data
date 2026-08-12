! (C) Copyright 2024- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

module scm_nc_output_mod

  use fckit_log_module, only : log

  use yomvar, only : JPIM, JPRB, TLOCATION, TINFO

  use config_handler_mod,     only : config_handler
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
    character(len=:), allocatable :: output_dir
    logical             :: has_output_dir = .false.
    logical             :: append_output  = .true.
    ! Maximum number of steps (time records) batched into one file.
    ! <= 0 means "no limit": a single file per (proc, location).
    integer(kind=JPIM)  :: nsteps_per_file = 0
    ! Step spacing between two consecutive executed steps (run_every) and
    ! first executed step: together they define the batch windows.
    integer(kind=JPIM)  :: step_stride     = 1
    integer(kind=JPIM)  :: anchor_step     = 0
  contains
    procedure, pass(self) :: init     => output_writer_init
    procedure, pass(self) :: write    => output_writer_write
    procedure, pass(self) :: finalize => output_writer_finalize
  end type output_writer

contains


  ! Everything the writer needs comes from the configuration handler: the
  ! append_output / append_output_nsteps options, and run_every / init_step,
  ! which determine at which steps this writer is called and hence how the
  ! batch windows are laid out when append_output_nsteps is used.
  subroutine output_writer_init(self, cfg)
    class(output_writer),  intent(inout) :: self
    type(config_handler),  intent(in)    :: cfg

    integer(kind=JPIM) :: nsteps_int
    character(len=512) :: msg

    ! append_output: 0 -> one file per step (legacy), 1 -> append to a single
    ! file per (proc, location).
    self%append_output = cfg%get_append_output()

    ! append_output_nsteps: maximum number of steps batched into one file when
    ! appending.  0 -> no limit (a single file per location).
    nsteps_int = cfg%get_append_output_nsteps()

    ! The plugin runs at the multiples of run_every that are >= init_step: the
    ! windows are anchored at the first of those steps and are step_stride
    ! apart, so all points share the same file boundaries.
    self%step_stride = max(1_JPIM, cfg%get_run_every())
    self%anchor_step = max(0_JPIM, cfg%get_init_step())
    if (mod(self%anchor_step, self%step_stride) /= 0) then
      self%anchor_step = (self%anchor_step / self%step_stride + 1) * self%step_stride
    endif

    if (self%append_output) then
      self%nsteps_per_file = max(0_JPIM, nsteps_int)
      ! A window spans nsteps_per_file*step_stride steps: fall back to a single
      ! file rather than overflowing on absurdly large batch sizes.
      if (self%nsteps_per_file > huge(self%nsteps_per_file) / self%step_stride) then
        write(msg,'(A,I0,A)') "scm_nc_output: append_output_nsteps=", nsteps_int, &
          & " is too large - batching disabled (single file per location)"
        call log%warning(msg)
        self%nsteps_per_file = 0
      endif
    else
      ! append_output_nsteps is meaningless without appending; the handler
      ! already warned about the combination.
      self%nsteps_per_file = 0
    endif

    if (self%append_output) then
      if (self%nsteps_per_file > 0) then
        write(msg,'(A,I0,A,I0,A,I0,A)') &
          & "scm_nc_output: append_output=1, append_output_nsteps=", self%nsteps_per_file, &
          & " - up to ", self%nsteps_per_file, " steps per file per (proc, location), windows of ", &
          & self%nsteps_per_file * self%step_stride, " steps"
      else
        write(msg,'(A)') "scm_nc_output: append_output=1 - one file per (proc, location)"
      endif
    else
      write(msg,'(A)') "scm_nc_output: append_output=0 - one file per (proc, location, step)"
    endif
    call log%info(msg)

    ! Output directory: resolved by the configuration handler from the
    ! environment (see PLUME_PLUGINS_OUTPUT_DIR).
    self%has_output_dir = cfg%has_output_dir()
    self%output_dir     = cfg%get_output_dir()

    if (self%has_output_dir) then
      write(msg,'(A,A)') "scm_nc_output: output directory = ", trim(self%output_dir)
      call log%info(msg)
    else
      write(msg,'(A)') "scm_nc_output: no output directory set, writing to the current directory"
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

    self%has_output_dir  = .false.
    if (allocated(self%output_dir)) deallocate(self%output_dir)
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
    ! scheduled below init_step never reach here, but be safe) still map to a
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
