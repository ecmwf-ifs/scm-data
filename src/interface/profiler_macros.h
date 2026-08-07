
#ifdef WITH_SCM_PLUME_PLUGIN_PROFILER

! Note: these macros deliberately do not pass __FILE__ through to the profiler.
! Expanding a long absolute build path inline would push the generated statement
! past the 132-column free-form Fortran limit. The fully qualified region name is
! unambiguous on its own, and it is what atlas' trace report keys on.

! Name-based API: convenient, but does a linear name lookup on every call.
! Use it for coarse regions entered a handful of times per step.
#define START_PLUGIN_TIMER(name) call start_timer(name)
#define STOP_PLUGIN_TIMER(name)  call stop_timer(name)

! Handle-based API: register the name once (outside the hot path), then start/stop
! by integer handle. Use it for regions inside loops, where the name lookup would
! otherwise show up in the measurement itself.
#define DECLARE_PLUGIN_TIMER(h)  integer, save :: h = 0
#define REGISTER_PLUGIN_TIMER(h,name) h = register_timer(name)
#define START_PLUGIN_TIMER_H(h)  call start_timer_h(h)
#define STOP_PLUGIN_TIMER_H(h)   call stop_timer_h(h)

#define PRINT_PLUGIN_TIMER(comm) call print_timers(comm)
#define RESET_PLUGIN_TIMER()     call reset_timers()

#else

#define START_PLUGIN_TIMER(name)
#define STOP_PLUGIN_TIMER(name)
#define DECLARE_PLUGIN_TIMER(h)
#define REGISTER_PLUGIN_TIMER(h,name)
#define START_PLUGIN_TIMER_H(h)
#define STOP_PLUGIN_TIMER_H(h)
#define PRINT_PLUGIN_TIMER(comm)
#define RESET_PLUGIN_TIMER()

#endif
