
#ifdef WITH_SCM_PLUME_PLUGIN_PROFILER
#define START_PLUGIN_TIMER(name) call start_timer(name)
#define STOP_PLUGIN_TIMER(name)  call stop_timer(name)
#define PRINT_PLUGIN_TIMER(comm) call print_timers(comm)
#else
#define START_PLUGIN_TIMER(name)
#define STOP_PLUGIN_TIMER(name)
#define PRINT_PLUGIN_TIMER(comm)
#endif
