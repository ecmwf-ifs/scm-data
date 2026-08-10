# Plume plugin: Single Column Model Data

This Plume plugin extracts IFS model fields at a user-defined set of latitude/longitude locations and writes the output to NetCDF files in a format compatible with the Single Column Model (SCM). For more information about the Plume plugin system and instructions on how to run plugins, see the [Plume](https://github.com/ecmwf/plume) documentation.


## Compilation options

These options are defined in the top-level [CMakeLists.txt](../../CMakeLists.txt) and can be passed to `cmake` as `-DENABLE_<option>=ON|OFF`.

| Option | Description | Default |
|---|---|---|
| `SCM_TOOLS` | Build the command line tools | `ON` |
| `SCM_TESTS` | Build SCM tests | `ON` |
| `NETCDF` | Support for NetCDF format | `ON` |
| `OMP` | Enable OpenMP support for SCM-DATA | `OFF` |
| `SCM_PLUME_PLUGIN` | Build the plume plugin (requires `plume`; `atlas` is always required) | `OFF` |
| `SCM_PLUME_PLUGIN_PROFILER` | Enable the plugin profiler (requires `SCM_PLUME_PLUGIN`). See [Profiling](#profiling) | `OFF` |
| `SCM_GRIB2_FIELDS` | Read soil temperature/moisture/ice-temperature as multi-level fields (`sot`/`vsw`/`sit`) instead of the twelve single-level fields `stl1..4`/`swvl1..4`/`istl1..4` (requires `SCM_PLUME_PLUGIN`) | `ON` |
| `SINGLE_PRECISION` | Build the SCM tools and the plugin in single precision | `OFF` |
| `PLUME_PLUGINS_SINGLE_PRECISION` | Build the plugin variant for single precision fields | `OFF` |
| `PLUME_PLUGINS_DOUBLE_PRECISION` | Build the plugin variant for double precision fields | `ON` |

## Runtime configuration options

The plugin is configured through a plume JSON configuration file, under
`core-config` for the plugin instance, and per-point under `points`.

### `core-config` options

| Option | Type | Description | Default |
|---|---|---|---|
| `LPROGNOSTIC` | int (0/1) | Whether the fields extracted are prognostic (used for output metadata) | - |
| `LAREA` | int (0/1) | Whether the fields extracted are area-averaged (used for output metadata) | - |
| `RUN_EVERY` | int | Run the plugin every `N` time steps | `1` |
| `INIT_STEP` | int | First time step at which the plugin runs | `0` |
| `FINAL_STEP` | int | Last time step at which the plugin runs (`-1` = no limit) | `-1` |
| `DATAID` | string | Identifier used to tag the extracted data / output | - |
| `DELTA` | real | Maximum search radius (degrees) used to match a point to the nearest model grid point. If not specified, a kdtree search for the nearest point is used instead | - |
| `APPEND_OUTPUT` | int (0/1) | `1`: append every extraction to a single NetCDF file per (proc, location) named `scm_in_proc_<myproc>_pt_<iloc>.nc`. `0`: write one file per (proc, location, step) named `scm_in_proc_<myproc>_pt_<iloc>_step_<nstep>.nc` | `1` |
| `APPEND_OUTPUT_NSTEPS` | int | Maximum number of steps batched into one appended file (see below). `0` or negative means no limit, i.e. a single file per (proc, location). Only used when `APPEND_OUTPUT=1` | `0` |
| `points` | array | List of point definitions (see below) | - |

#### Batching appended output

With `APPEND_OUTPUT=1` and `APPEND_OUTPUT_NSTEPS=N` (`N > 0`), the extractions
are batched into files holding at most `N` time records each, named after the
window of steps they cover:

```
scm_in_proc_00001_pt_00012_step_00004_to_00023.nc
```

The windows are `N * RUN_EVERY` model steps wide and are anchored at the first
step the plugin runs at (the first multiple of `RUN_EVERY` greater than or equal
to `INIT_STEP`), so all points share the same file boundaries and the file names
are reproducible from the configuration alone. With `RUN_EVERY=1`,
`INIT_STEP=4` and `APPEND_OUTPUT_NSTEPS=20`, the windows are steps `4-23`,
`24-43`, ...

The name gives the step window, not the content: a file holds fewer than `N`
records when the point is only extracted at some of the steps of the window
(per-point `timesteps`), or when the run stops before the window is complete.

### `points` entry options

| Option | Type | Description | Default |
|---|---|---|---|
| `ID` | int | Point identifier | - |
| `name` | string | Optional point name (for readability) | - |
| `lat` | real | Latitude of the point | - |
| `lon` | real | Longitude of the point (negative values are wrapped to 0-360) | - |
| `timesteps` | array of int | List of time steps at which this point is extracted (preferred form) | always extract |
| `timestep` | int | Single time step at which this point is extracted (legacy, use `timesteps` instead) | always extract |
| `nstep` | int | Same as `timestep` (legacy alias) | always extract |

If none of `timesteps`, `timestep` or `nstep` is set for a point, it is extracted at every time step the plugin runs.

The output directory can be set with the `PLUME_PLUGINS_OUTPUT_DIR` environment variable.

## Example configurations

Extracting data from a small number of points, at every time step the plugin runs:

```jsonc
{
    "plugins": [
        {
            "name": "PluginSCMData",
            "lib": "plugin_scm_data_dp",
            "core-config": {
                "LAREA": 0,
                "LPROGNOSTIC": 1,
                "RUN_EVERY": 1,
                "APPEND_OUTPUT": 1,
                "DATAID": "data_ifs_test_t21",
                "DELTA": 6.0,
                "points": [
                    { "ID": 0, "lat": 33.33, "lon": 7.77 },
                    { "ID": 1, "lat": 44.44, "lon": 8.88 },
                    { "ID": 2, "lat": 66.66, "lon": 9.99 }
                ]
            }
        }
    ]
}
```

Extracting data from a few points, each one only at specific time steps:

```jsonc
{
    "plugins": [
        {
            "name": "PluginSCMData",
            "lib": "plugin_scm_data_dp",
            "core-config": {
                "LAREA": 0,
                "LPROGNOSTIC": 1,
                "APPEND_OUTPUT": 0,
                "DATAID": "test_timestep_filter",
                "DELTA": 0.75,
                "points": [
                    { "ID": 1, "lat": 11.11, "lon": 22.22, "timesteps": [1, 3, 5] },
                    { "ID": 2, "lat": 33.33, "lon": 44.44, "timesteps": [2, 4, 6] },
                    { "ID": 3, "lat": 55.55, "lon": 66.66, "timesteps": [1, 2, 3] }
                ]
            }
        }
    ]
}
```

Extracting at every step, batching at most 20 steps per output file
(`scm_in_proc_00001_pt_00001_step_00000_to_00019.nc`,
`..._step_00020_to_00039.nc`, ...):

```jsonc
{
    "plugins": [
        {
            "name": "PluginSCMData",
            "lib": "plugin_scm_data_dp",
            "core-config": {
                "LAREA": 0,
                "LPROGNOSTIC": 1,
                "RUN_EVERY": 1,
                "APPEND_OUTPUT": 1,
                "APPEND_OUTPUT_NSTEPS": 20,
                "DATAID": "test_append_batch",
                "DELTA": 0.75,
                "points": [
                    { "ID": 1, "lat": 11.11, "lon": 22.22 },
                    { "ID": 2, "lat": 33.33, "lon": 44.44 }
                ]
            }
        }
    ]
}
```

## Profiling

Build with `-DENABLE_SCM_PLUME_PLUGIN_PROFILER=ON` to compile in the plugin profiler.
When the option is `OFF` the instrumentation macros expand to nothing, so there is no
runtime cost and no profiler symbol is referenced.

Regions are named with a dot-separated path (`scm_run.process_plume_fields.gradients_sp.halo`)
which the report renders as a tree. At the end of `scm_teardown` a table is written to the
fckit log on rank 0, prefixed `[SCM-TIMER]`:

| Column | Meaning |
|---|---|
| `Region` | leaf name, indented by nesting depth |
| `Calls` | number of times the region was entered, summed over ranks |
| `Self` | `Total` minus the time spent in nested regions, i.e. time unaccounted for by children (per-rank mean) |
| `Total` | inclusive time in the region (per-rank mean) |
| `%Tot` | `Total` as a percentage of the sum of the root regions |
| `Min` / `Max` | smallest / largest `Total` across ranks |
| `Imbal` | `Max / Total`; 1.0 means perfectly balanced |
| `MaxRk` | the rank holding `Max`, i.e. the straggler |

`Min` and `Max` are taken across ranks per region independently, so they do **not** add up
across the tree — only `Total` and `Self` do.

`print_timers` is collective on the plugin communicator: every rank must reach the end of
`scm_teardown`. The list of regions is taken from rank 0 and broadcast, so ranks that
registered a different set of regions cannot deadlock the reduction; any region missing
from rank 0's list is reported as a warning instead of being silently dropped.

Unbalanced instrumentation (a stop without a matching start, a region left open at report
time, regions closed out of order) is reported as a `[SCM-TIMER]` warning rather than being
folded into the numbers.

Timing uses the intrinsic `SYSTEM_CLOCK`, so the profiler has no dependency on fiat's
`timef`. In addition, each region opens an `atlas_Trace` of the same name. Running with
`ATLAS_TRACE_REPORT=1` therefore yields atlas' own nested report as well, in which the
atlas-internal costs (`halo_exchange`, `nabla`, mesh generation) appear underneath the
plugin regions.

### Adding a timer

Include the macros and use the name-based API for coarse regions:

```fortran
#include "profiler_macros.h"
...
START_PLUGIN_TIMER("scm_run.my_region")
call do_something()
STOP_PLUGIN_TIMER("scm_run.my_region")
```

Inside a loop, use the handle-based API instead so the name lookup stays out of the
measured region. Register the name outside any rank-dependent branch, so that all ranks
end up with the same set of regions:

```fortran
DECLARE_PLUGIN_TIMER(ih_mine)          ! declaration section
...
REGISTER_PLUGIN_TIMER(ih_mine, "scm_run.my_region.inner")
do jfld = 1, nfields
  START_PLUGIN_TIMER_H(ih_mine)
  call do_something(jfld)
  STOP_PLUGIN_TIMER_H(ih_mine)
enddo
```

Every path out of a timed region must stop it, including early `return`s.
