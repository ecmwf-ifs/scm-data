# Plume plugin: Single Column Model Data

This Plume plugin extracts IFS model fields at a user-defined set of latitude/longitude locations and writes the output to NetCDF files in a format compatible with the Single Column Model (SCM). For more information about the Plume plugin system and instructions on how to run plugins, see the [Plume](https://github.com/ecmwf/plume) documentation.


## Compilation options

These options are defined in the top-level [CMakeLists.txt](../../CMakeLists.txt) and can be passed to `cmake` as `-DENABLE_<option>=ON|OFF`.

| Option | Description | Default |
|---|---|---|
| `SCM_TOOLS` | Build the command line tools | `ON` |
| `SCM_TESTS` | Build SCM tests | `ON` |
| `ATLAS` | Support for atlas | required for the plugin |
| `TRANS` | Support for trans | - |
| `NETCDF` | Support for NetCDF format | `ON` |
| `OMP` | Enable OpenMP support for SCM-DATA | `OFF` |
| `SCM_PLUME_PLUGIN` | Build the plume plugin (requires `ATLAS` and `plume`) | `OFF` |
| `SCM_PLUME_PLUGIN_PROFILER` | Enable a simple profiler for the plume plugin (requires `SCM_PLUME_PLUGIN`) | `OFF` |
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
| `points` | array | List of point definitions (see below) | - |

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
