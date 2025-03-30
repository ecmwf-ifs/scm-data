# SCM Data

## Description
This is the repository for creating netcdf initial and forcing data for the single-column-model of the IFS.
The programme uses Atlas to compute derivatives and the required advective tendencies on model levels.

## Disclaimer
- *This package is made available to support research collaborations and is not officially supported by ECMWF*
- *This software is still under development and not yet ready for operational use*


# How to use
This package provides 2 versions of the *getini1c* tool to generate netcdf files for the SCM model:

  - *getini1c*: (legacy code) it generates initial and forcing data for the SCM model.
    It expects the following files in the working directory:
    - sfc_grib: grib containing surface parameters
    - cld_grib: grib containing cloud parameters
    - spec_grib: grib containing spectral parameters
    - namelist_1c: namelist file containing configuration parameters

  - *getini1c_cli*: it generates initial and forcing data for the SCM model.
    same as above, but the file paths are passed as command line arguments, i.e:
    
    getini1c <sfc_grib_file> <cld_grib_file> <spec_grib_file> <namelist_file>

In addition, this package also contains a Plume plugin for Earth System Models that can generate SCM input files on-the-fly (directly from model data in memory). More details on how to use Plume can be found in https://github.com/ecmwf/plume. Example plume plugins are also available at https://github.com/ecmwf/plume-examples.

### Requirements
Build dependencies:

- C/C++ compiler (C++17)
- Fortran 2008 compiler
- CMake >= 3.16 --- For use and installation see http://www.cmake.org/
- ecbuild >= 3.8.5 --- ECMWF library of CMake macros (https://github.com/ecmwf/ecbuild)

Runtime dependencies:
  - eckit >= 1.28.5 (https://github.com/ecmwf/eckit)
  - eccodes >= 2.38.3 (https://github.com/ecmwf/eccodes)
  - fckit >= 0.13.1 (https://github.com/ecmwf/fckit)
  - atlas >= 0.39.0 (https://github.com/ecmwf/atlas)
  - fiat >= 1.1.0 (https://github.com/ecmwf-ifs/fiat)
  - ectrans >= 1.2.0 (https://github.com/ecmwf-ifs/ectrans)

### Installation
scm-data employs an out-of-source build/install based on CMake.

#### Easy Install
To easily install the scm-data package with all the ECMWF dependencies, follow the steps below:
```
cd <scm-data-directory>/bundle
```
create the bundle (download all the necessary ECMWF dependencies)
```
./scm-data-bundle create
```
Build the bundle. Optionally specify build-type and build-dir if default is not suitable:
```
./scm-data-bundle build --build-type=<Release|Debug|RelWithDebInfo> --build-dir=<build-dir>
```
NOTE: for building the bundle in the ECMWF HPC system, an ecbuild configuration file (that loads the necessary system modules) is also provided. To use it, simply add the `--arch` option:
```
./scm-data-bundle build --build-type=<Release|Debug|RelWithDebInfo> --build-dir=<build-dir> --arch arch/ecmwf/hpc2020/intel
```
Other building options to can be queried via:
```
./scm-data-bundle build --help
```

#### Advanced Install
Make sure ecbuild is installed and the ecbuild executable script is found ( `which ecbuild` ).
Now proceed with installation as follows:

```bash
# Environment --- Edit as needed
srcdir=$(pwd)
builddir=build
installdir=$HOME/local  

# 1. Create the build directory:
mkdir $builddir
cd $builddir

# 2. Run CMake
ecbuild --prefix=$installdir -- \
  -Deckit_ROOT=<path/to/eckit/install> \
  -Deccodes_ROOT=<path/to/eccodes/install> \
  -Dfckit_ROOT=<path/to/fckit/install> \
  -Dfiat_ROOT=<path/to/fiat/install> \
  -Dectrans_ROOT=<path/to/ectrans/install> \
  -Datlas_ROOT=<path/to/atlas/install> $srcdir

# 3. Compile / Install
make -j10
make install
```


## Prepare SCM-data Input
The directory *scripts* contains python scripts which act as wrappers for the fortran program `getini1c`. The scripts extract and/or copy ERA data or OpenIFS output, check and standardise the data naming convention in preparation for `getini1c` execution, which produces the netcdf files required for the SCM.


### Install Environment for running the scripts
Requirements:
 - python3 > 3.10

To install all the necessary python packages, execute:
```
source <scm-data-directory>/scripts/env.sh
```
this creates a python virtual env called *scm_env* in a directory $(pwd)/envs. The first time the script *env.sh* is executed, it will take a few minutes to install all the necessary python packages. Subsequent invocations of the script will only source the environment, and the execution time should be reduced to seconds (Note: on the ECMWF HPC system, the env.sh can be invoked as `source env.sh ec-hpc2020` and the script will only load the necessary system modules, without creating the virtual environment).

### Retrieve and pre-process data
The programs can be run by executing the following commands:
```
source $(pwd)/envs/scm_env/bin/activate
python3 scripts/run_scm_data.py <scm-data-configuration-file>
```
optionally a namelist setup file can also be specified with the -n option (otherwise the default file *config/scm_nml_setup.yml* will be used).

For example, the command:

```
python3 scripts/run_scm_data.py scripts/config/scm_data_era5_setup.yml -n scripts/config/scm_nml_setup.yml
```

will run *run_scm_data.py* using:

- *config/scm_data_era5_setup.yml*: user configuration file that defines dates, lat/lon points, paths etc, for the required SCM data.

- *config/scm_nml_setup.yml*: which defines the SCM namelist, used to run the SCM. 


### Configure the scripts
Once the pre-requisites are installed, loaded or built, the next step is to edit the user configuration file that defines paths to experiment or retrieved data, control settings, dates and experiment or retrieval information.

* *config/scm_data_oifs_setup.yml* - example of settings required to copy and organise OpenIFS experiment data to a standard (defined) location and then extract the requested SCM data 
* *config/scm_data_era5_setup.yml* - example of settings required to retrieve and copy ERA5 data from MARS to a standard (defined) location and then extract the requested SCM data 
  
These example files are very similar, with both consisting of the following dictionaries

* *paths* - includes the user defined directory paths for the source data (from ERA5 or OpenIFS) and the destination directory for the SCM netcdf forcing file.

* *scm* - includes switches to determine what to extract, i.e. one column fixed in space, an array of columns fixed in space or a column track that varies in space and time. In addition, this dictionary includes the user defined dates, latitude and longitude for which the forcing needs to be derived.

* *control* - includes user defined details about the source data, e.g. resolution, grid type and time details

* *grib_shortnames* - includes the shortnames for all the parameters required to create the SCM initial conditions and forcing. The shortnames are used in either the MARS retrieval for ERA or the copy of OpenIFS data

* *clim_vars* - includes the required climatological fields, e.g. surface fields etc,  which need to be interpolated and appended to both ERA and OpenIFS data to produce the SCM forcing files. 

It is important to note, that at the time of writing, all the dictionaries and associated variables need to be defined in the yaml file because there are no defaults set in the code. Failing to set something, will lead to unexpected behaviours. Hence, it is recommended that *scm_data_era5_setup.yml* and *scm_data_oifs_setup.yml* are modified, rather than writing a yaml from scratch.

<hr>
Click on "Details" for an overview of the different variables in <i>scm_data_era5_setup.yml</i> and <i>scm_data_oifs_setup.yml</i>, and their purpose. This extra information includes description of all the variables in all dictionaries except <i>grib_shortnames</i> and <i>clim_vars</i>, since these are commented in the yaml files.  
<details>
<div style="background-color:rgba(1, 191, 17, 0.10); text-align:left; vertical-align: middle; padding:20px 30px;">

<b>Overview of the scm_data setup yaml file</b><br> 

The first variable in `scm_data_era5_setup.yml`, and `scm_data_oifs_setup.yml`, is `name`, which is used to identify the data source. At the time of writing, the only options for `name` are `era5` or `openifs`. Anything else results in an error and the scripts exit. The `name` is followed by `platform`, which is an identifier for the system. At the time of writing, the only platform name is `ec-hpc2020`, which refers to ECMWF ATOS system.  

The first section in the `scm_data_era5_setup.yml` defines the `paths` dictionary, which includes the following user supplied directory paths. 

* `climate_data_toplevel`
  * path for the top-level directory that contains climate files and vtables. With OpenIFS, these files can be downloaded [openifs-data](https://sites.ecmwf.int/openifs/openifs-data/). 
  * If `platform = ec-hpc2020`, then oifs-data paths are constructed by the scripts. Otherwise, the scripts assume the climate data and vtables exist in `climate_data_toplevel`. 
    * If the climate files cannot be found, the scripts exit.
* `getini1c_bin` 
  * path for the directory containing the `getini1c` executable. See previous section, which describes how to build and install the getini1c executable. 
* `getini1c_data_top` 
  * top-level directory path for the store of either MARS retrievals or OpenIFS output. Upon successful execution of `run_scm_data.py`, this directory will contain a directory with the naming convention `data_<name>_<grid>`, where `name` is either `era5` or `openifs` and defines the data source used to generate the SCM forcing. `<grid>` is the resolution grid, e.g. `N320`. Both `name` and `grid` are user defined in the yaml.
  * When the data source is `era5`, the requested ERA5 data will be retrieved and store in an additional directory `data_era5_mars_retrieval`, which also exists in the top-level directory prescribed by `getini1c_data_top` .  
* `scm_forcing_out`
  *  directory path where the netcdf files for the SCM are output. This is the final location of the forcing files that can be used with the SCM. 

The next section in `scm_data_era5_setup.yml` and `scm_data_oifs_setup.yml` defines the `scm` dictionary, which includes the following switches to control the single column data extract 
* `extract_scm_column`
  * switch to request initial conditions forcing for one column, fixed in space
* `extract_scm_track` 
  * switch to request the forcing for one column following a track, with latitude and longitude changing over time
* `extract_scm_array`
  * switch to request initial conditions/forcing for an array of single columns, each fixed in space 
<div style="background-color:rgba(239,239, 72, 0.35); text-align:left; vertical-align: middle; padding:15px 15px;">
<b>Note:</b> Only one of the <code>extract</code> switches can be True at anyone time. If more that one is True (or none are True), the scripts exit with an error. Further, if any new switches are added, they must start with <code>extract_</code>.
</div>

As well as the above switches, the `scm` dictionary also includes the following

* `datebeg` 
  * The start date for the scm forcing, using the format YYYYMMDD, where YYYY is the year, MM is the month and DD is the day, e.g. 1st July 1987 would be 19870701
* `dateend`
  * The end date for scm forcing, also using the format YYYYMMDD
* `tstep`
  * The timestep or time difference between either OpenIFS output or MARS retrievals. This also represents the timestep for the resulting SCM forcing
* `lat`
  * Latitude of the column to be used in the SCM simulation
  * This is a list, with an aim to permit users to add multiple latitudes (not functioning yet)  
* `lon`
  * Longitude of the column to be used in the SCM simulation 
  * As with the `lat` this can be a list (not functioning) and the list length must be the same as `lat`
* `latlon_from_file`
  * If `False`, then latitude and longitude defined in `lat` and `lon` will be used for all dates between `datebeg` and `dateend` 
  * If `True`, then latitude and longitude will be a track defined in `latlonfile`, with a different `lat` and `lon` for each date
* `latlonfile`
  * file containing dates, latitude and longitude for the track of the SCM.

The `scm` dictionary is followed by the `control` dictionary, which includes control logic for scripts and description of the setup for the basic data source (OpenIFS or ERA5), which is used to produce the SCM data, e.g. resolution, grid type (linear or cubic octahedral), number of levels.  

`control` contains the following:

* `retrieve_data` 
  * If `True`, scritps will retrieve data from MARS
  * If `False`, the assumption is that the retrieved data exists. There is a check for this and if the retrieved data is not found, the scripts exit
* `copy_data` 
  * If `True`, retrieved data will be copied to path defined in `getini1c_data_top`  and the file naming convention will be standardised, i.e., same for ERA5 and OpenIFS. Using this function ensures that retrieved data or simulation output is not modified by appending climatologies. 
  * If `False`, the scripts check that all expected files exist. If the copied files cannot be found, the scripts will attempt to copy the files from the OpenIFS output. 
* `cleanup` - not coded yet
  * Aim, if `True`, any copied files will be removed
* `create_forcing_data` 
  * If `True` the scripts will run getini1c to produce a forcing file for a time and latitude and longitude.
* `concat_forcing_data` 
  * If true, scripts will concatinate the individual netcdf forcing files, created by getini1c, for each time, latitude and longitud into one file, which can be used in the SCM. This is a debug switch, which will only work if `create_forcing_data` = True or getini1c has already been run (this switch may be removed)
* `create_scm_namelist` 
  * If true the scripts will create an example namelist for the data generated. This switch enables the reading and use of scm_nml_setup.yml file 
* `append_clim` - commented out
  * if `True`, scripts will append the copied data `getini1c_data_top` , with climatological data, e.g. SST or albedos, if the climatologies do not exist in file already  
  
In addtion to the control logic switches, `control` also contains description of the cycle, grid, resolution and time variables, i.e.
* `cycle`
  * The OpenIFS or IFS cycle number, which is required to define some paths, e.g. climate version paths. 
  * For ERA5, this must be set to 43r3
* `iniclass` and `exp_version` 
  * Only used with ERA5, with both being required for the retrieval of the ERA5 data from MARS. It is recommended not to change these
* `id`
  * Only present in `scm_data_oifs_setup.yml` and should be the the experiment id that is output from the datahub. This id is used to set up the path for the OpenIFS source data. 
  * For ERA, id is set `era5` and is only used in the file name for the final SCM netcdf file.
* `res` 
  * resolution (spectral truncation) of the simulation/data. For ERA5 this will be `639`, while for OpenIFS this will be equal to the horizontal resolution defined in the datahub request.
* `gtype`
  * grid type (linear or cubic octahedral), i.e. set this to l_2 for linear-reduced (ERA5) or `_4` for cubic octahedral, e.g. tco319 will use `_4`. 
* `grid` 
  * number of latitude lines between equator and pole. For more details about how this is set and it relates to `gtype` and `res`, please visit [OpenIFS Horizontal resolution and configurations](https://confluence.ecmwf.int/display/OIFS/4.3+OpenIFS%3A+Horizontal+Resolution+and+Configurations).
* `levels`
  * Number of vertical levels. It is important to note that the number of levels defined here needs to be the same as `NFLEVG` in the SCM run namelist `&NAMDIM` in OpenIFS or IFS.  
* `init_time`
  * Initial time of the forecast from datahub or the initial retrieval time
* `tstep` 10800.00
  * timestep between retrievals (in seconds) or timestep between OpenIFS outputs
* `climvers`
  * sets the climate version, e.g., for ERA5 this needs to be set to `climate.v015` while for OpenIFS 48r1, this needs to be set `climate.v020`.

Finally, the `control` dictionary contains some getini1c specific variables that are required in addition to the above to set-up the `getini1c` namelist:
* `ZDELTA` 
  * distance in degrees acceptable to find nearest point to specified lat-lon important to select large enough envelope, this is tied to the resolution, i.e. 0.3 will work with `res` = 399 and 0.1 will work with `res` = 1279
* `dataid`: 
  * required as metadata for output to the netcdf file. Default format is valid_\<gtype\>\<res\> but there is no reason it has to be this format.    
* `prognostic`
  * Not sure but always seems to be set to .true.. Probably should not be in yaml file.

</div>
</details>
<hr>
