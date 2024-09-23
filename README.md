# SCM Data

## Description
This is the repository for creating netcdf initial and forcing data for the single-column-model of the IFS.
The programme uses Atlas to compute derivatives and the required advective tendencies on model levels.

## Disclaimer
This software is still under development and not yet ready for operational use.

## How to use
This package contains two main tools:

  - *getini1c*: (legacy code) it generates initial and forcing data for the SCM model.
    It expects the following files in the working directory:
    - sfc_grib: grib containing surface parameters
    - cld_grib: grib containing cloud parameters
    - spec_grib: grib containing spectral parameters
    - namelist_1c: namelist file containing configuration parameters

  - *getini1c_cli*: it generates initial and forcing data for the SCM model.
    same as above, but the file paths are passed as command line arguments, i.e:
    
    getini1c <sfc_grib_file> <cld_grib_file> <spec_grib_file> <namelist_file>

    