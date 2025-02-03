#!/bin/bash

# bash function to create the environment
create_env() {
  python3 -m venv envs/scm_env
  source envs/scm_env/bin/activate
  
  python3 -m pip install pandas
  python3 -m pip install pyyaml
  python3 -m pip install eccodes
  python3 -m pip install cdsapi

  python3 -m pip install numpy
  python3 -m pip install xarray
  python3 -m pip install netCDF4
}

# first argument is the platform (optional)
if [[ $# -gt 1 ]]; then
  echo "Usage: $0 <platform>"
  return 1
elif [[ $# -eq 1 ]]; then
  PLATFORM=$1
else
  PLATFORM="local"
fi

echo "Platform selected: $PLATFORM"

# check if the platform is supported
if [[ $PLATFORM == "ec-hpc2020" ]]; then
  module load python3
  module load ecmwf-toolbox
else
  if [[ ! -e envs/scm_env ]]; then
    echo "Creating environment in envs/scm_env.."
    create_env
  else
    echo "Source the environment in envs/scm_env.."
    source envs/scm_env/bin/activate
  fi
fi
