# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

############################################################################################

cmake_minimum_required( VERSION 3.3.2 FATAL_ERROR )

find_package( ecbuild REQUIRED )

project( scm-data LANGUAGES C CXX Fortran )
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

ecbuild_enable_fortran( REQUIRED MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/module )

ecbuild_find_package( eccodes REQUIRED )
ecbuild_find_package( fiat REQUIRED )


# Build command line SCM tools
ecbuild_add_option( FEATURE SCM_TOOLS
                    DESCRIPTION "Build the command line tools"
                    DEFAULT ON )

# Build SCM tests
ecbuild_add_option( FEATURE SCM_TESTS
                    DESCRIPTION "Build SCM tests"
                    DEFAULT ON )

ecbuild_add_option( FEATURE ATLAS
                    DESCRIPTION "scm: Support for atlas"
                    REQUIRED_PACKAGES "atlas" )
if( NOT HAVE_ATLAS )
    ecbuild_warn( "The scm executables requiring atlas cannot be built")
endif()


ecbuild_add_option( FEATURE TRANS
                    DESCRIPTION "scm: Support for trans"
                    REQUIRED_PACKAGES "ectrans" )
if( NOT HAVE_TRANS )
    ecbuild_warn( "The scm executables requiring trans cannot be built")
endif()

#NETCDF
ecbuild_add_option( FEATURE NETCDF
                    DEFAULT ON
                    DESCRIPTION "support for NETCDF format"
                    REQUIRED_PACKAGES "NetCDF COMPONENTS Fortran QUIET" )
if( ENABLE_NETCDF AND NOT HAVE_NETCDF )
  message( STATUS "Did you forget to 'module load netcdf'?")
endif()


ecbuild_add_option( FEATURE OMP
                    DESCRIPTION "Enable OpenMP support for SCM-DATA"
                    DEFAULT OFF
                    REQUIRED_PACKAGES OpenMP )

# enable the plume plugin
ecbuild_add_option( FEATURE SCM_PLUME_PLUGIN
                    DESCRIPTION "plume plugin"
                    DEFAULT OFF
                    CONDITION ENABLE_ATLAS
                    REQUIRED_PACKAGES plume )

# Enables a simple profiler for the plume plugin
ecbuild_add_option( FEATURE SCM_PLUME_PLUGIN_PROFILER
                    DESCRIPTION "plume plugin profiler"
                    DEFAULT OFF
                    CONDITION ENABLE_SCM_PLUME_PLUGIN
                    )
# When ON, the plume plugin skips execution unless NSTEP has changed since the
# previous call (i.e. it only runs once per unique step).
ecbuild_add_option( FEATURE SCM_PLUME_PLUGIN_UNIQUE_STEPS
                    DESCRIPTION "plume plugin: only execute once per unique NSTEP"
                    DEFAULT ON
                    CONDITION ENABLE_SCM_PLUME_PLUGIN
                    )
# When ON, the plume plugin (and its tester) expect three 4-level input fields
# (sot=260360, vsw=260199, sit=262024) instead of the twelve single-level fields
# stl1..stl4, swvl1..swvl4, istl1..istl4. Destination storage PSTL/PSWL/PTIA(ncss=4)
# and the produced NetCDF variables t_soil/q_soil/t_sea_ice are unchanged.
ecbuild_add_option( FEATURE SCM_GRIB2_FIELDS
                    DESCRIPTION "Read soil temp/moisture/ice-temp as multi-level fields (sot/vsw/sit)"
                    DEFAULT ON
                    CONDITION HAVE_SCM_PLUME_PLUGIN )

# *** Both the SCM tools and the plume plugin can be built in Single Precision (SP) or Double Precision (DP): ***
# -DENABLE_SINGLE_PRECISION=ON               : builds the tools and the plume plugin in SP (or DP if OFF)
# -DENABLE_PLUME_PLUGINS_SINGLE_PRECISION=ON : option used to build the plume plugin in single precision
# -DENABLE_PLUME_PLUGINS_DOUBLE_PRECISION=ON : option used to build the plume plugin in double precision
# ***************************************************************************************************************
ecbuild_add_option( FEATURE SINGLE_PRECISION
                    DESCRIPTION "SCM tools and plugin in single precision"
                    DEFAULT OFF )

ecbuild_add_option( FEATURE PLUME_PLUGINS_SINGLE_PRECISION
                    DESCRIPTION
                    "Compile plugin for single precision fields"
                    DEFAULT OFF)

ecbuild_add_option( FEATURE PLUME_PLUGINS_DOUBLE_PRECISION
                    DESCRIPTION
                    "Compile plugin for double precision fields"
                    DEFAULT ON)

# Definitions used for tools
if( HAVE_SINGLE_PRECISION )
  list(APPEND SCM_TOOLS_DEFINITIONS WITH_SCM_SINGLE_PRECISION )
endif()

# Definitions used for plugin
set( HAVE_SCM_PLUGIN_sp 0)
set( HAVE_SCM_PLUGIN_dp 0)

if (HAVE_SINGLE_PRECISION OR HAVE_PLUME_PLUGINS_SINGLE_PRECISION)
  set( PLUGINS_DEFINITIONS_sp WITH_SCM_SINGLE_PRECISION)

  # also add profiler definition if plume plugin profiler is enabled
  if (HAVE_SCM_PLUME_PLUGIN_PROFILER)
    list(APPEND PLUGINS_DEFINITIONS_sp WITH_SCM_PLUME_PLUGIN_PROFILER)
  endif()

  # also add unique-steps definition if enabled
  if (HAVE_SCM_PLUME_PLUGIN_UNIQUE_STEPS)
    list(APPEND PLUGINS_DEFINITIONS_sp WITH_SCM_PLUME_PLUGIN_UNIQUE_STEPS)
  endif()

  set( HAVE_SCM_PLUGIN_sp 1)
endif()

if (HAVE_PLUME_PLUGINS_DOUBLE_PRECISION)
  set( PLUGINS_DEFINITIONS_dp WITH_SCM_DOUBLE_PRECISION)

  # also add profiler definition if plume plugin profiler is enabled
  if (HAVE_SCM_PLUME_PLUGIN_PROFILER)
    list(APPEND PLUGINS_DEFINITIONS_dp WITH_SCM_PLUME_PLUGIN_PROFILER)
  endif()

  # also add unique-steps definition if enabled
  if (HAVE_SCM_PLUME_PLUGIN_UNIQUE_STEPS)
    list(APPEND PLUGINS_DEFINITIONS_dp WITH_SCM_PLUME_PLUGIN_UNIQUE_STEPS)
  endif()

  set( HAVE_SCM_PLUGIN_dp 1)
else()
  set( HAVE_SCM_PLUGIN_dp 0)
endif()

# Propagate the multi-level soil switch to every plugin precision variant and
# to the plugin tester (which compiles plugin_utils.F90 and grib_fields_provider.F90
# into a separate executable).
if( HAVE_SCM_GRIB2_FIELDS )
  list( APPEND PLUGINS_DEFINITIONS_sp WITH_SCM_GRIB2_FIELDS )
  list( APPEND PLUGINS_DEFINITIONS_dp WITH_SCM_GRIB2_FIELDS )
endif()

############################################################################################
# build executables

add_subdirectory( src )

if(HAVE_SCM_TESTS)
  add_subdirectory( tests )
endif()

############################################################################################
# finalize

ecbuild_install_project( NAME scm-data )

ecbuild_print_summary()
