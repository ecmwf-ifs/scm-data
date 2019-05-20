# (C) Copyright 1996-2016 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

############################################################################################

cmake_minimum_required( VERSION 3.3.2 FATAL_ERROR )

project( scm-data C Fortran )

set( ENABLE_OS_TYPES_TEST       OFF   CACHE BOOL " " )
set( ENABLE_OS_FUNCTIONS_TEST   OFF   CACHE BOOL " " )
set( ENABLE_LARGE_FILE_SUPPORT  OFF   CACHE BOOL " " )
set( ENABLE_OS_ENDINESS_TEST    OFF   CACHE BOOL " " )

include( ecbuild_system NO_POLICY_SCOPE )
ecbuild_requires_macro_version( 1.9 )
ecbuild_declare_project()
ecbuild_enable_fortran( REQUIRED MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/module )

ecbuild_add_option( FEATURE ATLAS
                    DESCRIPTION "scm: Support for atlas"
                    REQUIRED_PACKAGES "PROJECT atlas" )
if( NOT HAVE_ATLAS )
    ecbuild_warn( "The scm executables requiring atlas cannot be built")
endif()

ecbuild_add_option( FEATURE GRIB
                    DESCRIPTION "scm: Support for grib data format"
                    REQUIRED_PACKAGES "PROJECT eccodes" )
if( NOT HAVE_GRIB )
    ecbuild_error( "The scm executables require eccodes")
endif()

ecbuild_add_option( FEATURE TRANS
                    DESCRIPTION "scm: Support for trans"
                    REQUIRED_PACKAGES "PROJECT trans" )
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

############################################################################################
# build executables

add_subdirectory( src )

############################################################################################
# finalize

ecbuild_install_project( NAME scm-data )

ecbuild_print_summary()
