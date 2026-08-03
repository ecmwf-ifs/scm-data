# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

# Surrogate model serving SCM GRIB fields
ecbuild_add_executable(
    TARGET scm_plugin_tester

    SOURCES
    
        # test-specific sources
        fill_and_write.F90
        grib_fields_provider.F90
        scm_plugin_tester.F90

        # plugin sources
        ../../src/plume-plugin-scm/plugin_utils.F90
        
        # common sources
        ../../src/rdnam.F90
        ../../src/fillvar.F90
        ../../src/compute_fields.F90
        ../../src/module/yomvar.F90
        ../../src/nearest_distance.F90
        ../../src/su_wrt_nc.F90
        ../../src/wrt1c_nc.F90
        ../../src/calcgeost.F90
        ../../src/handle_err_nc.F90
        ../../src/ncdf_varsetup1c.F90
        ../../src/ncdf_varwrite1c.F90
        ../../src/read_grib_scm.F90

    INCLUDES
        $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}>
        $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}>
        "${eckit_INCLUDE_DIRS}"
        "${CMAKE_SOURCE_DIR}"
        testing_interface/
        ../../src/interface/

    # Keep plugin_utils.F90 (and the tester's own sources that reference the
    # multi-level soil field names) in sync with the double-precision plugin
    # build. The dp variant is the only one built for tests (see tests/CMakeLists.txt).
    DEFINITIONS
        ${PLUGINS_DEFINITIONS_dp}

    LIBS
        atlas_f
        fiat
        eccodes_f90
        fckit
        plume_f
        NetCDF::NetCDF_Fortran
)

# Set the Fortran module directory for scm_compare
set_target_properties(scm_plugin_tester
    PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/scm_plugin_tester_mods_dir
)