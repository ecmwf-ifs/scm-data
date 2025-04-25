# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


# Test files repository on Nexus
set(ECBUILD_DOWNLOAD_BASE_URL https://get.ecmwf.int/repository/scm-data-test-files)

# get test files t159 from NEXUS
set(test_files_t159_dir ${CMAKE_BINARY_DIR}/tests/data/input_files_t159)
add_custom_target(make_test_data_dir ALL COMMAND ${CMAKE_COMMAND} -E make_directory ${test_files_t159_dir})

ecbuild_get_test_multidata(TARGET test_files_t159
                           NAMES
                                cld_grib_t159
                                sfc_grib_t159
                                namelist_1c_t159
                                spec_grib_t159
                           DIRNAME
                                input_files_t159
                           DIRLOCAL
                                ${test_files_t159_dir}
                           NOCHECK
                           )


# get output test files t159 from NEXUS
set(test_output_files_t159_dir ${CMAKE_BINARY_DIR}/tests/data/output_files_t159)
add_custom_target(make_output_test_data_dir ALL COMMAND ${CMAKE_COMMAND} -E make_directory ${test_output_files_t159_dir})

ecbuild_get_test_multidata(TARGET test_output_files_t159
                           NAMES
                                scm_in_0001_01.nc
                                scm_in_0001_02.nc
                                scm_in_0001_03.nc
                                scm_in_0001_04.nc
                                scm_in_0001_05.nc
                                scm_in_0001_06.nc
                                scm_in_0001_07.nc
                                scm_in_0001_08.nc
                                scm_in_0001_09.nc
                                scm_in_0001_10.nc
                                scm_in_0001_11.nc
                                scm_in_0001_12.nc
                           DIRNAME 
                                output_files_t159
                           DIRLOCAL
                                ${test_output_files_t159_dir}
                           NOCHECK
                           )                           

set(TEST_DATA_DIR ${CMAKE_BINARY_DIR}/tests/data)




# Test getini1c and plugin test can only be enabled in double precision 
# Atlas scatter spectral field  does not support real64
if (HAVE_SINGLE_PRECISION AND NOT HAVE_PLUME_PLUGINS_DOUBLE_PRECISION)
    message("Double precision NOT enabled => scm_compare not built.")
else()

    # tool to compare the output of getini1c and/or plugin
    ecbuild_add_executable( TARGET scm_compare
                            SOURCES
                                ../src/module/yomvar.F90
                                ../src/handle_err_nc.F90
                                scm_compare.F90
                            INCLUDES
                                ../src/interface/
                                $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/src>
                                $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src>
                            LIBS
                                NetCDF::NetCDF_Fortran
                                fckit
                        )

    # Set the Fortran module directory for scm_compare
    set_target_properties(scm_compare
        PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/scm_compare_mods_dir
    )
endif()




# tests for getini tool
if ( HAVE_SCM_TOOLS)

    # Test getini1c can only be enabled in double precision 
    # Atlas scatter spectral field  does not support real64
    if (HAVE_SINGLE_PRECISION)
        message("Single precision is enabled => test_getini1c* tests not built.")
    else()

        # test of getini1c
        ecbuild_configure_file(
            test_getini1c.sh.in
            test_getini1c.sh @ONLY
        )
        ecbuild_add_test(
            TYPE     SCRIPT
            COMMAND  test_getini1c.sh
            TEST_DEPENDS test_files_t159 test_output_files_t159
        )

        # test of getini1c_cli
        ecbuild_configure_file(
            test_getini1c_cli.sh.in
            test_getini1c_cli.sh @ONLY
        )
        ecbuild_add_test(
            TYPE     SCRIPT
            COMMAND  test_getini1c_cli.sh
            TEST_DEPENDS test_files_t159 test_output_files_t159
        )

    endif()

endif()


# Copy plugin configs to build directory (if tests are enabled)
add_custom_command(OUTPUT ${CMAKE_BINARY_DIR}/tests/plugin_configs
                   COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/plugin_configs ${CMAKE_BINARY_DIR}/tests/plugin_configs
                   DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/plugin_configs)

add_custom_target(copy_configs_dir ALL DEPENDS ${CMAKE_BINARY_DIR}/tests/plugin_configs)


if (HAVE_SCM_PLUME_PLUGIN)

    # Test_scm_plugin can only be enabled in double precision 
    # Atlas scatter spectral field  does not support real64
    if (HAVE_PLUME_PLUGINS_DOUBLE_PRECISION)

        # driver for testing the scm-plugin
        add_subdirectory(plugin_tester)

        ecbuild_configure_file(
            test_scm_plugin.sh.in
            test_scm_plugin.sh @ONLY
        )

        ecbuild_add_test(
            TYPE     SCRIPT
            COMMAND  test_scm_plugin.sh
        )
    else()
        message("Double Precision is NOT enabled => test_scm_plugin is NOT built.")
    endif()

endif()
