# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


# Copy test data to build directory (if tests are enabled)
add_custom_command(OUTPUT ${CMAKE_BINARY_DIR}/tests/data
                   COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/data ${CMAKE_BINARY_DIR}/tests/data
                   DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/data)

add_custom_target(copy_data_dir ALL DEPENDS ${CMAKE_BINARY_DIR}/tests/data)

# tests for getini tool
if ( HAVE_BUILD_TOOLS)

    # tool to compare the output of getini1c
    ecbuild_add_executable( TARGET scm_compare
                            SOURCES
                                scm_compare.F90
                                ../src/handle_err_nc.F90
                            LIBS
                                NetCDF::NetCDF_Fortran
                                fckit
                          )

    

    # Test getini1c can only be enabled in double precision 
    # Atlas scatter spectral field  does not support real64
    if (HAVE_SCM_SINGLE_PRECISION)
        message("Single precision is enabled => test_getini1c not built.")
    else()

        ecbuild_configure_file(
            test_getini1c.sh.in
            test_getini1c.sh @ONLY
        )

        ecbuild_add_test(
            TYPE     SCRIPT
            COMMAND  test_getini1c.sh
        )
    endif()

endif()


# Copy plugin configs to build directory (if tests are enabled)
add_custom_command(OUTPUT ${CMAKE_BINARY_DIR}/tests/plugin_configs
                   COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/plugin_configs ${CMAKE_BINARY_DIR}/tests/plugin_configs
                   DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/plugin_configs)

add_custom_target(copy_configs_dir ALL DEPENDS ${CMAKE_BINARY_DIR}/tests/plugin_configs)


# Test getini1c can only be enabled in double precision 
# Atlas scatter spectral field  does not support real64
if (HAVE_SCM_SINGLE_PRECISION)
    message("Single precision is enabled => test_scm_plugin not built.")
else()

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

endif()