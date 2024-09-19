


# Copy test data to build directory (if tests are enabled)
add_custom_command(OUTPUT ${CMAKE_BINARY_DIR}/tests/data
                   COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/data ${CMAKE_BINARY_DIR}/tests/data
                   DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/data)

add_custom_target(copy_directory ALL DEPENDS ${CMAKE_BINARY_DIR}/tests/data)

# tests for getini tool
if ( HAVE_BUILD_TOOLS )


    # tool to compare the output of getini1c
    ecbuild_add_executable( TARGET scm_compare
                            SOURCES
                                scm_compare.F90
                                ../src/handle_err_nc.F90
                            LIBS
                                NetCDF::NetCDF_Fortran
                                fckit
                          )

    
    ecbuild_configure_file(
        test_getini1c.sh.in
        test_getini1c.sh @ONLY
    )

    ecbuild_add_test(
        TYPE     SCRIPT
        COMMAND  test_getini1c.sh
    )

endif()
