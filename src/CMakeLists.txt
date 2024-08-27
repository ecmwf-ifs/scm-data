
if( HAVE_BUILD_TOOLS )

        ecbuild_add_executable( TARGET getini1c
                                SOURCES 
                                        getini1c.F90
                                        read_grib_scm.F90
                                        module/yomvar.F90
                                        rdnam.F90
                                        nearest_distance.F90
                                        calcgeost.F90
                                        fillvar.F90
                                        compute_fields.F90
                                        handle_err_nc.F90
                                        ncdf_varsetup1c.F90
                                        ncdf_varwrite1c.F90
                                        su_wrt_nc.F90
                                        wrt1c_nc.F90
                                INCLUDES
                                        interface/
                                LIBS 
                                        atlas_f
                                        trans_dp
                                        fiat
                                        eccodes_f90
                                        NetCDF::NetCDF_Fortran
                              )
endif()


# plume plugin
if (HAVE_PLUME_PLUGIN)
        add_subdirectory( plume_plugin )
endif()

# Copy test data to build directory (if tests are enabled)
if (HAVE_TESTS)
        add_custom_command(TARGET getini1c POST_BUILD
                           COMMAND ${CMAKE_COMMAND} -E copy_directory
                           ${CMAKE_CURRENT_SOURCE_DIR}/../tests/data
                           ${CMAKE_BINARY_DIR}/tests/data
                           )
endif()