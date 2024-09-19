
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

    DEFINITIONS
        ${PLUGINS_DEFINITIONS}

    INCLUDES
        $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}>
        $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}>
        "${eckit_INCLUDE_DIRS}"
        "${CMAKE_SOURCE_DIR}"
        testing_interface/
        ../../src/interface/

    LIBS
        atlas_f
        trans_dp
        fiat
        eccodes_f90
        fckit
        plume_f
        NetCDF::NetCDF_Fortran
)
