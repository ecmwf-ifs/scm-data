
# Surrogate model serving SCM GRIB fields
ecbuild_add_executable(
    TARGET testing_model

    SOURCES
    
        # test-specific sources
        fill_and_write.F90
        grib_fields_provider.F90
        testing_model.F90

        # plugin sources
        ../plugin_utils.F90        
        
        # common sources
        ../../rdnam.F90
        ../../fillvar.F90
        ../../compute_fields.F90
        ../../module/yomvar.F90
        ../../nearest_distance.F90
        ../../su_wrt_nc.F90
        ../../wrt1c_nc.F90
        ../../calcgeost.F90
        ../../handle_err_nc.F90
        ../../ncdf_varsetup1c.F90
        ../../ncdf_varwrite1c.F90
        ../../read_grib_scm.F90

    DEFINITIONS
        ${PLUGINS_DEFINITIONS}        

    INCLUDES
        $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}>
        $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}>
        "${eckit_INCLUDE_DIRS}"
        "${CMAKE_SOURCE_DIR}"
        testing_interface/
        ../../interface/

    LIBS
        atlas_f
        trans_dp
        fiat
        eccodes_f90
        fckit
        plume_f
        NetCDF::NetCDF_Fortran
)
