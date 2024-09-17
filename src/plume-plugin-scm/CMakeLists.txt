
# Plume Fortran macro
include(${plume_INTERFACE_PLUGIN_CMAKE_FILE})


plume_plugin_interface(
    PLUGIN_TEMPLATE
        ${CMAKE_CURRENT_SOURCE_DIR}/plugin_scm_data.F90.in
    PLUGIN_NAME 
        "PluginSCMData"
    PLUGIN_VERSION 
        "0.1.0"
    PLUGIN_SHA 
        "undefined"
    PLUGIN_REQUIRED_PARAMS
        "NSTEP:INT"
        "stl1:ATLAS_FIELD"
        "stl2:ATLAS_FIELD"
        "stl3:ATLAS_FIELD"
        "stl4:ATLAS_FIELD"
        "swvl1:ATLAS_FIELD"
        "swvl2:ATLAS_FIELD"
        "swvl3:ATLAS_FIELD"
        "swvl4:ATLAS_FIELD"
        "sd:ATLAS_FIELD"
        "src:ATLAS_FIELD"
        "skt:ATLAS_FIELD"
        "ci:ATLAS_FIELD"
        "lmlt:ATLAS_FIELD"
        "lmld:ATLAS_FIELD"
        "lblt:ATLAS_FIELD"
        "ltlt:ATLAS_FIELD"
        "lshf:ATLAS_FIELD"
        "lict:ATLAS_FIELD"
        "licd:ATLAS_FIELD"
        "tsn:ATLAS_FIELD"
        "asn:ATLAS_FIELD"
        "rsn:ATLAS_FIELD"
        "sst:ATLAS_FIELD"
        "istl1:ATLAS_FIELD"
        "istl2:ATLAS_FIELD"
        "istl3:ATLAS_FIELD"
        "istl4:ATLAS_FIELD"
        "lsm:ATLAS_FIELD"
        "sr:ATLAS_FIELD"
        "al:ATLAS_FIELD"
        "aluvp:ATLAS_FIELD"
        "alnip:ATLAS_FIELD"
        "aluvd:ATLAS_FIELD"
        "alnid:ATLAS_FIELD"
        "lai_lv:ATLAS_FIELD"
        "lai_hv:ATLAS_FIELD"
        "sdfor:ATLAS_FIELD"
        "slt:ATLAS_FIELD"
        "sdor:ATLAS_FIELD"
        "isor:ATLAS_FIELD"
        "anor:ATLAS_FIELD"
        "slor:ATLAS_FIELD"
        "lsrh:ATLAS_FIELD"
        "cvh:ATLAS_FIELD"
        "cvl:ATLAS_FIELD"
        "tvh:ATLAS_FIELD"
        "tvl:ATLAS_FIELD"
        "cl:ATLAS_FIELD"
        "dl:ATLAS_FIELD"
        "cc:ATLAS_FIELD"
        "ciwc:ATLAS_FIELD"
        "clwc:ATLAS_FIELD"
        "crwc:ATLAS_FIELD"
        "cswc:ATLAS_FIELD"
        "d:ATLAS_FIELD"
        "lnsp:ATLAS_FIELD"
        "q:ATLAS_FIELD"
        "t:ATLAS_FIELD"
        "vo:ATLAS_FIELD"
        "z:ATLAS_FIELD"
        "w:ATLAS_FIELD"
        "u:ATLAS_FIELD"
        "v:ATLAS_FIELD"
        "100u:ATLAS_FIELD"
        "100v:ATLAS_FIELD"
        "tcw:ATLAS_FIELD"
    PLUGINCORE_NAME
        "PluginCoreSCMData"
)


# Append the correct extension (SP or DP)
if (HAVE_SCM_SINGLE_PRECISION)
    set(PLUGIN_NAME_EXT "SP")
else()
    set(PLUGIN_NAME_EXT "DP")
endif()


# SCM plugin Single Precision
ecbuild_add_library(
    TARGET plugin_scm_data_${PLUGIN_NAME_EXT}

    SOURCES
        ${INTERFACE_PLUGIN_SOURCES}
        ../module/yomvar.F90
        ../calcgeost.F90
        ../handle_err_nc.F90
        ../nearest_distance.F90
        ../ncdf_varsetup1c.F90
        ../ncdf_varwrite1c.F90
        ../su_wrt_nc.F90
        ../wrt1c_nc.F90
        create_nodefield.F90
        fillvar_from_plume.F90
        plugin_impl.F90
        plugin_utils.F90
        process_plume_fields.F90
        vert_coord_tables.F90

    DEFINITIONS
        ${PLUGINS_DEFINITIONS}

    PUBLIC_INCLUDES
       $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}>
       $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}>

    PRIVATE_INCLUDES
        "${eckit_INCLUDE_DIRS}"
        "${CMAKE_SOURCE_DIR}"
        ../interface/
        ./interface/

    PRIVATE_LIBS
        fckit
        fiat
        atlas_f
        NetCDF::NetCDF_Fortran
        plume_f
        plume_plugin
)


if (HAVE_TESTS)
    add_subdirectory(plugin_tester)
endif()
