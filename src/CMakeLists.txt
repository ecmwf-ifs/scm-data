# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

if( HAVE_BUILD_TOOLS )

        LIST( APPEND COMMON_SOURCES 
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
                getini1c_run.F90
        )

        LIST( APPEND COMMON_DEP_LIBS
                atlas_f
                trans_dp
                fiat
                eccodes_f90
                NetCDF::NetCDF_Fortran
        )

        # getini1c tool (legacy tool)
        ecbuild_add_executable( TARGET getini1c
                                SOURCES ${COMMON_SOURCES} getini1c.F90
                                INCLUDES interface/
                                DEFINITIONS ${PLUGINS_DEFINITIONS}
                                LIBS ${COMMON_DEP_LIBS}
                              )

        # getini1c_cli tool
        ecbuild_add_executable( TARGET getini1c_cli
                                SOURCES ${COMMON_SOURCES} getini1c_cli.F90
                                INCLUDES interface/
                                DEFINITIONS ${PLUGINS_DEFINITIONS}
                                LIBS ${COMMON_DEP_LIBS}
                              )

endif()


# plume plugin
if (HAVE_SCM_PLUME_PLUGIN)
        add_subdirectory( plume-plugin-scm )
endif()