# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

if( HAVE_SCM_TOOLS )

        # timeint tool
        ecbuild_add_executable( TARGET timeint

                SOURCES
                        timeint.cc
                        ec_datetime.c

                INCLUDES
                        interface/

                DEFINITIONS
                        ${SCM_TOOLS_DEFINITIONS}
                LIBS
                        eckit_option
                        eccodes
        )

endif()