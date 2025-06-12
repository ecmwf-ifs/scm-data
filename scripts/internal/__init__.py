# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

import os

# Get the path of the script
script_path = os.path.realpath(__file__)
script_dir = os.path.dirname(script_path)

# Set the path to the templates directory
templ_path = os.path.join(script_dir, "../templates/48r1")
