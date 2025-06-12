# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

import json


from internal import scm_data_input
from internal import scm_namelist


class UserConfigBase:
    """
    Base class for configuration
    """

    def __init__(self, config_file):
        self.config_file = config_file
        self.config = {}

    def check_config(self):
        pass

    def __getitem__(self, key):
        return self.config[key]
    
    def __setitem__(self, key, value):
        self.config[key] = value

    def __str__(self):
        return json.dumps(self.config, indent=4)


class UserConfig(UserConfigBase):
    """
    User configuration
    """

    def __init__(self, config_file):

        super().__init__(config_file)

        # Read the user configuration yaml file
        self.config, _ = scm_data_input.read_yaml(config_file)


class NamelistConfig(UserConfigBase):
    """
    Namelist configuration
    """

    def __init__(self, config_file):

        super().__init__(config_file)

        # Read the namelist configuration yaml file
        self.config = scm_namelist.read_yaml(config_file)
