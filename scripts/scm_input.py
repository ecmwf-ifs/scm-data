# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

import logging

from internal import scm_data_era5
from internal import scm_data_oifs
from internal import scm_data_files
from internal import scm_data_checks


class DataHandler:
    """
    Base class for handling data.
    """

    def __init__(self, user_config, path_dict, scm_datetime_dict, time_logger):
        self.user_config = user_config
        self.path_dict = path_dict
        self.scm_datetime_dict = scm_datetime_dict
        self.time_logger = time_logger

        self.global_data_source_dict_ = None
        self.getini1c_global_grib_dict_ = None

        # generates list of "source" files
        self.create_source_file_list()

        # generates list of "destination" files
        self.create_destination_file_list()

    def create_source_file_list(self):
        with self.time_logger.log_execution_time(f"{type(self).__name__}.create_source_file_list"):
            self.global_data_source_dict_ = self.create_source_file_list_()

    def retrieve_data(self):

        # retrieve data
        with self.time_logger.log_execution_time(f"{type(self).__name__}.retrieve_data"):
            self.retrieve_data_()

        # check that "source" files are in place
        self.check_source_file_list()

    def create_destination_file_list(self):
        with self.time_logger.log_execution_time(f"{type(self).__name__}.create_destination_file_list"):
            self.getini1c_global_grib_dict_ = scm_data_files.create_destination_file_lists(
                self.user_config["grib_shortnames"],
                self.path_dict["global_in_datadir"],
                self.scm_datetime_dict,
            )

    def check_source_file_list(self):
        with self.time_logger.log_execution_time(f"{type(self).__name__}.check_source_file_list"):
            for key, file_path_list in self.global_data_source_dict.items():
                for file_path in file_path_list:
                    scm_data_checks.paths(file_path, f"expected retrieved {key} file", quiet=True)

    def copy_grib_files(self):
        with self.time_logger.log_execution_time(f"{type(self).__name__}.copy_grib_files"):
            scm_data_files.copy_grib_list(
                self.global_data_source_dict,
                self.getini1c_global_grib_dict,
                self.user_config["grib_shortnames"],
                platform=self.user_config["platform"],
            )
        self.check_missing()

    def check_missing(self):
        with self.time_logger.log_execution_time(f"{type(self).__name__}.check_missing"):
            scm_data_files.check_missing(
                self.global_data_source_dict,
                self.getini1c_global_grib_dict,
                self.user_config["grib_shortnames"],
                platform=self.user_config["platform"],
            )

    @property
    def getini1c_global_grib_dict(self):
        if self.getini1c_global_grib_dict_ is None:
            logger = logging.getLogger(__name__)
            logger.error(f"getini1c_global_grib_dict_ is None! - EXITING")
            raise ValueError
        else:
            return self.getini1c_global_grib_dict_

    @property
    def global_data_source_dict(self):
        if self.global_data_source_dict_ is None:
            logger = logging.getLogger(__name__)
            logger.error(f"global_data_source_dict is None! - EXITING")
            raise ValueError
        else:
            return self.global_data_source_dict_

    def create_source_file_list_(self):
        raise NotImplementedError(f"Method create_source_file_list_ not implemented in class {type(self).__name__}")

    def retrieve_data_(self):
        raise NotImplementedError(f"Method retrieve_data_ not implemented in class {type(self).__name__}")


class DataERA5(DataHandler):
    """
    ERA5 data handler.
    """

    def __init__(self, user_config, path_dict, scm_datetime_dict, time_logger):
        super().__init__(user_config, path_dict, scm_datetime_dict, time_logger)

    def create_source_file_list_(self):
        global_data_source = self.path_dict[f"{self.user_config['data_source']}_data_source"]
        global_data_source_dict = scm_data_era5.create_era_source_file_lists(
            self.user_config, global_data_source, self.scm_datetime_dict
        )
        return global_data_source_dict

    def retrieve_data_(self):
        scm_data_era5.retrieve_era5_data(self.user_config, self.path_dict, self.scm_datetime_dict)


class DataOpenIFS(DataHandler):
    """
    OpenIFS data handler.
    """

    def __init__(self, user_config, path_dict, scm_datetime_dict, time_logger):
        super().__init__(user_config, path_dict, scm_datetime_dict, time_logger)

    def create_source_file_list_(self):
        global_data_source = self.path_dict[f"{self.user_config['data_source']}_data_source"]
        global_data_source_dict = scm_data_oifs.create_oifs_source_file_lists(
            self.user_config, global_data_source, self.scm_datetime_dict
        )
        return global_data_source_dict

    def retrieve_data_(self):
        pass


def factory(key):

    logger = logging.getLogger(__name__)

    classes = {"era5": DataERA5, "openifs": DataOpenIFS}

    try:
        return classes[key]
    except KeyError:
        logger.error(f"Data source input by user ({key}) not recognised. Choices [era5,openifs] - EXITING")
        raise KeyError
