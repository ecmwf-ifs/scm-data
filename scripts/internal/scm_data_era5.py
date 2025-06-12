# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

import os
import logging
import sys

from . import scm_data_checks

parent_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
if parent_dir not in sys.path:
    sys.path.append(parent_dir)

from scm_command import factories


def create_era_source_file_lists(user_input, global_data_source, datetime_dict):
    """
    Create a list of files for each param in user_input['grib_shortnames']
    """

    logger = logging.getLogger(__name__)

    global_data_source_dict = {}

    logger.info(f"{user_input['data_source']} stored or will be extracted to {global_data_source}.")

    for key in user_input["grib_shortnames"]:

        logger.info(
            f"""Create list of files for {key}, with file naming convention 
                    {global_data_source}/{key}_grib_<date_time>,
                    where date_time is {datetime_dict['datetime_fn_suffix'][0]} to {datetime_dict['datetime_fn_suffix'][-1]}"""
        )

        global_data_source_dict[key] = []

        for date_time_suffix in datetime_dict["datetime_fn_suffix"]:

            file_path = os.path.join(global_data_source, f"{key}_grib_{date_time_suffix}")
            global_data_source_dict[key].append(file_path)

    logger.info(f"""DONE - list of source data files created""")

    return global_data_source_dict


def retrieve_era5_data(user_input, path_dict, datetimes):

    # set up the path for storing the retrieved mars data
    # No need to makedirs this because already made in getinit1c_input
    mars_ret_dir = path_dict[user_input["data_source"] + "_data_source"]

    # set up the path for storing the mars requests
    #
    mars_req_dir = os.path.join(mars_ret_dir, "mars_requests")
    os.makedirs(mars_req_dir, exist_ok=True)
    scm_data_checks.paths(mars_req_dir, "Directory to store MARS requests for ERA extraction ")

    for param_type, grib_names in user_input["grib_shortnames"].items():

        factories[user_input["platform"]]["mars_request"](
            config_ctr=user_input["ctrl"],
            param_type=param_type,
            grib_names=grib_names,
            mars_req_dir=mars_req_dir,
            mars_ret_dir=mars_ret_dir,
            datetimes=datetimes,
        ).execute()
