#!/usr/bin/env python

import sys
import os
import logging
from datetime import datetime, timedelta

from . import scm_data_files
from . import scm_data_times
from . import scm_data_checks

parent_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
if parent_dir not in sys.path:
    sys.path.append(parent_dir)

from scm_command import factories


def find_oifs_dates_times(file_list, platform="ec-hpc2020"):

    logger = logging.getLogger(__name__)

    # extract the start and end date from the grib files
    for oifs_diag_path in file_list:
        scm_data_checks.paths(oifs_diag_path, "OpenIFS data path")

    # Set the start date using the first file to derive date
    grib_getdate_command = ["grib_get", "-w", "count=1", "-p", "dataDate", file_list[0]]
    logger.info(f"Grib get commamd to retrieve start date is {grib_getdate_command}")

    # grib_get command
    factory = factories[platform]
    command = factory.get("grib_get")
    result = command(filename=file_list[0], count=1, key="dataDate").execute()
    start_date = result.stdout.strip()
    logger.info(f"Start Date for OpenIFS run is {start_date}")  # Output is captured in stdout

    times_list = []
    for file_name in file_list:
        ind = file_name.find("+")
        if ind != -1:
            time_number = file_name[ind + 1 :]
        else:
            logger.error(f"{file_name} unexpected format - EXIT")
            sys.exit()

        # Remove leading zeros but keep at least the last two digits
        time_no_zero = time_number.lstrip("0")
        if len(time_no_zero) < 2:
            time_no_zero = time_number[-2:]  # If the result is less than 2 digits, take the last 2 digits

        times_list.append(time_no_zero)

    return start_date, times_list


def check_oifs_dates_and_times(global_data_source_dict, scm_datetime_dict, user_input):

    logger = logging.getLogger(__name__)

    # This routine needs to
    # check the datetime_dict[datetime_suffix] <= number of source files
    #    if it is les than, then source file list needs to be made equal
    #    Need to include simulation start date and time, so user can pick
    #    dates from within simulation output
    # check the timestep of source files = tstep
    oifs_init_time = user_input["ctrl"]["init_time"]
    oifs_output_tstep = user_input["ctrl"]["tstep"]

    scm_tstep = user_input["scm"]["tstep"]

    #
    # First work out the output times from the OIFS experiment, based on ICMSH files in oifs_fc_data
    #
    oifs_start_date, oifs_output_times = find_oifs_dates_times(global_data_source_dict["spec"], user_input["platform"])

    logger.info(f"Input forecast times available: {oifs_output_times}")

    # Create a list that includes the times in a day
    times_day = []
    for t in oifs_output_times:
        # add initial time so that times_day is alway
        # a days worth of times (not sure if this works with intepolation)
        # This code only works if init time is 00
        if int(t) < 24 + int(oifs_init_time):
            times_day.append(t)
        else:
            break

    # Include some checks to make sure that the defined initial time, lies within the output time for a day
    if oifs_init_time < oifs_output_times[0] or oifs_init_time > oifs_output_times[-1]:
        logger.error(
            f"""User defined initial time ({oifs_init_time}) outside range of simulation times 
                     ({oifs_output_times[0]} to {oifs_output_times[-1]} )Please check settings and/or simulation data - EXITING"""
        )
        sys.exit()

    # Check the user defined timestep versus the output timestep
    oifs_output_dt_integer = int(oifs_output_times[1]) - int(oifs_output_times[0])
    oifs_output_dt_s_float = float(oifs_output_dt_integer * 3600.0)

    if oifs_output_dt_s_float != oifs_output_tstep:
        logger.error(
            f"""user defined output tstep for openifs ({oifs_output_tstep}) is different 
                     to timestep derived from openifs output ({oifs_output_dt_s_float})
                     Please change oifs_output_tstep in the yml file so that it is the same as 
                     openifs output timestep ({oifs_output_dt_s_float})- EXITING"""
        )
        sys.exit()

    if scm_tstep != oifs_output_tstep:
        logger.error(
            f"""the user defined output tstep for openifs data ({oifs_output_tstep}) is 
                     different to scm timestep for forcing, this does not work - EXITING"""
        )
        sys.exit()

    # Once through th above obvious checks, now compare the dates and times from OIFS sim to the user derived dates
    #
    # Derive the number of days and a datetime_dict from oifs_output_times and compare to user defined date range
    #
    number_days = int(int(oifs_output_times[-1]) / 24)
    # Using the input date from testcase dictionary (not scm), which is the dictionary for the simulation, work out the
    # end date. Subtract 1 because the first day is day zero, need to be careful with non-midnight starts
    if number_days > 1:
        date_obj = datetime.strptime(oifs_start_date, "%Y%m%d") + timedelta(days=number_days)
        oifs_end_date = date_obj.strftime("%Y%m%d")
    else:
        oifs_end_date = oifs_start_date

    # Check that the user input for scm is within the daterange of the OIFS data
    if scm_datetime_dict["dates"][0] < oifs_start_date or scm_datetime_dict["dates"][0] > oifs_end_date:
        logger.error(
            f"""User input scm start date ({scm_datetime_dict['dates'][0]}) is out  of range.
                     OpenIFS start date = {oifs_start_date}, end date = {oifs_end_date} - EXITING"""
        )
        sys.exit()

    if scm_datetime_dict["dates"][-1] > oifs_end_date or scm_datetime_dict["dates"][-1] < oifs_start_date:
        logger.error(
            f"""User input scm end date ({scm_datetime_dict['dates'][-1]}) is out of range. 
                     OpenIFS start date = {oifs_start_date}, end date = {oifs_end_date} - EXITING"""
        )
        sys.exit()

    oifs_datetime_dict = scm_data_times.datetime_derivation(oifs_start_date, oifs_end_date, oifs_output_tstep, "oifs")
    #
    # because of the number_days definition, oifs_datetime_dict['datetime_fn_suffix'] will
    # have more values than in global_data_source_dict[key]
    #
    scm_indexes_in_global_source = [
        ind
        for ind, datetime in enumerate(oifs_datetime_dict["datetime_fn_suffix"])
        if datetime in scm_datetime_dict["datetime_fn_suffix"]
    ]
    for key in global_data_source_dict:
        if len(global_data_source_dict[key]) < len(oifs_datetime_dict["datetime_fn_suffix"]):

            logger.warning(
                f"""Length of derived openifs date time list is longer than data source list, 
                           set length to {len(global_data_source_dict[key])}, same global_data_source_dict[key]"""
            )

            oifs_datetime_dict["datetime_fn_suffix"] = oifs_datetime_dict["datetime_fn_suffix"][
                : len(global_data_source_dict[key])
            ]
            break

    for key in global_data_source_dict:

        logger.info(
            f"""Set list of OIFS global source files for {key} so 
                    that list only includes SCM indexes derived for dates and times input by user"""
        )

        global_source_temp = []
        for ind in scm_indexes_in_global_source:
            global_source_temp.append(global_data_source_dict[key][ind])
        # reset the list in global_data_source_dict, so that it only includes dates and
        # times derived from scm_datetime_dict
        global_data_source_dict[key] = global_source_temp

    return global_data_source_dict


def create_oifs_source_file_lists(user_input, global_data_source, datetime_dict):

    logger = logging.getLogger(__name__)

    global_data_source_dict = {}

    ifs_grib_file_prefix_dict = user_input["ifs_grib_file_prefix"]

    logger.info(f"{user_input['data_source']} stored or will be extracted to {global_data_source}.")

    # create a list of all the data files produced by OpenIFS.
    for key, icm_prefix in ifs_grib_file_prefix_dict.items():

        logger.info(
            f"""Create list of files for {key}, with file naming convention 
                    {global_data_source}/{key}_grib_<date_time>,
                    where date_time is {datetime_dict['datetime_fn_suffix'][0]} to {datetime_dict['datetime_fn_suffix'][-1]}"""
        )

        icm_expid_prefix = f"{icm_prefix}{user_input['ctrl']['id']}+0"
        file_list = scm_data_files.find_list(global_data_source, icm_expid_prefix)
        if not file_list:
            logger.error(f"{icm_expid_prefix}* not found in {global_data_source}, please check path - EXITING  ")
            sys.exit()
        else:
            logger.info(f"{icm_expid_prefix} files found in '{global_data_source}'")
        #
        # Use list compression to add path of the OpenIFS or IFS data to the file names in list
        global_data_source_dict[key] = [os.path.join(global_data_source, fn) for fn in file_list]

    global_data_source_dict = check_oifs_dates_and_times(global_data_source_dict, datetime_dict, user_input)

    logger.info(f"""DONE - list of source data files created""")

    return global_data_source_dict
