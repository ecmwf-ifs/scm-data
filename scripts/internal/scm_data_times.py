#!/usr/bin/env python

import datetime
import sys
import time
import logging
from contextlib import contextmanager


@contextmanager
def log_execution_time(key, execution_time_dict):
    # Testing a context manager to log the execution time of a block of code.
    # key is a string, which represents key to use in the execution_time_dict
    # Ideally the string is the module.function name
    # execution_time_dict is the dictionary to store the execution times

    start_time = time.time()
    yield
    execution_time_dict[key] = time.time() - start_time


def datetime_derivation(user_start_date, user_end_date, user_tstep, message_id):

    logger = logging.getLogger(__name__)

    datetime_dict = {}

    datetime_dict["dates"] = []
    datetime_dict["times_for_mars"] = []
    datetime_dict["times_for_each_day"] = []
    datetime_dict["datetime_fn_suffix"] = []

    date_format = "%Y%m%d"
    time_format = "%H:%M:%S"

    start_date = datetime.datetime.strptime(user_start_date, date_format)
    end_date = datetime.datetime.strptime(user_end_date, date_format)
    timestep_seconds = user_tstep

    timestep_hours = timestep_seconds / 3600.0

    # set current date object that is incremented by day
    current_date = start_date
    # write times in a day to datetime dict. We initially, assume the start time is
    # 00, so that all days get a full day with the defined timestep. If openifs then
    # trim date and start depending on user input
    init_time = "00"

    while current_date <= end_date:
        # Write dates to datetime_dict up to user defined end date,
        # using date format YY-MM-DD (used in mars request)
        #
        date_key = current_date.strftime("%Y%m%d")
        datetime_dict["dates"].append(date_key)
        #
        # Set index counter for looping over time
        t = int(init_time)
        #
        # Set current_date_time object, which is incremented by time
        current_date_time = current_date

        while t < 24:
            #  with each time for a date. The date format is YYYYMMDD, time format is
            # HH:MM:SS

            if current_date == start_date:
                # Create list of times for one day, this is needed for the mars request...
                datetime_dict["times_for_mars"].append(current_date_time.strftime(time_format))

            # ...also store a list of times for each day. Having 2 lists is not elegant but it is
            # functional for now
            datetime_dict["times_for_each_day"].append(current_date_time.strftime(time_format))

            # creates a list of date and time string, format is YYYYMMDD_HHMM. This acts as a file suffix for getini1c input
            # Creating the list here is a convenience
            datetime_dict["datetime_fn_suffix"].append(
                current_date.strftime(date_format) + current_date_time.strftime("%H") + current_date_time.strftime("%M")
            )

            t += int(timestep_hours)

            current_date_time += datetime.timedelta(hours=timestep_hours)

        current_date += datetime.timedelta(days=1)

    message_dict = {
        "scm": "scm forcing",
        "oifs": "oifs testcase source data",
    }

    for key, list_values in datetime_dict.items():
        logger.info(f"User defined {key} for {message_dict[message_id] }: {list_values}")

    return datetime_dict


def feature_track(latlonfile, date_time_list):

    import csv

    # function read file with date and time, latitude, longitude
    # compares the read date and time to date_time to check if
    # dates and times exist in date_time and then check
    # if the track date_time have same tstep as output tstep.

    # Open the feature tracking file - assumed to be csv, with
    # format date_time, lat, lon

    logger = logging.getLogger(__name__)

    track_dates = []
    date_time_tmp = []
    lat = []
    lon = []

    logger.info(f"Read Latitude and Longitude from track file {latlonfile}")

    with open(latlonfile, mode="r") as file:
        latlonfile_read = csv.reader(file)

        for row in latlonfile_read:
            track_dates.append(row[0])

    if len(track_dates) < len(date_time_list):
        logger.error(
            f"""Number of dates and times in feature track less than user defined/derived times
                         This could be the result of coarser output timestep or a problem - EXITING"""
        )
        sys.exit()

    # check for common dates to ensure there is overlap between track and the simulation
    # output dates
    common_dates = [date_time for date_time in track_dates if date_time in date_time_list]

    if len(common_dates) > 0:
        logger.info(
            f"There are common dates between feature track in {latlonfile} and the simulation output - Continue."
        )
    else:
        logger.error(
            f"There are NO common dates between feature track in {latlonfile} and the simulation output - EXITING."
        )
        sys.exit()

    with open(latlonfile, mode="r") as file:
        latlonfile_read = csv.reader(file)

        for row in latlonfile_read:
            for ind, dt in enumerate(date_time_list):
                if dt in row:
                    date_time_tmp.append(dt)
                    lat.append(row[1])
                    lon.append(row[2])
                    break

    logger.info(f"Latitude list is : {lat}")
    logger.info(f"Longitude list is : {lon}")

    logger.info(f"DONE - Latitude and Longitude read from track file {latlonfile}")

    return lat, lon
