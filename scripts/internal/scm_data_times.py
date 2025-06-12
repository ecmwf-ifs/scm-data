#!/usr/bin/env python
# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.


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

def datetime_derivation(user_start_date, user_end_date, user_tstep, message_id) :
    
    # This function creates a dictionary of dates and times for the SCM forcing data
    #
    # The dates are in the format YYYYMMDD, the times are in the format HH:MM:SS
    # The function takes the following arguments:
    #   user_start_date : start date in the format YYYYMMDD
    #   user_end_date : end date in the format YYYYMMDD
    #   user_tstep : time step in seconds
    #   user_start_time : start time in the format HH:MM:SS
    #   message_id : a string to identify the message in the log file
    # 
    # The function returns a dictionary with the following keys:
    #   'dates' : a list of dates in the format YYYYMMDD
    #   'times_for_mars' : a list of times in the format HH:MM:SS
    #   'times_for_each_day' : a list of times in the format HH:MM:SS
    #   'datetime_fn_suffix' : a list of date and time strings in the format YYYYMMDD_HHMM
    #
    # Note: 
    #   this function is run for all requests to understand the user input for dates and times. 
    #   If a feature track is used dictionary outputs will be checked and possibly modified by the 
    #   feature_track function.

    logger = logging.getLogger(__name__)

    datetime_dict = {
        'dates': [],
        'times_for_mars': [],
        'times_for_each_day': [],
        'datetime_fn_suffix': []
    }

    date_format = "%Y%m%d%H"
    time_format = "%H:%M:%S"
    
    start_datetime = datetime.datetime.strptime(user_start_date, date_format)
    end_datetime = datetime.datetime.strptime(user_end_date, date_format)
    timestep_seconds = user_tstep
    timestep_hours = timestep_seconds / 3600.0

    # Set current date object that is incremented by day
    current_datetime = start_datetime

    print(current_datetime)
    while current_datetime <= end_datetime:
        # Write dates to datetime_dict up to user-defined end date
        date_key = current_datetime.strftime("%Y%m%d")
        datetime_dict['dates'].append(date_key)

        print(current_datetime, date_key)
        # Extract time in the format HH:MM:SS
        formatted_time = current_datetime.strftime(time_format)
        datetime_dict['times_for_each_day'].append(formatted_time)        

        # Output current_datetime in the format YYYYMMDDHHMM, which the required format for the rest of scripts
        formatted_datetime = current_datetime.strftime("%Y%m%d%H%M")
        datetime_dict['datetime_fn_suffix'].append(formatted_datetime)

        current_datetime += datetime.timedelta(hours=timestep_hours)

    # Generate times for the mars request, this needs to be for one whole day, using the user defined timestep
    time_for_mars = []
    mars_time = datetime.datetime.strptime("00:00:00", time_format)
    end_of_day = datetime.datetime.strptime("23:59:59", time_format)

    while mars_time <= end_of_day:
        time_for_mars.append(mars_time.strftime(time_format))
        print(mars_time.strftime(time_format))
        mars_time += datetime.timedelta(hours=timestep_hours)

    # # Add time_for_mars to the dictionary
    datetime_dict['times_for_mars'] = time_for_mars


    # Log the results
    message_dict = {'scm': 'scm forcing', 'oifs': 'oifs testcase source data'}
    for key, list_values in datetime_dict.items():
        logger.info(f"User defined {key} for {message_dict[message_id]}: {list_values}")

    return datetime_dict