#!/usr/bin/env python
# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.


import sys
import os
import yaml
import logging

from . import scm_data_checks


def read_yaml(setup_yaml_file):

    # Load the input from the yaml file
    with open(setup_yaml_file, "r") as file:
        user_yaml_input = yaml.safe_load(file)

    # Create empty directory to populate with input from yaml
    user_input = {}

    # get the input from getini1c_setup.yml and set to variables
    user_input["data_source"] = user_yaml_input.get("name", "openifs")

    # get the platform name. Used in the paths for climate data
    user_input["platform"] = user_yaml_input.get("platform", "ec-hpc2020")

    # get the logging level for the script output
    user_input["log_level"] = user_yaml_input.get("log_level", "INFO").upper()

    # setup dictionaries containing user input from yml
    user_input["paths"] = user_yaml_input.get("paths", {})
    user_input["scm"] = user_yaml_input.get("scm", {})
    user_input["ctrl"] = user_yaml_input.get("control", {})

    # if retrieve_data is not set in the yaml file, set to False
    user_input["ctrl"]["retrieve_data"] = user_input["ctrl"].get("retrieve_data", False)
    user_input["ctrl"]["interp_append_clim"] = user_input["ctrl"].get("interp_append_clim", False)

    if user_input["data_source"] == "openifs":
        user_input["ifs_grib_file_prefix"] = user_yaml_input.get("ifs_grib_file_prefix", {})

    user_input["grib_shortnames"] = user_yaml_input.get("grib_shortnames", {})
    user_input["climatology_vars"] = user_yaml_input.get("clim_vars", {})

    # Create directory for the grib files that are used by getini1c, noting
    # this is not the same of the directory with the raw OpenIFS or IFS output.
    # This directory is also the directory to execute getini1c
    user_input["global_in_datadir"] = os.path.join(
        user_input["paths"]["getini1c_data_top"],
        f"data_{user_input['data_source']}_{user_input['ctrl']['grid']}",
    )

    # Create directory to store the netcdf SCM forcing files from getinit1c (there is a file for each time)
    user_input["scm_forcing_datadir"] = os.path.join(
        user_input["paths"]["scm_forcing_out"],
        f"{user_input['data_source']}_{user_input['ctrl']['grid']}",
    )
    # location of the log file to store output
    user_input["scm_forcing_logfile"] = os.path.join(
        user_input["paths"]["scm_forcing_out"],
        f"scm_log_{user_input['data_source']}_{user_input['ctrl']['grid']}.out",
    )

    return user_input, getattr(logging, user_input["log_level"], logging.INFO)


def check_switches_and_paths(user_input):

    logger = logging.getLogger(__name__)

    logger.info(f"Start checks on user-input from yaml file and the derived paths")

    logger.info(f"Data source is {user_input['data_source']} and cycle is {user_input['ctrl']['cycle']}")

    # create dictionary to store all the data paths, this is used in the rest of the script
    path_dict = {}

    # check if path to getini1c executable exists, exit if not
    scm_data_checks.paths(user_input["paths"]["getini1c_bin"], "getini1c_bin directory")
    path_dict["getini1c_exec"] = scm_data_checks.exec(user_input["paths"]["getini1c_bin"], "getini1c")

    # Create the directory and any necessary parent directories
    os.makedirs(user_input["global_in_datadir"], exist_ok=True)
    scm_data_checks.paths(
        user_input["global_in_datadir"],
        "Directory to copy OpenIFS/IFS or extract ERA5 data to",
    )
    path_dict["global_in_datadir"] = user_input["global_in_datadir"]

    # create directory to store the individual forcing files from getini1c (one for each time)
    os.makedirs(user_input["scm_forcing_datadir"], exist_ok=True)
    scm_data_checks.paths(
        user_input["scm_forcing_datadir"],
        "Directory to store individual SCM forcing files (one file for each time) from getini1c",
    )
    path_dict["scm_netcdf_out"] = user_input["scm_forcing_datadir"]
    # Then create a directory to store the full timeseries forcing file and the namelists
    path_dict["scm_nml_merge_nc_dir"] = os.path.join(path_dict["scm_netcdf_out"], "scm_nml_and_merged_nc")
    os.makedirs(path_dict["scm_nml_merge_nc_dir"], exist_ok=True)
    scm_data_checks.paths(
        path_dict["scm_nml_merge_nc_dir"],
        "Directory to store SCM namelists and the merged, full timeseries SCM forcing file (one file for all times) from concat_nc",
    )

    # binary path for getini1c
    bin_path = user_input["paths"]["getini1c_bin"]
    logger.info(f"bin_path is {bin_path}")

    # scm executable
    scm_exec = os.path.join(bin_path, "getini1c")
    logger.info(f"scm_exec is {scm_exec}")

    # timeint executable
    timeint = os.path.join(bin_path, "timeint")
    logger.info(f"timeint is {timeint}")

    scm_data_checks.paths(scm_exec, "Path for the SCM executable")
    path_dict["scm_exec"] = scm_exec

    if user_input["data_source"] == "openifs":

        # contains all the experiments, defined by id from datahub
        expt_dir_top_level = scm_data_checks.env_vars("OIFS_EXPT")
        # Path to the user defined experiment id
        expt_id_path = os.path.join(expt_dir_top_level, user_input["ctrl"]["id"])

        if len(os.listdir(expt_id_path)) > 1:

            logging.warning(
                f"""{expt_id_path} contains dates/directories {os.listdir(expt_id_path)}, 
                            using first date, {os.listdir(expt_id_path)}[0] - MAY BE WRONG!!"""
            )

        initial_run_datetime = os.listdir(expt_id_path)[0]

        path_dict[f"{user_input['data_source']}_data_source"] = os.path.join(expt_id_path, initial_run_datetime)

        logger.info(
            f"""global data for getini1c from {user_input['data_source']} will be 
                         copied from {path_dict[user_input['data_source']+'_data_source']} to 
                         {path_dict['global_in_datadir']} """
        )

    elif user_input["data_source"] == "era5":

        path_dict[f"{user_input['data_source']}_data_source"] = os.path.join(
            user_input["paths"]["getini1c_data_top"],
            f"data_{user_input['data_source']}_mars_retrieval",
        )
        os.makedirs(path_dict[user_input["data_source"] + "_data_source"], exist_ok=True)

        logger.info(
            f"""global data for getini1c from {user_input['data_source']} will be extracted to 
                     {path_dict[f"{user_input['data_source']}_data_source"]} then copied to 
                     {path_dict['global_in_datadir']}"""
        )
    else:
        logger.error(f"scm_data_driver.py ONLY works with OpenIFS at present - EXITING")
        sys.exit()

    if user_input["platform"]:
        # Set the full path for climate versions (this is a nightmare path construction!)
        path_dict["climate_data"] = os.path.join(
            user_input["paths"]["climate_data_toplevel"],
            user_input["ctrl"]["cycle"],
            user_input["ctrl"]["cycle"],
            user_input["ctrl"]["climvers"],
            f"{user_input['ctrl']['res']}{user_input['ctrl']['gtype']}",
        )
        path_dict["vtable"] = os.path.join(
            user_input["paths"]["climate_data_toplevel"],
            user_input["ctrl"]["cycle"],
            user_input["ctrl"]["cycle"],
            "vtables",
            f"vtable_L{str(user_input['ctrl']['levels'])}",
        )
    else:
        path_dict["climate_data"] = os.path.join(
            user_input["paths"]["climate_data_toplevel"],
            user_input["ctrl"]["climvers"],
            f"{user_input['ctrl']['res']}{user_input['ctrl']['gtype']}",
        )
        path_dict["vtable"] = os.path.join(
            user_input["paths"]["climate_data_toplevel"],
            "vtables",
            f"vtable_L{str(user_input['ctrl']['levels'])}",
        )

    scm_data_checks.paths(
        path_dict[f"{user_input['data_source']}_data_source"],
        f"Directory source data for {user_input['data_source']}, which will be copied",
    )
    scm_data_checks.paths(
        path_dict["global_in_datadir"],
        f"Directory containing global {user_input['data_source']} data for getini1c to process",
    )

    logger.info(
        f"""DONE - User input in yaml read succesfully and checks on user input and any derived paths passed."""
    )

    return timeint, path_dict


def latlon_track_check(user_input):

    import pandas as pd

    latlonfile = user_input['scm']['latlonfile']
    datetimebeg = user_input['scm']['datebeg']
    datetimeend = user_input['scm']['dateend']
    #start_time = user_input['scm']['start_time']
    tstep = user_input['scm']['tstep']

    logger = logging.getLogger(__name__)

    # Read the latlon track file
    track_coords = pd.read_csv(latlonfile)

    # Remove whitespace from column names to prevent keyerror in assignment to the list
    track_coords.columns = track_coords.columns.str.strip()

    # Ensure all data in track_coords is treated as strings, so that it matches the string 
    # input from user_input
    track_coords = track_coords.astype(str)

    # Normalize column names to lowercase
    track_coords.columns = track_coords.columns.str.lower()

    # Convert datetime column to pandas datetime format
    track_coords['datetime'] = pd.to_datetime(track_coords['datetime'], format='%Y%m%d%H')
    logger.info(f"""latlon_data dates for samples in {latlonfile} : {track_coords['datetime']}""")
    
    # Convert datetime column to string format for comparison
    track_coords['datetime_str'] = track_coords['datetime'].dt.strftime('%Y%m%d%H')

    # Check if any datetime starts with datebeg or dateend
    if not (track_coords['datetime_str'].str.startswith(datetimebeg).any() and 
            track_coords['datetime_str'].str.startswith(datetimeend).any()):
        logger.error(f"User-defined start date {datetimebeg} or end date {datetimeend} does not exist in the track data. Exiting.")
        sys.exit()
    
    logger.info(f"User-defined start date {datetimebeg} and end date {datetimeend} exist in the track data.")

    # Convert user-defined start and end dates to panadas datetime format
    datebeg_starttime = pd.to_datetime(datetimebeg, format='%Y%m%d%H')
    # Note: Increment dateend by one day to include the full day
    dateend_endtime = pd.to_datetime(datetimeend, format='%Y%m%d%H') + pd.Timedelta(days=1)
    print(f"datebeg_starttime = {datebeg_starttime}")
    print(f"dateend_endtime = {dateend_endtime}")

    # Compare dateend_endtime with the last datetime in the track data
    if track_coords['datetime'].iloc[-1] < dateend_endtime:
        dateend_endtime = track_coords['datetime'].iloc[-1]

    # Filter track data to include only rows within the specified date range
    filtered_track_coords = track_coords[
        track_coords['datetime'].between(datebeg_starttime, dateend_endtime)
    ]

    # Calculate time differences between consecutive rows
    time_diffs = filtered_track_coords['datetime'].diff().dt.total_seconds().dropna()

    # Check if all time differences are the same
    if time_diffs.nunique() == 1:
        logger.info(f"All time differences in the track file are the same: {time_diffs.iloc[0]} seconds.")
    else:
        logger.error(f"Time differences in the track file are not consistent: {time_diffs.tolist()}. Exiting.")
        sys.exit()

    # Check if the time difference is greater than or equal to 3600 seconds
    if time_diffs.iloc[0] < 3600:
        logger.warning(f"Time difference in the track file is less than 3600 seconds: {time_diffs.iloc[0]} seconds.")
    else:
        logger.info(f"Time difference in the track file is greater than or equal to 3600 seconds: {time_diffs.iloc[0]} seconds.")

    # Filter data to include only rows where datetime is on the hour
    filtered_track_coords = filtered_track_coords[
        filtered_track_coords['datetime'].dt.minute.eq(0) & filtered_track_coords['datetime'].dt.second.eq(0)
    ]

    if filtered_track_coords.empty:
        logger.error("No data points on the hour found in the track file. Exiting.")
        sys.exit()

    # Update latlon_data with filtered data
    latlon_data = {
        'datetime': filtered_track_coords['datetime'].dt.strftime('%Y%m%d%H').tolist(),
        'longitude': filtered_track_coords['longitude'].tolist(),
        'latitude': filtered_track_coords['latitude'].tolist(),
    }
    logger.info(f"""latlon_data dates for samples in {latlonfile} : {latlon_data}, """)
    latlon_data['tstep'] = time_diffs.iloc[0]

    latlon_data['end_time'] = filtered_track_coords['datetime'].iloc[-1].hour

    logger.info(f"Filtered data points to {len(filtered_track_coords)} entries on the hour.")
    logger.info(f"Time differences after filtering: {time_diffs.tolist()}")
    logger.info(f"End_time after filtering: {latlon_data['end_time']}")

    return latlon_data


def set_latlon_and_check(user_input):

    import pandas as pd

    logger = logging.getLogger(__name__)

    logger.info(f"Set latitude and longtitude from user input")

    # do checks to identify whether the user request
    # 1) one column (or lat and lon) with location that is fixed in space
    # 2) an array of columns with locations that are fixed in space - experimental
    # 3) one column with location that varies in space with time (lat, lon and dates read from file)
    #
    # Check the switch settings for the SCM extraction, i.e. one column, a track of one column or
    # an array of columns. Only one can be on at a time
    #
    scm_extract_switch = {key: value for key, value in user_input["scm"].items() if key.startswith("extract_")}

    if sum(scm_extract_switch.values()) == 1:
        if scm_extract_switch["extract_scm_column"]:
            if len(user_input["scm"]["lat"]) != 1:
                logger.error(
                    f"""extract_scm_column is True but an array of {len(user_input['scm']['lat'])} columns with different latitude and longitude has been requested
                           When extract_scm_column is True only one lat and lon must be provided, please correct this in setup.yml - EXITING"""
                )
                sys.exit()
            else:
                logger.info(
                    f"""extract_scm_column is true so data extract for column nearest to latitude = {user_input['scm']['lat'][0]} and longitude = {user_input['scm']['lon'][0]}"""
                )
        if scm_extract_switch["extract_scm_track"]:
            if user_input["scm"]["latlon_from_file"]:
                logger.info(
                    f"""extract_scm_track is true so data extract for columns along a lat-lon track defined in file {user_input['scm']['latlonfile']}"""
                )
            else:
                logger.error(
                    f"""extract_scm_track is true but latlon_from_file is False, this does not work. 
                             Please set latlon_from_file=True and check there is a valid path for latlonfile - EXITING"""
                )
                sys.exit()
        if scm_extract_switch["extract_scm_array"]:
            logger.info(
                f"""extract_scm_array is true, so data extract for columns nearest to the following lat-lon pairs 
                        latitude = {user_input['scm']['lat']} and longitude = {user_input['scm']['lon']}"""
            )
    else:
        logger.error(
            f"""Either none or more than one of the following switches are set to True 
                     {scm_extract_switch}. 
                     Please check the setup.yml and set ONLY one of these to True to proceed - EXITING"""
        )
        sys.exit()

    if scm_extract_switch["extract_scm_track"]:
        #
        # At present, there is no interpolation of track. It is therefore important to check
        # the dates for the track and ensure there are matching dates in the globa data
        #
        logger.info(f"Latitude and Longitude to be read from file {user_input['scm']['latlonfile']}")
        scm_data_checks.paths(user_input["scm"]["latlonfile"], "Latitude and Longitude track file")

        latlon_data = latlon_track_check(user_input)

    else:

        # set number of lon points and lat points
        lat_points = len(user_input["scm"]["lat"])
        lon_points = len(user_input["scm"]["lon"])

        # check lat and lon lists are the same size, if not exit
        if lat_points != lon_points:
            logger.error(
                f"Length of latitude ({lat_points}) and longitude ({lon_points}) lists are not equal, this does not work - EXITING"
            )
            sys.exit()

        logger.info(f"{lat_points} column(s) with different latitude and longitude, which are fixed in time, requested")

        latlon_data = {}
        latlon_data["latitude"] = []
        latlon_data["longitude"] = []
        latlon_data['tstep'] = user_input['scm']['tstep']

        for ind, lat in enumerate(user_input["scm"]["lat"]):
            latlon_data["latitude"].append(lat)
            latlon_data["longitude"].append(user_input["scm"]["lon"][ind])

    # create a latlon string for namelist file names in run.creat_nml
    latlon_data["latlon_str"] = []

    for ind, lat in enumerate(latlon_data["latitude"]):
        # Test to see if latitude is positive or negative and then
        # set string to North (N) or South (S) appropriately.
        if "-" in str(lat):
            lat_str = f"{lat}S"
            lat_str = lat_str.replace("-", "")
        else:
            lat_str = f"{lat}N"

        # Test to see if longitude is positive or negative and then
        # set string to East (E) or West (W) appropriately.
        if "-" in str(latlon_data["longitude"][ind]):
            lon_str = f"{latlon_data['longitude'][ind]}W"
            lon_str = lon_str.replace("-", "")
        else:
            lon_str = f"{latlon_data['longitude'][ind]}E"
        #
        # create a latlon string for namelist file names in run.creat_nml
        latlon_data["latlon_str"].append(f"{lat_str}_{lon_str}")

    logger.info(
        f"""DONE - Requested Latitude and Longitude pair(s) (used in the namelist and netcdf output filenames) are 
                {latlon_data['latlon_str']}"""
    )

    return latlon_data
