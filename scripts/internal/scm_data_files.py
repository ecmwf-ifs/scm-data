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
from pathlib import Path
import os
import shutil
import logging
import glob

parent_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
if parent_dir not in sys.path:
    sys.path.append(parent_dir)

from scm_command import factories


def find_list(dir_path, file_type="NONE"):

    logger = logging.getLogger(__name__)

    logger.debug(f"Searching for {file_type} using find_list")

    try:
        # Create a Path object for the directory
        path = Path(dir_path)
        if file_type == ".nc":
            files = list(path.glob("*.nc"))
        elif "ICM" in file_type:
            # List all files in the given directory that start with the given prefix
            # use sorted to ensure the list is ordered by diagnostics output time
            files = sorted([f.name for f in path.iterdir() if f.is_file() and f.name.startswith(file_type)])
        else:
            logger.error(f"{file_type} not recognised - EXITING!")
            sys.exit()
        return files
    except FileNotFoundError as e:
        logger.warning(f"{e}")
        return []
    except PermissionError as e:
        logger.warning(f"{e}")
        return []


def create_destination_file_lists(grib_fn_vars, getini1c_datadir, datetime_dict):

    logger = logging.getLogger(__name__)

    getini1c_global_grib_dict = {}

    for key in grib_fn_vars:

        logger.info(
            f"""Create a list of destination file pathnames with format 
                    {getini1c_datadir}/{key}_grib_<date_time_suffix>"""
        )

        getini1c_global_grib_dict[key] = []

        for date_time_suffix in datetime_dict["datetime_fn_suffix"]:

            file_path = os.path.join(getini1c_datadir, key + "_grib_" + date_time_suffix)

            getini1c_global_grib_dict[key].append(file_path)

    logger.info(f"""DONE - List of destination file pathnames for created. """)

    return getini1c_global_grib_dict


def copy_file_list(global_data_source_dict, getini1c_global_grib_dict):

    logger = logging.getLogger(__name__)

    for key, global_fn_list in global_data_source_dict.items():

        logger.info(
            f"""Start copy {key} source files, in {os.path.dirname(global_data_source_dict[key][0])} 
                    to {os.path.dirname(getini1c_global_grib_dict[key][0])}"""
        )
        # copyfiles
        for ind, global_fn in enumerate(global_fn_list):

            source_filepath = global_fn
            destination_filepath = getini1c_global_grib_dict[key][ind]

            logger.debug(f"Copy {source_filepath} to {destination_filepath}")
            shutil.copy(source_filepath, destination_filepath)

    logger.info(f"""DONE - All files copied to getini1c_datadir""")


def scm_netcdf_filelist(scm_dict, datetime, scm_forcing_datadir, latlon):

    logger = logging.getLogger(__name__)

    scm_netcdf_filelist_dict = {}

    logger.info(
        f"""Create the list of destination netcdf filenames for the extracted column data.
                Format for the final netcdf name is {scm_forcing_datadir}scm_in_<latlon_str>_<datetime>.nc"""
    )

    scm_netcdf_copypath = []

    if scm_dict["extract_scm_track"]:

        scm_netcdf_filelist_dict[latlon[0]] = []

        for ind, dt in enumerate(datetime):
            latlon_str = latlon[ind]
            scm_netcdf_filelist_dict[latlon[0]].append(
                os.path.join(scm_forcing_datadir, f"scm_in_{latlon_str}_{dt}.nc")
            )

    else:
        for latlon_str in latlon:

            scm_netcdf_filelist_dict[latlon_str] = []

            for dt in datetime:
                scm_netcdf_filelist_dict[latlon_str].append(
                    os.path.join(scm_forcing_datadir, f"scm_in_{latlon_str}_{dt}.nc")
                )

    return scm_netcdf_filelist_dict


def copy_grib_list(
    global_data_source_dict,
    getini1c_global_grib_dict,
    grib_shortnames,
    platform="ec-hpc2020",
):

    logger = logging.getLogger(__name__)
    logger.info(f"Copy variables from source to destination using grib_copy")

    for key, global_fn_list in global_data_source_dict.items():
        # create a string for the grib shortnames to copy
        # by combining the shortname list from yaml for each key into a string and
        # seperate each variable wit a '/'

        shortnm_str = "/".join(grib_shortnames[key])

        logger.info(
            f"Copy {global_fn_list[0]} - {global_fn_list[-1]} to {getini1c_global_grib_dict[key][0]} - {getini1c_global_grib_dict[key][-1]}"
        )

        # copyfiles
        for ind, global_fn in enumerate(global_fn_list):

            source_filepath = global_fn
            destination_filepath = getini1c_global_grib_dict[key][ind]

            # check if file exists and remove if necessary (this prevent grib appending)
            if os.path.exists(destination_filepath):
                logger.warning(f"{destination_filepath} exists - remove file before copy")
                os.remove(destination_filepath)

            logger.info(f"Running grib_copy command..")
            factories[platform]["grib_copy"](
                source_filepath=source_filepath,
                destination_filepath=destination_filepath,
                shortnm_str=shortnm_str,
            ).execute()

    logger.info(f"DONE - Copy variables from source to destination using grib_copy")


def check_missing(
    global_data_source_dict,
    getini1c_global_grib_dict,
    grib_shortnames,
    platform="ec-hpc2020",
):

    logger = logging.getLogger(__name__)
    logger.info(f"Check for any missing files, which are expected/required to create SCM forcing")

    for key, global_fn_list in global_data_source_dict.items():
        for ind, raw_fn in enumerate(global_fn_list):

            source_filepath = raw_fn
            destination_filepath = getini1c_global_grib_dict[key][ind]
            if not os.path.exists(destination_filepath):
                logger.warning(
                    f"""Following file is missing {destination_filepath} 
                               but copy_data=False, attempt to copy missing file"""
                )
                if os.path.exists(source_filepath):
                    logger.info(f"copy {source_filepath} to {destination_filepath}")

                    shortnm_str = "/".join(grib_shortnames[key][0])
                    factories[platform]["grib_copy"](
                        source_filepath=source_filepath,
                        destination_filepath=destination_filepath,
                        shortnm_str=shortnm_str,
                    ).execute()

                else:
                    logger.error(
                        f"""OpenIFS/IFS output file {source_filepath} does not exist. 
                                 Please check paths - EXITING """
                    )
                    sys.exit()
    logger.info(f"DONE - All required files exist in {os.path.dirname(destination_filepath)}, ready to run getini1c")


def copy_nc(getini1c_datadir, scm_netcdf_copypath, nml1c):

    logger = logging.getLogger(__name__)

    logger.debug(f"Check for netcdf output in {getini1c_datadir}, following the execution of getini1c")

    ncfile_list = find_list(getini1c_datadir, ".nc")

    if not ncfile_list:
        logger.error(
            f"""No SCM netcdf forcing files were found. This may be caused by 
                     delta in {nml1c} being too small - Exiting."""
        )
        sys.exit()
    else:
        if len(ncfile_list) == 1:

            logger.debug(f"Following SCM netcdf forcing files found - {str(ncfile_list[0])}.")

            shutil.copy(str(ncfile_list[0]), scm_netcdf_copypath)
        else:
            logger.error(
                f"""More than one SCM netcdf file found, this is probably wrong - {str(ncfile_list[:])}. 
                         This may be caused by delta in {nml1c} being too large - EXITING"""
            )
            sys.exit()

    logger.debug(f"DONE - SCM netcdf output {str(ncfile_list[0])} copied to {scm_netcdf_copypath}")


def concat_nc(
    scm_forcing_datadir,
    scm_out_dir,
    datetime_dict,
    latlon_str,
    scm_netcdf_filelist,
    tc_id,
):

    import xarray as xr
    import numpy as np
    import pandas as pd
    import datetime

    logger = logging.getLogger(__name__)

    date_time_list = datetime_dict["datetime_fn_suffix"]
    date_list = datetime_dict["dates"]
    times_list = datetime_dict["times_for_each_day"]

    # Choose paths for the output file
    scm_out_file = os.path.join(scm_out_dir, f"scm_{tc_id}_{latlon_str}_{date_list[0]}.nc")

    logger.info(f"Concatinate netcdf file in {scm_forcing_datadir} and output to {scm_out_dir}")

    # Load data
    ds_array = {}

    for ind, dt in enumerate(date_time_list):

        scm_file = scm_netcdf_filelist[ind]
        logger.debug(f"{ind} {scm_file} : {dt}")

        ds = xr.open_dataset(scm_file)
        logger.debug(f"{ds.time.data}")
        date = pd.to_datetime(np.array(ds.date.data, str))
        # derive hour for time index
        time_obj = datetime.datetime.strptime(times_list[ind], "%H:%M:%S")
        hour = time_obj.strftime("%H")
        time_in_seconds = int(hour) * 60 * 60
        init_time = pd.to_timedelta(time_in_seconds, unit="sec")
        logger.debug(f"hour = {hour}, time_in_s = {time_in_seconds}, init_time = {init_time}")
        datetime_new = date + init_time + ds.time.data
        ds["time"] = datetime_new
        ds_array[f"{dt}"] = ds

    # Merge data on time
    ds_merged = xr.concat(ds_array.values(), dim="time")

    # Revert to correct SCM forcing format (date, seconds variable)
    """
    date should for every step contain the start date.
    second should for every step contain the start time in seconds.
    hour should for every step contain the start time in seconds. (even though it does not seem to be used...)
    time should contain the runtime of the simulation, i.e. the datetime minus date and time
    """
    start_date = np.int32(pd.to_datetime(ds_merged.time.data[0]).date().strftime("%Y%m%d"))
    logger.debug(f"start_date = {start_date}")
    start_second = np.int32(pd.to_datetime(ds_merged.time.data[0]).time().hour * 60 * 60)
    logger.debug(f"start_second = {start_second}")
    sim_time = ds_merged.time.data - ds_merged.time.data[0]
    logger.debug(f"sim_time = {sim_time}")

    ds_merged.date.data = [start_date for date in ds_merged.date.data]
    ds_merged.second.data = [start_second for sec in ds_merged.second.data]
    ds_merged.hour.data = ds_merged.second.data

    time_attrs = ds_merged.time.attrs
    ds_merged.coords["time"] = sim_time
    ds_merged.time.attrs = time_attrs

    # Set the time unit for the exported file
    ds_merged.time.encoding["units"] = "seconds"

    # Save the output
    ds_merged.to_netcdf(scm_out_file)

    logger.info(f"DONE - Concatinate netcdf file in {scm_forcing_datadir} and output to {scm_out_dir}")

    return scm_out_file


def remove(file_path_to_remove):

    logger = logging.getLogger(__name__)

    logger.debug(f"""If present, remove {file_path_to_remove}""")

    if "scm*.nc" in file_path_to_remove:

        nc_files = glob.glob(file_path_to_remove)

        if len(nc_files) == 0:
            logger.debug(f"""{file_path_to_remove} not present, so continue to submit getini1c""")
        else:
            # Remove each file
            for file in nc_files:

                try:
                    os.remove(file)
                    logger.debug(f"Removing {file} prior to running getini1c")
                except OSError as e:
                    # Handle the error, e.g., if the file doesn't exist
                    logger.error(f"Error removing file {file}: {e}")

            # further test, to ensure all files removed
            nc_files = glob.glob(file_path_to_remove)
            # last check...
            if not nc_files:
                logger.debug(f"Old scm*.nc files removed - continue")
            else:
                logger.error(f"Old scm netcdf files present - EXIT")
                sys.exit()
    else:
        if os.path.exists(file_path_to_remove):
            if os.path.isfile(file_path_to_remove):
                logger.debug(f"Removing {file_path_to_remove}")
                os.remove(file_path_to_remove)
            else:
                logger.error(
                    f"""{file_path_to_remove} is not a file, but does exist, 
                             this wrong which - EXITING"""
                )
                sys.exit(1)

        else:
            logger.debug(f"{file_path_to_remove} does not exist - continue")
