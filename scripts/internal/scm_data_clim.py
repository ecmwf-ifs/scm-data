#!/usr/bin/env python

import subprocess
import sys
import os
import glob
import logging

parent_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
if parent_dir not in sys.path:
    sys.path.append(parent_dir)

from scm_command import factories


def interpolate(climatology_vars, climate_datadir, global_in_datadir, oifs_timeint, date_time):

    logger = logging.getLogger(__name__)

    for clim_key, grib_code in climatology_vars.items():

        month_clim_file = os.path.join(climate_datadir, f"month_{clim_key}")

        logger.info(
            f"""Begin interpolation of climatology {month_clim_file}, grib code = {grib_code} 
                    for {date_time[0]} to {date_time[-1]}"""
        )

        if not os.path.exists(month_clim_file):
            logger.error(
                f"""{month_clim_file} does not exist.
                            Interpolation of climatology with {oifs_timeint} will fail - EXITING"""
            )
            sys.exit()

        for date_time_suffix in date_time:

            interpolated_clim_file = global_in_datadir + "/" + clim_key + "_int_" + date_time_suffix

            timeint_command = [
                oifs_timeint,
                "--basetime",
                date_time_suffix,
                "--grib_code",
                grib_code,
                "--input_mon",
                month_clim_file,
                "--output",
                interpolated_clim_file,
            ]

            if os.path.exists(interpolated_clim_file):
                logger.info(
                    f"""{interpolated_clim_file} already exist. 
                                   {oifs_timeint} not run for {date_time_suffix}, grib code = {grib_code}"""
                )
            if not os.path.exists(interpolated_clim_file):
                logger.info(f"Run {oifs_timeint} for {date_time_suffix}, grib code = {grib_code}")
                try:
                    # Run the command
                    result = subprocess.run(timeint_command, capture_output=True, text=True, check=True)

                except subprocess.CalledProcessError as e:
                    logger.error(
                        f"""Climatology interpolation command '{' '.join(timeint_command)} FAILED' 
                                 Error message: {e.stderr}
                                 EXTING"""
                    )

        logger.info(f"DONE - interpolation of climatology {month_clim_file}")


def append_files_to_output(file_list, output_file):
    #
    # Called from append_control
    # Function to append climate files (binary) to the grib files (binary) derive
    # OpenIFS/IFS, for production of SCM forcing
    #
    with open(output_file, "ab") as output:
        for file in file_list:
            with open(file, "rb") as f:
                output.write(f.read())


def clim_check(grib_file, platform="ec-hpc2020"):

    logger = logging.getLogger(__name__)

    search_pattern = "aluvp"

    # get the grib_ls command
    grib_ls_output = factories[platform]["grib_ls"](filename=grib_file).execute()
    clim_test = grib_ls_output.count(search_pattern)

    # if clim_test < 0 :
    #     clim_test = 0
    if clim_test > 0:
        logger.debug(f"{search_pattern} found in {grib_file} - do not append")
    else:
        clim_test = 0
        logger.debug(f"{search_pattern} not found in {grib_file} - continue append")

    return clim_test


def append_control(climate_datadir, getini1c_datadir, date_time_list, platform=None):

    #### THIS FUNCTION IS NASTY, THERE MUST BE A BETTER WAY###########
    # This function vaguely replicates cat to append the interpolated
    # climatologies to the getini1c grib files.

    logger = logging.getLogger(__name__)

    logger.info(
        f"""Append spec and sfc_climate files to {getini1c_datadir}/spec_grib and sfc_grib
                for date range {date_time_list[0]} to {date_time_list[-1]}"""
    )

    for date_time in date_time_list:

        output_spec_grib = os.path.join(getini1c_datadir, "spec_grib_" + date_time)
        output_sfc_grib = os.path.join(getini1c_datadir, "sfc_grib_" + date_time)

        # this loop sets up a list of binary climatology files to be appended to
        # the existing source data for getini1c. This code will append whatever,
        # which is wrong. Hence, clim_check to see if clims already in file

        clim_test = clim_check(output_sfc_grib, platform)

        if clim_test == 0:
            # List of files for spec_grib
            spec_climate_fn = [os.path.join(climate_datadir, "sporog")]

            # List of files for sfc_grib
            sfc_climate_fn = [
                os.path.join(climate_datadir, "sfc"),
                os.path.join(climate_datadir, "sdfor"),
                os.path.join(climate_datadir, "clake"),
                *glob.glob(os.path.join(climate_datadir, "lake*")),
                os.path.join(climate_datadir, "slt"),
                os.path.join(climate_datadir, "lsmoro"),
                os.path.join(getini1c_datadir, "lail_int_" + date_time),
                os.path.join(getini1c_datadir, "laih_int_" + date_time),
                os.path.join(getini1c_datadir, "alb_int_" + date_time),
                os.path.join(getini1c_datadir, "aluvp_int_" + date_time),
                os.path.join(getini1c_datadir, "aluvd_int_" + date_time),
                os.path.join(getini1c_datadir, "alnip_int_" + date_time),
                os.path.join(getini1c_datadir, "alnid_int_" + date_time),
            ]

            # Append the sporog file to spec_grib output
            append_files_to_output(spec_climate_fn, output_spec_grib)

            # Append the sfc related files to sfc_grib output
            logger.debug(f"Append {output_sfc_grib} with {sfc_climate_fn}\n")
            append_files_to_output(sfc_climate_fn, output_sfc_grib)

    logger.info(f"""DONE - spec and sfc_climate files appended to {getini1c_datadir}/spec_grib and sfc_grib""")
