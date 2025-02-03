#!/usr/bin/env python

import argparse
import logging
import os
import sys
import textwrap
import yaml

import scm_input

from scm_logger import TimeLogger
from scm_config import UserConfig
from scm_config import NamelistConfig
from scm_action import SetupDirectories
from scm_action import SetLatLon
from scm_action import SetDateTime
from scm_climate import SCMClimate
from scm_runner import SCMRunner


help_string = textwrap.dedent(
    f"""
        run_scm_data and the associated modules setup the lists, retrieve and/or copy data 
        required by getini1c to produce SCM initial condition and forcing data. 
        Usage example:
            python3 run_scm_data.py /path/to/scm_setup_yaml/scm_data_era5_setup.yml
        The user configuration file scm_data_era5_setup.yml should be in the scm_setup_yaml directory."""
)


def setup_logging(logfile, level_str="INFO"):
    """
    Set up logging to write to a file and the screen
    """
    level = getattr(logging, level_str, logging.INFO)
    logging.basicConfig(
        level=level,
        format="[%(levelname)s] %(name)s.%(funcName)s : %(message)s",
        handlers=[logging.FileHandler(logfile, mode="w"), logging.StreamHandler()],
    )


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description=help_string, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("config_file", help="Path to user configuration yaml file")
    parser.add_argument(
        "--config_file_nml",
        "-n",
        help="Path to yaml configuration file for namelist creation",
    )
    args = parser.parse_args()

    # Check if the user input yaml file exists
    if not os.path.exists(args.config_file):
        print(f"[ERROR]: {args.config_file}, does not exist.")
        sys.exit()

    # Check if the namelist yaml file exists
    if args.config_file_nml is None:
        args.config_file_nml = os.path.join(os.getcwd(), f"scm_setup_yaml/scm_nml_setup.yml")

    if not os.path.exists(args.config_file_nml):
        print(f"[WARNING]: Namelist config file {args.config_file_nml} does not exist!")

    print(f"[INFO]: User configuration file {args.config_file}, Namelist configuration file {args.config_file_nml}")

    # User configuration
    user_config = UserConfig(args.config_file)

    # Execution time logger
    time_logger = TimeLogger()

    # Setup logging
    setup_logging(user_config["scm_forcing_logfile"], user_config["log_level"])

    logger = logging.getLogger(__name__)
    logger.info(f"Start prepare_data.py and write screen output to {user_config['scm_forcing_logfile']}")

    # Read the namelist configuration yaml file
    if user_config["ctrl"]["create_scm_namelist"]:
        nml_config = NamelistConfig(args.config_file_nml)

    # Actions to setup directories, and other information
    setup_actions = {
        "dirs": SetupDirectories(user_config, time_logger),
        "latlon": SetLatLon(user_config, time_logger),
        "datetime": SetDateTime(user_config, time_logger),
    }

    for action in setup_actions.values():
        action.execute()

    timeint, path_dict = setup_actions["dirs"].get_results()
    scm_datetime_dict = setup_actions["datetime"].get_results()
    latlon_data = setup_actions["latlon"].get_results()

    # Data handler
    data_handler_type = scm_input.factory(user_config["data_source"])
    data_handler = data_handler_type(user_config, path_dict, scm_datetime_dict, time_logger)

    # Runner of the SCM-data executable
    runner = SCMRunner(user_config, path_dict, scm_datetime_dict, latlon_data, time_logger)

    # retrieve data if requested
    if user_config["ctrl"]["retrieve_data"]:
        data_handler.retrieve_data()

    # Copy data from the source (e.g. era retrieval or openifs)
    # to the common directory where getini1c is executed
    if user_config["ctrl"]["copy_data"]:
        data_handler.copy_grib_files()

    # Climate data
    if user_config["ctrl"]["interp_append_clim"]:
        clim_handler = SCMClimate(user_config, path_dict, scm_datetime_dict, time_logger)
        clim_handler.interpolate(timeint)
        clim_handler.append_climate_to_datafiles()

    # Run getini1c
    if user_config["ctrl"]["create_forcing_data"]:
        runner.run(data_handler.getini1c_global_grib_dict)

    # Concatenate the forcing data
    if user_config["ctrl"]["concat_forcing_data"]:
        runner.concatenate_nc_files()

    # Create the namelist
    if user_config["ctrl"]["create_scm_namelist"]:
        runner.create_scm_namelist(nml_config)

    # Write the data to a YAML file
    scm_run_yaml = os.path.join(path_dict["scm_nml_merge_nc_dir"], "scm_run.yml")
    with open(scm_run_yaml, "w") as file:
        yaml.dump(
            runner.scm_final_data_and_nml,
            file,
            default_flow_style=False,
            sort_keys=False,
        )

    time_logger.print_logs()
