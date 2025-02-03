#!/usr/bin/env python

import sys
import os
import shutil
import logging


def env_vars(variable_name):

    logger = logging.getLogger(__name__)

    var = os.getenv(variable_name)

    if var:
        logger.info(f"Environment variable '{variable_name}' is set and the path is {var}")
    else:
        logger.error(
            f"""Environment variable '{variable_name}' is NOT set - EXITING. 
                      If model is OpenIFS, user must source the oifs-config_editme.sh, e.g.,
                      source ~/openifs-48r1/oifs-config.edit_me.sh"""
        )
        sys.exit()
    return var


def paths(abs_path, print_text, quiet=False):

    logger = logging.getLogger(__name__)

    if os.path.exists(abs_path):
        if not quiet:
            logger.info(f"{print_text} is {abs_path}")
    else:
        logger.error(
            f""" {print_text} is {abs_path}, does not exist. 
                      Please check the path and directory - EXITING"""
        )
        sys.exit()


def file_lists(file_list, key, data_source):

    logger = logging.getLogger(__name__)

    # Only print if the file does not exist
    #
    for file_path in file_list:

        if not os.path.exists(file_path):

            logger.error(
                f""" {file_path} does not exist in {data_source}. 
                          Please check the path, directory, and/or the extraction - EXITING"""
            )
            sys.exit()

    logger.info(f"DONE - All expected {key} files from {data_source} are present - Continue")


def bin_lib_paths(path_name):

    logger = logging.getLogger(__name__)

    # need to make this more generic

    logger.info(f"Checking {path_name} and any associated paths")

    if os.path.exists(path_name):

        logger.info(f"{path_name} exists, check for build/bin and lib")

        bin = os.path.join(path_name, "build/bin")
        lib = os.path.join(path_name, "build/lib")

        if os.path.exists(bin) and os.path.exists(lib):

            if "openifs" in path_name:
                os.environ["LD_LIBRARY_PATH"] = lib
                logger.info(f"LD_LIBRARY_PATH set to: {os.environ['LD_LIBRARY_PATH']}")
            logger.info(f"{bin} exists")
            logger.info(f"{lib} exists")
            return bin

        else:

            logger.error(
                f"""{bin} and/or {lib} do not exist. 
                          Please check that both OpenIFS and getini1c have been built - EXITING"""
            )
            sys.exit()

    else:
        logger.error(
            f"""{path_name} does not exist, either OpenIFS or getini1c path is wrong.
                     Both OpenIFS and getini1c are needed to produce forcing, please check - EXITING"""
        )
        sys.exit()


def exec(path_name, exec_name):

    logger = logging.getLogger(__name__)

    logger.info(f"checking if {exec_name} exists in {path_name}")

    exec_path = os.path.join(path_name, exec_name)

    if os.path.exists(exec_path):

        logger.info(f"Path for {exec_name} is {exec_path}")
        return exec_path

    else:

        logger.error(
            f"""{exec_path} does not exist. 
                      Please check that both OpenIFS and getini1c have been built - EXITING"""
        )
        sys.exit()


def command(comm_str):

    logger = logging.getLogger(__name__)

    if shutil.which(comm_str):
        logger.info(f"{comm_str} is available")
    else:
        logger.error(
            f""" {comm_str} does not exist. 
                      Please ensure correct libs, e.g. eccodes are loaded - EXITING"""
        )
        sys.exit()
