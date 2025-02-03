#!/usr/bin/env python

import sys
import os
import yaml
import logging

from . import templ_path


def read_yaml(scm_nml_yaml_file):

    if not os.path.exists(scm_nml_yaml_file):
        print(
            f"""[ERROR]: {scm_nml_yaml_file}, which includes the user entries for the SCM namelists, does not exist. 
         User has requested namelist creation, but creation depends on settings in {scm_nml_yaml_file} 
         Please check path and re-run - EXITING."""
        )
        sys.exit()

    # Load the input from the yaml file
    with open(scm_nml_yaml_file, "r") as file:
        scm_nml_yaml_input = yaml.safe_load(file)

    scm_nml = {}

    scm_nml["caseid"] = scm_nml_yaml_input.get("caseid", "SCM")
    scm_nml["tstep"] = scm_nml_yaml_input.get("tstep", 1800.00)
    scm_nml["run_length"] = scm_nml_yaml_input.get("run_length", "h24")

    scm_nml["nml_entries"] = scm_nml_yaml_input.get("nml_entries", {})

    return scm_nml


def write_scm_namelist(scm_nml, user_input, scm_out_dir, date_list, latlon_key, file_id, vtable):

    logger = logging.getLogger(__name__)

    nml_entries = scm_nml["nml_entries"]

    # update with entries from other vars or the control yml
    nml_entries["cycle"] = user_input["ctrl"]["cycle"]
    nml_entries["run_length"] = scm_nml["run_length"]
    nml_entries["levels"] = user_input["ctrl"]["levels"]

    scm_nml_path = []

    logger.info(f"Setting up namelist file id as {scm_nml['caseid']}_{file_id}")
    nml_entries["file_id"] = f"{scm_nml['caseid']}_{file_id}"

    # Read the template file
    templ_file = os.path.join(templ_path, "scm_nml_template.txt")
    with open(templ_file, "r") as nml_file_template:
        nml_template_content = nml_file_template.read()

    # Read the vtable content
    with open(vtable, "r") as f:
        vtable_content = f.read()

    # Read the file with empty namelist entries
    with open(os.path.join(templ_path, "scm_empty_nml_append_48r1.txt"), "r") as f:
        scm_empty_nml_append_content = f.read()

    for time_step in scm_nml["tstep"]:
        # Using caseid and data source, start date and lat-lon from user name,
        # create namelist name
        scm_nml_file = os.path.join(scm_out_dir, f"namelist.{file_id}_dt{time_step}_{latlon_key}_{date_list[0]}")

        logger.info(
            f"Produce SCM namelist, {scm_nml_file}, with timstep = {time_step}, for {latlon_key} and start-date, {date_list[0]}"
        )
        nml_entries["time_step"] = time_step

        scm_nml_content = nml_template_content.format(**nml_entries)

        with open(scm_nml_file, "w", encoding="utf-8") as namelist:
            # write scm specific namelist
            namelist.write(scm_nml_content)
            # Add the vtables to the namelist
            namelist.write("\n" + vtable_content)
            # Add the empty namelists to the end of the file
            namelist.write(scm_empty_nml_append_content)

        scm_nml_path.append(scm_nml_file)

        logger.info(f"DONE - {scm_nml_file} written")

    return scm_nml_path
