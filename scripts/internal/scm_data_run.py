#!/usr/bin/env python

import logging
import os
import sys

from . import scm_data_checks
from . import templ_path


parent_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
if parent_dir not in sys.path:
    sys.path.append(parent_dir)

from scm_command import factories


def write_nml_contents(ctrl, lat, lon, nml1c, vtable):

    getini_templ = os.path.join(templ_path, "getini1c_nml_template.txt")
    with open(getini_templ, "r") as f:
        getini1c_template_content = f.read()

    with open(vtable, "r") as f:
        vtable_content = f.readlines()

    nml_entries = ctrl
    nml_entries["lat"] = lat
    nml_entries["lon"] = lon

    getini1c_nml1c_content = getini1c_template_content.format(**nml_entries)

    with open(nml1c, "w") as f:

        f.write(getini1c_nml1c_content + "\n")

        for line in vtable_content:
            # Write everything from vtable but the IFS namelist name
            if "NAM" not in line:
                f.write(line)


def create_nml(getini1c_datadir, scm_dict, latlon_data, ctrl_dict, date_time_list, vtable):

    logger = logging.getLogger(__name__)

    # Create the directory and any necessary parent directories
    getini1c_nmldir = os.path.join(getini1c_datadir, "namelists")
    os.makedirs(getini1c_nmldir, exist_ok=True)
    scm_data_checks.paths(getini1c_nmldir, "Directory to store getini1c namelists (namelists_1c*) ")

    # Important: when using the latlon file, length of latlon_str is the same as
    #            length of date_time_list
    #            However, when latlon_file false, then latlon_str size is equal
    #            to the number of columns requested (lenghth of input lat)
    #            and this is fixed for each datetime.
    #            As a result, the for loops are different depending on whether
    #            a latlon_file is used. (This is a bit nasty!!)
    #
    nml1c_filepath_dict = {}

    if scm_dict["extract_scm_track"]:
        logger.info(
            f"""Latitude and Longitude change in time (read from {scm_dict['latlon_file']})) 
                    getini1c namelist produced for each date in track"""
        )

        nml1c_filepath_dict[latlon_data["latlon_str"][0]] = []

        for ind, date_time in enumerate(date_time_list):

            nml1c_filepath_dict[latlon_data["latlon_str"][0]].append(
                os.path.join(
                    getini1c_nmldir,
                    f"nml1c_{latlon_data['latlon_str'][ind]}_{date_time}",
                )
            )

            write_nml_contents(
                ctrl_dict,
                latlon_data["latitude"][ind],
                latlon_data["longitude"][ind],
                nml1c_filepath_dict[latlon_data["latlon_str"][0]][ind],
                vtable,
            )

            logger.debug(
                f"""{nml1c_filepath_dict[latlon_data['latlon_str'][0]][ind]} created for date and time = {date_time}, 
                        latitude = {latlon_data['latitude'][ind]}, longitude = {latlon_data['longitude'][ind]}"""
            )

    else:

        date_time = date_time_list[0]

        for ind, latlon_str in enumerate(latlon_data["latlon_str"]):

            nml1c_filepath_dict[latlon_str] = []

            logger.info(
                f"""Latitude ({latlon_data['latitude'][ind]}) and Longitude ({latlon_data['longitude'][ind]})
                        are fixed so only one namelist produced getini1c"""
            )

            nml1c_filepath_dict[latlon_str].append(os.path.join(getini1c_nmldir, f"nml1c_{latlon_str}_{date_time}"))

            write_nml_contents(
                ctrl_dict,
                latlon_data["latitude"][ind],
                latlon_data["longitude"][ind],
                nml1c_filepath_dict[latlon_str][0],
                vtable,
            )

            logger.info(
                f"""{nml1c_filepath_dict[latlon_str][0]} created for date and time = {date_time}, 
                        latitude = {latlon_data['latitude'][ind]}, longitude = {latlon_data['longitude'][ind]}"""
            )

    return nml1c_filepath_dict


def update_symlink(target, link_pathname):

    logger = logging.getLogger(__name__)

    # Check link_pathname, which should be a path is a link
    if os.path.exists(link_pathname):

        if not os.path.islink(link_pathname):
            logger.error(f"{link_pathname} is not a symbolic link, this is wrong and will cause problems - EXITING")
            sys.exit()

        try:
            logger.debug(f"{link_pathname} exists and is an old symbolic link - remove ")
            os.remove(link_pathname)
        except OSError:
            pass
    else:
        logger.debug(f"{link_pathname} is a symbolic link, but path does not exist, remove link")
        try:
            os.remove(link_pathname)
        except OSError:
            pass

    # Create new links to getini1c namelist
    logger.debug(f"Create new link to {target} with link name {link_pathname}")
    os.symlink(target, link_pathname)

    # This is overkill but check if the link is a link...
    if not os.path.islink(link_pathname):
        logger.error(f"{link_pathname} is not a symbolic link, this is wrong and will cause problems - EXITING")
        sys.exit()
    # ... and if target for the link exists
    if not os.path.exists(os.readlink(link_pathname)):
        logger.error(
            f"{link_pathname} target for symbolic link does not exist, this is wrong and will cause problems - EXITING"
        )
        sys.exit()
