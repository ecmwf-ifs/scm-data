import logging
import os
import sys

from internal import scm_data_files
from internal import scm_data_run
from internal import scm_namelist

parent_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
if parent_dir not in sys.path:
    sys.path.append(parent_dir)

from scm_command import factories


class SCMRunner:
    """
    Base class for running the scm-data application.
    """

    def __init__(self, user_config, path_dict, scm_datetime_dict, latlon_data, time_logger):
        self.user_config = user_config
        self.path_dict = path_dict
        self.scm_datetime_dict = scm_datetime_dict
        self.latlon_data = latlon_data
        self.time_logger = time_logger

        self.scm_final_data_and_nml = {
            "scm_exec": path_dict["scm_exec"],
            "scm_datadir": path_dict["scm_netcdf_out"],
        }

        # build list of expected netcdf files
        self.scm_netcdf_filelist_dict_ = self.build_scm_netcdf_filelist_()

    def build_scm_netcdf_filelist_(self):
        """
        Build the netcdf file list for the scm data.
        """
        scm_netcdf_filelist_dict_ = {}
        with self.time_logger.log_execution_time(f"{type(self).__name__}.scm_netcdf_filelist"):
            scm_netcdf_filelist_dict_ = scm_data_files.scm_netcdf_filelist(
                self.user_config["scm"],
                self.scm_datetime_dict["datetime_fn_suffix"],
                self.path_dict["scm_netcdf_out"],
                self.latlon_data["latlon_str"],
            )
        return scm_netcdf_filelist_dict_

    @property
    def scm_netcdf_filelist_dict(self):
        if self.scm_netcdf_filelist_dict_ is None:
            logger = logging.getLogger(__name__)
            logger.error(f"scm_netcdf_filelist_dict_ is None! - EXITING")
            raise ValueError
        else:
            return self.scm_netcdf_filelist_dict_

    def run(self, getini1c_global_grib_dict):

        with self.time_logger.log_execution_time(f"{type(self).__name__}.run"):
            logger = logging.getLogger(__name__)

            # Prepare namelists for getini1c and submit getini1c to the HPC
            if self.user_config["ctrl"]["create_forcing_data"]:

                with self.time_logger.log_execution_time(f"{type(self).__name__}.run::create nml"):
                    nml1c_filepath_dict = scm_data_run.create_nml(
                        self.path_dict["global_in_datadir"],
                        self.user_config["scm"],
                        self.latlon_data,
                        self.user_config["ctrl"],
                        self.scm_datetime_dict["datetime_fn_suffix"],
                        self.path_dict["vtable"],
                    )

                # set symbolic link name for getini1c namelist
                nml_linkname = os.path.join(self.path_dict["global_in_datadir"], "namelist_1c")

                # Reminder: the key will be the latitude and longitude string. For a track, where the lat and lon
                # change, the key is the initial lat-lon string
                for latlon_key in self.scm_netcdf_filelist_dict:

                    # For single or array of columns, the namelist is the first element, since no variation with
                    # datetime
                    scm_data_run.update_symlink(nml1c_filepath_dict[latlon_key][0], nml_linkname)
                    nml1c = nml1c_filepath_dict[latlon_key][0]

                    logger.info(
                        f"""Submit getini1c for {latlon_key} for dates {self.scm_datetime_dict['datetime_fn_suffix'][0]} to {self.scm_datetime_dict['datetime_fn_suffix'][-1]}.
                                Depending on size of request, this can take sometime, e.g. about 20s (+ queue time) 
                                for each time requested (on ECMWF ATOS, with 1 node). 
                                Please {self.path_dict['scm_netcdf_out']} for netcdf files"""
                    )

                    with self.time_logger.log_execution_time(
                        f"{type(self).__name__}.run::submit_{latlon_key}_for_all_dates"
                    ):

                        for ind, dt in enumerate(self.scm_datetime_dict["datetime_fn_suffix"]):

                            # remove any scm_in*.nc files to ensure that there is no data corruption
                            # by accidentally including old data
                            file_path_to_remove = os.path.join(self.path_dict["global_in_datadir"], "scm*.nc")
                            scm_data_files.remove(file_path_to_remove)

                            if self.user_config["scm"]["extract_scm_track"]:
                                # Update existing symbolic link for namelist by overwriting link set in outer loop
                                scm_data_run.update_symlink(nml1c_filepath_dict[latlon_key][ind], nml_linkname)
                                nml1c = nml1c_filepath_dict[latlon_key][ind]

                            # Now update links to the grib datetime files
                            for grib_file_key in getini1c_global_grib_dict:
                                gribfile_linkname = os.path.join(
                                    self.path_dict["global_in_datadir"],
                                    f"{grib_file_key}_grib",
                                )
                                scm_data_run.update_symlink(
                                    getini1c_global_grib_dict[grib_file_key][ind],
                                    gribfile_linkname,
                                )

                            factories[self.user_config["platform"]]["submit_job"](
                                getini1c_exec=self.path_dict["getini1c_exec"],
                                global_in_datadir=self.path_dict["global_in_datadir"],
                                datetime=dt,
                            ).execute()

                            scm_data_files.copy_nc(
                                self.path_dict["global_in_datadir"],
                                self.scm_netcdf_filelist_dict[latlon_key][ind],
                                nml1c,
                            )

                        logger.info(
                            f"""DONE - getini1c completed for {latlon_key} for dates {self.scm_datetime_dict['datetime_fn_suffix'][0]} to {self.scm_datetime_dict['datetime_fn_suffix'][-1]}.
                                    Please check {self.path_dict['scm_netcdf_out']} for netcdf files"""
                        )

    def concatenate_nc_files(self):
        with self.time_logger.log_execution_time(f"{type(self).__name__}.concatenate_nc_files"):
            logger = logging.getLogger(__name__)
            file_id = self.getfile_id_(self.user_config)
            self.scm_final_data_and_nml["forcing_nc_path"] = []
            for latlon_key, latlon_val in self.scm_netcdf_filelist_dict.items():
                scm_init_force_paths = scm_data_files.concat_nc(
                    self.path_dict["scm_netcdf_out"],
                    self.path_dict["scm_nml_merge_nc_dir"],
                    self.scm_datetime_dict,
                    latlon_key,
                    latlon_val,
                    file_id,
                )

                self.scm_final_data_and_nml["forcing_nc_path"].append(scm_init_force_paths)
                logger.info(f"SCM forcing file for {latlon_key} is {scm_init_force_paths}")

    def create_scm_namelist(self, nml_config):
        with self.time_logger.log_execution_time(f"{type(self).__name__}.concatenate_nc_files"):
            logger = logging.getLogger(__name__)
            file_id = self.getfile_id_(self.user_config)
            self.scm_final_data_and_nml["namelist_path"] = []
            for latlon_key in self.scm_netcdf_filelist_dict:

                scm_namelist_paths = scm_namelist.write_scm_namelist(
                    nml_config,
                    self.user_config,
                    self.path_dict["scm_nml_merge_nc_dir"],
                    self.scm_datetime_dict["dates"],
                    latlon_key,
                    file_id,
                    self.path_dict["vtable"],
                )
                for file_path in scm_namelist_paths:
                    # scm_namelists_paths is returned as list of selected timesteps, append each item
                    # to the final scm_final_data_and_nml['namelist_path'] dictionary key
                    self.scm_final_data_and_nml["namelist_path"].append(file_path)

                logger.info(f"SCM namelist for {latlon_key} are {scm_namelist_paths}")

    @staticmethod
    def getfile_id_(config):

        # Set the file id, which is used in the namelist and data file name
        if config["data_source"] == "openifs":
            file_id = config["ctrl"]["id"]
        else:
            file_id = config["data_source"]

        return file_id
