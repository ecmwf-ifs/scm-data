from internal import scm_data_clim


class SCMClimate:
    """
    Handles climate data.
    """

    def __init__(self, user_config, path_dict, scm_datetime_dict, time_logger):
        self.user_config = user_config
        self.path_dict = path_dict
        self.scm_datetime_dict = scm_datetime_dict
        self.time_logger = time_logger

    def interpolate(self, interp_executable):
        with self.time_logger.log_execution_time(f"{type(self).__name__}.create_source_file_list"):

            scm_data_clim.interpolate(
                self.user_config["climatology_vars"],
                self.path_dict["climate_data"],
                self.path_dict["global_in_datadir"],
                interp_executable,
                self.scm_datetime_dict["datetime_fn_suffix"],
            )

    def append_climate_to_datafiles(self):
        with self.time_logger.log_execution_time(f"{type(self).__name__}.append_climate_to_datafiles"):
            scm_data_clim.append_control(
                self.path_dict["climate_data"],
                self.path_dict["global_in_datadir"],
                self.scm_datetime_dict["datetime_fn_suffix"],
                platform=self.user_config["platform"],
            )
