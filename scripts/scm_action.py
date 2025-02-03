import logging

from internal import scm_data_input
from internal import scm_data_times


class SCMAction:

    def __init__(self, config, time_logger):
        self.config = config
        self.time_logger = time_logger

        self.logger = logging.getLogger(__name__)
        self.executed_flag = False

    def execute(self):
        with self.time_logger.log_execution_time(type(self).__name__):
            self.execute_()
        self.executed_flag = True

    def execute_(self):
        raise NotImplementedError(f"Method execute_ not implemented, this is class {type(self).__name__}")

    def get_results(self):
        if not self.executed_flag:
            self.logger.warning(f"Method execute() was not called before get_results() in {type(self).__name__}")
        else:
            return self.get_results_()


class SetupDirectories(SCMAction):
    def __init__(self, config, time_logger):
        super().__init__(config, time_logger)
        self.timeint = None
        self.path_dict = None

    def execute_(self):
        self.timeint, self.path_dict = scm_data_input.check_switches_and_paths(self.config)

    def get_results_(self):
        return self.timeint, self.path_dict


class SetLatLon(SCMAction):
    def __init__(self, config, time_logger):
        super().__init__(config, time_logger)
        self.latlon_data = None

    def execute_(self):
        self.latlon_data = scm_data_input.set_latlon_and_check(self.config)

    def get_results_(self):
        return self.latlon_data


class SetDateTime(SCMAction):
    def __init__(self, config, time_logger):
        super().__init__(config, time_logger)
        self.scm_datetime_dict = None

    def execute_(self):
        datebeg = self.config["scm"]["datebeg"]
        dateend = self.config["scm"]["dateend"]
        tstep = self.config["scm"]["tstep"]
        self.scm_datetime_dict = scm_data_times.datetime_derivation(datebeg, dateend, tstep, "scm")

    def get_results_(self):
        return self.scm_datetime_dict
