# (C) Copyright 2024- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

import time
import logging
from contextlib import contextmanager


class TimeLogger:

    def __init__(self):
        self.own_start_time = time.time()
        self.execution_time_dict = {}

    @contextmanager
    def log_execution_time(self, key):
        start_time = time.time()
        yield
        self.execution_time_dict[key] = time.time() - start_time

    def get_logs(self):
        return self.execution_time_dict

    def print_logs(self):

        logger = logging.getLogger(__name__)

        for key, time_diff in self.execution_time_dict.items():
            logger.info(f"{key:50} | {time_diff:>10.3f} seconds")
