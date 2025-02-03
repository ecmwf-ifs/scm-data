import json
import logging
import os
import subprocess
import sys
import time

from eccodes import codes_grib_new_from_file
from eccodes import codes_get
from eccodes import codes_release
from eccodes import codes_write
from eccodes import CodesInternalError


class SCMCommand:

    required_args = []

    def __init__(self, *args, **kwargs):
        self.args = args
        self.kwargs = kwargs
        self.validate_optional_args()

    def execute(self):
        raise NotImplementedError(f"Method execute not implemented for class {type(self).__name__}")

    def validate_optional_args(self):
        for arg in self.required_args:
            if arg not in self.kwargs:
                raise ValueError(f"Missing required argument {arg} in class {type(self).__name__}")


# ----- Grib Ls -----
class SCMCommand_GribLs(SCMCommand):
    """
    List the contents of a GRIB file
    """

    required_args = ["filename"]

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def execute(self):
        raise NotImplementedError(f"Method execute not implemented for class {type(self).__name__}")


class SCMCommand_GribLs_CL(SCMCommand_GribLs):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def execute(self):
        grib_file = self.kwargs["filename"]
        grib_command = f"grib_ls {grib_file}"
        output = subprocess.check_output(grib_command, shell=True, text=True)
        return output


class SCMCommand_GribLs_EcCodes(SCMCommand_GribLs):

    # Print some basic metadata
    keys = ["dataDate", "dataTime", "stepRange", "shortName", "level", "parameterName"]

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def execute(self):

        logger = logging.getLogger(__name__)

        try:
            # Open the GRIB file
            f = open(self.kwargs["filename"], "rb")
        except IOError as e:
            print(f"Cannot open file {self.kwargs['filename']}: {e}")
            return

        ls_output = [" ".join([f"{k:15}" for k in self.keys])]

        # Loop through each message in the GRIB file
        while True:
            try:
                # Get handle to the next message in the file
                gid = codes_grib_new_from_file(f)
                if gid is None:
                    break

                val_str = ""
                for key in self.keys:

                    try:
                        value = codes_get(gid, key)
                        val_str = val_str + f"{value:15} "
                    except CodesInternalError as e:
                        logger.info(f"Cannot get key {key}: {e}")

                ls_output.append(val_str)

                # Release the handle
                codes_release(gid)

            except CodesInternalError as e:
                logger.debug(f"GRIB decoding error: {e}")
                break

        # Close the file
        f.close()

        return "\n".join(ls_output)


# ----------------------


# ----- Grib Copy -----
class SCMCommand_GribCopy(SCMCommand):
    """
    Copy the contents of a GRIB file
    """

    required_args = ["source_filepath", "destination_filepath"]

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def execute(self):
        raise NotImplementedError(f"Method execute not implemented for class {type(self).__name__}")


class SCMCommand_GribCopy_CL(SCMCommand_GribCopy):

    required_args = SCMCommand_GribCopy.required_args + ["shortnm_str"]

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def execute(self):
        logger = logging.getLogger(__name__)

        grib_copy_command = [
            "grib_copy",
            "-w",
            f"shortName={self.kwargs['shortnm_str']}",
            self.kwargs["source_filepath"],
            self.kwargs["destination_filepath"],
        ]
        try:
            logger.info(f"Running grib_copy command: {' '.join(grib_copy_command)}")
            _ = subprocess.run(grib_copy_command, capture_output=True, text=True, check=True)

        except subprocess.CalledProcessError as e:
            logger.error(
                f"""Grib copy commamd is {' '.join(grib_copy_command)} - FAILED 
                         Error message: {e.stderr} EXTING"""
            )
            raise e


class SCMCommand_GribCopy_EcCodes(SCMCommand_GribCopy):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def execute(self, filter_criteria=None, wmode="w"):
        logger = logging.getLogger(__name__)

        # Filter criteria (use shortName as key)
        if filter_criteria is None:
            filter_criteria = {"shortName": self.kwargs["shortnm_str"]}

        print(self.kwargs["source_filepath"])
        logger.info(
            f" ===> Copying GRIB file {self.kwargs['source_filepath']} to {self.kwargs['destination_filepath']} with filter criteria {filter_criteria}"
        )

        with open(self.kwargs["source_filepath"], "rb") as infile, open(self.kwargs["destination_filepath"], f"{wmode}b") as outfile:

            while True:

                # Get the next message in the file
                gid = codes_grib_new_from_file(infile)
                if gid is None:
                    break

                # Check if the message matches the filter criteria
                matches = True
                for fkey, fval_string in filter_criteria.items():
                    fvalues = fval_string.split("/")                    

                    try:
                        val = codes_get(gid, fkey)
                    except CodesInternalError as e:
                        break

                    if str(val) not in fvalues:
                        matches = False
                        break

                if matches:
                    codes_write(gid, outfile)

                codes_release(gid)


# ----------------------


# ------ Grib get ------
class SCMCommand_GribGet(SCMCommand):

    required_args = ["filename", "key", "count"]

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def execute(self):
        raise NotImplementedError(f"Method execute not implemented for class {type(self).__name__}")


class SCMCommand_GribGet_CL(SCMCommand_GribGet):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def execute(self):

        logger = logging.getLogger(__name__)

        grib_get_command = [
            "grib_get",
            "-p",
            self.kwargs["key"],
            "-w",
            f"count={self.kwargs['count']}",
            self.kwargs["filename"],
        ]
        logger.info(f"Running grib_get command: {' '.join(grib_get_command)}")

        try:
            result = subprocess.run(grib_get_command, capture_output=True, text=True, check=True)
            return result
        except subprocess.CalledProcessError as e:
            logger.error(
                f"""Grib get commamd is {' '.join(grib_get_command)} - FAILED 
                         Error message: {e.stderr} EXTING"""
            )
            raise e


class SCMCommand_GribGet_EcCodes(SCMCommand_GribGet):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def execute(self):

        logger = logging.getLogger(__name__)

        try:
            # Open the GRIB file
            f = open(self.kwargs["filename"], "rb")
        except IOError as e:
            logger.error(f"Cannot open file {self.kwargs['filename']}: {e}")
            sys.exit()

        # Loop through each message in the GRIB file
        count = 1
        while True:
            try:
                # Get handle to the next message in the file
                gid = codes_grib_new_from_file(f)
                if gid is None:
                    break

                count = count + 1

                # Get the value of the key
                value = codes_get(gid, self.kwargs["key"])

                # Release the handle
                codes_release(gid)

                if count == self.kwargs["count"]:
                    break

            except CodesInternalError as e:
                logger.debug(f"GRIB decoding error: {e}")
                break

        # Close the file
        f.close()

        return value


# ----------------------


# ---- MARS request ----
class SCMCommand_ExecuteMarsRequest(SCMCommand):

    required_args = [
        "config_ctr",
        "param_type",
        "grib_names",
        "mars_req_dir",
        "mars_ret_dir",
        "datetimes",
    ]

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        self.config_ctr = self.kwargs["config_ctr"]
        self.param_type = self.kwargs["param_type"]
        self.grib_names = self.kwargs["grib_names"]
        self.mars_req_dir = self.kwargs["mars_req_dir"]
        self.mars_ret_dir = self.kwargs["mars_ret_dir"]
        self.datetimes = self.kwargs["datetimes"]

        self.levels = self.config_ctr["levels"]
        self.iniclass = self.config_ctr["iniclass"]
        self.exp_version = self.config_ctr["exp_version"]

    def execute(self):
        raise NotImplementedError(f"Method execute not implemented for class {type(self).__name__}")

    @classmethod
    def paramtype_2_levels(cls, ptype, levels):

        if ptype == "sfc":
            level_type = "sfc"
            level_set = "OFF"
        else:
            level_type = "ml"
            level_set = f"1/to/{levels}"

        return level_type, level_set


class SCMCommand_ExecuteMarsRequest_CL(SCMCommand_ExecuteMarsRequest):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def execute(self):

        mars_request_file = self.write_mars_request(
            self.param_type,
            self.grib_names,
            self.mars_req_dir,
            self.mars_ret_dir,
            self.datetimes,
        )
        self.submit_mars_request(mars_request_file, self.mars_ret_dir, self.param_type, self.datetimes)

    def write_mars_request(self, param_type, grib_names, mars_req_dir, mars_ret_dir, datetimes):
        """
        write and store the mars request in a file
        """

        logger = logging.getLogger(__name__)

        time_string_mars_format = "/".join(datetimes["times_for_mars"])
        param_string_mars_format = "/".join(grib_names)

        level_type, level_set = SCMCommand_ExecuteMarsRequest.paramtype_2_levels(param_type, self.levels)

        output = os.path.join(mars_ret_dir, param_type + "_grib_[date][time]")

        mars_retrieve_command = f"""retrieve,
    class = {self.iniclass},
    expver = {self.exp_version},
    date = {datetimes['dates'][0]}/to/{datetimes['dates'][-1]},
    time = {time_string_mars_format},
    type = an,
    levtype = {level_type},
    levelist = {level_set},
    TARGET="{output}",
    PARAM={param_string_mars_format}
"""

        mars_request_file = os.path.join(mars_req_dir, f"{param_type}_mars_request")

        logger.info(
            f"""Setup and write mars retrieval for {param_type} and variables {grib_names} 
                    to {mars_request_file}")"""
        )

        with open(mars_request_file, "w", encoding="utf-8") as marsRequest:
            marsRequest.write(mars_retrieve_command)

        logger.info(f"DONE - {mars_request_file} for {datetimes['dates'][0]} and {datetimes['dates'][-1]}")

        return mars_request_file

    def submit_mars_request(self, mars_request_file, mars_ret_dir, key, datetimes):
        """
        Submit the mars request
        """

        logger = logging.getLogger(__name__)

        file_out = os.path.join(mars_ret_dir, "mars_request.out_" + key)

        logger.info(
            f"Submit mars retrieval {mars_request_file} for {datetimes['dates'][0]} to {datetimes['dates'][-1]}"
        )

        # set this env var so that the file naming format
        # from the MARS retrieval is correct for the script
        os.environ["MARS_MULTITARGET_STRICT_FORMAT"] = "1"

        start_time = time.time()

        with open(file_out, "w") as outfile:
            _ = subprocess.run(
                ["mars", mars_request_file],
                cwd=mars_ret_dir,
                stdout=outfile,
                stderr=subprocess.STDOUT,
                check=True,
            )

        end_time = time.time()
        execution_time = end_time - start_time
        logger.info(f"DONE - mars retrieval completed for {key} in {execution_time:.4f} seconds")


class SCMCommand_ExecuteMarsRequest_CDSAPI(SCMCommand_ExecuteMarsRequest):    

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def execute(self):

        import cdsapi

        logger = logging.getLogger(__name__)

        time_string_mars_format = "/".join(self.datetimes["times_for_mars"])
        param_string_mars_format = "/".join(self.grib_names)

        level_type, level_set = SCMCommand_ExecuteMarsRequest.paramtype_2_levels(self.param_type, self.levels)

        # Name of output file
        output_base = os.path.join(self.mars_ret_dir, self.param_type + "_grib")
        output_all_datetimes = output_base + "_all_dates_times"

        mars_request = {
            "class": self.iniclass,
            "date": f"{self.datetimes['dates'][0]}/to/{self.datetimes['dates'][-1]}",
            "expver": self.exp_version,
            "levtype": level_type,
            "param": param_string_mars_format,
            "stream": "oper",
            "time": time_string_mars_format,
            "type": "an",
        }

        if level_type == "ml":
            mars_request["levelist"] = level_set

        mars_request_file = os.path.join(self.mars_req_dir, f"{self.param_type}_mars_request_CDSAPI_TEST")

        logger.info(
            f"""Setup and write mars retrieval for {self.param_type} and variables {self.grib_names} to {mars_request_file}")"""
        )

        with open(mars_request_file, "w", encoding="utf-8") as marsRequest:
            marsRequest.write(json.dumps(mars_request, indent=2))

        client = cdsapi.Client()
        client.retrieve("reanalysis-era5-complete", mars_request, output_all_datetimes)

        # split into grib files according to date/time
        logger.info(f"Splitting {output_all_datetimes} into separate GRIB files")
        for date in self.datetimes["dates"]:
            for time in self.datetimes["times_for_mars"]:

                fsource = output_all_datetimes
                ftime = "".join(time.split(":")[:2])
                fdest = output_base+f"_{date}{ftime}"

                logger.info(f"Source file {fsource} -> destination {fdest} | for date {date} and time {ftime}" )

                copier = SCMCommand_GribCopy_EcCodes(source_filepath=fsource, destination_filepath=fdest)

                filter = {
                    "dataDate": date,
                    "dataTime": str(int(ftime)) # remove leading 0
                }
                copier.execute(filter_criteria=filter, wmode="a")

        logger.info(f"DONE - {mars_request_file} for {self.datetimes['dates'][0]} and {self.datetimes['dates'][-1]}")

# ----------------------


# ---- Submit Job ------
class SCMCommand_SubmitJob(SCMCommand):

    required_args = ["getini1c_exec", "global_in_datadir", "datetime"]

    run_args = None

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        self.getini1c_exec = self.kwargs["getini1c_exec"]
        self.global_in_datadir = self.kwargs["global_in_datadir"]
        self.datetime = self.kwargs["datetime"]

        if not isinstance(self.run_args, list):
            raise ValueError(f"Class {type(self).__name__} is abstract!")
        else:
            self.run_command = self.run_args + [self.getini1c_exec]

    def execute(self):

        logger = logging.getLogger(__name__)
        logger.info(f"Submit srun in {self.global_in_datadir} for {self.datetime}")

        # Run the command and redirect stdout and stderr to file_out
        file_out = os.path.join(self.global_in_datadir, f"getini1c.out_{self.datetime}")

        start_time = time.time()
        with open(file_out, "w") as outfile:
            process = subprocess.Popen(
                self.run_command,
                cwd=self.global_in_datadir,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                text=True,
            )

            # Netcdf error from getini1c does not trigger an srun fail but the program
            # has failed. Hence, monitor output for netcdf error text
            for line in iter(process.stdout.readline, ""):
                outfile.write(line)  # Write the output to the file
                outfile.flush()  # Ensure it's written immediately
                if "Stopped with NetCDF error" in line:
                    logger.error(
                        f"""{self.run_command} failed for {self.datetime} with 'Stopped with NetCDF error'
                                in {file_out} - EXITING"""
                    )
                    process.terminate()  # Stop the process
                    process.wait()  # Ensure cleanup
                    sys.exit(1)  # Exit with an error status

            # Wait for the process to complete
            process.wait()

        end_time = time.time()

        if process.returncode != 0:
            logger.error(f"{self.run_command} failed for {self.datetime} - EXITING")
            sys.exit(1)

        execution_time = end_time - start_time
        logger.debug(f"getini1c completed for {self.datetime} in {execution_time:.4f} seconds")


class SCMCommand_SubmitJob_SLURM(SCMCommand_SubmitJob):

    run_args = [
        "srun",
        "-K0",
        "--wait=300",
        "--export=ALL",
        "-n",
        "32",
        "--hint=nomultithread",
        "--cpus-per-task",
        "4",
        "--partition=par",
        "--distribution=block:cyclic",
    ]

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)


class SCMCommand_SubmitJob_Local(SCMCommand_SubmitJob):

    run_args = []

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

# ----------------------


# ---- SCM Command Factory ----
factory_local = {
    "grib_ls": SCMCommand_GribLs_EcCodes,
    "grib_copy": SCMCommand_GribCopy_EcCodes,
    "grib_get": SCMCommand_GribGet_EcCodes,
    "mars_request": SCMCommand_ExecuteMarsRequest_CDSAPI,
    "submit_job": SCMCommand_SubmitJob_Local,
}

factory_ecmwf_hpc = {
    "grib_ls": SCMCommand_GribLs_CL,
    "grib_copy": SCMCommand_GribCopy_CL,
    "grib_get": SCMCommand_GribGet_CL,
    "mars_request": SCMCommand_ExecuteMarsRequest_CL,
    "submit_job": SCMCommand_SubmitJob_SLURM,
}

factories = {"local": factory_local, "ec-hpc2020": factory_ecmwf_hpc}
# -----------------------------
