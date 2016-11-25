# This directory provides test-data for the software created in scm-data/src.

# NOTE: For testing you may want to reduce the JPMAXGRID dimensions (set for TCo1279) in
#       read_grib_scm.F90 interface/read_grib_scm.h
#

tests would be:

mpirun -np 4 /tmp/data/scm-data/install/scm-data/bin/getini1c 1>log 2>&1

ddt /tmp/data/scm-data/install/scm-data/bin/getini1c
valgrind /tmp/data/scm-data/install/scm-data/bin/getini1c

To add debug info on the executable:
cd into the directory ... builds/scm-data
cmake . -DCMAKE_BUILD_TYPE=debug
make install
