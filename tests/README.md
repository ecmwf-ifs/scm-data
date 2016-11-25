# This directory provides test-data for the software created in scm-data/src.

# NOTE: For testing you may want to reduce the JPMAXGRID dimensions (set for TCo1279) in
#       read_grib_scm.F90 AND interface/read_grib_scm.h
#       because otherwise some error may occur with respect to truncated MPI recv messages or the program gets stuck

tests would be:

mpirun -np 4 /tmp/data/scm-data-install/bin/getini1c 1>log 2>&1

ddt /tmp/data/scm-data-install/bin/getini1c
valgrind /tmp/data/scm-data-install/bin/getini1c

To add debug info on the executable:

cd into the directory /tmp/data/scm-data-build/scm-data

cmake . -DCMAKE_BUILD_TYPE=debug

make install
