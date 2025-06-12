/**
 * (C) Copyright 2024- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 *
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */


#include <iomanip>
#include <cmath>
#include <iostream>
#include <cstdio>

#include "eckit/log/Log.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"
#include "eckit/option/VectorOption.h"
#include "eckit/runtime/Main.h"

#include "ec_datetime.c"
#include "eccodes.h"


using namespace eckit;
using namespace eckit::option;


void usage(const std::string&) {

    Log::info() << std::endl
                << "-------------------------------" << std::endl
                << "Time integration tool" << std::endl
                << "-------------------------------" << std::endl
                << std::endl
                << "Interpolates data from an input climate GRIB file at a given time"
                << "USAGE: timeint --basetime=<YYYYMMDDHH> --grib_code=<> --input_mon=<> --output=<>"
                << std::endl;
}

int main(int argc, char* argv[])
{
    Main::initialise(argc, argv);
    std::vector<Option*> options;

    long long int iyyyymdh;
    std::string iyyyymdh_str;

    int iyyyy, imm, idd, ihh, ihdiff2, ihdiff;
    long int idate;

    int icodemo, ierr, iret, itable;
    FILE* imonin;
    FILE* imonout;
    int ifoundfi, iyo, imo, imt1, imt2, iyt1, iyt2;
    long int icode;

    long int ibitmap, ilev;
    long int nvalues, nlevs, nv;

    double zmiss = 9999.0; // Placeholder for missing value
    double zwei1, zwei2, zcodemo;

    bool lfirst;
    char* ydate;

    options.push_back(new SimpleOption<std::string>("basetime", ""));
    options.push_back(new SimpleOption<double>("grib_code", ""));
    options.push_back(new SimpleOption<std::string>("input_mon", ""));
    options.push_back(new SimpleOption<std::string>("output", ""));


    CmdArgs args(&usage, options, 0, 0, true);

    // Input path
    iyyyymdh_str = args.getString("basetime");
    zcodemo = args.getDouble("grib_code");
    std::string yinpfi_str = args.getString("input_mon");
    std::string youtfi_str = args.getString("output");


    Log::info() << "Basetime: " << iyyyymdh_str
                << ", Grib code: " << zcodemo
                << ", Input month: " << yinpfi_str
                << ", Output: " << youtfi_str << std::endl;
    
    const char* yinpfi = yinpfi_str.c_str();
    const char* youtfi = youtfi_str.c_str();

    // Open input file
    ierr = 0;
    imonin = fopen(yinpfi, "r");
    if (!imonin) {
        std::cerr << "Error opening file _mon_in" << std::endl;
        std::abort();
    }
    
    // Read first record
    codes_handle* igribin = codes_handle_new_from_file(NULL, imonin, PRODUCT_GRIB, &iret);
    if (iret != GRIB_SUCCESS) {
        std::cerr << "Error reading monthly climate file, iret = " << iret << std::endl;
        std::abort();
    }
    
    // Clone the GRIB message
    codes_handle* igribout = codes_handle_clone(igribin);
    
    // Unpack header records to determine data lengths
    codes_get_long(igribin, "bitmapPresent", &ibitmap);
    if (ibitmap == 1) {
        codes_set_double(igribin, "missingValue", zmiss);
    }
    
    codes_get_long(igribin, "numberOfValues", &nvalues);
    codes_get_long(igribin, "NV", &nv);
    
    if (nv > 0) {
        nlevs = nv / 2 - 1;
    } else {
        nlevs = 1;
    }

    Log::info() << "Number of values: " << nvalues << std::endl;

    // Dynamic allocation equivalent to Fortran's ALLOCATE
    std::vector<std::vector<double>> clim1(nvalues, std::vector<double>(nlevs));
    std::vector<std::vector<double>> clim2(nvalues, std::vector<double>(nlevs));
    std::vector<double> expt(nvalues);

    Log::info() << "Datetime: " << iyyyymdh_str << std::endl;

    // Convert integer to string and parse year, month, day, hour
    iyyyymdh = std::stoll(iyyyymdh_str);
    Log::info() << "iyyyymdh: " << iyyyymdh << std::endl;

    iyyyy = std::stoi(iyyyymdh_str.substr(0, 4));
    Log::info() << "iyyyy: " << iyyyy << std::endl;

    imm = std::stoi(iyyyymdh_str.substr(4, 2));
    idd = std::stoi(iyyyymdh_str.substr(6, 2));
    ihh = std::stoi(iyyyymdh_str.substr(8, 2));

    Log::info() << "Year: " << iyyyy
                << ", Month: " << imm
                << ", Day: " << idd
                << ", Hour: " << ihh
                << std::endl;

    // Calculate left/right time bounds    
    if (idd >= 15) {
        imt1 = imm;
        imt2 = 1 + (imm % 12);
        iyt1 = iyyyy;
        iyt2 = (imt2 == 1) ? iyt1 + 1 : iyt1;
    } else {
        imt1 = 1 + ((imm + 10) % 12);
        imt2 = imm;
        iyt1 = (imt1 == 12) ? iyyyy - 1 : iyyyy;
        iyt2 = iyyyy;
    }

    Log::info() <<  "Interpolating between month: " << imt1
                << " and month: " << imt2
                << std::endl; 

    ifoundfi = 0;

    // Loop over climate file, reading GRIB records
    lfirst = true;
    while (true) {

        if (!lfirst) {
            igribin = codes_handle_new_from_file(nullptr, imonin, PRODUCT_GRIB, &iret);
            if (!igribin) {
                break;
            }
        }

        lfirst = false;

        codes_get_long(igribin, "paramId", &icode);

        // Handle possible parameter.table notation
        icodemo = std::floor(zcodemo);
        itable = std::round(1000 * (zcodemo - icodemo));
        if (itable != 128) {
            icodemo = 1000 * itable + icodemo;
        }

        if (icodemo == icode) {

            codes_get_long(igribin, "bitmapPresent", &ibitmap);

            if (ibitmap == 1) {
                codes_set_double(igribin, "missingValue", zmiss);
            }

            codes_get_long(igribin, "dataDate", &idate);

            std::string idate_str = std::to_string(idate);
            iyo = std::stoi(idate_str.substr(0, 4));
            imo = std::stoi(idate_str.substr(4, 2));

            if (imo == imt1 || imo == imt2) {
                ifoundfi++;

                if (nv > 0) {
                    codes_get_long(igribin, "level", &ilev);
                } else {
                    ilev = 1;
                }

                std::vector<double> values;
                size_t nvalues;
                codes_get_size(igribin, "values", &nvalues);
                values.resize(nvalues);

                codes_get_double_array(igribin, "values", values.data(), &nvalues);
                
                if (imo == imt1) {
                    for (size_t i=0; i<nvalues; i++) {
                        clim1[i][ilev - 1] = values[i];
                    }
                }

                if (imo == imt2) {
                    for (size_t i=0; i<nvalues; i++) {
                        clim2[i][ilev - 1] = values[i];
                    }
                };
            }
        }
    }

    Log::info() << "Successfully read all GRIB records" << std::endl;

    // =======================================================================

    //           3.        Recode and write
    //                     ----------------

    //             3.1     Open output file

    int fifteen = 15;
    int zero = 0;
    HOURDIFF(&iyt2, &imt2, &fifteen, &zero, &iyyyy, &imm, &idd, &ihh, &ihdiff2, &iret);
    HOURDIFF(&iyt2, &imt2, &fifteen, &zero, &iyt1, &imt1, &fifteen, &zero, &ihdiff, &iret);

    zwei1=float(ihdiff2)/float(ihdiff);
    zwei2=1.-zwei1;

    for (ilev = 0; ilev<nlevs; ilev++) {

        if(ibitmap == 0) {
            for (size_t i=0; i<nvalues; i++) {
                expt[i] = zwei1*clim1[i][ilev] + zwei2*clim2[i][ilev];
            }
        } else {

            for (size_t i=0; i<nvalues; i++) {
                if (clim1[i][ilev] == zmiss || clim2[i][ilev] == zmiss) {
                    expt[i] = zmiss;
                } else {
                    expt[i] = zwei1 * clim1[i][ilev] + zwei2 * clim2[i][ilev];
                }
            }
        }

        //           3.3     Recode GRIB

        codes_set_long(igribout, "paramId", icodemo);
        codes_set_long(igribout, "dataDate", iyyyy*10000+imm*100+idd);
        codes_set_long(igribout, "dataTime", ihh*100);

        if (nv > 0) {
            codes_set_long(igribout, "level", ilev);
        }

        codes_set_double_array(igribout, "values", expt.data(), expt.size());

        // !            3.4     Write modified GRIB record to file _mon_out

        iret = codes_write_message(igribout, youtfi, "w");
        if (iret != CODES_SUCCESS) {
            Log::error() << "(Error writing GRIB record)" << std::endl;
            Log::error() << "(iret: )" << iret << std::endl;
            Log::error() << "(icode: )" << icode << std::endl;
            return 1;
        }

    }

    // release the grib message handle
    codes_handle_delete(igribin);

    fclose(imonin);

    Log::info() << " Code " << icodemo << " Date/time= " << iyyyymdh << std::endl;
    Log::info() <<  "Interpolated between month: " << imt1
                << " with weight: " << zwei1
                << " and month: " << imt2 << " with weight: " << zwei2
                << std::endl; 

}