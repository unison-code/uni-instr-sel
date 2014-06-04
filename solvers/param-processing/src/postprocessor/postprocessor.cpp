/**
 * Copyright (c) 2014, Gabriel Hjort Blindell <ghb@kth.se>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include "../common/constraintprocessor.h"
#include "../common/jsonprinting.h"
#include "../common/preparams.h"
#include "../../../common/exceptions/exception.h"
#include "../../../common/model/types.h"
#include "../../../common/optionparser/optionparser.h"
#include <fstream>
#include <iostream>
#include <list>
#include <sstream>
#include <string>
#include <vector>

using namespace Model;
using std::cerr;
using std::cout;
using std::endl;
using std::ifstream;
using std::list;
using std::ofstream;
using std::ostream;
using std::string;
using std::stringstream;
using std::vector;



//======================
// COMMAND-LINE OPTIONS
//======================

enum optionIndex {
    PRE,
    HELP,
    SF,
    PPF
};

const option::Descriptor usage[] =
{
    {
        PRE,
        0,
        "",
        "",
        option::Arg::None,
        "USAGE: input-gen [OPTIONS] INPUTFILE\n" \
        "Options:"
    },
    {
        HELP,
        0,
        "h",
        "help",
        option::Arg::None,
        "  -h, --help\n" \
        "\tPrints this menu."
    },
    {
        SF,
        0,
        "",
        "sf",
        option::Arg::Required,
        "  --sf=FILE\n" \
        "\tJSON file containing the solution."
    },
    {
        PPF,
        0,
        "",
        "ppf",
        option::Arg::Required,
        "  --ppf=FILE\n" \
        "\tJSON file containing the post-processing parameters."
    },
    // Termination sentinel
    { 0, 0, 0, 0, 0, 0 }
};

int
main(int argc, char** argv) {
    // Parse command-line arguments
    argc -= (argc > 0); argv += (argc > 0); // Skip program name if present
    option::Stats stats(usage, argc, argv);
    option::Option options[stats.options_max], buffer[stats.buffer_max];
    option::Parser cmdparser(usage, argc, argv, options, buffer);
    if (cmdparser.error()) {
        return 1;
    }
    if (options[HELP] || argc == 0) {
        option::printUsage(cout, usage);
        return 0;
    }
    if (!options[SF]) {
        cerr << "No solution file" << endl;
        return 1;
    }
    if (!options[PPF]) {
        cerr << "No post-processing params file" << endl;
        return 1;
    }
    if (cmdparser.nonOptionsCount() >= 1) {
        cerr << "Unknown option '" << cmdparser.nonOption(0) << "'" << endl;
        return 1;
    }

    try {
        // Parse the solution JSON file
        // TODO: implement

        // Parse the post-processing JSON file into internal parameters objects
        Preparams preparams;
        {
            string json_file(options[PPF].arg);
            ifstream sfile(json_file);
            if (!sfile.good()) {
                THROW(Exception,
                      string("'") + json_file + "' does not exist or is not "
                      + "readable");
            }
            stringstream ss;
            ss << sfile.rdbuf();
            const string json_content(ss.str());
            Json::Value json_root;
            Json::Reader reader;
            if (!reader.parse(json_content, json_root)) {
                THROW(Exception, reader.getFormattedErrorMessages());
            }
            const Json::Value& preparams_root = json_root["preparams"];
            if (preparams_root.isNull()) {
                THROW(Exception, "No 'preparams' field found");
            }
            Preparams::parseJson(preparams_root, preparams);
        }

        // Output final solution
        // TODO: implement

        return 0;
    }
    catch (Exception& ex) {
        cerr << "ERROR: " << ex.toString() << endl;
        return 1;
    }
}
