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

#include "constraintprocessor.h"
#include "params.h"
#include "common/exceptions/exception.h"
#include "common/model/types.h"
#include "common/optionparser/optionparser.h"
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



//================
// HELP FUNCTIONS
//================

template <typename T>
void
printJsonList(ostream&, const T&);

template <typename T>
void
printJsonValue(ostream& out, const list<T>& l) {
    printJsonList(out, l);
}

template <typename T>
void
printJsonValue(ostream& out, const vector<T>& v) {
    printJsonList(out, v);
}

template <typename T>
void
printJsonValue(ostream& out, const T& v) {
    out << v;
}

template <>
void
printJsonValue(ostream& out, const bool& v) {
    out << (v ? "true" : "false");
}

template <typename T>
void
printJsonList(ostream& out, const T& l) {
    out << "[";
    bool isFirst = true;
    for (const auto& e : l) {
        if (isFirst) isFirst = false;
        else out << ",";
        printJsonValue(out, e);
    }
    out << "]";
}

void outputModelParams(
    const Params& params,
    ostream& out
) {
    ConstraintProcessor cprocessor(params);

    out << "{" << endl;

    // Function graph data

    out << "\"num-func-anodes\" : ";
    printJsonValue(out, params.getNumActionNodesInF());
    out << "," << endl
        << "\"num-func-dnodes\" : ";
    printJsonValue(out, params.getNumDataNodesInF());
    out << "," << endl
        << "\"num-func-snodes\" : ";
    printJsonValue(out, params.getNumStateNodesInF());
    out << "," << endl
        << "\"num-func-lnodes\" : ";
    printJsonValue(out, params.getNumLabelNodesInF());

    out << "," << endl
        << "\"func-root-label\" : ";
    printJsonValue(out,
                   params.getIndexForLabelNodeInF(params.getRootLabelInF()));

    out << "," << endl
        << "\"func-label-domsets\" : ";
    {
        size_t num_nodes = params.getNumLabelNodesInF();
        vector< list<ArrayIndex> > node_lists(num_nodes);
        for (const ID& id : params.getIDsForAllLabelNodesInF()) {
            const auto& domset = params.getDomsetForLabelNodeInF(id);
            node_lists[params.getIndexForLabelNodeInF(id)] =
                params.getIndicesForLabelNodesInF(domset);
        }
        printJsonValue(out, node_lists);
    }

    out << "," << endl
        << "\"func-constraints\" : ";
    {
        list<string> cs_str;
        const list<const Constraint*>& cs = params.getConstraintsForF();
        for (const Constraint* c : cs) {
            string str;
            const Constraint* new_c = cprocessor.processConstraintForF(c);
            str += "\"" + new_c->toLisp() + "\"";
            cs_str.push_back(str);
            delete new_c;
        }
        printJsonValue(out, cs_str);
    }

    // Target machine data

    out << "," << endl
        << "\"num-registers\" : ";
    printJsonValue(out, params.getNumRegistersInM());

    // Pattern instance data

    out << "," << endl
        << "\"num-pattern-instances\" : ";
    printJsonValue(out, params.getNumPIs());

    out << "," << endl
        << "\"pat-inst-anodes-covered\" : ";
    {
        size_t num_instances = params.getNumPIs();
        vector< list<ArrayIndex> > node_lists(num_instances);
        for (const ID& id : params.getIDsForAllPIs()) {
            const list<ID>& nodes = params.getActionNodesCoveredByPI(id);
            node_lists[params.getIndexForPI(id)] =
                params.getIndicesForActionNodesInF(nodes);
        }
        printJsonValue(out, node_lists);
    }

    out << "," << endl
        << "\"pat-inst-dnodes-defined\" : ";
    {
        size_t num_instances = params.getNumPIs();
        vector< list<ArrayIndex> > node_lists(num_instances);
        for (const ID& id : params.getIDsForAllPIs()) {
            const list<ID>& nodes = params.getDataNodesDefinedByPI(id);
            node_lists[params.getIndexForPI(id)] =
                params.getIndicesForDataNodesInF(nodes);
        }
        printJsonValue(out, node_lists);
    }

    out << "," << endl
        << "\"pat-inst-snodes-defined\" : ";
    {
        size_t num_instances = params.getNumPIs();
        vector< list<ArrayIndex> > node_lists(num_instances);
        for (const ID& id : params.getIDsForAllPIs()) {
            const list<ID>& nodes = params.getStateNodesDefinedByPI(id);
            node_lists[params.getIndexForPI(id)] =
                params.getIndicesForStateNodesInF(nodes);
        }
        printJsonValue(out, node_lists);
    }

    out << "," << endl
        << "\"pat-inst-dnodes-used\" : ";
    {
        size_t num_instances = params.getNumPIs();
        vector< list<ArrayIndex> > node_lists(num_instances);
        for (const ID& id : params.getIDsForAllPIs()) {
            const list<ID>& nodes = params.getDataNodesUsedByPI(id);
            node_lists[params.getIndexForPI(id)] =
                params.getIndicesForDataNodesInF(nodes);
        }
        printJsonValue(out, node_lists);
    }

    out << "," << endl
        << "\"pat-inst-snodes-used\" : ";
    {
        size_t num_instances = params.getNumPIs();
        vector< list<ArrayIndex> > node_lists(num_instances);
        for (const ID& id : params.getIDsForAllPIs()) {
            const list<ID>& nodes = params.getStateNodesUsedByPI(id);
            node_lists[params.getIndexForPI(id)] =
                params.getIndicesForStateNodesInF(nodes);
        }
        printJsonValue(out, node_lists);
    }

    out << "," << endl
        << "\"pat-inst-lnodes-referred\" : ";
    {
        size_t num_instances = params.getNumPIs();
        vector< list<ArrayIndex> > node_lists(num_instances);
        for (const ID& id : params.getIDsForAllPIs()) {
            const list<ID>& nodes = params.getLabelNodesReferredByPI(id);
            node_lists[params.getIndexForPI(id)] =
                params.getIndicesForLabelNodesInF(nodes);
        }
        printJsonValue(out, node_lists);
    }

    out << "," << endl
        << "\"pat-inst-code-sizes\" : ";
    {
        vector<int> code_sizes(params.getNumPIs());
        for (const ID& id : params.getIDsForAllPIs()) {
            code_sizes[params.getIndexForPI(id)] = params.getCodeSizeForPI(id);
        }
        printJsonValue(out, code_sizes);
    }

    out << "," << endl
        << "\"pat-inst-latencies\" : ";
    {
        vector<int> latencies(params.getNumPIs());
        for (const ID& id : params.getIDsForAllPIs()) {
            latencies[params.getIndexForPI(id)] = params.getLatencyForPI(id);
        }
        printJsonValue(out, latencies);
    }

    out << "," << endl
        << "\"pat-inst-apply-use-def-dom-constraints\" : ";
    {
        vector<bool> settings(params.getNumPIs());
        for (const ID& id : params.getIDsForAllPIs()) {
            settings[params.getIndexForPI(id)] =
                params.getAUDDCSettingForPI(id);
        }
        printJsonValue(out, settings);
    }

    out << "," << endl
        << "\"pat-inst-constraints\" : ";
    {
        vector< list<string> > all_cs_str(params.getNumPIs());
        for (const ID& id : params.getIDsForAllPIs()) {
            list<string> cs_str;
            const list<const Constraint*>& cs = params.getConstraintsForPI(id);
            for (const Constraint* c : cs) {
                string str;
                const Constraint* new_c =
                    cprocessor.processConstraintForPI(c, id);
                str += "\"" + new_c->toLisp() + "\"";
                cs_str.push_back(str);
                delete new_c;
            }
            all_cs_str[params.getIndexForPI(id)] = cs_str;
        }
        printJsonValue(out, all_cs_str);
    }

    out << endl
        << "}" << endl;
}

void
outputPostprocessingParams(
    const Params& params,
    const string& json_content,
    ostream& out
) {
    out << "{" << endl;

    // Output all the original CP model parameters as a separate field
    out << "\"model-params\": "
        << json_content;

    out << "," << endl;
    out << "\"array-index-to-id-maps\": {" << endl;

    out << "\"pattern-instances\": ";
    printJsonValue(
        out,
        params.getIDsOfPIs(createArrayIndices(0, params.getNumPIs()))
    );

    out << "," << endl
        << "\"label-nodes\": ";
    printJsonValue(
        out,
        params.getIDsOfLabelNodesInF(
            createArrayIndices(0, params.getNumLabelNodesInF())
        )
    );

    out << "," << endl
        << "\"data-nodes\": ";
    printJsonValue(
        out,
        params.getIDsOfDataNodesInF(
            createArrayIndices(0, params.getNumDataNodesInF())
        )
    );

    out << endl
        << "}" << endl;

    out << endl
        << "}" << endl;
}



//======================
// COMMAND-LINE OPTIONS
//======================

enum optionIndex {
    PRE,
    HELP,
    MPF,
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
        MPF,
        0,
        "",
        "mpf",
        option::Arg::Required,
        "  --mpf=FILE\n" \
        "\tFile to which the model parameters will be output."
    },
    {
        PPF,
        0,
        "",
        "ppf",
        option::Arg::Required,
        "  --ppf=FILE\n" \
        "\tFile to which the post-processing parameters will be output."
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
    if (!options[MPF]) {
        cerr << "No model params file" << endl;
        return 1;
    }
    if (!options[PPF]) {
        cerr << "No post-processing params file" << endl;
        return 1;
    }
    if (cmdparser.nonOptionsCount() > 1) {
        cerr << "Unknown option '" << cmdparser.nonOption(0) << "'" << endl;
        return 1;
    }
    if (cmdparser.nonOptionsCount() == 0) {
        cerr << "No input file" << endl;
        return 1;
    }

    try {
        // Parse JSON file into an internal model parameters object
        string json_file(cmdparser.nonOption(0));
        ifstream file(json_file);
        if (!file.good()) {
            THROW(Exception,
                  string("'") + json_file + "' does not exist or is not "
                  + "readable");
        }
        stringstream ss;
        ss << file.rdbuf();
        const string json_content(ss.str());
        Params params;
        Params::parseJson(json_content, params);

        // Output model params
        ofstream mfile;
        const string mfile_str(options[MPF].arg);
        mfile.open(mfile_str);
        if (!mfile.is_open()) {
            THROW(Exception, string("Failed to open file '") + mfile_str + "'");
        }
        outputModelParams(params, mfile);
        mfile.close();

        // Output postprocessing params
        ofstream pfile;
        const string pfile_str(options[PPF].arg);
        pfile.open(pfile_str);
        if (!pfile.is_open()) {
            THROW(Exception, string("Failed to open file '") + pfile_str + "'");
        }
        outputPostprocessingParams(params, json_content, pfile);
        pfile.close();

        return 0;
    }
    catch (Exception& ex) {
        cerr << "ERROR: " << ex.toString() << endl;
        return 1;
    }
}
