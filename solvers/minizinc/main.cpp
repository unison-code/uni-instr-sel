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
#include "../common/exceptions/exception.h"
#include "../common/model/params.h"
#include "../common/model/types.h"
#include "../common/optionparser/optionparser.h"
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
printMinizincList(ostream&, const T&, const string&, const string&);

template <typename T>
void
printMinizincValue(
    ostream& out,
    const list<T>& l,
    const string& dl = "[",
    const string& dr = "]"
) {
    printMinizincList(out, l, dl, dr);
}

template <typename T>
void
printMinizincValue(
    ostream& out,
    const vector<T>& v,
    const string& dl = "[",
    const string& dr = "]"
) {
    printMinizincList(out, v, dl, dr);
}

template <typename T>
void
printMinizincValue(
    ostream& out,
    const T& v,
    const string&,
    const string&
) {
    out << v;
}

template <>
void
printMinizincValue(
    ostream& out,
    const bool& v,
    const string&,
    const string&
) {
    out << (v ? "true" : "false");
}

template <typename T>
void
printMinizincList(
    ostream& out,
    const T& l,
    const string& dl = "[",
    const string& dr = "]"
) {
    out << dl;
    bool isFirst = true;
    for (const auto& e : l) {
        if (isFirst) isFirst = false;
        else out << ",";
        printMinizincValue(out, e, "{", "}");
    }
    out << dr;
}

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

void
generateModelFunctionParameters(
    const Params& params,
    ostream& out,
    ostream& debug
) {
    debug << "% Action node mappings" << endl;
    for (const ID& id : params.getIDsForAllActionNodesInF()) {
        debug << "% ID " << id << " -> index "
              << params.getIndexForActionNodeInF(id)
              << endl;
    }
    debug << endl;
    debug << "% Data node mappings" << endl;
    for (const ID& id : params.getIDsForAllDataNodesInF()) {
        debug << "% ID " << id << " -> index "
              << params.getIndexForDataNodeInF(id)
              << endl;
    }
    debug << endl;
    debug << "% State node mappings" << endl;
    for (const ID& id : params.getIDsForAllStateNodesInF()) {
        debug << "% ID " << id << " -> index "
              << params.getIndexForStateNodeInF(id)
              << endl;
    }
    debug << endl;
    debug << "% Label node mappings" << endl;
    for (const ID& id : params.getIDsForAllLabelNodesInF()) {
        debug << "% ID " << id << " -> index "
              << params.getIndexForLabelNodeInF(id)
              << endl;
    }
    debug << endl;

    out << "% Function data" << endl;

    out << "numFuncActionNodes = " << params.getNumActionNodesInF() << ";"
         << endl;
    out << "numFuncDataNodes = " << params.getNumDataNodesInF() << ";"
         << endl;
    out << "numFuncStateNodes = " << params.getNumStateNodesInF() << ";"
         << endl;
    out << "numFuncLabelNodes = " << params.getNumLabelNodesInF() << ";"
         << endl;

    out << "rootLabel = "
        << params.getIndexForLabelNodeInF(params.getRootLabelInF())
         << ";"
         << endl;

    out << "funcLabelDomsets = array1d(allFuncLabelNodes, ";
    {
        size_t num_nodes = params.getNumLabelNodesInF();
        vector< list<ArrayIndex> > node_lists(num_nodes);
        for (const ID& id : params.getIDsForAllLabelNodesInF()) {
            const auto& domset = params.getDomsetForLabelNodeInF(id);
            node_lists[params.getIndexForLabelNodeInF(id)] =
                params.getIndicesForLabelNodesInF(domset);
        }
        printMinizincValue(out, node_lists);
    }
    out << ");" << endl;
}

void
generateModelTargetMachineParameters(
    const Params& params,
    ostream& out,
    ostream& debug
) {
    out << "% Target machine data" << endl;

    out << "numRegisters = " << params.getNumRegistersInM() << ";"
         << endl;
}

void
generateModelPatternInstanceParameters(
    const Params& params,
    ostream& out,
    ostream& debug
) {
    debug << "% Pattern instance mappings" << endl;
    for (const ID& id : params.getIDsForAllPIs()) {
        debug << "% ID " << id << " -> index "
              << params.getIndexForPI(id) << endl;
    }
    debug << endl;

    out << "% Pattern instance data" << endl;

    out << "numPatternInstances = " << params.getNumPIs() << ";"
         << endl;

    out << "patInstActionsCovered = array1d(allPatternInstances, ";
    {
        size_t num_instances = params.getNumPIs();
        vector< list<ArrayIndex> > node_lists(num_instances);
        for (const ID& id : params.getIDsForAllPIs()) {
            const list<ID>& nodes = params.getActionNodesCoveredByPI(id);
            node_lists[params.getIndexForPI(id)] =
                params.getIndicesForActionNodesInF(nodes);
        }
        printMinizincValue(out, node_lists);
    }
    out << ");" << endl;

    out << "patInstDataDefined = array1d(allPatternInstances, ";
    {
        size_t num_instances = params.getNumPIs();
        vector< list<ArrayIndex> > node_lists(num_instances);
        for (const ID& id : params.getIDsForAllPIs()) {
            const list<ID>& nodes = params.getDataNodesDefinedByPI(id);
            node_lists[params.getIndexForPI(id)] =
                params.getIndicesForDataNodesInF(nodes);
        }
        printMinizincValue(out, node_lists);
    }
    out << ");" << endl;

    out << "patInstStateDefined = array1d(allPatternInstances, ";
    {
        size_t num_instances = params.getNumPIs();
        vector< list<ArrayIndex> > node_lists(num_instances);
        for (const ID& id : params.getIDsForAllPIs()) {
            const list<ID>& nodes = params.getStateNodesDefinedByPI(id);
            node_lists[params.getIndexForPI(id)] =
                params.getIndicesForStateNodesInF(nodes);
        }
        printMinizincValue(out, node_lists);
    }
    out << ");" << endl;

    out << "patInstDataUsed = array1d(allPatternInstances, ";
    {
        size_t num_instances = params.getNumPIs();
        vector< list<ArrayIndex> > node_lists(num_instances);
        for (const ID& id : params.getIDsForAllPIs()) {
            const list<ID>& nodes = params.getDataNodesUsedByPI(id);
            node_lists[params.getIndexForPI(id)] =
                params.getIndicesForDataNodesInF(nodes);
        }
        printMinizincValue(out, node_lists);
    }
    out << ");" << endl;

    out << "patInstStateUsed = array1d(allPatternInstances, ";
    {
        size_t num_instances = params.getNumPIs();
        vector< list<ArrayIndex> > node_lists(num_instances);
        for (const ID& id : params.getIDsForAllPIs()) {
            const list<ID>& nodes = params.getStateNodesUsedByPI(id);
            node_lists[params.getIndexForPI(id)] =
                params.getIndicesForStateNodesInF(nodes);
        }
        printMinizincValue(out, node_lists);
    }
    out << ");" << endl;

    debug << "% Pattern instance-destination label mappings" << endl;
    out << "patInstAndLabelMappings = "
         << "array2d(allPatternInstances, allFuncLabelNodes, ";
    {
        size_t num_instances = params.getNumPIs();
        size_t num_labels = params.getNumLabelNodesInF();
        vector<int> mappings(num_instances * num_labels, -1);
        int index = 0;
        for (const ID& pat_id : params.getIDsForAllPIs()) {
            for (const ID& node_id : params.getLabelNodesReferredByPI(pat_id)) {
                debug << "% Pattern instance ID " << pat_id << " + "
                      << "label ID " << node_id << " -> index "
                      << index << endl;
                ArrayIndex pat_index = params.getIndexForPI(pat_id);
                ArrayIndex node_index = params.getIndexForLabelNodeInF(node_id);
                mappings[pat_index * num_labels + node_index] = index++;
            }
        }
        printMinizincValue(out, mappings);
    }
    out << ");" << endl;
    debug << endl;

    out << "patInstCodeSizes = array1d(allPatternInstances, ";
    {
        vector<int> code_sizes(params.getNumPIs());
        for (const ID& id : params.getIDsForAllPIs()) {
            code_sizes[params.getIndexForPI(id)] = params.getCodeSizeForPI(id);
        }
        printMinizincValue(out, code_sizes);
    }
    out << ");" << endl;

    out << "patInstLatencies = array1d(allPatternInstances, ";
    {
        vector<int> latencies(params.getNumPIs());
        for (const ID& id : params.getIDsForAllPIs()) {
            latencies[params.getIndexForPI(id)] = params.getLatencyForPI(id);
        }
        printMinizincValue(out, latencies);
    }
    out << ");" << endl;

    out << "patInstNoUseDefDomConstraints = array1d(allPatternInstances, ";
    {
        vector<bool> settings(params.getNumPIs());
        for (const ID& id : params.getIDsForAllPIs()) {
            settings[params.getIndexForPI(id)] =
                params.getNoUseDefDomConstraintsSettingForPI(id);
        }
        printMinizincValue(out, settings);
    }
    out << ");" << endl;
}

void
generateModelParameters(
    const Params& params,
    ostream& out,
    ostream& debug
) {
    generateModelFunctionParameters(params, out, debug);
    out << endl;
    generateModelTargetMachineParameters(params, out, debug);
    out << endl;
    generateModelPatternInstanceParameters(params, out, debug);
}

void
generateModelConstraintsForF(
    const Params& params,
    ostream& out,
    ostream& debug
) {
    ConstraintProcessor cprocessor(params);
    const list<const Constraint*>& cs = params.getConstraintsForF();
    if (cs.size() > 0) {
        for (const Constraint* c : cs) {
            out << cprocessor.processConstraintForF(c) << endl;
        }
    }
}

void
generateModelConstraintsForPIs(
    const Params& params,
    ostream& out,
    ostream& debug
) {
    ConstraintProcessor cprocessor(params);
    for (const ID& id : params.getIDsForAllPIs()) {
        const list<const Constraint*>& cs = params.getConstraintsForPI(id);
        if (cs.size() > 0) {
            out << "% ID " << id << endl;
            for (const Constraint* c : cs) {
                out << cprocessor.processConstraintForPI(c, id) << endl;
            }
            out << endl;
        }
    }
}

void outputModelParams(
    const Params& params,
    ostream& out
) {
    stringstream sout;
    stringstream sdebug;

    sdebug << "%============" << endl;
    sdebug << "% DEBUG INFO" << endl;
    sdebug << "%============" << endl;
    sdebug << endl;

    sout << "%============" << endl;
    sout << "% PARAMETERS" << endl;
    sout << "%============" << endl;
    sout << endl;
    generateModelParameters(params, sout, sdebug);
    sout << endl
         << endl
         << endl;

    sout << "%============================" << endl;
    sout << "% FUNCTION GRAPH CONSTRAINTS" << endl;
    sout << "%============================" << endl;
    sout << endl;
    generateModelConstraintsForF(params, sout, sdebug);
    sout << endl
         << endl
         << endl;

    sout << "%==============================" << endl;
    sout << "% PATTERN INSTANCE CONSTRAINTS" << endl;
    sout << "%==============================" << endl;
    sout << endl;
    generateModelConstraintsForPIs(params, sout, sdebug);

    out << "% AUTO-GENERATED" << endl;
    out << endl;
    out << sdebug.str();
    out << endl
        << endl;
    out << sout.str();
}

void outputPostprocessingParams(
    const Params& params,
    ostream& out
) {
    out << "{" << endl;

    out << "\"array-indices-to-func-action-node-id-maps\": ";
    printJsonValue(
        out,
        params.getIDsOfActionNodesInF(
            createArrayIndices(0, params.getNumActionNodesInF())
        )
    );
    out << endl;

    out << "\"array-indices-to-func-data-node-id-maps\": ";
    printJsonValue(
        out,
        params.getIDsOfDataNodesInF(
            createArrayIndices(0, params.getNumDataNodesInF())
        )
    );
    out << endl;

    out << "\"array-indices-to-func-state-node-id-maps\": ";
    printJsonValue(
        out,
        params.getIDsOfStateNodesInF(
            createArrayIndices(0, params.getNumStateNodesInF())
        )
    );
    out << endl;

    out << "\"array-indices-to-func-label-node-id-maps\": ";
    printJsonValue(
        out,
        params.getIDsOfLabelNodesInF(
            createArrayIndices(0, params.getNumLabelNodesInF())
        )
    );
    out << endl;

    out << "\"array-indices-to-machine-register-id-maps\": ";
    printJsonValue(
        out,
        params.getIDsOfRegistersInM(
            createArrayIndices(0, params.getNumRegistersInM())
        )
    );
    out << endl;

    out << "\"array-indices-to-pattern-instance-id-maps\": ";
    printJsonValue(
        out,
        params.getIDsOfPIs(
            createArrayIndices(0, params.getNumPIs())
        )
    );
    out << endl;

    out << "}" << endl;
}



//======================
// COMMAND-LINE OPTIONS
//======================

enum optionIndex {
    PRE,
    HELP,
    MODEL_PARAMS_FILE,
    POSTPROCESSING_PARAMS_FILE
};

const option::Descriptor usage[] =
{
    {
        PRE,
        0,
        "",
        "",
        option::Arg::None,
        "USAGE: input-gen [OPTIONS] JSON_FILE\n" \
        "Options:"
    },
    {
        HELP,
        0,
        "h",
        "help",
        option::Arg::None,
        "  -h, --help" \
        "\tPrints this menu."
    },
    {
        MODEL_PARAMS_FILE,
        0,
        "",
        "model-params-file",
        option::Arg::Required,
        "  --model-params-file=FILE" \
        "\tWhere the model parameters will be output."
    },
    {
        POSTPROCESSING_PARAMS_FILE,
        0,
        "",
        "postprocessing-params-file",
        option::Arg::Required,
        "  --postprocessing-params-file=FILE" \
        "\tWhere the post-processing parameters will be output."
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
    if (!options[MODEL_PARAMS_FILE]) {
        cerr << "No model params file" << endl;
        return 1;
    }
    if (!options[POSTPROCESSING_PARAMS_FILE]) {
        cerr << "No post-processing params file" << endl;
        return 1;
    }
    if (cmdparser.nonOptionsCount() > 1) {
        cerr << "Unknown option '" << cmdparser.nonOption(0) << "'" << endl;
        return 1;
    }
    if (cmdparser.nonOptionsCount() == 0) {
        cerr << "No JSON input file" << endl;
        return 1;
    }

    try {
        // Parse JSON file into an internal model parameters object
        string json_file(cmdparser.nonOption(0));
        ifstream file(json_file);
        if (!file.good()) {
            cerr << "ERROR: '" << json_file << "' does not exist or is "
                 << "unreadable" << endl;
            return 1;
        }
        stringstream ss;
        ss << file.rdbuf();
        const string json_content(ss.str());
        Params params;
        Params::parseJson(json_content, params);

        // Output model params
        ofstream mfile;
        const string mfile_str(options[MODEL_PARAMS_FILE].arg);
        mfile.open(mfile_str);
        if (!mfile.is_open()) {
            THROW(Exception, string("Failed to open file '") + mfile_str + "'");
        }
        outputModelParams(params, mfile);
        mfile.close();

        // Output postprocessing params
        ofstream pfile;
        const string pfile_str(options[POSTPROCESSING_PARAMS_FILE].arg);
        pfile.open(pfile_str);
        if (!pfile.is_open()) {
            THROW(Exception, string("Failed to open file '") + pfile_str + "'");
        }
        outputPostprocessingParams(params, pfile);
        pfile.close();

        return 0;
    }
    catch (Exception& ex) {
        cerr << "ERROR: " << ex.toString() << endl;
        return 1;
    }
}
