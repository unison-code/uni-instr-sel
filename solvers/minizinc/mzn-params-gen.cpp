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
#include "../common/exceptions/exception.h"
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
    ostream& out
) {
    out << "% Function data" << endl;

    out << "numFuncActionNodes = " << params.getNumActionNodesInF() << ";"
         << endl;
    out << "numFuncDataNodes = " << params.getNumDataNodesInF() << ";"
         << endl;
    out << "numFuncStateNodes = " << params.getNumStateNodesInF() << ";"
         << endl;
    out << "numFuncLabelNodes = " << params.getNumLabelNodesInF() << ";"
         << endl;

    out << "rootLabel = " << params.getRootLabelInF() << ";"
         << endl;

    out << "funcLabelDomsets = array1d(allFuncLabelNodes, ";
    printMinizincValue(out, params.getDomsetForAllLabelNodesInF());
    out << ");" << endl;
}

void
generateModelTargetMachineParameters(
    const Params& params,
    ostream& out
) {
    out << "% Target machine data" << endl;

    out << "numRegisters = " << params.getNumRegistersInM() << ";"
         << endl;
}

void
generateModelPatternInstanceParameters(
    const Params& params,
    ostream& out
) {
    out << "% Pattern instance data" << endl;

    out << "numPatternInstances = " << params.getNumPIs() << ";"
         << endl;

    out << "patInstActionsCovered = array1d(allPatternInstances, ";
    printMinizincValue(out, params.getActionNodesCoveredByAllPIs());
    out << ");" << endl;

    out << "patInstDataDefined = array1d(allPatternInstances, ";
    printMinizincValue(out, params.getDataNodesDefinedByAllPIs());
    out << ");" << endl;

    out << "patInstStateDefined = array1d(allPatternInstances, ";
    printMinizincValue(out, params.getStateNodesDefinedByAllPIs());
    out << ");" << endl;

    out << "patInstDataUsed = array1d(allPatternInstances, ";
    printMinizincValue(out, params.getDataNodesUsedByAllPIs());
    out << ");" << endl;

    out << "patInstStateUsed = array1d(allPatternInstances, ";
    printMinizincValue(out, params.getStateNodesUsedByAllPIs());
    out << ");" << endl;

    out << "patInstAndLabelMappings = "
         << "array2d(allPatternInstances, allFuncLabelNodes, ";
    {
        size_t num_instances = params.getNumPIs();
        size_t num_labels = params.getNumLabelNodesInF();
        vector<int> mappings(num_instances * num_labels, -1);
        int index = 0;
        ArrayIndex pat_index = 0;
        for (const auto& entries : params.getLabelNodesReferredByAllPIs()) {
            for (const ArrayIndex& node_index : entries) {
                mappings[pat_index * num_labels + node_index] = index++;
            }
            pat_index++;
        }
        printMinizincValue(out, mappings);
    }
    out << ");" << endl;

    out << "patInstCodeSizes = array1d(allPatternInstances, ";
    printMinizincValue(out, params.getCodeSizesForAllPIs());
    out << ");" << endl;

    out << "patInstLatencies = array1d(allPatternInstances, ";
    printMinizincValue(out, params.getLatenciesForAllPIs());
    out << ");" << endl;

    out << "patInstNoUseDefDomConstraints = array1d(allPatternInstances, ";
    printMinizincValue(out, params.getNoUseDefDomConstraintsSettingForAllPIs());
    out << ");" << endl;
}

void
generateModelParameters(
    const Params& params,
    ostream& out
) {
    generateModelFunctionParameters(params, out);
    out << endl;
    generateModelTargetMachineParameters(params, out);
    out << endl;
    generateModelPatternInstanceParameters(params, out);
}

void
generateModelConstraintsForF(
    const Params& params,
    ostream& out
) {
    ConstraintProcessor cprocessor;
    const list<const Constraint*>& cs = params.getConstraintsForF();
    for (const Constraint* c : cs) {
        out << cprocessor.processConstraintForF(c) << endl;
    }
}

void
generateModelConstraintsForPIs(
    const Params& params,
    ostream& out
) {
    ConstraintProcessor cprocessor;
    ArrayIndex pi = 0;
    for (const auto& cs : params.getConstraintsForAllPIs()) {
        if (cs.size() > 0) {
            for (const Constraint* c : cs) {
                out << cprocessor.processConstraintForPI(c, pi) << endl;
            }
            out << endl;
        }
        pi++;
    }
}

void outputModelParams(
    const Params& params,
    ostream& out
) {
    stringstream sout;

    sout << "%============" << endl;
    sout << "% PARAMETERS" << endl;
    sout << "%============" << endl;
    sout << endl;
    generateModelParameters(params, sout);
    sout << endl
         << endl
         << endl;

    sout << "%============================" << endl;
    sout << "% FUNCTION GRAPH CONSTRAINTS" << endl;
    sout << "%============================" << endl;
    sout << endl;
    generateModelConstraintsForF(params, sout);
    sout << endl
         << endl
         << endl;

    sout << "%==============================" << endl;
    sout << "% PATTERN INSTANCE CONSTRAINTS" << endl;
    sout << "%==============================" << endl;
    sout << endl;
    generateModelConstraintsForPIs(params, sout);

    out << "% AUTO-GENERATED" << endl;
    out << endl;
    out << sout.str();
}



int
main(int argc, char** argv) {
    if (argc < 2) {
        cerr << "No JSON file." << endl;
        return 1;
    }
    else if (argc > 2) {
        cerr << "Too many arguments." << endl;
        return 1;
    }

    try {
        // Parse JSON file into an internal model parameters object
        string json_file(argv[1]);
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
        outputModelParams(params, cout);

        return 0;
    }
    catch (Exception& ex) {
        cerr << "ERROR: " << ex.toString() << endl;
        return 1;
    }
}