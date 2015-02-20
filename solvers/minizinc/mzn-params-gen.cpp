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
#include "common/exceptions/exception.h"
#include "common/model/modelparams.h"
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
    const ModelParams& params,
    ostream& out
) {
    out << "% Function data" << endl;

    out << "numOperationsInFunction = "
        << params.getNumOperationNodesInF()
        << ";"
        << endl;
    out << "numEntitiesInFunction = " << params.getNumEntityNodesInF() << ";"
        << endl;
    out << "numLabelsInFunction = " << params.getNumLabelNodesInF() << ";"
        << endl;

    out << "entryLabelOfFunction = " << params.getEntryLabelInF() << ";"
        << endl;

    out << "domSetOfLabelInFunction = array1d(allLabelsInFunction, ";
    printMinizincValue(out, params.getLabelDomSetsInF());
    out << ");" << endl;
    out << "invDomSetOfLabelInFunction = array1d(allLabelsInFunction, ";
    printMinizincValue(out, params.getLabelInvDomSetsInF());
    out << ");" << endl;

    out << "domEdgesFromLabelInFunction = array1d(allLabelsInFunction, ";
    printMinizincValue(out, params.getLabelToEntityDomEdgesInF());
    out << ");" << endl;
    out << "domEdgesToLabelInFunction = array1d(allLabelsInFunction, ";
    printMinizincValue(out, params.getEntityToLabelDomEdgesInF());
    out << ");" << endl;

    out << "essentialOperationsInFunction = ";
    printMinizincValue(out, params.getAllEssentialOpNodesInF());
    out << ";" << endl;

    out << "execFrequencyOfLabelInFunction = array1d(allLabelsInFunction, ";
    printMinizincValue(out, params.getExecFreqOfAllBBsInF());
    out << ");" << endl;
}

void
generateModelTargetMachineParameters(
    const ModelParams& params,
    ostream& out
) {
    out << "% Target machine data" << endl;

    out << "numRegisters = " << params.getNumRegistersInM() << ";"
         << endl;
}

void
generateModelMatchParameters(
    const ModelParams& params,
    ostream& out
) {
    out << "% Match data" << endl;

    out << "numMatches = " << params.getNumMatches() << ";"
         << endl;

    out << "operationsCoveredByMatch = array1d(allMatches, ";
    printMinizincValue(out, params.getOperationNodesCoveredByAllMatches());
    out << ");" << endl;

    out << "entitiesDefinedByMatch = array1d(allMatches, ";
    printMinizincValue(out, params.getEntityNodesDefinedByAllMatches());
    out << ");" << endl;

    out << "entitiesUsedByMatch = array1d(allMatches, ";
    printMinizincValue(out, params.getEntityNodesUsedByAllMatches());
    out << ");" << endl;

    out << "entryLabelOfMatch = array1d(allMatches, ";
    printMinizincValue(out, params.getEntryLabelNodeOfAllMatches());
    out << ");" << endl;

    out << "nonEntryLabelsInMatch = array1d(allMatches, ";
    printMinizincValue(out, params.getNonEntryLabelNodesInAllMatches());
    out << ");" << endl;

    out << "applyDefDomUseConstraintForMatch = array1d(allMatches, ";
    printMinizincValue(out, params.getADDUCSettingForAllMatches());
    out << ");" << endl;

    out << "indexOfMatchLabelMapping = "
         << "array2d(allMatches, allLabelsInFunction, ";
    {
        size_t num_matches = params.getNumMatches();
        size_t num_labels = params.getNumLabelNodesInF();
        vector<int> mappings(num_matches * num_labels, -1);
        int index = 0;
        ArrayIndex pat_index = 0;
        for (const auto& entries : params.getNonEntryLabelNodesInAllMatches()) {
            for (const ArrayIndex& node_index : entries) {
                mappings[pat_index * num_labels + node_index] = index++;
            }
            pat_index++;
        }
        printMinizincValue(out, mappings);
    }
    out << ");" << endl;

    out << "codeSizeOfMatch = array1d(allMatches, ";
    printMinizincValue(out, params.getCodeSizesForAllMatches());
    out << ");" << endl;

    out << "latencyOfMatch = array1d(allMatches, ";
    printMinizincValue(out, params.getLatenciesForAllMatches());
    out << ");" << endl;
}

void
generateModelParameters(
    const ModelParams& params,
    ostream& out
) {
    generateModelFunctionParameters(params, out);
    out << endl;
    generateModelTargetMachineParameters(params, out);
    out << endl;
    generateModelMatchParameters(params, out);
}

void
generateModelConstraintsForF(
    const ModelParams& params,
    ostream& out
) {
    ConstraintProcessor cprocessor;
    const list<const Constraint*>& cs = params.getConstraintsForF();
    for (const Constraint* c : cs) {
        out << cprocessor.processConstraintForF(c) << endl;
    }
}

void
generateModelConstraintsForMatches(
    const ModelParams& params,
    ostream& out
) {
    ConstraintProcessor cprocessor;
    ArrayIndex pi = 0;
    for (const auto& cs : params.getConstraintsForAllMatches()) {
        if (cs.size() > 0) {
            for (const Constraint* c : cs) {
                out << cprocessor.processConstraintForMatch(c, pi) << endl;
            }
            out << endl;
        }
        pi++;
    }
}

void outputModelParams(
    const ModelParams& params,
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
    generateModelConstraintsForMatches(params, sout);

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
        ModelParams params;
        ModelParams::parseJson(json_content, params);

        // Output model params
        outputModelParams(params, cout);

        return 0;
    }
    catch (Exception& ex) {
        cerr << "ERROR: " << ex.toString() << endl;
        return 1;
    }
}
