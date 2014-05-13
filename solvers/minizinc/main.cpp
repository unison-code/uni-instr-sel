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
using std::ostream;
using std::string;
using std::stringstream;
using std::vector;

// Forward declaration
template <typename T>
void
printList(ostream&, const T&, const string&, const string&);

template <typename T>
void
print(
    ostream& out,
    const vector<T>& v,
    const string& dl = "[",
    const string& dr = "]"
) {
    printList(out, v, dl, dr);
}

template <typename T>
void
print(
    ostream& out,
    const list<T>& l,
    const string& dl = "[",
    const string& dr = "]"
) {
    printList(out, l, dl, dr);
}

template <typename T>
void
print(
    ostream& out,
    const T& v,
    const string&,
    const string&
) {
    out << v;
}

template <>
void
print(
    ostream& out,
    const bool& v,
    const string&,
    const string&
) {
    out << (v ? "true" : "false");
}

template <typename T>
void
printList(
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
        print(out, e, "{", "}");
    }
    out << dr;
}

void
outputFunctionParameters(
    const Params& params,
    ostream& out,
    ostream& debug
) {
    debug << "% Action node mappings" << endl;
    for (const Id& id : params.getAllActionNodeIds()) {
        debug << "% ID " << id << " -> index "
              << params.getIndexOfActionNode(id)
              << endl;
    }
    debug << endl;
    debug << "% Data node mappings" << endl;
    for (const Id& id : params.getAllDataNodeIds()) {
        debug << "% ID " << id << " -> index " << params.getIndexOfDataNode(id)
              << endl;
    }
    debug << endl;
    debug << "% State node mappings" << endl;
    for (const Id& id : params.getAllStateNodeIds()) {
        debug << "% ID " << id << " -> index " << params.getIndexOfStateNode(id)
              << endl;
    }
    debug << endl;
    debug << "% Label node mappings" << endl;
    for (const Id& id : params.getAllLabelNodeIds()) {
        debug << "% ID " << id << " -> index " << params.getIndexOfLabelNode(id)
              << endl;
    }
    debug << endl;

    out << "% Function data" << endl;

    out << "numFuncActionNodes = " << params.getNumActionNodes() << ";"
         << endl;
    out << "numFuncDataNodes = " << params.getNumDataNodes() << ";"
         << endl;
    out << "numFuncStateNodes = " << params.getNumStateNodes() << ";"
         << endl;
    out << "numFuncLabelNodes = " << params.getNumLabelNodes() << ";"
         << endl;

    out << "rootLabel = " << params.getIndexOfLabelNode(params.getRootLabel())
         << ";"
         << endl;

    out << "funcLabelDomsets = array1d(allFuncLabelNodes, ";
    {
        size_t num_nodes = params.getNumLabelNodes();
        vector< list<ArrayIndex> > node_lists(num_nodes);
        for (const Id& id : params.getAllLabelNodeIds()) {
            node_lists[params.getIndexOfLabelNode(id)] =
                params.getIndicesOfLabelNodes(params.getDomsetOfLabel(id));
        }
        print(out, node_lists);
    }
    out << ");" << endl;
}

void
outputTargetMachineParameters(
    const Params& params,
    ostream& out,
    ostream& debug
) {
    out << "% Target machine data" << endl;

    out << "numRegisters = " << params.getNumRegisters() << ";"
         << endl;
}

void
outputPatternInstanceParameters(
    const Params& params,
    ostream& out,
    ostream& debug
) {
    debug << "% Pattern instance mappings" << endl;
    for (const Id& id : params.getAllInstanceIds()) {
        debug << "% ID " << id << " -> index "
              << params.getIndexOfInstance(id) << endl;
    }
    debug << endl;

    out << "% Pattern instance data" << endl;

    out << "numPatternInstances = " << params.getNumInstances() << ";"
         << endl;

    out << "patInstActionsCovered = array1d(allPatternInstances, ";
    {
        size_t num_instances = params.getNumInstances();
        vector< list<ArrayIndex> > node_lists(num_instances);
        for (const Id& id : params.getAllInstanceIds()) {
            const list<Id>& nodes = params.getActionNodesCoveredByInstance(id);
            node_lists[params.getIndexOfInstance(id)] =
                params.getIndicesOfActionNodes(nodes);
        }
        print(out, node_lists);
    }
    out << ");" << endl;

    out << "patInstDataDefined = array1d(allPatternInstances, ";
    {
        size_t num_instances = params.getNumInstances();
        vector< list<ArrayIndex> > node_lists(num_instances);
        for (const Id& id : params.getAllInstanceIds()) {
            const list<Id>& nodes = params.getDataNodesDefinedByInstance(id);
            node_lists[params.getIndexOfInstance(id)] =
                params.getIndicesOfDataNodes(nodes);
        }
        print(out, node_lists);
    }
    out << ");" << endl;

    out << "patInstStateDefined = array1d(allPatternInstances, ";
    {
        size_t num_instances = params.getNumInstances();
        vector< list<ArrayIndex> > node_lists(num_instances);
        for (const Id& id : params.getAllInstanceIds()) {
            const list<Id>& nodes = params.getStateNodesDefinedByInstance(id);
            node_lists[params.getIndexOfInstance(id)] =
                params.getIndicesOfStateNodes(nodes);
        }
        print(out, node_lists);
    }
    out << ");" << endl;

    out << "patInstDataUsed = array1d(allPatternInstances, ";
    {
        size_t num_instances = params.getNumInstances();
        vector< list<ArrayIndex> > node_lists(num_instances);
        for (const Id& id : params.getAllInstanceIds()) {
            const list<Id>& nodes = params.getDataNodesUsedByInstance(id);
            node_lists[params.getIndexOfInstance(id)] =
                params.getIndicesOfDataNodes(nodes);
        }
        print(out, node_lists);
    }
    out << ");" << endl;

    out << "patInstStateUsed = array1d(allPatternInstances, ";
    {
        size_t num_instances = params.getNumInstances();
        vector< list<ArrayIndex> > node_lists(num_instances);
        for (const Id& id : params.getAllInstanceIds()) {
            const list<Id>& nodes = params.getStateNodesUsedByInstance(id);
            node_lists[params.getIndexOfInstance(id)] =
                params.getIndicesOfStateNodes(nodes);
        }
        print(out, node_lists);
    }
    out << ");" << endl;

    debug << "% Pattern instance-label mappings" << endl;
    out << "patInstAndLabelMappings = "
         << "array2d(allPatternInstances, allFuncLabelNodes, ";
    {
        size_t num_instances = params.getNumInstances();
        size_t num_labels = params.getNumLabelNodes();
        vector<int> mappings(num_instances * num_labels, -1);
        int index = 0;
        for (const Id& pat_id : params.getAllInstanceIds()) {
            for (const Id& node_id
                 : params.getLabelNodesReferredByInstance(pat_id))
            {
                debug << "% Instance ID " << pat_id << " + "
                      << "label ID " << node_id << " -> index "
                      << index << endl;
                ArrayIndex pat_index = params.getIndexOfInstance(pat_id);
                ArrayIndex node_index = params.getIndexOfLabelNode(node_id);
                mappings[pat_index * num_labels + node_index] = index++;
            }
        }
        print(out, mappings);
    }
    out << ");" << endl;
    debug << endl;

    out << "patInstCodeSizes = array1d(allPatternInstances, ";
    {
        vector<int> code_sizes(params.getNumInstances());
        for (const Id& id : params.getAllInstanceIds()) {
            code_sizes[params.getIndexOfInstance(id)] =
                params.getCodeSizeOfInstance(id);
        }
        print(out, code_sizes);
    }
    out << ");" << endl;

    out << "patInstLatencies = array1d(allPatternInstances, ";
    {
        vector<int> latencies(params.getNumInstances());
        for (const Id& id : params.getAllInstanceIds()) {
            latencies[params.getIndexOfInstance(id)] =
                params.getLatencyOfInstance(id);
        }
        print(out, latencies);
    }
    out << ");" << endl;

    out << "patInstNoUseDefDomConstraints = array1d(allPatternInstances, ";
    {
        vector<bool> settings(params.getNumInstances());
        for (const Id& id : params.getAllInstanceIds()) {
            settings[params.getIndexOfInstance(id)] =
                params.getNoUseDefDomConstraintsSettingForInstance(id);
        }
        print(out, settings);
    }
    out << ");" << endl;
}

void
outputParameters(
    const Params& params,
    ostream& out,
    ostream& debug
) {
    outputFunctionParameters(params, out, debug);
    out << endl;
    outputTargetMachineParameters(params, out, debug);
    out << endl;
    outputPatternInstanceParameters(params, out, debug);
}

void
outputConstraints(
    const Params& params,
    ostream& out,
    ostream& debug
) {
    ConstraintProcessor cprocessor(params);
    for (const Id& id : params.getAllInstanceIds()) {
        const list<const Constraint*>& cs = params.getConstraintsOfInstance(id);
        if (cs.size() > 0) {
            out << "% ID " << id << endl;
            for (const Constraint* c : cs) {
                out << cprocessor.process(id, c) << endl;
            }
            out << endl;
        }
    }
}

int
main(int argc, char** argv) {
    // Check input arguments
    if (argc < 2) {
        cerr << "ERROR: no JSON input file"
             << endl;
        return 1;
    }
    else if (argc > 2) {
        cerr << "ERROR: too many input arguments"
             << endl;
        return 1;
    }

    try {
        // Parse JSON file into an internal model parameters object
        string json_file(argv[1]);
        ifstream file(json_file);
        if (!file.good()) {
            cerr << "ERROR: " << json_file << " does not exist or is unreadable"
                 << endl;
            return 1;
        }
        stringstream ss;
        ss << file.rdbuf();
        string json_content(ss.str());
        Params params;
        Params::parseJson(json_content, params);

        stringstream sout;
        stringstream sdebug;
        sdebug << "%------------" << endl;
        sdebug << "% DEBUG INFO" << endl;
        sdebug << "%------------" << endl;
        sdebug << endl;

        sout << "%------------" << endl;
        sout << "% PARAMETERS" << endl;
        sout << "%------------" << endl;
        sout << endl;
        outputParameters(params, sout, sdebug);
        sout << endl
             << endl
             << endl;
        sout << "%------------------------------" << endl;
        sout << "% PATTERN INSTANCE CONSTRAINTS" << endl;
        sout << "%------------------------------" << endl;
        sout << endl;
        outputConstraints(params, sout, sdebug);

        cout << "% AUTO-GENERATED" << endl;
        cout << endl;
        cout << sdebug.str();
        cout << endl
             << endl;
        cout << sout.str();
    }
    catch (Exception& ex) {
        cerr << "ERROR: " << ex.toString() << endl;
        return 1;
    }

    return 0;
}
