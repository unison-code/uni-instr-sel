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
using std::string;
using std::stringstream;
using std::vector;

// Forward declaration
template <typename T>
void
printList(const T& l, const string&, const string&);

template <typename T>
void
print(const vector<T>& v, const string& dl = "[", const string& dr = "]") {
    printList(v, dl, dr);
}

template <typename T>
void
print(const list<T>& l, const string& dl = "[", const string& dr = "]") {
    printList(l, dl, dr);
}

template <typename T>
void
print(const T& v, const string&, const string&) {
    cout << v;
}

template <>
void
print(const bool& v, const string&, const string&) {
    cout << (v ? "true" : "false");
}

template <typename T>
void
printList(const T& l, const string& dl = "[", const string& dr = "]") {
    cout << dl;
    bool isFirst = true;
    for (const auto& e : l) {
        if (isFirst) isFirst = false;
        else cout << ",";
        print(e, "{", "}");
    }
    cout << dr;
}

void
outputDebugInfo(const Params& params) {
    cout << "% Action node mappings" << endl;
    for (const Id& id : params.getAllActionNodeIds()) {
        cout << "% ID " << id << " -> index " << params.getIndexOfActionNode(id)
             << endl;
    }
    cout << endl;
    cout << "% Data node mappings" << endl;
    for (const Id& id : params.getAllDataNodeIds()) {
        cout << "% ID " << id << " -> index " << params.getIndexOfDataNode(id)
             << endl;
    }
    cout << endl;
    cout << "% State node mappings" << endl;
    for (const Id& id : params.getAllStateNodeIds()) {
        cout << "% ID " << id << " -> index " << params.getIndexOfStateNode(id)
             << endl;
    }
    cout << endl;
    cout << "% Label node mappings" << endl;
    for (const Id& id : params.getAllLabelNodeIds()) {
        cout << "% ID " << id << " -> index " << params.getIndexOfLabelNode(id)
             << endl;
    }
    cout << endl;
    cout << "% Pattern instance mappings" << endl;
    for (const Id& id : params.getAllInstanceIds()) {
        cout << "% ID " << id << " -> index "
             << params.getIndexOfInstance(id) << endl;
    }
}

void
outputFunctionParameters(const Params& params) {
    cout << "% Function data" << endl;

    cout << "numFuncActionNodes = " << params.getNumActionNodes() << ";"
         << endl;
    cout << "numFuncDataNodes = " << params.getNumDataNodes() << ";"
         << endl;
    cout << "numFuncStateNodes = " << params.getNumStateNodes() << ";"
         << endl;
    cout << "numFuncLabelNodes = " << params.getNumLabelNodes() << ";"
         << endl;

    cout << "rootLabel = " << params.getIndexOfLabelNode(params.getRootLabel())
         << ";"
         << endl;

    cout << "funcLabelDomsets = array1d(allFuncLabelNodes, ";
    {
        size_t num_nodes = params.getNumLabelNodes();
        vector< list<ArrayIndex> > node_lists(num_nodes);
        for (const Id& id : params.getAllLabelNodeIds()) {
            node_lists[params.getIndexOfLabelNode(id)] =
                params.getIndicesOfLabelNodes(params.getDomsetOfLabel(id));
        }
        print(node_lists);
    }
    cout << ");" << endl;
}

void
outputTargetMachineParameters(const Params& params) {
    cout << "% Target machine data" << endl;

    cout << "numRegisters = " << params.getNumRegisters() << ";"
         << endl;
}

void
outputPatternInstanceParameters(const Params& params) {
    cout << "% Pattern instance data" << endl;

    cout << "numPatternInstances = " << params.getNumInstances() << ";"
         << endl;

    cout << "patInstActionsCovered = array1d(allPatternInstances, ";
    {
        size_t num_instances = params.getNumInstances();
        vector< list<ArrayIndex> > node_lists(num_instances);
        for (const Id& id : params.getAllInstanceIds()) {
            const list<Id>& nodes = params.getActionNodesCoveredByInstance(id);
            node_lists[params.getIndexOfInstance(id)] =
                params.getIndicesOfActionNodes(nodes);
        }
        print(node_lists);
    }
    cout << ");" << endl;

    cout << "patInstDataDefined = array1d(allPatternInstances, ";
    {
        size_t num_instances = params.getNumInstances();
        vector< list<ArrayIndex> > node_lists(num_instances);
        for (const Id& id : params.getAllInstanceIds()) {
            const list<Id>& nodes = params.getDataNodesDefinedByInstance(id);
            node_lists[params.getIndexOfInstance(id)] =
                params.getIndicesOfDataNodes(nodes);
        }
        print(node_lists);
    }
    cout << ");" << endl;

    cout << "patInstStateDefined = array1d(allPatternInstances, ";
    {
        size_t num_instances = params.getNumInstances();
        vector< list<ArrayIndex> > node_lists(num_instances);
        for (const Id& id : params.getAllInstanceIds()) {
            const list<Id>& nodes = params.getStateNodesDefinedByInstance(id);
            node_lists[params.getIndexOfInstance(id)] =
                params.getIndicesOfStateNodes(nodes);
        }
        print(node_lists);
    }
    cout << ");" << endl;

    cout << "patInstDataUsed = array1d(allPatternInstances, ";
    {
        size_t num_instances = params.getNumInstances();
        vector< list<ArrayIndex> > node_lists(num_instances);
        for (const Id& id : params.getAllInstanceIds()) {
            const list<Id>& nodes = params.getDataNodesUsedByInstance(id);
            node_lists[params.getIndexOfInstance(id)] =
                params.getIndicesOfDataNodes(nodes);
        }
        print(node_lists);
    }
    cout << ");" << endl;

    cout << "patInstStateUsed = array1d(allPatternInstances, ";
    {
        size_t num_instances = params.getNumInstances();
        vector< list<ArrayIndex> > node_lists(num_instances);
        for (const Id& id : params.getAllInstanceIds()) {
            const list<Id>& nodes = params.getStateNodesUsedByInstance(id);
            node_lists[params.getIndexOfInstance(id)] =
                params.getIndicesOfStateNodes(nodes);
        }
        print(node_lists);
    }
    cout << ");" << endl;

    cout << "patInstAndLabelMappings = "
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
                ArrayIndex pat_index = params.getIndexOfInstance(pat_id);
                ArrayIndex node_index = params.getIndexOfLabelNode(node_id);
                mappings[pat_index * num_labels + node_index] = index++;
            }
        }
        print(mappings);
    }
    cout << ");" << endl;

    cout << "patInstCodeSizes = array1d(allPatternInstances, ";
    {
        vector<int> code_sizes(params.getNumInstances());
        for (const Id& id : params.getAllInstanceIds()) {
            code_sizes[params.getIndexOfInstance(id)] =
                params.getCodeSizeOfInstance(id);
        }
        print(code_sizes);
    }
    cout << ");" << endl;

    cout << "patInstLatencies = array1d(allPatternInstances, ";
    {
        vector<int> latencies(params.getNumInstances());
        for (const Id& id : params.getAllInstanceIds()) {
            latencies[params.getIndexOfInstance(id)] =
                params.getLatencyOfInstance(id);
        }
        print(latencies);
    }
    cout << ");" << endl;

    cout << "patInstNoUseDefDomConstraints = array1d(allPatternInstances, ";
    {
        vector<bool> settings(params.getNumInstances());
        for (const Id& id : params.getAllInstanceIds()) {
            settings[params.getIndexOfInstance(id)] =
                params.getNoUseDefDomConstraintsSettingForInstance(id);
        }
        print(settings);
    }
    cout << ");" << endl;
}

void
outputParameters(const Params& params) {
    outputFunctionParameters(params);
    cout << endl;
    outputTargetMachineParameters(params);
    cout << endl;
    outputPatternInstanceParameters(params);
}

void
outputConstraints(const Params& params) {
    ConstraintProcessor cprocessor(params);
    for (const Id& id : params.getAllInstanceIds()) {
        const list<const Constraint*>& cs = params.getConstraintsOfInstance(id);
        if (cs.size() > 0) {
            cout << "% ID " << id << endl;
            for (const Constraint* c : cs) {
                cout << cprocessor.process(id, c) << endl;
            }
            cout << endl;
        }
    }
}

int
main(int argc, char** argv) {
    try {
        // Parse JSON file into an internal model parameters object
        ifstream file(argv[1]);
        stringstream ss;
        ss << file.rdbuf();
        string json_content(ss.str());
        Params params;
        Params::parseJson(json_content, params);

        cout << "% AUTO-GENERATED" << endl;
        cout << endl;
        cout << "%------------" << endl;
        cout << "% DEBUG INFO" << endl;
        cout << "%------------" << endl;
        cout << endl;
        outputDebugInfo(params);
        cout << endl
             << endl
             << endl;
        cout << "%------------" << endl;
        cout << "% PARAMETERS" << endl;
        cout << "%------------" << endl;
        cout << endl;
        outputParameters(params);
        cout << endl
             << endl
             << endl;
        cout << "%------------------------------" << endl;
        cout << "% PATTERN INSTANCE CONSTRAINTS" << endl;
        cout << "%------------------------------" << endl;
        cout << endl;
        outputConstraints(params);
    }
    catch (Exception& ex) {
        cerr << "ERROR: " << ex.toString() << endl;
        return 1;
    }

    return 0;
}
