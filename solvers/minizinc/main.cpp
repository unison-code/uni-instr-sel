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

#include "../common/exceptions/exception.h"
#include "../common/model/params.h"
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

        cout << "numFuncActionNodes = "
             << params.getNumFunctionActionNodes()
             << ";" << endl;
        cout << "numFuncEntityNodes = "
             << params.getNumFunctionEntityNodes()
             << ";" << endl;
        cout << "numFuncLabelNodes = "
             << params.getNumFunctionLabelNodes()
             << ";" << endl;
        cout << "numPatternInstances = "
             << params.getNumPatternInstances()
             << ";" << endl;

        cout << "patInstCosts = array1d(0..numPatternInstances-1, [";
        {
            vector<int> costs(params.getNumPatternInstances());
            for (const Id& id : params.getAllPatternInstanceIds()) {
                costs[params.getIndexOfPattern(id)] =
                    params.getCostOfPattern(id);
            }
            bool is_first = true;
            for (auto& cost : costs) {
                if (is_first) is_first = false;
                else cout << ",";
                cout << cost;
            }
        }
        cout << "]);" << endl;

        cout << "patInstActionsCovered = array1d(0..numPatternInstances-1, [";
        {
            vector< list<Id> > node_lists(params.getNumPatternInstances());
            for (const Id& id : params.getAllPatternInstanceIds()) {
                node_lists[params.getIndexOfPattern(id)] =
                    params.getActionNodesCoveredByPattern(id);
            }
            bool is_first1 = true;
            for (auto& nodes : node_lists) {
                if (is_first1) is_first1 = false;
                else cout << ",";
                bool is_first2 = true;
                cout << "{";
                for (auto& id : nodes) {
                    if (is_first2) is_first2 = false;
                    else cout << ",";
                    cout << id;
                }
                cout << "}";
            }
        }
        cout << "]);" << endl;

        cout << "patInstEntitiesDefined = array1d(0..numPatternInstances-1, [";
        {
            vector< list<Id> > node_lists(params.getNumPatternInstances());
            for (const Id& id : params.getAllPatternInstanceIds()) {
                node_lists[params.getIndexOfPattern(id)] =
                    params.getEntityNodesDefinedByPattern(id);
            }
            bool is_first1 = true;
            for (auto& nodes : node_lists) {
                if (is_first1) is_first1 = false;
                else cout << ",";
                bool is_first2 = true;
                cout << "{";
                for (auto& id : nodes) {
                    if (is_first2) is_first2 = false;
                    else cout << ",";
                    cout << id;
                }
                cout << "}";
            }
        }
        cout << "]);" << endl;

        cout << "patInstEntitiesUsed = array1d(0..numPatternInstances-1, [";
        {
            vector< list<Id> > node_lists(params.getNumPatternInstances());
            for (const Id& id : params.getAllPatternInstanceIds()) {
                node_lists[params.getIndexOfPattern(id)] =
                    params.getEntityNodesUsedByPattern(id);
            }
            bool is_first1 = true;
            for (auto& nodes : node_lists) {
                if (is_first1) is_first1 = false;
                else cout << ",";
                bool is_first2 = true;
                cout << "{";
                for (auto& id : nodes) {
                    if (is_first2) is_first2 = false;
                    else cout << ",";
                    cout << id;
                }
                cout << "}";
            }
        }
        cout << "]);" << endl;

        // TODO: output more parameters
    }
    catch (Exception& ex) {
        cerr << "ERROR: " << ex.toString() << endl;
        return 1;
    }

    return 0;
}
