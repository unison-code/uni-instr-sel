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
        Model::Params params;
        Model::Params::parseJson(json_content, params);

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
        cout << "patInstCosts = [";
        {
            vector<int> costs(params.getNumPatternInstances());
            for (const Model::Id& id : params.getPatternInstanceIds()) {
                costs[params.getIndexOfPatternInstance(id)] =
                    params.getPatternInstanceCost(id);
            }
            for (size_t i = 0; i < costs.size(); i++) {
                if (i > 0) cout << ",";
                cout << costs[i];
            }
        }
        cout << "];" << endl;

        // TODO: output more parameters
    }
    catch (Exception& ex) {
        cerr << "ERROR: " << ex.toString() << endl;
        return 1;
    }

    return 0;
}
