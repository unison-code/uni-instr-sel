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

#include "params.h"
#include "../json/json.h"
#include "../exceptions/exception.h"

// TODO: remove
#include <iostream>
using std::cout;
using std::endl;

using namespace Json;
using namespace Model;
using std::runtime_error;
using std::string;
using std::vector;

void
Params::parseJson(const string& str, Params& param) {
    Value root;
    Reader reader;
    if (!reader.parse(str, root)) {
        THROW(Exception, reader.getFormattedErrorMessages());
    }

    // TODO: Count number of function action nodes
    // TODO: Count number of function data nodes
    // TODO: Count number of function label nodes
    // TODO: Count number of function state nodes
    // TODO: Set function label dominator sets

    // Count number of pattern instances and set matchset ID-to-index mappings
    size_t& num_instances(param.pat_num_instances_);
    num_instances = 0;
    size_t matchset_index = 0;
    for (Value& pattern : getValue(root, "patterns")) {
        for (Value& matchset : getValue(pattern, "matchsets")) {
            num_instances++;
            Value matchset_id(getValue(matchset, "matchset-id"));
            if (!matchset_id.isUInt()) {
                THROW(Exception, "Invalid data type");
            }
            addMatchsetMapping(matchset_id.asUInt(), matchset_index++, param);
        }
    }

    // TODO: remove
    //cout << root << endl;
}

Value
Params::getValue(const Value& value, const string& name) {
    Value sought_value = value[name];
    if (sought_value.isNull()) {
        THROW(Exception, string("No '") + name + "' field found");
    }
    return sought_value;
}

void
Params::addMatchsetMapping(size_t id, size_t index, Params& param) {
    // TODO: implement
}
