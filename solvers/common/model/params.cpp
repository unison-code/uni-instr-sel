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

using namespace Json;
using namespace Model;
using std::list;
using std::map;
using std::string;

Params::Params(void) {}

Params::~Params(void) {}

size_t
Params::getNumFunctionActionNodes(void) const {
    return func_action_node_mappings_.size();
}

size_t
Params::getNumFunctionEntityNodes(void) const {
    return func_entity_node_mappings_.size();
}

size_t
Params::getNumFunctionLabelNodes(void) const {
    return func_label_node_mappings_.size();
}

size_t
Params::getNumPatternInstances(void) const {
    return pat_matchset_mappings_.size();
}

void
Params::parseJson(const string& str, Params& param) {
    Value root;
    Reader reader;
    if (!reader.parse(str, root)) {
        THROW(Exception, reader.getFormattedErrorMessages());
    }

    computeMappingsForFunctionActionNodes(root, param);
    computeMappingsForFunctionEntityNodes(root, param);
    computeMappingsAndDomsetsForFunctionLabelNodes(root, param);
    computeMatchsetMappingsForPatternInstances(root, param);
}

Value
Params::getValue(const Value& value, const string& name) {
    Value sought_value = value[name];
    if (sought_value.isNull()) {
        THROW(Exception, string("No '") + name + "' field found");
    }
    return sought_value;
}

unsigned int
Params::toId(const Value& value) {
    if (!value.isUInt()) {
        THROW(Exception, "Not a JSON unsigned integer");
    }
    return value.asUInt();
}

void
Params::computeMappingsForFunctionActionNodes(
    const Value& root,
    Params& param
) {
    Value function(getValue(root, "function-data"));
    ArrayIndex index = 0;
    for (Value& node_id : getValue(function, "action-nodes")) {
        addMapping(toId(node_id),
                   index++,
                   param.func_action_node_mappings_);
    }
}

void
Params::computeMappingsForFunctionEntityNodes(
    const Value& root,
    Params& param
) {
    Value function(getValue(root, "function-data"));
    ArrayIndex index = 0;
    for (Value& node_id : getValue(function, "entity-nodes")) {
        addMapping(toId(node_id),
                   index++,
                   param.func_entity_node_mappings_);
    }
}

void
Params::computeMappingsAndDomsetsForFunctionLabelNodes(
    const Value& root,
    Params& param
) {
    Value function(getValue(root, "function-data"));
    ArrayIndex index = 0;
    for (Value& entry : getValue(function, "label-nodes")) {
        Id node_id = toId(getValue(entry, "node"));
        addMapping(node_id,
                   index++,
                   param.func_label_node_mappings_);
        list<Id> domset;
        for (Value& dominator_node : getValue(entry, "domset")) {
            domset.push_back(toId(dominator_node));
        }
        addMapping(node_id,
                   domset,
                   param.func_label_domsets_);
    }
}

void
Params::computeMatchsetMappingsForPatternInstances(
    const Value& root,
    Params& param
) {
    ArrayIndex index = 0;
    for (Value& pattern : getValue(root, "pattern-instance-data")) {
        addMapping(toId(getValue(pattern, "matchset-id")),
                   index++,
                   param.pat_matchset_mappings_);
    }
}
