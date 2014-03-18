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
Params::getNumActionNodes(void) const {
    return func_action_node_mappings_.size();
}

size_t
Params::getNumEntityNodes(void) const {
    return func_entity_node_mappings_.size();
}

size_t
Params::getNumLabelNodes(void) const {
    return func_label_node_mappings_.size();
}

size_t
Params::getNumPatternInstances(void) const {
    return pat_inst_mappings_.size();
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
    computeMappingsForPatterns(root, param);
    setPatternCosts(root, param);
    setActionNodesCoveredByPatterns(root, param);
    setEntityNodesDefinedByPatterns(root, param);
    setEntityNodesUsedByPatterns(root, param);
}

Value
Params::getJsonValue(const Value& value, const string& name) {
    const Value& sought_value = value[name];
    if (sought_value.isNull()) {
        THROW(Exception, string("No '") + name + "' field found");
    }
    return sought_value;
}

Id
Params::toId(const Value& value) {
    if (!value.isUInt()) {
        THROW(Exception, "Not a JSON unsigned integer");
    }
    return value.asUInt();
}

int
Params::toInt(const Value& value) {
    if (!value.isInt()) {
        THROW(Exception, "Not a JSON integer");
    }
    return value.asInt();
}

void
Params::computeMappingsForFunctionActionNodes(
    const Value& root,
    Params& param
) {
    const Value& function = getJsonValue(root, "function-data");
    ArrayIndex index = 0;
    for (Value& node_id : getJsonValue(function, "action-nodes")) {
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
    const Value& function = getJsonValue(root, "function-data");
    ArrayIndex index = 0;
    for (Value& node_id : getJsonValue(function, "entity-nodes")) {
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
    const Value& function =getJsonValue(root, "function-data");
    ArrayIndex index = 0;
    for (Value& entry : getJsonValue(function, "label-nodes")) {
        const Id& node_id = toId(getJsonValue(entry, "node"));
        addMapping(node_id,
                   index++,
                   param.func_label_node_mappings_);
        list<Id> domset;
        for (Value& dominator_node : getJsonValue(entry, "domset")) {
            domset.push_back(toId(dominator_node));
        }
        addMapping(node_id,
                   domset,
                   param.func_label_domsets_);
    }
}

void
Params::computeMappingsForPatterns(
    const Value& root,
    Params& param
) {
    ArrayIndex index = 0;
    for (Value& pattern : getJsonValue(root, "pattern-instance-data")) {
        addMapping(toId(getJsonValue(pattern, "instance-id")),
                   index++,
                   param.pat_inst_mappings_);
    }
}

int
Params::getCostOfPattern(const Id& instance) const {
    return getMappedValue(instance, pat_inst_costs_);
}

void
Params::setPatternCosts(const Json::Value& root, Params& param) {
    for (Value& pattern : getJsonValue(root, "pattern-instance-data")) {
        const Id& instance_id = toId(getJsonValue(pattern, "instance-id"));
        int cost = toInt(getJsonValue(pattern, "cost"));
        addMapping(instance_id,
                   cost,
                   param.pat_inst_costs_);
    }
}

list<Id>
Params::getAllPatternInstanceIds(void) const {
    list<Id> ids;
    for (auto& kv : pat_inst_mappings_) {
        ids.push_back(kv.first);
    }
    return ids;
}

ArrayIndex
Params::getIndexOfPattern(const Id& id) const {
    return getMappedValue(id, pat_inst_mappings_);
}


void
Params::setActionNodesCoveredByPatterns(
    const Json::Value& root,
    Params& param
) {
    for (Value& pattern : getJsonValue(root, "pattern-instance-data")) {
        const Id& instance_id = toId(getJsonValue(pattern, "instance-id"));
        list<Id> covers;
        for (Value& node_id : getJsonValue(pattern, "action-nodes-covered")) {
            covers.push_back(toId(node_id));
        }
        addMapping(instance_id,
                   covers,
                   param.pat_inst_actions_covered_);
    }
}

void
Params::setEntityNodesDefinedByPatterns(
    const Json::Value& root,
    Params& param
) {
    for (Value& pattern : getJsonValue(root, "pattern-instance-data")) {
        const Id& instance_id = toId(getJsonValue(pattern, "instance-id"));
        list<Id> covers;
        for (Value& node_id : getJsonValue(pattern, "entity-nodes-defined")) {
            covers.push_back(toId(node_id));
        }
        addMapping(instance_id,
                   covers,
                   param.pat_inst_entities_defined_);
    }
}

void
Params::setEntityNodesUsedByPatterns(
    const Json::Value& root,
    Params& param
) {
    for (Value& pattern : getJsonValue(root, "pattern-instance-data")) {
        const Id& instance_id = toId(getJsonValue(pattern, "instance-id"));
        list<Id> covers;
        for (Value& node_id : getJsonValue(pattern, "entity-nodes-used")) {
            covers.push_back(toId(node_id));
        }
        addMapping(instance_id,
                   covers,
                   param.pat_inst_entities_used_);
    }
}

list<Id>
Params::getActionNodesCoveredByPattern(const Id& instance) const {
    return getMappedValue(instance, pat_inst_actions_covered_);
}

list<Id>
Params::getEntityNodesDefinedByPattern(const Id& instance) const {
    return getMappedValue(instance, pat_inst_entities_defined_);
}

list<Id>
Params::getEntityNodesUsedByPattern(const Id& instance) const {
    return getMappedValue(instance, pat_inst_entities_used_);
}

list<Id>
Params::getDomsetOfLabel(const Id& id) const {
    return getMappedValue(id, func_label_domsets_);
}

ArrayIndex
Params::getIndexOfActionNode(const Id& id) const {
    return getMappedValue(id, func_action_node_mappings_);
}

ArrayIndex
Params::getIndexOfEntityNode(const Id& id) const {
    return getMappedValue(id, func_entity_node_mappings_);
}

ArrayIndex
Params::getIndexOfLabelNode(const Id& id) const {
    return getMappedValue(id, func_label_node_mappings_);
}

list<Id>
Params::getAllActionNodeIds(void) const {
    list<Id> ids;
    for (auto& kv : func_action_node_mappings_) {
        ids.push_back(kv.first);
    }
    return ids;
}

list<Id>
Params::getAllEntityNodeIds(void) const {
    list<Id> ids;
    for (auto& kv : func_entity_node_mappings_) {
        ids.push_back(kv.first);
    }
    return ids;
}

list<Id>
Params::getAllLabelNodeIds(void) const {
    list<Id> ids;
    for (auto& kv : func_label_node_mappings_) {
        ids.push_back(kv.first);
    }
    return ids;
}
