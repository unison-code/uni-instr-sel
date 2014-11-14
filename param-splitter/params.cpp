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
#include "common/exceptions/exception.h"
#include "common/model/constraintparser.h"
#include "common/utils/string.h"

using namespace Json;
using namespace Model;
using std::list;
using std::map;
using std::string;

DefPlaceEdgeData::DefPlaceEdgeData(const ID& entity, const ID& label)
    : entity(entity), label(label)
{}

Params::Params(void) {}

Params::~Params(void) {
    destroyConstraintsForF();
    destroyConstraintsForMatches();
}

size_t
Params::getNumOperationNodesInF(void) const {
    return func_operation_node_kv_mappings_.size();
}

size_t
Params::getNumDataNodesInF(void) const {
    return func_data_node_kv_mappings_.size();
}

size_t
Params::getNumStateNodesInF(void) const {
    return func_state_node_kv_mappings_.size();
}

size_t
Params::getNumLabelNodesInF(void) const {
    return func_label_node_kv_mappings_.size();
}

size_t
Params::getNumMatches(void) const {
    return match_kv_mappings_.size();
}

size_t
Params::getNumRegistersInM(void) const {
    return mach_reg_kv_mappings_.size();
}

void
Params::parseJson(const string& str, Params& p) {
    Value root;
    Reader reader;
    if (!reader.parse(str, root)) {
        THROW(Exception, reader.getFormattedErrorMessages());
    }

    computeMappingsForOperationNodesInF(root, p);
    computeMappingsForDataNodesInF(root, p);
    computeMappingsForStateNodesInF(root, p);
    computeMappingsAndDomsetsForLabelNodesInF(root, p);
    computeMappingsForMatches(root, p);
    computeMappingsForRegistersInM(root, p);
    setDefPlaceEdgesForF(root, p);
    setRootLabelInF(root, p);
    setConstraintsForF(root, p);
    setCodeSizesForMatches(root, p);
    setLatenciesForMatches(root, p);
    setConstraintsForMatches(root, p);
    setADDUCSettingsForMatches(root, p);
    setOperationNodesCoveredByMatches(root, p);
    setDataNodesDefinedByMatches(root, p);
    setDataNodesUsedByMatches(root, p);
    setStateNodesDefinedByMatches(root, p);
    setStateNodesUsedByMatches(root, p);
    setRootLabelNodeOfMatches(root, p);
    setNonRootLabelNodesInMatches(root, p);
}

bool
Params::hasJsonValue(const Value& value, const string& name) {
    return !value[name].isNull();
}

const Value&
Params::getJsonValue(const Value& value, const string& name) {
    const Value& sought_value = value[name];
    if (sought_value.isNull()) {
        THROW(Exception, string("No '") + name + "' field found");
    }
    return sought_value;
}

ID
Params::toID(const Value& value) {
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

bool
Params::toBool(const Value& value) {
    if (!value.isBool()) {
        THROW(Exception, "Not a JSON Boolean");
    }
    return value.asBool();
}

string
Params::toString(const Value& value) {
    if (!value.isString()) {
        THROW(Exception, "Not a JSON string");
    }
    return value.asString();
}

DefPlaceEdgeData
Params::toDefPlaceEdgeData(const Value& value) {
    if (value.size() != 2) {
        THROW(Exception, "Not a JSON list with 2 elements");
    }
    return DefPlaceEdgeData(toID(value[0]), toID(value[1]));
}

void
Params::computeMappingsForOperationNodesInF(const Value& root, Params& p) {
    const Value& function = getJsonValue(root, "function-data");
    ArrayIndex index = 0;
    for (auto entry : getJsonValue(function, "operation-nodes")) {
        const ID& node_id = toID(entry);
        addMapping(node_id, index, p.func_operation_node_kv_mappings_);
        addMapping(index, node_id, p.func_operation_node_vk_mappings_);
        index++;
    }
}

void
Params::computeMappingsForDataNodesInF(const Value& root, Params& p) {
    const Value& function = getJsonValue(root, "function-data");
    ArrayIndex index = 0;
    for (auto entry : getJsonValue(function, "data-nodes")) {
        const ID& node_id = toID(entry);
        addMapping(node_id, index, p.func_data_node_kv_mappings_);
        addMapping(index, node_id, p.func_data_node_vk_mappings_);
        index++;
    }
}

void
Params::computeMappingsForStateNodesInF(const Value& root, Params& p) {
    const Value& function = getJsonValue(root, "function-data");
    ArrayIndex index = 0;
    for (auto entry : getJsonValue(function, "state-nodes")) {
        const ID& node_id = toID(entry);
        addMapping(node_id, index, p.func_state_node_kv_mappings_);
        addMapping(index, node_id, p.func_state_node_vk_mappings_);
        index++;
    }
}

void
Params::computeMappingsAndDomsetsForLabelNodesInF(
    const Value& root,
    Params& p
) {
    const Value& function = getJsonValue(root, "function-data");
    ArrayIndex index = 0;
    for (auto entry : getJsonValue(function, "label-nodes")) {
        const ID& node_id = toID(getJsonValue(entry, "node"));
        addMapping(node_id, index, p.func_label_node_kv_mappings_);
        addMapping(index, node_id, p.func_label_node_vk_mappings_);
        index++;
        list<ID> domset;
        for (auto dominator_node : getJsonValue(entry, "domset")) {
            domset.push_back(toID(dominator_node));
        }
        addMapping(node_id, domset, p.func_label_domsets_);
    }
}

void
Params::computeMappingsForRegistersInM(const Value& root, Params& p) {
    const Value& machine = getJsonValue(root, "machine-data");
    ArrayIndex index = 0;
    for (auto entry : getJsonValue(machine, "registers")) {
        const ID& reg_id = toID(entry);
        addMapping(reg_id, index, p.mach_reg_kv_mappings_);
        addMapping(index, reg_id, p.mach_reg_vk_mappings_);
        index++;
    }
}

void
Params::computeMappingsForMatches(const Value& root, Params& p) {
    ArrayIndex index = 0;
    for (auto entry : getJsonValue(root, "match-data")) {
        const ID& pi_id = toID(getJsonValue(entry, "match-id"));
        addMapping(pi_id, index, p.match_kv_mappings_);
        addMapping(index, pi_id, p.match_vk_mappings_);
        index++;
    }
}

int
Params::getCodeSizeForMatch(const ID& match) const {
    return getMappedValue(match, match_code_sizes_);
}

int
Params::getLatencyForMatch(const ID& match) const {
    return getMappedValue(match, match_latencies_);
}

void
Params::setCodeSizesForMatches(const Json::Value& root, Params& p) {
    for (auto match : getJsonValue(root, "match-data")) {
        const ID& match_id = toID(getJsonValue(match, "match-id"));
        int code_size = toInt(getJsonValue(match, "code-size"));
        addMapping(match_id, code_size, p.match_code_sizes_);
    }
}

void
Params::setLatenciesForMatches(const Json::Value& root, Params& p) {
    for (auto match : getJsonValue(root, "match-data")) {
        const ID& match_id = toID(getJsonValue(match, "match-id"));
        int latency = toInt(getJsonValue(match, "latency"));
        addMapping(match_id, latency, p.match_latencies_);
    }
}

ArrayIndex
Params::getIndexForRegisterInM(const ID& id) const {
    return getMappedValue(id, mach_reg_kv_mappings_);
}

list<ArrayIndex>
Params::getIndicesForRegistersInM(const list<ID>& ids) const {
    return getMappedValues(ids, mach_reg_kv_mappings_);
}

list<ID>
Params::getIDsForAllRegisterInM(void) const {
    return getAllKeys(mach_reg_kv_mappings_);
}

list<ID>
Params::getIDsForAllMatches(void) const {
    return getAllKeys(match_kv_mappings_);
}

ArrayIndex
Params::getIndexForMatch(const ID& id) const {
    return getMappedValue(id, match_kv_mappings_);
}

list<const Constraint*>
Params::getConstraintsForF(void) const {
    return func_constraints_;
}

list<const Constraint*>
Params::getConstraintsForMatch(const ID& id) const {
    return getMappedValue(id, match_constraints_);
}

void
Params::setOperationNodesCoveredByMatches(const Json::Value& root, Params& p) {
    for (auto match : getJsonValue(root, "match-data")) {
        const ID& match_id = toID(getJsonValue(match, "match-id"));
        list<ID> covers;
        for (auto node_id : getJsonValue(match, "operation-nodes-covered")) {
            covers.push_back(toID(node_id));
        }
        addMapping(match_id, covers, p.match_operations_covered_);
    }
}

void
Params::setDataNodesDefinedByMatches(const Json::Value& root, Params& p) {
    for (auto match : getJsonValue(root, "match-data")) {
        const ID& match_id = toID(getJsonValue(match, "match-id"));
        list<ID> covers;
        for (auto node_id : getJsonValue(match, "data-nodes-defined")) {
            covers.push_back(toID(node_id));
        }
        addMapping(match_id, covers, p.match_data_defined_);
    }
}

void
Params::setStateNodesDefinedByMatches(const Json::Value& root, Params& p) {
    for (auto match : getJsonValue(root, "match-data")) {
        const ID& match_id = toID(getJsonValue(match, "match-id"));
        list<ID> covers;
        for (auto node_id : getJsonValue(match, "state-nodes-defined")) {
            covers.push_back(toID(node_id));
        }
        addMapping(match_id, covers, p.match_states_defined_);
    }
}

void
Params::setDataNodesUsedByMatches(const Json::Value& root, Params& p) {
    for (auto match : getJsonValue(root, "match-data")) {
        const ID& match_id = toID(getJsonValue(match, "match-id"));
        list<ID> covers;
        for (auto node_id : getJsonValue(match, "data-nodes-used")) {
            covers.push_back(toID(node_id));
        }
        addMapping(match_id, covers, p.match_data_used_);
    }
}

void
Params::setStateNodesUsedByMatches(const Json::Value& root, Params& p) {
    for (auto match : getJsonValue(root, "match-data")) {
        const ID& match_id = toID(getJsonValue(match, "match-id"));
        list<ID> covers;
        for (auto node_id : getJsonValue(match, "state-nodes-used")) {
            covers.push_back(toID(node_id));
        }
        addMapping(match_id, covers, p.match_states_used_);
    }
}

void
Params::setNonRootLabelNodesInMatches(const Json::Value& root, Params& p) {
    for (auto match : getJsonValue(root, "match-data")) {
        const ID& match_id = toID(getJsonValue(match, "match-id"));
        list<ID> refs;
        for (auto node_id : getJsonValue(match, "non-root-label-nodes")) {
            refs.push_back(toID(node_id));
        }
        addMapping(match_id, refs, p.match_non_root_labels_);
    }
}

void
Params::setRootLabelNodeOfMatches(const Json::Value& root, Params& p) {
    for (auto match : getJsonValue(root, "match-data")) {
        const ID& match_id = toID(getJsonValue(match, "match-id"));
        if (hasJsonValue(match, "root-label-node")) {
            addMapping(match_id,
                       toID(getJsonValue(match, "root-label-node")),
                       p.match_root_label_);
        }
    }
}

list<ID>
Params::getOperationNodesCoveredByMatch(const ID& match) const {
    return getMappedValue(match, match_operations_covered_);
}

list<ID>
Params::getDataNodesDefinedByMatch(const ID& match) const {
    return getMappedValue(match, match_data_defined_);
}

list<ID>
Params::getStateNodesDefinedByMatch(const ID& match) const {
    return getMappedValue(match, match_states_defined_);
}

list<ID>
Params::getDataNodesUsedByMatch(const ID& match) const {
    return getMappedValue(match, match_data_used_);
}

list<ID>
Params::getStateNodesUsedByMatch(const ID& match) const {
    return getMappedValue(match, match_states_used_);
}

list<ID>
Params::getNonRootLabelNodesInMatch(const ID& match) const {
    return getMappedValue(match, match_non_root_labels_);
}

list<DefPlaceEdgeData>
Params::getDefPlaceEdgesForDataNodesInF(void) const {
    list<DefPlaceEdgeData> edges;
    for (auto& edge : func_def_place_edges_) {
        if (isDataNodeInF(edge.entity)) {
            edges.push_back(edge);
        }
    }
    return edges;
}

list<DefPlaceEdgeData>
Params::getDefPlaceEdgesForStateNodesInF(void) const {
    list<DefPlaceEdgeData> edges;
    for (auto& edge : func_def_place_edges_) {
        if (isStateNodeInF(edge.entity)) {
            edges.push_back(edge);
        }
    }
    return edges;
}

list<ID>
Params::getDomsetForLabelNodeInF(const ID& id) const {
    return getMappedValue(id, func_label_domsets_);
}

ArrayIndex
Params::getIndexForOperationNodeInF(const ID& id) const {
    return getMappedValue(id, func_operation_node_kv_mappings_);
}

ArrayIndex
Params::getIndexForDataNodeInF(const ID& id) const {
    return getMappedValue(id, func_data_node_kv_mappings_);
}

ArrayIndex
Params::getIndexForStateNodeInF(const ID& id) const {
    return getMappedValue(id, func_state_node_kv_mappings_);
}

ArrayIndex
Params::getIndexForLabelNodeInF(const ID& id) const {
    return getMappedValue(id, func_label_node_kv_mappings_);
}

list<ID>
Params::getIDsForAllOperationNodesInF(void) const {
    return getAllKeys(func_operation_node_kv_mappings_);
}

list<ID>
Params::getIDsForAllDataNodesInF(void) const {
    return getAllKeys(func_data_node_kv_mappings_);
}

list<ID>
Params::getIDsForAllStateNodesInF(void) const {
    return getAllKeys(func_state_node_kv_mappings_);
}

list<ID>
Params::getIDsForAllLabelNodesInF(void) const {
    return getAllKeys(func_label_node_kv_mappings_);
}

list<ArrayIndex>
Params::getIndicesForOperationNodesInF(const list<ID>& ids) const {
    return getMappedValues(ids, func_operation_node_kv_mappings_);
}

list<ArrayIndex>
Params::getIndicesForDataNodesInF(const list<ID>& ids) const {
    return getMappedValues(ids, func_data_node_kv_mappings_);
}

list<ArrayIndex>
Params::getIndicesForStateNodesInF(const list<ID>& ids) const {
    return getMappedValues(ids, func_state_node_kv_mappings_);
}

list<ArrayIndex>
Params::getIndicesForLabelNodesInF(const list<ID>& ids) const {
    return getMappedValues(ids, func_label_node_kv_mappings_);
}

void
Params::destroyConstraintsForF(void) {
    for (auto& c : func_constraints_) {
        delete c;
    }
}

void
Params::destroyConstraintsForMatches(void) {
    for (auto& kv : match_constraints_) {
        for (auto c : kv.second) {
            delete c;
        }
    }
}

void
Params::setConstraintsForF(const Value& root, Params& p) {
    const Value& function = getJsonValue(root, "function-data");
    for (auto expr : getJsonValue(function, "constraints")) {
        ConstraintParser parser;
        Constraint* c = parser.parseConstraint(toString(expr));
        p.func_constraints_.push_back(c);
    }
}

void
Params::setConstraintsForMatches(const Value& root, Params& p) {
    for (auto match : getJsonValue(root, "match-data")) {
        const ID& match_id = toID(getJsonValue(match, "match-id"));
        list<const Constraint*> constraints;
        ConstraintParser parser;
        for (auto expr : getJsonValue(match, "constraints")) {
            Constraint* c = parser.parseConstraint(toString(expr));
            constraints.push_back(c);
        }
        addMapping(match_id, constraints, p.match_constraints_);
    }
}

bool
Params::isOperationNodeInF(const ID& id) const {
    for (const ID& c_id : getIDsForAllOperationNodesInF()) {
        if (c_id == id) return true;
    }
    return false;
}

bool
Params::isDataNodeInF(const ID& id) const {
    for (const ID& c_id : getIDsForAllDataNodesInF()) {
        if (c_id == id) return true;
    }
    return false;
}

bool
Params::isStateNodeInF(const ID& id) const {
    for (const ID& c_id : getIDsForAllStateNodesInF()) {
        if (c_id == id) return true;
    }
    return false;
}

bool
Params::isLabelNodeInF(const ID& id) const {
    for (const ID& c_id : getIDsForAllLabelNodesInF()) {
        if (c_id == id) return true;
    }
    return false;
}

bool
Params::getADDUCSettingForMatch(const ID& match) const {
    return getMappedValue(match, match_apply_def_dom_use_constraint_);
}

void
Params::setADDUCSettingsForMatches(
    const Json::Value& root,
    Params& p
) {
    for (auto match : getJsonValue(root, "match-data")) {
        const ID& match_id = toID(getJsonValue(match, "match-id"));
        const string field_name("apply-def-dom-use-constraint");
        addMapping(match_id,
                   toBool(getJsonValue(match, field_name)),
                   p.match_apply_def_dom_use_constraint_);
    }
}

void
Params::setRootLabelInF(const Json::Value& root, Params& p) {
    const Value& function = getJsonValue(root, "function-data");
    p.func_root_label_ = toID(getJsonValue(function, "root-label"));
}

void
Params::setDefPlaceEdgesForF(const Json::Value& root, Params& p) {
    const Value& function = getJsonValue(root, "function-data");
    for (auto e : getJsonValue(function, "def-place-edges")) {
        p.func_def_place_edges_.push_back(toDefPlaceEdgeData(e));
    }
}

ID
Params::getRootLabelInF(void) const {
    return func_root_label_;
}

ID
Params::getIDOfOperationNodeInF(const ArrayIndex& i) const {
    return getMappedValue(i, func_operation_node_vk_mappings_);
}

ID
Params::getIDOfDataNodeInF(const ArrayIndex& i) const {
    return getMappedValue(i, func_data_node_vk_mappings_);
}

ID
Params::getIDOfStateNodeInF(const ArrayIndex& i) const {
    return getMappedValue(i, func_state_node_vk_mappings_);
}

ID
Params::getIDOfLabelNodeInF(const ArrayIndex& i) const {
    return getMappedValue(i, func_label_node_vk_mappings_);
}

ID
Params::getIDOfRegisterInM(const ArrayIndex& i) const {
    return getMappedValue(i, mach_reg_vk_mappings_);
}

ID
Params::getIDOfMatch(const ArrayIndex& i) const {
    return getMappedValue(i, match_vk_mappings_);
}

list<ID>
Params::getIDsOfOperationNodesInF(const list<ArrayIndex>& is) const {
    return getMappedValues(is, func_operation_node_vk_mappings_);
}

list<ID>
Params::getIDsOfDataNodesInF(const list<ArrayIndex>& is) const {
    return getMappedValues(is, func_data_node_vk_mappings_);
}

list<ID>
Params::getIDsOfStateNodesInF(const list<ArrayIndex>& is) const {
    return getMappedValues(is, func_state_node_vk_mappings_);
}

list<ID>
Params::getIDsOfLabelNodesInF(const list<ArrayIndex>& is) const {
    return getMappedValues(is, func_label_node_vk_mappings_);
}

list<ID>
Params::getIDsOfRegistersInM(const list<ArrayIndex>& is) const {
    return getMappedValues(is, mach_reg_vk_mappings_);
}

list<ID>
Params::getIDsOfMatches(const list<ArrayIndex>& is) const {
    return getMappedValues(is, match_vk_mappings_);
}

bool
Params::hasMatchRootLabel(const ID& match) const {
    try {
        getMappedValue(match, match_root_label_);
        return true;
    }
    catch (Exception&) {
        return false;
    }
}

Model::ID
Params::getRootLabelOfMatch(const ID& match) const {
    return getMappedValue(match, match_root_label_);
}
