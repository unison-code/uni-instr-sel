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

#include "preparams.h"
#include "../../../common/exceptions/exception.h"
#include "../../../common/model/constraintparser.h"
#include "../../../common/utils/string.h"

using namespace Json;
using namespace Model;
using std::list;
using std::map;
using std::string;

Preparams::Preparams(void) {}

Preparams::~Preparams(void) {
    destroyConstraintsForF();
    destroyConstraintsForPIs();
}

size_t
Preparams::getNumActionNodesInF(void) const {
    return func_action_node_kv_mappings_.size();
}

size_t
Preparams::getNumDataNodesInF(void) const {
    return func_data_node_kv_mappings_.size();
}

size_t
Preparams::getNumStateNodesInF(void) const {
    return func_state_node_kv_mappings_.size();
}

size_t
Preparams::getNumLabelNodesInF(void) const {
    return func_label_node_kv_mappings_.size();
}

size_t
Preparams::getNumPIs(void) const {
    return pat_inst_kv_mappings_.size();
}

size_t
Preparams::getNumRegistersInM(void) const {
    return mach_reg_kv_mappings_.size();
}

void
Preparams::parseJson(const string& str, Preparams& p) {
    Value root;
    Reader reader;
    if (!reader.parse(str, root)) {
        THROW(Exception, reader.getFormattedErrorMessages());
    }
    parseJson(root, p);
}

void
Preparams::parseJson(const Value& root, Preparams& p) {
    computeMappingsForActionNodesInF(root, p);
    computeMappingsForDataNodesInF(root, p);
    computeMappingsForStateNodesInF(root, p);
    computeMappingsAndDomsetsForLabelNodesInF(root, p);
    computeMappingsForPIs(root, p);
    computeMappingsForRegistersInM(root, p);
    setRootLabelInF(root, p);
    setConstraintsForF(root, p);
    setCodeSizesForPIs(root, p);
    setLatenciesForPIs(root, p);
    setConstraintsForPIs(root, p);
    setNoUseDefDomConstraintsSettingsForPIs(root, p);
    setActionNodesCoveredByPIs(root, p);
    setDataNodesDefinedByPIs(root, p);
    setDataNodesUsedByPIs(root, p);
    setStateNodesDefinedByPIs(root, p);
    setStateNodesUsedByPIs(root, p);
    setLabelNodesReferredByPIs(root, p);
}

bool
Preparams::hasJsonValue(const Value& value, const string& name) {
    return !value[name].isNull();
}

const Value&
Preparams::getJsonValue(const Value& value, const string& name) {
    const Value& sought_value = value[name];
    if (sought_value.isNull()) {
        THROW(Exception, string("No '") + name + "' field found");
    }
    return sought_value;
}

ID
Preparams::toID(const Value& value) {
    if (!value.isUInt()) {
        THROW(Exception, "Not a JSON unsigned integer");
    }
    return value.asUInt();
}

int
Preparams::toInt(const Value& value) {
    if (!value.isInt()) {
        THROW(Exception, "Not a JSON integer");
    }
    return value.asInt();
}

bool
Preparams::toBool(const Value& value) {
    if (!value.isBool()) {
        THROW(Exception, "Not a JSON Boolean");
    }
    return value.asBool();
}

string
Preparams::toString(const Value& value) {
    if (!value.isString()) {
        THROW(Exception, "Not a JSON string");
    }
    return value.asString();
}

void
Preparams::computeMappingsForActionNodesInF(const Value& root, Preparams& p) {
    const Value& function = getJsonValue(root, "function-data");
    ArrayIndex index = 0;
    for (auto entry : getJsonValue(function, "action-nodes")) {
        const ID& node_id = toID(entry);
        addMapping(node_id, index, p.func_action_node_kv_mappings_);
        addMapping(index, node_id, p.func_action_node_vk_mappings_);
        index++;
    }
}

void
Preparams::computeMappingsForDataNodesInF(const Value& root, Preparams& p) {
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
Preparams::computeMappingsForStateNodesInF(const Value& root, Preparams& p) {
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
Preparams::computeMappingsAndDomsetsForLabelNodesInF(
    const Value& root,
    Preparams& p
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
Preparams::computeMappingsForRegistersInM(const Value& root, Preparams& p) {
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
Preparams::computeMappingsForPIs(const Value& root, Preparams& p) {
    ArrayIndex index = 0;
    for (auto entry : getJsonValue(root, "pattern-instance-data")) {
        const ID& pi_id = toID(getJsonValue(entry, "pattern-instance-id"));
        addMapping(pi_id, index, p.pat_inst_kv_mappings_);
        addMapping(index, pi_id, p.pat_inst_vk_mappings_);
        index++;
    }
}

int
Preparams::getCodeSizeForPI(const ID& instance) const {
    return getMappedValue(instance, pat_inst_code_sizes_);
}

int
Preparams::getLatencyForPI(const ID& instance) const {
    return getMappedValue(instance, pat_inst_latencies_);
}

void
Preparams::setCodeSizesForPIs(const Json::Value& root, Preparams& p) {
    for (auto instance : getJsonValue(root, "pattern-instance-data")) {
        const ID& instance_id = toID(getJsonValue(instance,
                                                  "pattern-instance-id"));
        int code_size = toInt(getJsonValue(instance, "code-size"));
        addMapping(instance_id, code_size, p.pat_inst_code_sizes_);
    }
}

void
Preparams::setLatenciesForPIs(const Json::Value& root, Preparams& p) {
    for (auto instance : getJsonValue(root, "pattern-instance-data")) {
        const ID& instance_id = toID(getJsonValue(instance,
                                                  "pattern-instance-id"));
        int latency = toInt(getJsonValue(instance, "latency"));
        addMapping(instance_id, latency, p.pat_inst_latencies_);
    }
}

ArrayIndex
Preparams::getIndexForRegisterInM(const ID& id) const {
    return getMappedValue(id, mach_reg_kv_mappings_);
}

list<ArrayIndex>
Preparams::getIndicesForRegistersInM(const list<ID>& ids) const {
    return getMappedValues(ids, mach_reg_kv_mappings_);
}

list<ID>
Preparams::getIDsForAllRegisterInM(void) const {
    return getAllKeys(mach_reg_kv_mappings_);
}

list<ID>
Preparams::getIDsForAllPIs(void) const {
    return getAllKeys(pat_inst_kv_mappings_);
}

ArrayIndex
Preparams::getIndexForPI(const ID& id) const {
    return getMappedValue(id, pat_inst_kv_mappings_);
}

list<const Constraint*>
Preparams::getConstraintsForF(void) const {
    return func_constraints_;
}

list<const Constraint*>
Preparams::getConstraintsForPI(const ID& id) const {
    return getMappedValue(id, pat_inst_constraints_);
}

void
Preparams::setActionNodesCoveredByPIs(const Json::Value& root, Preparams& p) {
    for (auto instance : getJsonValue(root, "pattern-instance-data")) {
        const ID& instance_id = toID(getJsonValue(instance,
                                                  "pattern-instance-id"));
        list<ID> covers;
        for (auto node_id : getJsonValue(instance, "action-nodes-covered")) {
            covers.push_back(toID(node_id));
        }
        addMapping(instance_id, covers, p.pat_inst_actions_covered_);
    }
}

void
Preparams::setDataNodesDefinedByPIs(const Json::Value& root, Preparams& p) {
    for (auto instance : getJsonValue(root, "pattern-instance-data")) {
        const ID& instance_id = toID(getJsonValue(instance,
                                                  "pattern-instance-id"));
        list<ID> covers;
        for (auto node_id : getJsonValue(instance, "data-nodes-defined")) {
            covers.push_back(toID(node_id));
        }
        addMapping(instance_id, covers, p.pat_inst_data_defined_);
    }
}

void
Preparams::setStateNodesDefinedByPIs(const Json::Value& root, Preparams& p) {
    for (auto instance : getJsonValue(root, "pattern-instance-data")) {
        const ID& instance_id = toID(getJsonValue(instance,
                                                  "pattern-instance-id"));
        list<ID> covers;
        for (auto node_id : getJsonValue(instance, "state-nodes-defined")) {
            covers.push_back(toID(node_id));
        }
        addMapping(instance_id, covers, p.pat_inst_states_defined_);
    }
}

void
Preparams::setDataNodesUsedByPIs(const Json::Value& root, Preparams& p) {
    for (auto instance : getJsonValue(root, "pattern-instance-data")) {
        const ID& instance_id = toID(getJsonValue(instance,
                                                  "pattern-instance-id"));
        list<ID> covers;
        for (auto node_id : getJsonValue(instance, "data-nodes-used")) {
            covers.push_back(toID(node_id));
        }
        addMapping(instance_id, covers, p.pat_inst_data_used_);
    }
}

void
Preparams::setStateNodesUsedByPIs(const Json::Value& root, Preparams& p) {
    for (auto instance : getJsonValue(root, "pattern-instance-data")) {
        const ID& instance_id = toID(getJsonValue(instance,
                                                  "pattern-instance-id"));
        list<ID> covers;
        for (auto node_id : getJsonValue(instance, "state-nodes-used")) {
            covers.push_back(toID(node_id));
        }
        addMapping(instance_id, covers, p.pat_inst_states_used_);
    }
}

void
Preparams::setLabelNodesReferredByPIs(const Json::Value& root, Preparams& p) {
    for (auto instance : getJsonValue(root, "pattern-instance-data")) {
        const ID& instance_id = toID(getJsonValue(instance,
                                                  "pattern-instance-id"));
        list<ID> refs;
        for (auto node_id : getJsonValue(instance, "label-nodes-referred")) {
            refs.push_back(toID(node_id));
        }
        addMapping(instance_id, refs, p.pat_inst_labels_referred_);
    }
}

list<ID>
Preparams::getActionNodesCoveredByPI(const ID& instance) const {
    return getMappedValue(instance, pat_inst_actions_covered_);
}

list<ID>
Preparams::getDataNodesDefinedByPI(const ID& instance) const {
    return getMappedValue(instance, pat_inst_data_defined_);
}

list<ID>
Preparams::getStateNodesDefinedByPI(const ID& instance) const {
    return getMappedValue(instance, pat_inst_states_defined_);
}

list<ID>
Preparams::getDataNodesUsedByPI(const ID& instance) const {
    return getMappedValue(instance, pat_inst_data_used_);
}

list<ID>
Preparams::getStateNodesUsedByPI(const ID& instance) const {
    return getMappedValue(instance, pat_inst_states_used_);
}

list<ID>
Preparams::getLabelNodesReferredByPI(const ID& instance) const {
    return getMappedValue(instance, pat_inst_labels_referred_);
}

list<ID>
Preparams::getDomsetForLabelNodeInF(const ID& id) const {
    return getMappedValue(id, func_label_domsets_);
}

ArrayIndex
Preparams::getIndexForActionNodeInF(const ID& id) const {
    return getMappedValue(id, func_action_node_kv_mappings_);
}

ArrayIndex
Preparams::getIndexForDataNodeInF(const ID& id) const {
    return getMappedValue(id, func_data_node_kv_mappings_);
}

ArrayIndex
Preparams::getIndexForStateNodeInF(const ID& id) const {
    return getMappedValue(id, func_state_node_kv_mappings_);
}

ArrayIndex
Preparams::getIndexForLabelNodeInF(const ID& id) const {
    return getMappedValue(id, func_label_node_kv_mappings_);
}

list<ID>
Preparams::getIDsForAllActionNodesInF(void) const {
    return getAllKeys(func_action_node_kv_mappings_);
}

list<ID>
Preparams::getIDsForAllDataNodesInF(void) const {
    return getAllKeys(func_data_node_kv_mappings_);
}

list<ID>
Preparams::getIDsForAllStateNodesInF(void) const {
    return getAllKeys(func_state_node_kv_mappings_);
}

list<ID>
Preparams::getIDsForAllLabelNodesInF(void) const {
    return getAllKeys(func_label_node_kv_mappings_);
}

list<ArrayIndex>
Preparams::getIndicesForActionNodesInF(const list<ID>& ids) const {
    return getMappedValues(ids, func_action_node_kv_mappings_);
}

list<ArrayIndex>
Preparams::getIndicesForDataNodesInF(const list<ID>& ids) const {
    return getMappedValues(ids, func_data_node_kv_mappings_);
}

list<ArrayIndex>
Preparams::getIndicesForStateNodesInF(const list<ID>& ids) const {
    return getMappedValues(ids, func_state_node_kv_mappings_);
}

list<ArrayIndex>
Preparams::getIndicesForLabelNodesInF(const list<ID>& ids) const {
    return getMappedValues(ids, func_label_node_kv_mappings_);
}

void
Preparams::destroyConstraintsForF(void) {
    for (auto& c : func_constraints_) {
        delete c;
    }
}

void
Preparams::destroyConstraintsForPIs(void) {
    for (auto& kv : pat_inst_constraints_) {
        for (auto c : kv.second) {
            delete c;
        }
    }
}

void
Preparams::setConstraintsForF(const Value& root, Preparams& p) {
    const Value& function = getJsonValue(root, "function-data");
    for (auto expr : getJsonValue(function, "constraints")) {
        ConstraintParser parser;
        Constraint* c = parser.parseConstraint(toString(expr));
        p.func_constraints_.push_back(c);
    }
}

void
Preparams::setConstraintsForPIs(const Value& root, Preparams& p) {
    for (auto instance : getJsonValue(root, "pattern-instance-data")) {
        const ID& instance_id = toID(getJsonValue(instance,
                                                  "pattern-instance-id"));
        list<const Constraint*> constraints;
        ConstraintParser parser;
        for (auto expr : getJsonValue(instance, "constraints")) {
            Constraint* c = parser.parseConstraint(toString(expr));
            constraints.push_back(c);
        }
        addMapping(instance_id, constraints, p.pat_inst_constraints_);
    }
}

bool
Preparams::isActionNodeInF(const ID& id) const {
    for (const ID& c_id : getIDsForAllActionNodesInF()) {
        if (c_id == id) return true;
    }
    return false;
}

bool
Preparams::isDataNodeInF(const ID& id) const {
    for (const ID& c_id : getIDsForAllDataNodesInF()) {
        if (c_id == id) return true;
    }
    return false;
}

bool
Preparams::isStateNodeInF(const ID& id) const {
    for (const ID& c_id : getIDsForAllStateNodesInF()) {
        if (c_id == id) return true;
    }
    return false;
}

bool
Preparams::isLabelNodeInF(const ID& id) const {
    for (const ID& c_id : getIDsForAllLabelNodesInF()) {
        if (c_id == id) return true;
    }
    return false;
}

bool
Preparams::getNoUseDefDomConstraintsSettingForPI(const ID& instance) const {
    return getMappedValue(instance, pat_inst_no_use_def_dom_constraints_);
}

void
Preparams::setNoUseDefDomConstraintsSettingsForPIs(
    const Json::Value& root,
    Preparams& p
) {
    for (auto instance : getJsonValue(root, "pattern-instance-data")) {
        const ID& instance_id = toID(getJsonValue(instance,
                                                  "pattern-instance-id"));
        const string field_name("no-use-def-dom-constraints");
        bool setting = hasJsonValue(instance, field_name)
                       ? toBool(getJsonValue(instance, field_name))
                       : false;
        addMapping(instance_id,
                   setting,
                   p.pat_inst_no_use_def_dom_constraints_);
    }
}

void
Preparams::setRootLabelInF(const Json::Value& root, Preparams& p) {
    const Value& function = getJsonValue(root, "function-data");
    p.func_root_label_ = toID(getJsonValue(function, "root-label"));
}

ID
Preparams::getRootLabelInF(void) const {
    return func_root_label_;
}

ID
Preparams::getIDOfActionNodeInF(const ArrayIndex& i) const {
    return getMappedValue(i, func_action_node_vk_mappings_);
}

ID
Preparams::getIDOfDataNodeInF(const ArrayIndex& i) const {
    return getMappedValue(i, func_data_node_vk_mappings_);
}

ID
Preparams::getIDOfStateNodeInF(const ArrayIndex& i) const {
    return getMappedValue(i, func_state_node_vk_mappings_);
}

ID
Preparams::getIDOfLabelNodeInF(const ArrayIndex& i) const {
    return getMappedValue(i, func_label_node_vk_mappings_);
}

ID
Preparams::getIDOfRegisterInM(const ArrayIndex& i) const {
    return getMappedValue(i, mach_reg_vk_mappings_);
}

ID
Preparams::getIDOfPI(const ArrayIndex& i) const {
    return getMappedValue(i, pat_inst_vk_mappings_);
}

list<ID>
Preparams::getIDsOfActionNodesInF(const list<ArrayIndex>& is) const {
    return getMappedValues(is, func_action_node_vk_mappings_);
}

list<ID>
Preparams::getIDsOfDataNodesInF(const list<ArrayIndex>& is) const {
    return getMappedValues(is, func_data_node_vk_mappings_);
}

list<ID>
Preparams::getIDsOfStateNodesInF(const list<ArrayIndex>& is) const {
    return getMappedValues(is, func_state_node_vk_mappings_);
}

list<ID>
Preparams::getIDsOfLabelNodesInF(const list<ArrayIndex>& is) const {
    return getMappedValues(is, func_label_node_vk_mappings_);
}

list<ID>
Preparams::getIDsOfRegistersInM(const list<ArrayIndex>& is) const {
    return getMappedValues(is, mach_reg_vk_mappings_);
}

list<ID>
Preparams::getIDsOfPIs(const list<ArrayIndex>& is) const {
    return getMappedValues(is, pat_inst_vk_mappings_);
}