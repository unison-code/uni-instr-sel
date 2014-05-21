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
#include "../utils/string.h"

using namespace Json;
using namespace Model;
using std::list;
using std::map;
using std::string;

Params::Params(void) {}

Params::~Params(void) {
    destroyConstraintsForF();
    destroyConstraintsForPIs();
}

size_t
Params::getNumActionNodesInF(void) const {
    return func_action_node_kv_mappings_.size();
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
Params::getNumPIs(void) const {
    return pat_inst_kv_mappings_.size();
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

void
Params::computeMappingsForActionNodesInF(const Value& root, Params& p) {
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
        addMapping(index, reg_id, p.mach_reg_kv_mappings_);
        index++;
    }
}

void
Params::computeMappingsForPIs(const Value& root, Params& p) {
    ArrayIndex index = 0;
    for (auto entry : getJsonValue(root, "pattern-instance-data")) {
        const ID& pi_id = toID(getJsonValue(entry, "instance-id"));
        addMapping(pi_id, index, p.pat_inst_kv_mappings_);
        addMapping(index, pi_id, p.pat_inst_vk_mappings_);
        index++;
    }
}

int
Params::getCodeSizeForPI(const ID& instance) const {
    return getMappedValue(instance, pat_inst_code_sizes_);
}

int
Params::getLatencyForPI(const ID& instance) const {
    return getMappedValue(instance, pat_inst_latencies_);
}

void
Params::setCodeSizesForPIs(const Json::Value& root, Params& p) {
    for (auto instance : getJsonValue(root, "pattern-instance-data")) {
        const ID& instance_id = toID(getJsonValue(instance, "instance-id"));
        int code_size = toInt(getJsonValue(instance, "code-size"));
        addMapping(instance_id, code_size, p.pat_inst_code_sizes_);
    }
}

void
Params::setLatenciesForPIs(const Json::Value& root, Params& p) {
    for (auto instance : getJsonValue(root, "pattern-instance-data")) {
        const ID& instance_id = toID(getJsonValue(instance, "instance-id"));
        int latency = toInt(getJsonValue(instance, "latency"));
        addMapping(instance_id, latency, p.pat_inst_latencies_);
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
Params::getIDsForAllPIs(void) const {
    return getAllKeys(pat_inst_kv_mappings_);
}

ArrayIndex
Params::getIndexForPI(const ID& id) const {
    return getMappedValue(id, pat_inst_kv_mappings_);
}

list<const Constraint*>
Params::getConstraintsForF(void) const {
    return func_constraints_;
}

list<const Constraint*>
Params::getConstraintsForPI(const ID& id) const {
    return getMappedValue(id, pat_inst_constraints_);
}

void
Params::setActionNodesCoveredByPIs(const Json::Value& root, Params& p) {
    for (auto instance : getJsonValue(root, "pattern-instance-data")) {
        const ID& instance_id = toID(getJsonValue(instance, "instance-id"));
        list<ID> covers;
        for (auto node_id : getJsonValue(instance, "action-nodes-covered")) {
            covers.push_back(toID(node_id));
        }
        addMapping(instance_id, covers, p.pat_inst_actions_covered_);
    }
}

void
Params::setDataNodesDefinedByPIs(const Json::Value& root, Params& p) {
    for (auto instance : getJsonValue(root, "pattern-instance-data")) {
        const ID& instance_id = toID(getJsonValue(instance, "instance-id"));
        list<ID> covers;
        for (auto node_id : getJsonValue(instance, "data-nodes-defined")) {
            covers.push_back(toID(node_id));
        }
        addMapping(instance_id,
                   covers,
                   p.pat_inst_data_defined_);
    }
}

void
Params::setStateNodesDefinedByPIs(const Json::Value& root, Params& p) {
    for (auto instance : getJsonValue(root, "pattern-instance-data")) {
        const ID& instance_id = toID(getJsonValue(instance, "instance-id"));
        list<ID> covers;
        for (auto node_id : getJsonValue(instance, "state-nodes-defined")) {
            covers.push_back(toID(node_id));
        }
        addMapping(instance_id,
                   covers,
                   p.pat_inst_states_defined_);
    }
}

void
Params::setDataNodesUsedByPIs(const Json::Value& root, Params& p) {
    for (auto instance : getJsonValue(root, "pattern-instance-data")) {
        const ID& instance_id = toID(getJsonValue(instance, "instance-id"));
        list<ID> covers;
        for (auto node_id : getJsonValue(instance, "data-nodes-used")) {
            covers.push_back(toID(node_id));
        }
        addMapping(instance_id,
                   covers,
                   p.pat_inst_data_used_);
    }
}

void
Params::setStateNodesUsedByPIs(const Json::Value& root, Params& p) {
    for (auto instance : getJsonValue(root, "pattern-instance-data")) {
        const ID& instance_id = toID(getJsonValue(instance, "instance-id"));
        list<ID> covers;
        for (auto node_id : getJsonValue(instance, "state-nodes-used")) {
            covers.push_back(toID(node_id));
        }
        addMapping(instance_id,
                   covers,
                   p.pat_inst_states_used_);
    }
}

void
Params::setLabelNodesReferredByPIs(const Json::Value& root, Params& p) {
    for (auto instance : getJsonValue(root, "pattern-instance-data")) {
        const ID& instance_id = toID(getJsonValue(instance, "instance-id"));
        list<ID> refs;
        for (auto node_id : getJsonValue(instance, "label-nodes-referred")) {
            refs.push_back(toID(node_id));
        }
        addMapping(instance_id,
                   refs,
                   p.pat_inst_labels_referred_);
    }
}

list<ID>
Params::getActionNodesCoveredByPI(const ID& instance) const {
    return getMappedValue(instance, pat_inst_actions_covered_);
}

list<ID>
Params::getDataNodesDefinedByPI(const ID& instance) const {
    return getMappedValue(instance, pat_inst_data_defined_);
}

list<ID>
Params::getStateNodesDefinedByPI(const ID& instance) const {
    return getMappedValue(instance, pat_inst_states_defined_);
}

list<ID>
Params::getDataNodesUsedByPI(const ID& instance) const {
    return getMappedValue(instance, pat_inst_data_used_);
}

list<ID>
Params::getStateNodesUsedByPI(const ID& instance) const {
    return getMappedValue(instance, pat_inst_states_used_);
}

list<ID>
Params::getLabelNodesReferredByPI(const ID& instance) const {
    return getMappedValue(instance, pat_inst_labels_referred_);
}

list<ID>
Params::getDomsetForLabelNodeInF(const ID& id) const {
    return getMappedValue(id, func_label_domsets_);
}

ArrayIndex
Params::getIndexForActionNodeInF(const ID& id) const {
    return getMappedValue(id, func_action_node_kv_mappings_);
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
Params::getIDsForAllActionNodesInF(void) const {
    return getAllKeys(func_action_node_kv_mappings_);
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
Params::getIndicesForActionNodesInF(const list<ID>& ids) const {
    return getMappedValues(ids, func_action_node_kv_mappings_);
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
Params::destroyConstraintsForPIs(void) {
    for (auto& kv : pat_inst_constraints_) {
        for (auto c : kv.second) {
            delete c;
        }
    }
}

void
Params::setConstraintsForF(const Value& root, Params& p) {
    const Value& function = getJsonValue(root, "function-data");
    for (auto expr : getJsonValue(function, "constraints")) {
        Constraint* c = parseConstraint(toString(expr));
        p.func_constraints_.push_back(c);
    }
}

void
Params::setConstraintsForPIs(const Value& root, Params& p) {
    for (auto instance : getJsonValue(root, "pattern-instance-data")) {
        const ID& instance_id = toID(getJsonValue(instance, "instance-id"));
        list<const Constraint*> constraints;
        for (auto expr : getJsonValue(instance, "constraints")) {
            Constraint* c = parseConstraint(toString(expr));
            constraints.push_back(c);
        }
        addMapping(instance_id, constraints, p.pat_inst_constraints_);
    }
}

Constraint*
Params::parseConstraint(const string& str) {
    // Turn string into a single-line string
    string sl_str(Utils::searchReplace(str, "\n", " "));
    sl_str = Utils::searchReplace(sl_str, "\r", " ");
    sl_str = Utils::searchReplace(sl_str, "\t", " ");

    Constraint* c;
    string sl_str_copy(sl_str);
    // Check if it's an 'Data-Node-Is-Integer-Constant' constraint
    bool was_parse_successful = false;
    if (eat("(", sl_str_copy)) {
        eatWhitespace(sl_str_copy);
        if (eat("dnode-is-int-const ", sl_str_copy)) {
            c = new DataNodeIsIntConstantConstraint(parseNodeID(sl_str_copy));
            if (!eat(")", sl_str_copy)) {
                THROW(Exception,
                      "Invalid constraint expression (missing ')' char)");
            }
            sl_str = sl_str_copy;
            was_parse_successful = true;
        }
    }
    if (!was_parse_successful) {
        // Assume it's a Boolean expression constraint
        c = new BoolExprConstraint(parseBoolExpr(sl_str));
    }

    eatWhitespace(sl_str);
    if (sl_str.length() != 0) {
        delete c;
        THROW(Exception, "Invalid constraint expression (has trailing part)");
    }
    return c;
}

BoolExpr*
Params::parseBoolExpr(string& str) {
    BoolExpr* expr = NULL;

    eatWhitespace(str);
    if (eat("(", str)) {
        eatWhitespace(str);
        if (eat("== ", str)) {
            auto lhs = parseNumExpr(str);
            auto rhs = parseNumExpr(str);
            expr = new EqExpr(lhs, rhs);
        }
        else if (eat("!= ", str)) {
            auto lhs = parseNumExpr(str);
            auto rhs = parseNumExpr(str);
            expr = new NeqExpr(lhs, rhs);
        }
        else if (eat("> ", str)) {
            auto lhs = parseNumExpr(str);
            auto rhs = parseNumExpr(str);
            expr = new GTExpr(lhs, rhs);
        }
        else if (eat(">= ", str)) {
            auto lhs = parseNumExpr(str);
            auto rhs = parseNumExpr(str);
            expr = new GEExpr(lhs, rhs);
        }
        else if (eat("< ", str)) {
            auto lhs = parseNumExpr(str);
            auto rhs = parseNumExpr(str);
            expr = new LTExpr(lhs, rhs);
        }
        else if (eat("<= ", str)) {
            auto lhs = parseNumExpr(str);
            auto rhs = parseNumExpr(str);
            expr = new LEExpr(lhs, rhs);
        }
        else if (eat("&& ", str)) {
            auto lhs = parseBoolExpr(str);
            auto rhs = parseBoolExpr(str);
            expr = new AndExpr(lhs, rhs);
        }
        else if (eat("|| ", str)) {
            auto lhs = parseBoolExpr(str);
            auto rhs = parseBoolExpr(str);
            expr = new OrExpr(lhs, rhs);
        }
        else if (eat("-> ", str)) {
            auto lhs = parseBoolExpr(str);
            auto rhs = parseBoolExpr(str);
            expr = new ImpExpr(lhs, rhs);
        }
        else if (eat("<-> ", str)) {
            auto lhs = parseBoolExpr(str);
            auto rhs = parseBoolExpr(str);
            expr = new EqvExpr(lhs, rhs);
        }
        else if (eat("! ", str)) {
            auto e = parseBoolExpr(str);
            expr = new NotExpr(e);
        }
        else if (eat("in-set ", str)) {
            auto lhs = parseSetElemExpr(str);
            auto rhs = parseSetExpr(str);
            expr = new InSetExpr(lhs, rhs);
        }
        else {
            THROW(Exception, "Invalid constraint expression (unknown keyword)");
        }

        eatWhitespace(str);
        if (!eat(")", str)) {
            THROW(Exception,
                  "Invalid constraint expression (missing ')' char)");
        }
    }
    else {
        THROW(Exception, "Invalid constraint expression (missing '(' char)");
    }

    return expr;
}

NumExpr*
Params::parseNumExpr(string& str) {
    NumExpr* expr = NULL;

    eatWhitespace(str);
    if (eat("(", str)) {
        eatWhitespace(str);
        if (eat("+ ", str)) {
            auto lhs = parseNumExpr(str);
            auto rhs = parseNumExpr(str);
            expr = new PlusExpr(lhs, rhs);
        }
        else if (eat("- ", str)) {
            auto lhs = parseNumExpr(str);
            auto rhs = parseNumExpr(str);
            expr = new MinusExpr(lhs, rhs);
        }
        else if (eat("int-to-num ", str)) {
            auto e = parseIntExpr(str);
            expr = new IntToNumExpr(e);
        }
        else if (eat("bool-to-num ", str)) {
            auto e = parseBoolExpr(str);
            expr = new BoolToNumExpr(e);
        }
        else if (eat("node-id-to-num ", str)) {
            auto e = parseNodeIDExpr(str);
            expr = new NodeIDToNumExpr(e);
        }
        else if (eat("insta-id-to-num ", str)) {
            auto e = parsePatternInstanceIDExpr(str);
            expr = new PatternInstanceIDToNumExpr(e);
        }
        else if (eat("instr-id-to-num ", str)) {
            auto e = parseInstructionIDExpr(str);
            expr = new InstructionIDToNumExpr(e);
        }
        else if (eat("pat-id-to-num ", str)) {
            auto e = parsePatternIDExpr(str);
            expr = new PatternIDToNumExpr(e);
        }
        else if (eat("lab-id-to-num ", str)) {
            auto e = parseLabelIDExpr(str);
            expr = new LabelIDToNumExpr(e);
        }
        else if (eat("reg-id-to-num ", str)) {
            auto e = parseRegisterIDExpr(str);
            expr = new RegisterIDToNumExpr(e);
        }
        else if (eat("dist-pat-to-lab ", str)) {
            auto lhs = parsePatternInstanceIDExpr(str);
            auto rhs = parseLabelIDExpr(str);
            expr = new DistanceBetweenInstanceAndLabelExpr(lhs, rhs);
        }
        else {
            THROW(Exception, "Invalid constraint expression (unknown keyword)");
        }

        eatWhitespace(str);
        if (!eat(")", str)) {
            THROW(Exception,
                  "Invalid constraint expression (missing ')' char)");
        }
    }
    else {
        THROW(Exception, "Invalid constraint expression (missing '(' char)");
    }

    return expr;
}

IntExpr*
Params::parseIntExpr(string& str) {
    IntExpr* expr = NULL;

    eatWhitespace(str);
    if (eat("(", str)) {
        eatWhitespace(str);
        if (eat("int-const-val-of-dnode ", str)) {
            auto e = parseNodeIDExpr(str);
            expr = new IntConstValueOfDataNodeExpr(e);
        }
        else {
            THROW(Exception, "Invalid constraint expression (unknown keyword)");
        }

        eatWhitespace(str);
        if (!eat(")", str)) {
            THROW(Exception,
                  "Invalid constraint expression (missing ')' char)");
        }
    }
    else {
        int num = eatInt(str);
        expr = new AnIntegerExpr(num);
    }

    return expr;
}

NodeIDExpr*
Params::parseNodeIDExpr(string& str) {
    eatWhitespace(str);
    int num = eatInt(str);
    return new ANodeIDExpr(num);
}

PatternInstanceIDExpr*
Params::parsePatternInstanceIDExpr(string& str) {
    PatternInstanceIDExpr* expr = NULL;

    eatWhitespace(str);
    if (eat("(", str)) {
        eatWhitespace(str);
        if (eat("cov-of-anode ", str)) {
            auto e = parseNodeIDExpr(str);
            expr = new CovererOfActionNodeExpr(e);
        }
        else if (eat("def-of-dnode ", str)) {
            auto e = parseNodeIDExpr(str);
            expr = new DefinerOfDataNodeExpr(e);
        }
        else if (eat("def-of-snode ", str)) {
            auto e = parseNodeIDExpr(str);
            expr = new DefinerOfStateNodeExpr(e);
        }
        else {
            THROW(Exception, "Invalid constraint expression (unknown keyword)");
        }

        eatWhitespace(str);
        if (!eat(")", str)) {
            THROW(Exception,
                  "Invalid constraint expression (missing ')' char)");
        }
    }
    else {
        if (eat("this", str)) {
            expr = new ThisPatternInstanceIDExpr;
        }
        else {
            int num = eatInt(str);
            expr = new APatternInstanceIDExpr(num);
        }
    }

    return expr;
}

InstructionIDExpr*
Params::parseInstructionIDExpr(string& str) {
    InstructionIDExpr* expr = NULL;

    eatWhitespace(str);
    if (eat("(", str)) {
        eatWhitespace(str);
        if (eat("instr-of-pat ", str)) {
            auto e = parsePatternIDExpr(str);
            expr = new InstructionIDOfPatternExpr(e);
        }
        else {
            THROW(Exception, "Invalid constraint expression (unknown keyword)");
        }

        eatWhitespace(str);
        if (!eat(")", str)) {
            THROW(Exception,
                  "Invalid constraint expression (missing ')' char)");
        }
    }
    else {
        int num = eatInt(str);
        expr = new AnInstructionIDExpr(num);
    }

    return expr;
}

PatternIDExpr*
Params::parsePatternIDExpr(string& str) {
    PatternIDExpr* expr = NULL;

    eatWhitespace(str);
    if (eat("(", str)) {
        eatWhitespace(str);
        if (eat("pat-of-insta ", str)) {
            auto e = parsePatternInstanceIDExpr(str);
            expr = new PatternIDOfInstanceExpr(e);
        }
        else {
            THROW(Exception, "Invalid constraint expression (unknown keyword)");
        }

        eatWhitespace(str);
        if (!eat(")", str)) {
            THROW(Exception,
                  "Invalid constraint expression (missing ')' char)");
        }
    }
    else {
        int num = eatInt(str);
        expr = new APatternIDExpr(num);
    }

    return expr;
}

LabelIDExpr*
Params::parseLabelIDExpr(string& str) {
    LabelIDExpr* expr = NULL;

    eatWhitespace(str);
    if (eat("(", str)) {
        eatWhitespace(str);
        if (eat("lab-alloc-to-insta ", str)) {
            auto e = parsePatternInstanceIDExpr(str);
            expr = new LabelIDAllocatedToInstanceExpr(e);
        }
        else if (eat("lab-id-of-node ", str)) {
            auto e = parseNodeIDExpr(str);
            expr = new LabelIDOfLabelNodeExpr(e);
        }
        else {
            THROW(Exception, "Invalid constraint expression (unknown keyword)");
        }

        eatWhitespace(str);
        if (!eat(")", str)) {
            THROW(Exception,
                  "Invalid constraint expression (missing ')' char)");
        }
    }
    else {
        THROW(Exception, "Invalid constraint expression (missing '(' char)");
    }

    return expr;
}

RegisterIDExpr*
Params::parseRegisterIDExpr(string& str) {
    RegisterIDExpr* expr = NULL;

    eatWhitespace(str);
    if (eat("(", str)) {
        eatWhitespace(str);
        if (eat("reg-alloc-to-dnode ", str)) {
            auto e = parseNodeIDExpr(str);
            expr = new RegisterIDAllocatedToDataNodeExpr(e);
        }
        else {
            THROW(Exception, "Invalid constraint expression (unknown keyword)");
        }

        eatWhitespace(str);
        if (!eat(")", str)) {
            THROW(Exception,
                  "Invalid constraint expression (missing ')' char)");
        }
    }
    else {
        int num = eatInt(str);
        expr = new ARegisterIDExpr(num);
    }

    return expr;
}

list<const RegisterIDExpr*>
Params::parseListOfRegisterIDExpr(string& str) {
    list<const RegisterIDExpr*> expr;

    eatWhitespace(str);
    if (eat("(", str)) {
        while (true) {
            eatWhitespace(str);
            expr.push_back(parseRegisterIDExpr(str));
            if (eat(" ", str)) continue;
            if (eat(")", str)) break;
        }
    }

    return expr;
}

SetExpr*
Params::parseSetExpr(string& str) {
    SetExpr* expr = NULL;

    eatWhitespace(str);
    if (eat("(", str)) {
        eatWhitespace(str);
        if (eat("union ", str)) {
            auto lhs = parseSetExpr(str);
            auto rhs = parseSetExpr(str);
            expr = new UnionSetExpr(lhs, rhs);
        }
        else if (eat("intersect ", str)) {
            auto lhs = parseSetExpr(str);
            auto rhs = parseSetExpr(str);
            expr = new IntersectSetExpr(lhs, rhs);
        }
        else if (eat("diff ", str)) {
            auto lhs = parseSetExpr(str);
            auto rhs = parseSetExpr(str);
            expr = new DiffSetExpr(lhs, rhs);
        }
        else if (eat("domset-of ", str)) {
            auto e = parseLabelIDExpr(str);
            expr = new DomSetOfLabelIDExpr(e);
        }
        else if (eat("reg-class ", str)) {
            auto es = parseListOfRegisterIDExpr(str);
            expr = new RegisterClassExpr(es);
        }
        else {
            THROW(Exception, "Invalid constraint expression (unknown keyword)");
        }

        eatWhitespace(str);
        if (!eat(")", str)) {
            THROW(Exception,
                  "Invalid constraint expression (missing ')' char)");
        }
    }
    else {
        THROW(Exception, "Invalid constraint expression (missing '(' char)");
    }

    return expr;
}

SetElemExpr*
Params::parseSetElemExpr(string& str) {
    SetElemExpr* expr = NULL;

    eatWhitespace(str);
    if (eat("(", str)) {
        eatWhitespace(str);
        if (eat("lab-id-to-set-elem ", str)) {
            auto e = parseLabelIDExpr(str);
            expr = new LabelIDToSetElemExpr(e);
        }
        else if (eat("reg-id-to-set-elem ", str)) {
            auto e = parseRegisterIDExpr(str);
            expr = new RegisterIDToSetElemExpr(e);
        }
        else {
            THROW(Exception, "Invalid constraint expression (unknown keyword)");
        }

        eatWhitespace(str);
        if (!eat(")", str)) {
            THROW(Exception,
                  "Invalid constraint expression (missing ')' char)");
        }
    }
    else {
        THROW(Exception, "Invalid constraint expression (missing '(' char)");
    }

    return expr;
}

ID
Params::parseNodeID(string& str) {
    eatWhitespace(str);
    return eatInt(str);
}

void
Params::eatWhitespace(string& str) {
    while (str.length() > 0 && Utils::isWhitespace(str[0])) {
        str = str.substr(1);
    }
}

bool
Params::eat(const string& search, string& str) {
    if (search.length() <= str.length()) {
        if (search == str.substr(0, search.length())) {
            str = str.substr(search.length());
            return true;
        }
    }
    return false;
}

int
Params::eatInt(string& str) {
    string int_str;
    if (str[0] == '-') {
        int_str += str[0];
        str = str.substr(1);
    }
    while (str.length() > 0 && Utils::isNumeric(str[0])) {
        int_str += str[0];
        str = str.substr(1);
    }
    if (!Utils::isNumeric(int_str)) {
        THROW(Exception, "Invalid constraint expression (not an integer)");
    }
    return Utils::toInt(int_str);
}

bool
Params::isActionNodeInF(const ID& id) const {
    for (const ID& c_id : getIDsForAllActionNodesInF()) {
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
Params::getNoUseDefDomConstraintsSettingForPI(const ID& instance) const {
    return getMappedValue(instance, pat_inst_no_use_def_dom_constraints_);
}

void
Params::setNoUseDefDomConstraintsSettingsForPIs(
    const Json::Value& root,
    Params& p
) {
    for (auto instance : getJsonValue(root, "pattern-instance-data")) {
        const ID& instance_id = toID(getJsonValue(instance, "instance-id"));
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
Params::setRootLabelInF(const Json::Value& root, Params& p) {
    const Value& function = getJsonValue(root, "function-data");
    p.func_root_label_ = toID(getJsonValue(function, "root-label"));
}

ID
Params::getRootLabelInF(void) const {
    return func_root_label_;
}

ID
Params::getIDOfActionNodeInF(const ArrayIndex& i) const {
    return getMappedValue(i, func_action_node_vk_mappings_);
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
Params::getIDOfPI(const ArrayIndex& i) const {
    return getMappedValue(i, pat_inst_vk_mappings_);
}

list<ID>
Params::getIDsOfActionNodesInF(const list<ArrayIndex>& is) const {
    return getMappedValues(is, func_action_node_vk_mappings_);
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
Params::getIDsOfPIs(const list<ArrayIndex>& is) const {
    return getMappedValues(is, pat_inst_vk_mappings_);
}
