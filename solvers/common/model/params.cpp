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
    destroyConstraints();
}

size_t
Params::getNumActionNodes(void) const {
    return func_action_node_mappings_.size();
}

size_t
Params::getNumDataNodes(void) const {
    return func_data_node_mappings_.size();
}

size_t
Params::getNumStateNodes(void) const {
    return func_state_node_mappings_.size();
}

size_t
Params::getNumLabelNodes(void) const {
    return func_label_node_mappings_.size();
}

size_t
Params::getNumInstances(void) const {
    return pat_inst_mappings_.size();
}

size_t
Params::getNumRegisters(void) const {
    return mach_reg_mappings_.size();
}

void
Params::parseJson(const string& str, Params& p) {
    Value root;
    Reader reader;
    if (!reader.parse(str, root)) {
        THROW(Exception, reader.getFormattedErrorMessages());
    }

    computeMappingsForFunctionActionNodes(root, p);
    computeMappingsForFunctionDataNodes(root, p);
    computeMappingsForFunctionStateNodes(root, p);
    computeMappingsAndDomsetsForFunctionLabelNodes(root, p);
    computeMappingsForPatternInstances(root, p);
    computeMappingsForMachineRegisters(root, p);
    setFunctionRootLabel(root, p);
    setPatternInstanceCodeSizes(root, p);
    setPatternInstanceLatencies(root, p);
    setPatternInstanceConstraints(root, p);
    setPatternInstanceNoUseDefDomConstraintsSettings(root, p);
    setActionNodesCoveredByPatternInstances(root, p);
    setDataNodesDefinedByPatternInstances(root, p);
    setDataNodesUsedByPatternInstances(root, p);
    setStateNodesDefinedByPatternInstances(root, p);
    setStateNodesUsedByPatternInstances(root, p);
    setLabelNodesReferredByPatternInstances(root, p);
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
Params::computeMappingsForFunctionActionNodes(
    const Value& root,
    Params& p
) {
    const Value& function = getJsonValue(root, "function-data");
    ArrayIndex index = 0;
    for (auto node_id : getJsonValue(function, "action-nodes")) {
        addMapping(toId(node_id),
                   index++,
                   p.func_action_node_mappings_);
    }
}

void
Params::computeMappingsForFunctionDataNodes(
    const Value& root,
    Params& p
) {
    const Value& function = getJsonValue(root, "function-data");
    ArrayIndex index = 0;
    for (auto node_id : getJsonValue(function, "data-nodes")) {
        addMapping(toId(node_id),
                   index++,
                   p.func_data_node_mappings_);
    }
}

void
Params::computeMappingsForFunctionStateNodes(
    const Value& root,
    Params& p
) {
    const Value& function = getJsonValue(root, "function-data");
    ArrayIndex index = 0;
    for (auto node_id : getJsonValue(function, "state-nodes")) {
        addMapping(toId(node_id),
                   index++,
                   p.func_state_node_mappings_);
    }
}

void
Params::computeMappingsAndDomsetsForFunctionLabelNodes(
    const Value& root,
    Params& p
) {
    const Value& function = getJsonValue(root, "function-data");
    ArrayIndex index = 0;
    for (auto entry : getJsonValue(function, "label-nodes")) {
        const Id& node_id = toId(getJsonValue(entry, "node"));
        addMapping(node_id,
                   index++,
                   p.func_label_node_mappings_);
        list<Id> domset;
        for (auto dominator_node : getJsonValue(entry, "domset")) {
            domset.push_back(toId(dominator_node));
        }
        addMapping(node_id,
                   domset,
                   p.func_label_domsets_);
    }
}

void
Params::computeMappingsForMachineRegisters(
    const Value& root,
    Params& p
) {
    const Value& machine = getJsonValue(root, "machine-data");
    ArrayIndex index = 0;
    for (auto register_id : getJsonValue(machine, "registers")) {
        addMapping(toId(register_id),
                   index++,
                   p.mach_reg_mappings_);
    }
}

void
Params::computeMappingsForPatternInstances(
    const Value& root,
    Params& p
) {
    ArrayIndex index = 0;
    for (auto instance : getJsonValue(root, "pattern-instance-data")) {
        addMapping(toId(getJsonValue(instance, "instance-id")),
                   index++,
                   p.pat_inst_mappings_);
    }
}

int
Params::getCodeSizeOfInstance(const Id& instance) const {
    return getMappedValue(instance, pat_inst_code_sizes_);
}

int
Params::getLatencyOfInstance(const Id& instance) const {
    return getMappedValue(instance, pat_inst_latencies_);
}

void
Params::setPatternInstanceCodeSizes(const Json::Value& root, Params& p) {
    for (auto instance : getJsonValue(root, "pattern-instance-data")) {
        const Id& instance_id = toId(getJsonValue(instance, "instance-id"));
        int code_size = toInt(getJsonValue(instance, "code-size"));
        addMapping(instance_id,
                   code_size,
                   p.pat_inst_code_sizes_);
    }
}

void
Params::setPatternInstanceLatencies(const Json::Value& root, Params& p) {
    for (auto instance : getJsonValue(root, "pattern-instance-data")) {
        const Id& instance_id = toId(getJsonValue(instance, "instance-id"));
        int latency = toInt(getJsonValue(instance, "latency"));
        addMapping(instance_id,
                   latency,
                   p.pat_inst_latencies_);
    }
}

ArrayIndex
Params::getIndexOfRegister(const Id& id) const {
    return getMappedValue(id, mach_reg_mappings_);
}

list<ArrayIndex>
Params::getIndicesOfRegisters(const list<Id>& ids) const {
    list<Id> indices;
    for (auto& id : ids) {
        indices.push_back(getIndexOfRegister(id));
    }
    return indices;
}

list<Id>
Params::getAllRegisterIds(void) const {
    list<Id> ids;
    for (auto& kv : mach_reg_mappings_) {
        ids.push_back(kv.first);
    }
    return ids;
}

list<Id>
Params::getAllInstanceIds(void) const {
    list<Id> ids;
    for (auto& kv : pat_inst_mappings_) {
        ids.push_back(kv.first);
    }
    return ids;
}

ArrayIndex
Params::getIndexOfInstance(const Id& id) const {
    return getMappedValue(id, pat_inst_mappings_);
}

list<const Constraint*>
Params::getConstraintsOfInstance(const Id& id) const {
    return getMappedValue(id, pat_inst_constraints_);
}

void
Params::setActionNodesCoveredByPatternInstances(
    const Json::Value& root,
    Params& p
) {
    for (auto instance : getJsonValue(root, "pattern-instance-data")) {
        const Id& instance_id = toId(getJsonValue(instance, "instance-id"));
        list<Id> covers;
        for (auto node_id : getJsonValue(instance, "action-nodes-covered")) {
            covers.push_back(toId(node_id));
        }
        addMapping(instance_id,
                   covers,
                   p.pat_inst_actions_covered_);
    }
}

void
Params::setDataNodesDefinedByPatternInstances(
    const Json::Value& root,
    Params& p
) {
    for (auto instance : getJsonValue(root, "pattern-instance-data")) {
        const Id& instance_id = toId(getJsonValue(instance, "instance-id"));
        list<Id> covers;
        for (auto node_id : getJsonValue(instance, "data-nodes-defined")) {
            covers.push_back(toId(node_id));
        }
        addMapping(instance_id,
                   covers,
                   p.pat_inst_data_defined_);
    }
}

void
Params::setStateNodesDefinedByPatternInstances(
    const Json::Value& root,
    Params& p
) {
    for (auto instance : getJsonValue(root, "pattern-instance-data")) {
        const Id& instance_id = toId(getJsonValue(instance, "instance-id"));
        list<Id> covers;
        for (auto node_id : getJsonValue(instance, "state-nodes-defined")) {
            covers.push_back(toId(node_id));
        }
        addMapping(instance_id,
                   covers,
                   p.pat_inst_states_defined_);
    }
}

void
Params::setDataNodesUsedByPatternInstances(
    const Json::Value& root,
    Params& p
) {
    for (auto instance : getJsonValue(root, "pattern-instance-data")) {
        const Id& instance_id = toId(getJsonValue(instance, "instance-id"));
        list<Id> covers;
        for (auto node_id : getJsonValue(instance, "data-nodes-used")) {
            covers.push_back(toId(node_id));
        }
        addMapping(instance_id,
                   covers,
                   p.pat_inst_data_used_);
    }
}

void
Params::setStateNodesUsedByPatternInstances(
    const Json::Value& root,
    Params& p
) {
    for (auto instance : getJsonValue(root, "pattern-instance-data")) {
        const Id& instance_id = toId(getJsonValue(instance, "instance-id"));
        list<Id> covers;
        for (auto node_id : getJsonValue(instance, "state-nodes-used")) {
            covers.push_back(toId(node_id));
        }
        addMapping(instance_id,
                   covers,
                   p.pat_inst_states_used_);
    }
}

void
Params::setLabelNodesReferredByPatternInstances(
    const Json::Value& root,
    Params& p
) {
    for (auto instance : getJsonValue(root, "pattern-instance-data")) {
        const Id& instance_id = toId(getJsonValue(instance, "instance-id"));
        list<Id> refs;
        for (auto node_id : getJsonValue(instance, "label-nodes-referred")) {
            refs.push_back(toId(node_id));
        }
        addMapping(instance_id,
                   refs,
                   p.pat_inst_labels_referred_);
    }
}

list<Id>
Params::getActionNodesCoveredByInstance(const Id& instance) const {
    return getMappedValue(instance, pat_inst_actions_covered_);
}

list<Id>
Params::getDataNodesDefinedByInstance(const Id& instance) const {
    return getMappedValue(instance, pat_inst_data_defined_);
}

list<Id>
Params::getStateNodesDefinedByInstance(const Id& instance) const {
    return getMappedValue(instance, pat_inst_states_defined_);
}

list<Id>
Params::getDataNodesUsedByInstance(const Id& instance) const {
    return getMappedValue(instance, pat_inst_data_used_);
}

list<Id>
Params::getStateNodesUsedByInstance(const Id& instance) const {
    return getMappedValue(instance, pat_inst_states_used_);
}

list<Id>
Params::getLabelNodesReferredByInstance(const Id& instance) const {
    return getMappedValue(instance, pat_inst_labels_referred_);
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
Params::getIndexOfDataNode(const Id& id) const {
    return getMappedValue(id, func_data_node_mappings_);
}

ArrayIndex
Params::getIndexOfStateNode(const Id& id) const {
    return getMappedValue(id, func_state_node_mappings_);
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
Params::getAllDataNodeIds(void) const {
    list<Id> ids;
    for (auto& kv : func_data_node_mappings_) {
        ids.push_back(kv.first);
    }
    return ids;
}

list<Id>
Params::getAllStateNodeIds(void) const {
    list<Id> ids;
    for (auto& kv : func_state_node_mappings_) {
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

list<ArrayIndex>
Params::getIndicesOfActionNodes(const list<Id>& ids) const {
    list<Id> indices;
    for (auto& id : ids) {
        indices.push_back(getIndexOfActionNode(id));
    }
    return indices;
}

list<ArrayIndex>
Params::getIndicesOfDataNodes(const list<Id>& ids) const {
    list<Id> indices;
    for (auto& id : ids) {
        indices.push_back(getIndexOfDataNode(id));
    }
    return indices;
}

list<ArrayIndex>
Params::getIndicesOfStateNodes(const list<Id>& ids) const {
    list<Id> indices;
    for (auto& id : ids) {
        indices.push_back(getIndexOfStateNode(id));
    }
    return indices;
}

list<ArrayIndex>
Params::getIndicesOfLabelNodes(const list<Id>& ids) const {
    list<Id> indices;
    for (auto& id : ids) {
        indices.push_back(getIndexOfLabelNode(id));
    }
    return indices;
}

void
Params::destroyConstraints(void) {
    for (auto& kv : pat_inst_constraints_) {
        for (auto c : kv.second) {
            delete c;
        }
    }
}

void
Params::setPatternInstanceConstraints(const Value& root, Params& p) {
    for (auto instance : getJsonValue(root, "pattern-instance-data")) {
        const Id& instance_id = toId(getJsonValue(instance, "instance-id"));
        list<const Constraint*> constraints;
        for (auto expr : getJsonValue(instance, "constraints")) {
            Constraint* c = parseConstraintExpression(toString(expr));
            constraints.push_back(c);
        }
        addMapping(instance_id,
                   constraints,
                   p.pat_inst_constraints_);
    }
}

Constraint*
Params::parseConstraintExpression(const string& str) {
    string str_copy(Utils::searchReplace(str, "\n", " "));
    str_copy = Utils::searchReplace(str_copy, "\r", " ");
    str_copy = Utils::searchReplace(str_copy, "\t", " ");
    Constraint* c = new Constraint(parseBoolExpr(str_copy));
    eatWhitespace(str_copy);
    if (str_copy.length() != 0) {
        THROW(Exception, "Invalid constraint expression (trailing part)");
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
        else if (eat("bool-to-num ", str)) {
            auto e = parseBoolExpr(str);
            expr = new BoolToNumExpr(e);
        }
        else if (eat("node-id-to-num ", str)) {
            auto e = parseNodeIdExpr(str);
            expr = new NodeIdToNumExpr(e);
        }
        else if (eat("insta-id-to-num ", str)) {
            auto e = parseInstanceIdExpr(str);
            expr = new InstanceIdToNumExpr(e);
        }
        else if (eat("instr-id-to-num ", str)) {
            auto e = parseInstructionIdExpr(str);
            expr = new InstructionIdToNumExpr(e);
        }
        else if (eat("pat-id-to-num ", str)) {
            auto e = parsePatternIdExpr(str);
            expr = new PatternIdToNumExpr(e);
        }
        else if (eat("lab-id-to-num ", str)) {
            auto e = parseLabelIdExpr(str);
            expr = new LabelIdToNumExpr(e);
        }
        else if (eat("reg-id-to-num ", str)) {
            auto e = parseRegisterIdExpr(str);
            expr = new RegisterIdToNumExpr(e);
        }
        else if (eat("dist-pat-to-lab ", str)) {
            auto lhs = parseInstanceIdExpr(str);
            auto rhs = parseLabelIdExpr(str);
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
        int num = eatInt(str);
        expr = new AnIntegerExpr(num);
    }

    return expr;
}

NodeIdExpr*
Params::parseNodeIdExpr(string& str) {
    eatWhitespace(str);
    int num = eatInt(str);
    return new ANodeIdExpr(num);
}

InstanceIdExpr*
Params::parseInstanceIdExpr(string& str) {
    InstanceIdExpr* expr = NULL;

    eatWhitespace(str);
    if (eat("(", str)) {
        eatWhitespace(str);
        if (eat("cov-of-anode ", str)) {
            auto e = parseNodeIdExpr(str);
            expr = new CovererOfActionNodeExpr(e);
        }
        else if (eat("def-of-dnode ", str)) {
            auto e = parseNodeIdExpr(str);
            expr = new DefinerOfDataNodeExpr(e);
        }
        else if (eat("def-of-snode ", str)) {
            auto e = parseNodeIdExpr(str);
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
            expr = new ThisInstanceIdExpr;
        }
        else {
            int num = eatInt(str);
            expr = new AnInstanceIdExpr(num);
        }
    }

    return expr;
}

InstructionIdExpr*
Params::parseInstructionIdExpr(string& str) {
    InstructionIdExpr* expr = NULL;

    eatWhitespace(str);
    if (eat("(", str)) {
        eatWhitespace(str);
        if (eat("instr-of-pat ", str)) {
            auto e = parsePatternIdExpr(str);
            expr = new InstructionIdOfPatternExpr(e);
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
        expr = new AnInstructionIdExpr(num);
    }

    return expr;
}

PatternIdExpr*
Params::parsePatternIdExpr(string& str) {
    PatternIdExpr* expr = NULL;

    eatWhitespace(str);
    if (eat("(", str)) {
        eatWhitespace(str);
        if (eat("pat-of-insta ", str)) {
            auto e = parseInstanceIdExpr(str);
            expr = new PatternIdOfInstanceExpr(e);
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
        expr = new APatternIdExpr(num);
    }

    return expr;
}

LabelIdExpr*
Params::parseLabelIdExpr(string& str) {
    LabelIdExpr* expr = NULL;

    eatWhitespace(str);
    if (eat("(", str)) {
        eatWhitespace(str);
        if (eat("lab-alloc-to-insta ", str)) {
            auto e = parseInstanceIdExpr(str);
            expr = new LabelIdAllocatedToInstanceExpr(e);
        }
        else if (eat("lab-id-of-node ", str)) {
            auto e = parseNodeIdExpr(str);
            expr = new LabelIdOfLabelNodeExpr(e);
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

RegisterIdExpr*
Params::parseRegisterIdExpr(string& str) {
    RegisterIdExpr* expr = NULL;

    eatWhitespace(str);
    if (eat("(", str)) {
        eatWhitespace(str);
        if (eat("reg-alloc-to-dnode ", str)) {
            auto e = parseNodeIdExpr(str);
            expr = new RegisterIdAllocatedToDataNodeExpr(e);
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
        expr = new ARegisterIdExpr(num);
    }

    return expr;
}

list<const RegisterIdExpr*>
Params::parseListOfRegisterIdExpr(string& str) {
    list<const RegisterIdExpr*> expr;

    eatWhitespace(str);
    if (eat("(", str)) {
        while (true) {
            eatWhitespace(str);
            expr.push_back(parseRegisterIdExpr(str));
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
            auto e = parseLabelIdExpr(str);
            expr = new DomSetOfLabelIdExpr(e);
        }
        else if (eat("reg-class ", str)) {
            auto es = parseListOfRegisterIdExpr(str);
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
            auto e = parseLabelIdExpr(str);
            expr = new LabelIdToSetElemExpr(e);
        }
        else if (eat("reg-id-to-set-elem ", str)) {
            auto e = parseRegisterIdExpr(str);
            expr = new RegisterIdToSetElemExpr(e);
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
Params::isActionNode(const Id& id) const {
    for (const Id& c_id : getAllActionNodeIds()) {
        if (c_id == id) return true;
    }
    return false;
}

bool
Params::isDataNode(const Id& id) const {
    for (const Id& c_id : getAllDataNodeIds()) {
        if (c_id == id) return true;
    }
    return false;
}

bool
Params::isStateNode(const Id& id) const {
    for (const Id& c_id : getAllStateNodeIds()) {
        if (c_id == id) return true;
    }
    return false;
}

bool
Params::isLabelNode(const Id& id) const  {
    for (const Id& c_id : getAllLabelNodeIds()) {
        if (c_id == id) return true;
    }
    return false;
}

bool
Params::getNoUseDefDomConstraintsSettingForInstance(const Id& instance) const {
    return getMappedValue(instance, pat_inst_no_use_def_dom_constraints_);
}

void
Params::setPatternInstanceNoUseDefDomConstraintsSettings(
    const Json::Value& root,
    Params& p
) {
    for (auto instance : getJsonValue(root, "pattern-instance-data")) {
        const Id& instance_id = toId(getJsonValue(instance, "instance-id"));
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
Params::setFunctionRootLabel(const Json::Value& root, Params& p) {
    const Value& function = getJsonValue(root, "function-data");
    p.func_root_label_ = toId(getJsonValue(function, "root-label"));
}

Id
Params::getRootLabel(void) const {
    return func_root_label_;
}
