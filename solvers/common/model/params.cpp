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
    setPatternCodeSizes(root, param);
    setPatternLatencies(root, param);
    setPatternConstraints(root, param);
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
Params::getCodeSizeOfPattern(const Id& instance) const {
    return getMappedValue(instance, pat_inst_code_sizes_);
}

int
Params::getLatencyOfPattern(const Id& instance) const {
    return getMappedValue(instance, pat_inst_latencies_);
}

void
Params::setPatternCodeSizes(const Json::Value& root, Params& param) {
    for (Value& pattern : getJsonValue(root, "pattern-instance-data")) {
        const Id& instance_id = toId(getJsonValue(pattern, "instance-id"));
        int code_size = toInt(getJsonValue(pattern, "code-size"));
        addMapping(instance_id,
                   code_size,
                   param.pat_inst_code_sizes_);
    }
}

void
Params::setPatternLatencies(const Json::Value& root, Params& param) {
    for (Value& pattern : getJsonValue(root, "pattern-instance-data")) {
        const Id& instance_id = toId(getJsonValue(pattern, "instance-id"));
        int latency = toInt(getJsonValue(pattern, "latency"));
        addMapping(instance_id,
                   latency,
                   param.pat_inst_latencies_);
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

list<ArrayIndex>
Params::getIndicesOfActionNodes(const list<Id>& ids) const {
    list<Id> indices;
    for (auto& id : ids) {
        indices.push_back(getIndexOfActionNode(id));
    }
    return indices;
}

list<ArrayIndex>
Params::getIndicesOfEntityNodes(const list<Id>& ids) const {
    list<Id> indices;
    for (auto& id : ids) {
        indices.push_back(getIndexOfEntityNode(id));
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
Params::setPatternConstraints(const Value& root, Params& param) {
    for (Value& pattern : getJsonValue(root, "pattern-instance-data")) {
        const Id& instance_id = toId(getJsonValue(pattern, "instance-id"));
        list<Constraint*> constraints;
        for (Value& expr : getJsonValue(pattern, "constraints")) {
            Constraint* c = parseConstraintExpression(toString(expr));
            constraints.push_back(c);
        }
        addMapping(instance_id,
                   constraints,
                   param.pat_inst_constraints_);
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
        else if (eat("def-of-enode ", str)) {
            auto e = parseNodeIdExpr(str);
            expr = new DefinerOfEntityNodeExpr(e);
        }
        else if (eat("this", str)) {
            expr = new ThisInstanceIdExpr;
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
        expr = new AnInstanceIdExpr(num);
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
