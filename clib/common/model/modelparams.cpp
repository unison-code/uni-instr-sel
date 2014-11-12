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

#include "modelparams.h"
#include "constraintparser.h"
#include "../exceptions/exception.h"
#include "../json/json.h"
#include "../utils/string.h"

using namespace Json;
using namespace Model;
using std::list;
using std::string;
using std::vector;

ModelParams::ModelParams(void) {}

ModelParams::~ModelParams(void) {
    destroyConstraintsForF();
    destroyConstraintsForMatches();
}

size_t
ModelParams::getNumOperationNodesInF(void) const {
    return num_func_operation_nodes_;
}

size_t
ModelParams::getNumDataNodesInF(void) const {
    return num_func_data_nodes_;
}

size_t
ModelParams::getNumStateNodesInF(void) const {
    return num_func_state_nodes_;
}

size_t
ModelParams::getNumLabelNodesInF(void) const {
    return num_func_label_nodes_;
}

size_t
ModelParams::getNumMatches(void) const {
    return num_pis_;
}

size_t
ModelParams::getNumRegistersInM(void) const {
    return num_regs_;
}

ID
ModelParams::getRootLabelInF(void) const {
    return func_root_label_;
}

void
ModelParams::parseJson(const string& str, ModelParams& p) {
    Value root;
    Reader reader;
    if (!reader.parse(str, root)) {
        THROW(Exception, reader.getFormattedErrorMessages());
    }

    setNumValues(root, p);
    setRootLabelInF(root, p);
    setDomsetsForLabelNodesInF(root, p);
    setConstraintsForF(root, p);
    setCodeSizesForMatches(root, p);
    setLatenciesForMatches(root, p);
    setConstraintsForMatches(root, p);
    setAUDDCSettingsForMatches(root, p);
    setOperationNodesCoveredByMatches(root, p);
    setDataNodesDefinedByMatches(root, p);
    setDataNodesUsedByMatches(root, p);
    setStateNodesDefinedByMatches(root, p);
    setStateNodesUsedByMatches(root, p);
    setLabelNodesReferredByMatches(root, p);
}

bool
ModelParams::hasJsonValue(const Value& value, const string& name) {
    return !value[name].isNull();
}

const Value&
ModelParams::getJsonValue(const Value& value, const string& name) {
    const Value& sought_value = value[name];
    if (sought_value.isNull()) {
        THROW(Exception, string("No '") + name + "' field found");
    }
    return sought_value;
}

ArrayIndex
ModelParams::toArrayIndex(const Value& value) {
    if (!value.isUInt()) {
        THROW(Exception, "Not a JSON unsigned integer");
    }
    return value.asUInt();
}

int
ModelParams::toInt(const Value& value) {
    if (!value.isInt()) {
        THROW(Exception, "Not a JSON integer");
    }
    return value.asInt();
}

bool
ModelParams::toBool(const Value& value) {
    if (!value.isBool()) {
        THROW(Exception, "Not a JSON Boolean");
    }
    return value.asBool();
}

string
ModelParams::toString(const Value& value) {
    if (!value.isString()) {
        THROW(Exception, "Not a JSON string");
    }
    return value.asString();
}

vector<int>
ModelParams::getCodeSizesForAllMatches(void) const {
    return match_code_sizes_;
}

vector<int>
ModelParams::getLatenciesForAllMatches(void) const {
    return match_latencies_;
}

void
ModelParams::setCodeSizesForMatches(const Json::Value& root, ModelParams& p) {
    for (auto entry : getJsonValue(root, "match-code-sizes")) {
        p.match_code_sizes_.push_back(toInt(entry));
    }
}

void
ModelParams::setLatenciesForMatches(const Json::Value& root, ModelParams& p) {
    for (auto entry : getJsonValue(root, "match-latencies")) {
        p.match_latencies_.push_back(toInt(entry));
    }
}

list<const Constraint*>
ModelParams::getConstraintsForF(void) const {
    return func_constraints_;
}

vector< list<const Constraint*> >
ModelParams::getConstraintsForAllMatches(void) const {
    return match_constraints_;
}

void
ModelParams::setOperationNodesCoveredByMatches(
    const Json::Value& root,
    ModelParams& p
) {
    for (auto jsonlist : getJsonValue(root, "match-onodes-covered")) {
        list<ArrayIndex> covers;
        for (auto entry : jsonlist) {
            covers.push_back(toArrayIndex(entry));
        }
        p.match_operations_covered_.push_back(covers);
    }
}

void
ModelParams::setDataNodesDefinedByMatches(
    const Json::Value& root,
    ModelParams& p
) {
    for (auto jsonlist : getJsonValue(root, "match-dnodes-defined")) {
        list<ArrayIndex> covers;
        for (auto entry : jsonlist) {
            covers.push_back(toArrayIndex(entry));
        }
        p.match_data_defined_.push_back(covers);
    }
}

void
ModelParams::setStateNodesDefinedByMatches(
    const Json::Value& root,
    ModelParams& p
) {
    for (auto jsonlist : getJsonValue(root, "match-snodes-defined")) {
        list<ArrayIndex> covers;
        for (auto entry : jsonlist) {
            covers.push_back(toArrayIndex(entry));
        }
        p.match_states_defined_.push_back(covers);
    }
}

void
ModelParams::setDataNodesUsedByMatches(const Json::Value& root, ModelParams& p)
{
    for (auto jsonlist : getJsonValue(root, "match-dnodes-used")) {
        list<ArrayIndex> covers;
        for (auto entry : jsonlist) {
            covers.push_back(toArrayIndex(entry));
        }
        p.match_data_used_.push_back(covers);
    }
}

void
ModelParams::setStateNodesUsedByMatches(const Json::Value& root, ModelParams& p)
{
    for (auto jsonlist : getJsonValue(root, "match-snodes-used")) {
        list<ArrayIndex> covers;
        for (auto entry : jsonlist) {
            covers.push_back(toArrayIndex(entry));
        }
        p.match_states_used_.push_back(covers);
    }
}

void
ModelParams::setLabelNodesReferredByMatches(
    const Json::Value& root,
    ModelParams& p
) {
    for (auto jsonlist : getJsonValue(root, "match-lnodes-referred")) {
        list<ArrayIndex> covers;
        for (auto entry : jsonlist) {
            covers.push_back(toArrayIndex(entry));
        }
        p.match_labels_referred_.push_back(covers);
    }
}

vector< list<ID> >
ModelParams::getOperationNodesCoveredByAllMatches(void) const {
    return match_operations_covered_;
}

vector< list<ID> >
ModelParams::getDataNodesDefinedByAllMatches(void) const {
    return match_data_defined_;
}

vector< list<ID> >
ModelParams::getStateNodesDefinedByAllMatches(void) const {
    return match_states_defined_;
}

vector< list<ID> >
ModelParams::getDataNodesUsedByAllMatches(void) const {
    return match_data_used_;
}

vector< list<ID> >
ModelParams::getStateNodesUsedByAllMatches(void) const {
    return match_states_used_;
}

vector< list<ID> >
ModelParams::getLabelNodesReferredByAllMatches(void) const {
    return match_labels_referred_;
}

vector< list<ID> >
ModelParams::getDomsetForAllLabelNodesInF(void) const {
    return func_label_domsets_;
}

void
ModelParams::destroyConstraintsForF(void) {
    for (auto& c : func_constraints_) {
        delete c;
    }
}

void
ModelParams::destroyConstraintsForMatches(void) {
    for (auto& cs : match_constraints_) {
        for (auto c : cs) {
            delete c;
        }
    }
}

void
ModelParams::setConstraintsForF(const Value& root, ModelParams& p) {
    for (auto entry : getJsonValue(root, "func-constraints")) {
        ConstraintParser parser;
        Constraint* c = parser.parseConstraint(toString(entry));
        p.func_constraints_.push_back(c);
    }
}

void
ModelParams::setConstraintsForMatches(const Value& root, ModelParams& p) {
    for (auto jsonlist : getJsonValue(root, "match-constraints")) {
        list<const Constraint*> cs;
        ConstraintParser parser;
        for (auto entry : jsonlist) {
            Constraint* c = parser.parseConstraint(toString(entry));
            cs.push_back(c);
        }
        p.match_constraints_.push_back(cs);
    }
}

vector<bool>
ModelParams::getAUDDCSettingForAllMatches(void) const {
    return match_use_def_dom_constraints_;
}

void
ModelParams::setAUDDCSettingsForMatches(
    const Json::Value& root,
    ModelParams& p
) {
    for (auto entry
             : getJsonValue(root, "match-apply-use-def-dom-constraints"))
    {
        p.match_use_def_dom_constraints_.push_back(toBool(entry));
    }
}

void
ModelParams::setRootLabelInF(const Json::Value& root, ModelParams& p) {
    p.func_root_label_ = toArrayIndex(getJsonValue(root, "func-root-label"));
}

void
ModelParams::setNumValues(const Json::Value& root, ModelParams& p) {
    p.num_func_operation_nodes_ = toInt(getJsonValue(root, "num-func-onodes"));
    p.num_func_data_nodes_ = toInt(getJsonValue(root, "num-func-dnodes"));
    p.num_func_state_nodes_ = toInt(getJsonValue(root, "num-func-snodes"));
    p.num_func_label_nodes_ = toInt(getJsonValue(root, "num-func-lnodes"));
    p.num_regs_ = toInt(getJsonValue(root, "num-registers"));
    p.num_pis_ = toInt(getJsonValue(root, "num-matches"));
}

void
ModelParams::setDomsetsForLabelNodesInF(const Value& root, ModelParams& p) {
    for (auto jsonlist : getJsonValue(root, "func-label-domsets")) {
        list<ArrayIndex> domset;
        for (auto entry : jsonlist) {
            domset.push_back(toArrayIndex(entry));
        }
        p.func_label_domsets_.push_back(domset);
    }
}
