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
#include "../common/exceptions/exception.h"
#include "../common/json/json.h"
#include "../common/model/constraintparser.h"
#include "../common/utils/string.h"

using namespace Json;
using namespace Model;
using std::list;
using std::string;
using std::vector;

Params::Params(void) {}

Params::~Params(void) {
    destroyConstraintsForF();
    destroyConstraintsForPIs();
}

size_t
Params::getNumActionNodesInF(void) const {
    return num_func_action_nodes_;
}

size_t
Params::getNumDataNodesInF(void) const {
    return num_func_data_nodes_;
}

size_t
Params::getNumStateNodesInF(void) const {
    return num_func_state_nodes_;
}

size_t
Params::getNumLabelNodesInF(void) const {
    return num_func_label_nodes_;
}

size_t
Params::getNumPIs(void) const {
    return num_pis_;
}

size_t
Params::getNumRegistersInM(void) const {
    return num_regs_;
}

ID
Params::getRootLabelInF(void) const {
    return func_root_label_;
}

void
Params::parseJson(const string& str, Params& p) {
    Value root;
    Reader reader;
    if (!reader.parse(str, root)) {
        THROW(Exception, reader.getFormattedErrorMessages());
    }

    setNumValues(root, p);
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

ArrayIndex
Params::toArrayIndex(const Value& value) {
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

vector<int>
Params::getCodeSizesForAllPIs(void) const {
    return pat_inst_code_sizes_;
}

vector<int>
Params::getLatenciesForAllPIs(void) const {
    return pat_inst_latencies_;
}

void
Params::setCodeSizesForPIs(const Json::Value& root, Params& p) {
    for (auto entry : getJsonValue(root, "pat-inst-code-sizes")) {
        p.pat_inst_code_sizes_.push_back(toInt(entry));
    }
}

void
Params::setLatenciesForPIs(const Json::Value& root, Params& p) {
    for (auto entry : getJsonValue(root, "pat-inst-latencies")) {
        p.pat_inst_latencies_.push_back(toInt(entry));
    }
}

list<const Constraint*>
Params::getConstraintsForF(void) const {
    return func_constraints_;
}

vector< list<const Constraint*> >
Params::getConstraintsForAllPIs(void) const {
    return pat_inst_constraints_;
}

void
Params::setActionNodesCoveredByPIs(const Json::Value& root, Params& p) {
    for (auto jsonlist : getJsonValue(root, "pat-inst-anodes-covered")) {
        list<ArrayIndex> covers;
        for (auto entry : jsonlist) {
            covers.push_back(toArrayIndex(entry));
        }
        p.pat_inst_actions_covered_.push_back(covers);
    }
}

void
Params::setDataNodesDefinedByPIs(const Json::Value& root, Params& p) {
    for (auto jsonlist : getJsonValue(root, "pat-inst-dnodes-defined")) {
        list<ArrayIndex> covers;
        for (auto entry : jsonlist) {
            covers.push_back(toArrayIndex(entry));
        }
        p.pat_inst_data_defined_.push_back(covers);
    }
}

void
Params::setStateNodesDefinedByPIs(const Json::Value& root, Params& p) {
    for (auto jsonlist : getJsonValue(root, "pat-inst-snodes-defined")) {
        list<ArrayIndex> covers;
        for (auto entry : jsonlist) {
            covers.push_back(toArrayIndex(entry));
        }
        p.pat_inst_states_defined_.push_back(covers);
    }
}

void
Params::setDataNodesUsedByPIs(const Json::Value& root, Params& p) {
    for (auto jsonlist : getJsonValue(root, "pat-inst-dnodes-used")) {
        list<ArrayIndex> covers;
        for (auto entry : jsonlist) {
            covers.push_back(toArrayIndex(entry));
        }
        p.pat_inst_data_used_.push_back(covers);
    }
}

void
Params::setStateNodesUsedByPIs(const Json::Value& root, Params& p) {
    for (auto jsonlist : getJsonValue(root, "pat-inst-snodes-used")) {
        list<ArrayIndex> covers;
        for (auto entry : jsonlist) {
            covers.push_back(toArrayIndex(entry));
        }
        p.pat_inst_states_used_.push_back(covers);
    }
}

void
Params::setLabelNodesReferredByPIs(const Json::Value& root, Params& p) {
    for (auto jsonlist : getJsonValue(root, "pat-inst-lnodes-referred")) {
        list<ArrayIndex> covers;
        for (auto entry : jsonlist) {
            covers.push_back(toArrayIndex(entry));
        }
        p.pat_inst_labels_referred_.push_back(covers);
    }
}

vector< list<ID> >
Params::getActionNodesCoveredByAllPIs(void) const {
    return pat_inst_actions_covered_;
}

vector< list<ID> >
Params::getDataNodesDefinedByAllPIs(void) const {
    return pat_inst_data_defined_;
}

vector< list<ID> >
Params::getStateNodesDefinedByAllPIs(void) const {
    return pat_inst_states_defined_;
}

vector< list<ID> >
Params::getDataNodesUsedByAllPIs(void) const {
    return pat_inst_data_used_;
}

vector< list<ID> >
Params::getStateNodesUsedByAllPIs(void) const {
    return pat_inst_states_used_;
}

vector< list<ID> >
Params::getLabelNodesReferredByAllPIs(void) const {
    return pat_inst_labels_referred_;
}

vector< list<ID> >
Params::getDomsetForAllLabelNodesInF(void) const {
    return func_label_domsets_;
}

void
Params::destroyConstraintsForF(void) {
    for (auto& c : func_constraints_) {
        delete c;
    }
}

void
Params::destroyConstraintsForPIs(void) {
    for (auto& cs : pat_inst_constraints_) {
        for (auto c : cs) {
            delete c;
        }
    }
}

void
Params::setConstraintsForF(const Value& root, Params& p) {
    for (auto entry : getJsonValue(root, "func-constraints")) {
        ConstraintParser parser;
        Constraint* c = parser.parseConstraint(toString(entry));
        p.func_constraints_.push_back(c);
    }
}

void
Params::setConstraintsForPIs(const Value& root, Params& p) {
    for (auto jsonlist : getJsonValue(root, "pat-inst-constraints")) {
        list<const Constraint*> cs;
        ConstraintParser parser;
        for (auto entry : jsonlist) {
            Constraint* c = parser.parseConstraint(toString(entry));
            cs.push_back(c);
        }
        p.pat_inst_constraints_.push_back(cs);
    }
}

vector<bool>
Params::getNoUseDefDomConstraintsSettingForAllPIs(void) const {
    return pat_inst_no_use_def_dom_constraints_;
}

void
Params::setNoUseDefDomConstraintsSettingsForPIs(
    const Json::Value& root,
    Params& p
) {
    for (auto entry
             : getJsonValue(root, "pat-inst-no-use-def-dom-constraints"))
    {
        p.pat_inst_no_use_def_dom_constraints_.push_back(
            toBool(entry));
    }
}

void
Params::setRootLabelInF(const Json::Value& root, Params& p) {
    p.func_root_label_ = toArrayIndex(getJsonValue(root, "func-root-label"));
}

void
Params::setNumValues(const Json::Value& root, Params& p) {
    p.num_func_action_nodes_ = toInt(getJsonValue(root, "num-func-anodes"));
    p.num_func_data_nodes_ = toInt(getJsonValue(root, "num-func-dnodes"));
    p.num_func_state_nodes_ = toInt(getJsonValue(root, "num-func-snodes"));
    p.num_func_label_nodes_ = toInt(getJsonValue(root, "num-func-lnodes"));
    p.num_regs_ = toInt(getJsonValue(root, "num-registers"));
    p.num_pis_ = toInt(getJsonValue(root, "num-pattern-instances"));
}
