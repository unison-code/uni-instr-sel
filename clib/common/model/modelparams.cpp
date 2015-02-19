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
ModelParams::getNumEntityNodesInF(void) const {
    return num_func_entity_nodes_;
}

size_t
ModelParams::getNumLabelNodesInF(void) const {
    return num_func_label_nodes_;
}

size_t
ModelParams::getNumMatches(void) const {
    return num_matches_;
}

size_t
ModelParams::getNumRegistersInM(void) const {
    return num_regs_;
}

ID
ModelParams::getEntryLabelInF(void) const {
    return func_entry_label_;
}

void
ModelParams::parseJson(const string& str, ModelParams& p) {
    Value root;
    Reader reader;
    if (!reader.parse(str, root)) {
        THROW(Exception, reader.getFormattedErrorMessages());
    }

    setNumValues(root, p);
    setEntryLabelInF(root, p);
    setLabelDomSetsInF(root, p);
    setLabelPostDomSetsInF(root, p);
    setLabelDomEdgesInF(root, p);
    setLabelPostDomEdgesInF(root, p);
    setEssentialOpNodesInF(root, p);
    setExecFreqOfBBsInF(root, p);
    setConstraintsForF(root, p);
    setCodeSizesForMatches(root, p);
    setLatenciesForMatches(root, p);
    setConstraintsForMatches(root, p);
    setADDUCSettingsForMatches(root, p);
    setOperationNodesCoveredByMatches(root, p);
    setEntityNodesDefinedByMatches(root, p);
    setEntityNodesUsedByMatches(root, p);
    setEntryLabelNodeOfMatches(root, p);
    setNonEntryLabelNodesInMatches(root, p);
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
    for (auto jsonlist : getJsonValue(root, "match-op-nodes-covered")) {
        list<ArrayIndex> covers;
        for (auto entry : jsonlist) {
            covers.push_back(toArrayIndex(entry));
        }
        p.match_operations_covered_.push_back(covers);
    }
}

void
ModelParams::setEntityNodesDefinedByMatches(
    const Json::Value& root,
    ModelParams& p
) {
    for (auto jsonlist : getJsonValue(root, "match-entity-nodes-defined")) {
        list<ArrayIndex> covers;
        for (auto entry : jsonlist) {
            covers.push_back(toArrayIndex(entry));
        }
        p.match_entities_defined_.push_back(covers);
    }
}

void
ModelParams::setEntityNodesUsedByMatches(
    const Json::Value& root,
    ModelParams& p
) {
    for (auto jsonlist : getJsonValue(root, "match-entity-nodes-used")) {
        list<ArrayIndex> covers;
        for (auto entry : jsonlist) {
            covers.push_back(toArrayIndex(entry));
        }
        p.match_entities_used_.push_back(covers);
    }
}

void
ModelParams::setEntryLabelNodeOfMatches(
    const Json::Value& root,
    ModelParams& p
) {
    for (auto jsonlist : getJsonValue(root, "match-entry-label-nodes")) {
        list<ArrayIndex> covers;
        for (auto entry : jsonlist) {
            covers.push_back(toArrayIndex(entry));
        }
        p.match_entry_label_.push_back(covers);
    }
}

void
ModelParams::setNonEntryLabelNodesInMatches(
    const Json::Value& root,
    ModelParams& p
) {
    for (auto jsonlist : getJsonValue(root, "match-non-entry-label-nodes")) {
        list<ArrayIndex> covers;
        for (auto entry : jsonlist) {
            covers.push_back(toArrayIndex(entry));
        }
        p.match_non_entry_labels_.push_back(covers);
    }
}

vector< list<ID> >
ModelParams::getOperationNodesCoveredByAllMatches(void) const {
    return match_operations_covered_;
}

vector< list<ID> >
ModelParams::getEntityNodesDefinedByAllMatches(void) const {
    return match_entities_defined_;
}

vector< list<ID> >
ModelParams::getEntityNodesUsedByAllMatches(void) const {
    return match_entities_used_;
}

vector< list<ID> >
ModelParams::getEntryLabelNodeOfAllMatches(void) const {
    return match_entry_label_;
}

vector< list<ID> >
ModelParams::getNonEntryLabelNodesInAllMatches(void) const {
    return match_non_entry_labels_;
}

vector< list<ID> >
ModelParams::getLabelDomSetsInF(void) const {
    return func_label_dom_sets_;
}

vector< list<ID> >
ModelParams::getLabelPostDomSetsInF(void) const {
    return func_label_postdom_sets_;
}

vector< list<ID> >
ModelParams::getLabelDomEdgesInF(void) const {
    return func_label_dom_edges_;
}

vector< list<ID> >
ModelParams::getLabelPostDomEdgesInF(void) const {
    return func_label_postdom_edges_;
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
ModelParams::setEssentialOpNodesInF(const Value& root, ModelParams& p) {
    for (auto entry : getJsonValue(root, "fun-essential-op-nodes")) {
        p.func_essential_op_nodes_.push_back(toArrayIndex(entry));
    }
}

void
ModelParams::setExecFreqOfBBsInF(const Value& root, ModelParams& p) {
    for (auto entry : getJsonValue(root, "fun-bb-exec-freqs")) {
        p.func_bb_exec_freq_.push_back(toInt(entry));
    }
}

list<ArrayIndex>
ModelParams::getAllEssentialOpNodesInF(void) const {
    return func_essential_op_nodes_;
}

vector<int>
ModelParams::getExecFreqOfAllBBsInF(void) const {
    return func_bb_exec_freq_;
}

void
ModelParams::setConstraintsForF(const Value& root, ModelParams& p) {
    for (auto entry : getJsonValue(root, "fun-constraints")) {
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
ModelParams::getADDUCSettingForAllMatches(void) const {
    return match_apply_def_dom_use_constraint_;
}

void
ModelParams::setADDUCSettingsForMatches(
    const Json::Value& root,
    ModelParams& p
) {
    for (auto entry
             : getJsonValue(root, "match-adduc-settings"))
    {
        p.match_apply_def_dom_use_constraint_.push_back(toBool(entry));
    }
}

void
ModelParams::setEntryLabelInF(const Json::Value& root, ModelParams& p) {
    p.func_entry_label_ =
        toArrayIndex(getJsonValue(root, "fun-entry-label-node"));
}

void
ModelParams::setNumValues(const Json::Value& root, ModelParams& p) {
    p.num_func_operation_nodes_ = toInt(getJsonValue(root, "fun-num-op-nodes"));
    p.num_func_entity_nodes_ =
        toInt(getJsonValue(root, "fun-num-entity-nodes"));
    p.num_func_label_nodes_ = toInt(getJsonValue(root, "fun-num-label-nodes"));
    p.num_regs_ = toInt(getJsonValue(root, "num-registers"));
    p.num_matches_ = toInt(getJsonValue(root, "num-matches"));
}

void
ModelParams::setLabelDomSetsInF(const Value& root, ModelParams& p) {
    for (auto jsonlist : getJsonValue(root, "fun-label-dom-sets")) {
        list<ArrayIndex> domset;
        for (auto entry : jsonlist) {
            domset.push_back(toArrayIndex(entry));
        }
        p.func_label_dom_sets_.push_back(domset);
    }
}

void
ModelParams::setLabelPostDomSetsInF(const Value& root, ModelParams& p) {
    for (auto jsonlist : getJsonValue(root, "fun-label-postdom-sets")) {
        list<ArrayIndex> domset;
        for (auto entry : jsonlist) {
            domset.push_back(toArrayIndex(entry));
        }
        p.func_label_postdom_sets_.push_back(domset);
    }
}

void
ModelParams::setLabelDomEdgesInF(const Value& root, ModelParams& p) {
    for (auto jsonlist : getJsonValue(root, "fun-label-dom-edges")) {
        list<ArrayIndex> entities;
        for (auto entry : jsonlist) {
            entities.push_back(toArrayIndex(entry));
        }
        p.func_label_dom_edges_.push_back(entities);
    }
}

void
ModelParams::setLabelPostDomEdgesInF(const Value& root, ModelParams& p) {
    for (auto jsonlist : getJsonValue(root, "fun-label-postdom-edges")) {
        list<ArrayIndex> entities;
        for (auto entry : jsonlist) {
            entities.push_back(toArrayIndex(entry));
        }
        p.func_label_postdom_edges_.push_back(entities);
    }
}
