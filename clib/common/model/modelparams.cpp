/**
 * Copyright (c) 2013-2015, Gabriel Hjort Blindell <ghb@kth.se>
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
ModelParams::getNumBlockNodesInF(void) const {
    return num_func_block_nodes_;
}

size_t
ModelParams::getNumMatches(void) const {
    return num_matches_;
}

size_t
ModelParams::getNumLocationsInM(void) const {
    return num_locations_;
}

ID
ModelParams::getEntryBlockInF(void) const {
    return func_entry_block_;
}

void
ModelParams::parseJson(const string& str, ModelParams& p) {
    Value root;
    Reader reader;
    if (!reader.parse(str, root)) {
        THROW(Exception, reader.getFormattedErrorMessages());
    }

    setNumValues(root, p);
    setStateEntitiesInF(root, p);
    setEntryBlockInF(root, p);
    setBlockDomSetsInF(root, p);
    setDefEdgesInF(root, p);
    setExecFreqOfBlocksInF(root, p);
    setConstraintsForF(root, p);
    setCodeSizesForMatches(root, p);
    setLatenciesForMatches(root, p);
    setNonCopyInstrMatches(root, p);
    setConstraintsForMatches(root, p);
    setADDUCSettingsForMatches(root, p);
    setOperationNodesCoveredByMatches(root, p);
    setEntityNodesDefinedByMatches(root, p);
    setEntityNodesUsedByMatches(root, p);
    setEntryBlockNodeOfMatches(root, p);
    setNonEntryBlockNodesInMatches(root, p);
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

list<ArrayIndex>
ModelParams::getNonCopyInstrMatches(void) const {
    return match_non_copy_instrs_;
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

void
ModelParams::setNonCopyInstrMatches(const Value& root, ModelParams& p) {
    for (auto entry : getJsonValue(root, "match-non-copy-instrs")) {
        p.match_non_copy_instrs_.push_back(toArrayIndex(entry));
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
        list<ArrayIndex> defs;
        for (auto entry : jsonlist) {
            defs.push_back(toArrayIndex(entry));
        }
        p.match_entities_defined_.push_back(defs);
    }
}

void
ModelParams::setEntityNodesUsedByMatches(
    const Json::Value& root,
    ModelParams& p
) {
    for (auto jsonlist : getJsonValue(root, "match-entity-nodes-used")) {
        list<ArrayIndex> uses;
        for (auto entry : jsonlist) {
            uses.push_back(toArrayIndex(entry));
        }
        p.match_entities_used_.push_back(uses);
    }
}

void
ModelParams::setEntryBlockNodeOfMatches(
    const Json::Value& root,
    ModelParams& p
) {
    for (auto entry : getJsonValue(root, "match-entry-block-nodes")) {
        list<ArrayIndex> block;
        if (!entry.isNull()) block.push_back(toArrayIndex(entry));
        p.match_entry_block_.push_back(block);
    }
}

void
ModelParams::setNonEntryBlockNodesInMatches(
    const Json::Value& root,
    ModelParams& p
) {
    for (auto jsonlist : getJsonValue(root, "match-non-entry-block-nodes")) {
        list<ArrayIndex> blocks;
        for (auto entry : jsonlist) {
            blocks.push_back(toArrayIndex(entry));
        }
        p.match_non_entry_blocks_.push_back(blocks);
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
ModelParams::getEntryBlockNodeOfAllMatches(void) const {
    return match_entry_block_;
}

vector< list<ID> >
ModelParams::getNonEntryBlockNodesInAllMatches(void) const {
    return match_non_entry_blocks_;
}

vector< list<ID> >
ModelParams::getBlockDomSetsInF(void) const {
    return func_block_dom_sets_;
}

vector< list<ID> >
ModelParams::getDefEdgesInF(void) const {
    return func_def_edges_;
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
ModelParams::setStateEntitiesInF(const Value& root, ModelParams& p) {
    for (auto entry : getJsonValue(root, "fun-state-nodes")) {
        p.func_state_entities_.push_back(toArrayIndex(entry));
    }
}

void
ModelParams::setExecFreqOfBlocksInF(const Value& root, ModelParams& p) {
    for (auto entry : getJsonValue(root, "fun-bb-exec-freqs")) {
        p.func_block_exec_freq_.push_back(toInt(entry));
    }
}

list<ArrayIndex>
ModelParams::getAllStateEntitiesInF(void) const {
    return func_state_entities_;
}

vector<int>
ModelParams::getExecFreqOfAllBlocksInF(void) const {
    return func_block_exec_freq_;
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
ModelParams::setEntryBlockInF(const Json::Value& root, ModelParams& p) {
    p.func_entry_block_ =
        toArrayIndex(getJsonValue(root, "fun-entry-block-node"));
}

void
ModelParams::setNumValues(const Json::Value& root, ModelParams& p) {
    p.num_func_operation_nodes_ = toInt(getJsonValue(root, "fun-num-op-nodes"));
    p.num_func_entity_nodes_ =
        toInt(getJsonValue(root, "fun-num-entity-nodes"));
    p.num_func_block_nodes_ = toInt(getJsonValue(root, "fun-num-block-nodes"));
    p.num_locations_ = toInt(getJsonValue(root, "num-locations"));
    p.num_matches_ = toInt(getJsonValue(root, "num-matches"));
}

void
ModelParams::setBlockDomSetsInF(const Value& root, ModelParams& p) {
    for (auto jsonlist : getJsonValue(root, "fun-block-dom-sets")) {
        list<ArrayIndex> domset;
        for (auto entry : jsonlist) {
            domset.push_back(toArrayIndex(entry));
        }
        p.func_block_dom_sets_.push_back(domset);
    }
}

void
ModelParams::setDefEdgesInF(const Value& root, ModelParams& p) {
    for (auto jsonlist : getJsonValue(root, "fun-def-edges")) {
        list<ArrayIndex> entities;
        for (auto entry : jsonlist) {
            entities.push_back(toArrayIndex(entry));
        }
        p.func_def_edges_.push_back(entities);
    }
}
