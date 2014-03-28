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

#include "constraintprocessor.h"
#include "../common/utils/string.h"

using namespace Model;
using std::string;

// TODO: remove
#include <iostream>
using std::cout;
using std::endl;

ConstraintProcessor::ConstraintProcessor(const Model::Params& p)
    : p_(p)
{}

ConstraintProcessor::~ConstraintProcessor(void) {}

string
ConstraintProcessor::process(const Id& id, const Constraint* c) {
    instance_id_ = id;
    return string("constraint\n")
        + getInstanceSelectedArrayString() + "["
        + Utils::toString(p_.getIndexOfInstance(instance_id_))
        + "] -> " + process(c->getExpr()) + ";";
}

string
ConstraintProcessor::process(const BoolExpr* e) {
    if (const EqExpr* de = dynamic_cast<const EqExpr*>(e)) {
        return string("(") + process(de->getLhs()) + " == "
            + process(de->getRhs()) + ")";
    }
    else if (const NeqExpr* de = dynamic_cast<const NeqExpr*>(e)) {
        return string("(") + process(de->getLhs()) + " != "
            + process(de->getRhs()) + ")";
    }
    else if (const GTExpr* de = dynamic_cast<const GTExpr*>(e)) {
        return string("(") + process(de->getLhs()) + " > "
            + process(de->getRhs()) + ")";
    }
    else if (const GEExpr* de = dynamic_cast<const GEExpr*>(e)) {
        return string("(") + process(de->getLhs()) + " >= "
            + process(de->getRhs()) + ")";
    }
    else if (const LTExpr* de = dynamic_cast<const LTExpr*>(e)) {
        return string("(") + process(de->getLhs()) + " < "
            + process(de->getRhs()) + ")";
    }
    else if (const LEExpr* de = dynamic_cast<const LEExpr*>(e)) {
        return string("(") + process(de->getLhs()) + " <= "
            + process(de->getRhs()) + ")";
    }
    else if (const AndExpr* de = dynamic_cast<const AndExpr*>(e)) {
        return string("(") + process(de->getLhs()) + " /\\ "
            + process(de->getRhs()) + ")";
    }
    else if (const OrExpr* de = dynamic_cast<const OrExpr*>(e)) {
        return string("(") + process(de->getLhs()) + " \\/ "
            + process(de->getRhs()) + ")";
    }
    else if (const ImpExpr* de = dynamic_cast<const ImpExpr*>(e)) {
        return string("(") + process(de->getLhs()) + " -> "
            + process(de->getRhs()) + ")";
    }
    else if (const EqvExpr* de = dynamic_cast<const EqvExpr*>(e)) {
        return string("(") + process(de->getLhs()) + " <-> "
            + process(de->getRhs()) + ")";
    }
    else if (const NotExpr* de = dynamic_cast<const NotExpr*>(e)) {
        return string("not ") + process(de->getExpr());
    }
    else if (const InSetExpr* de = dynamic_cast<const InSetExpr*>(e)) {
        return string("(") + process(de->getLhs()) + " in "
            + process(de->getRhs()) + ")";
    }
    else {
        THROW(Exception, "BoolExpr of unknown derived class");
    }
}

string
ConstraintProcessor::process(const NumExpr* e) {
    if (const PlusExpr* de = dynamic_cast<const PlusExpr*>(e)) {
        return string("(") + process(de->getLhs()) + " + "
            + process(de->getRhs()) + ")";
    }
    else if (const MinusExpr* de = dynamic_cast<const MinusExpr*>(e)) {
        return string("(") + process(de->getLhs()) + " - "
            + process(de->getRhs()) + ")";
    }
    else if (const AnIntegerExpr* de = dynamic_cast<const AnIntegerExpr*>(e)) {
        return Utils::toString(de->getValue());
    }
    else if (const NodeIdToNumExpr* de =
             dynamic_cast<const NodeIdToNumExpr*>(e))
    {
        return process(de->getExpr());
    }
    else if (const InstanceIdToNumExpr* de =
             dynamic_cast<const InstanceIdToNumExpr*>(e))
    {
        return process(de->getExpr());
    }
    else if (const InstructionIdToNumExpr* de =
             dynamic_cast<const InstructionIdToNumExpr*>(e))
    {
        return process(de->getExpr());
    }
    else if (const PatternIdToNumExpr* de =
             dynamic_cast<const PatternIdToNumExpr*>(e))
    {
        return process(de->getExpr());
    }
    else if (const LabelIdToNumExpr* de =
             dynamic_cast<const LabelIdToNumExpr*>(e))
    {
        return process(de->getExpr());
    }
    else if (const RegisterIdToNumExpr* de =
             dynamic_cast<const RegisterIdToNumExpr*>(e))
    {
        return process(de->getExpr());
    }
    else {
        THROW(Exception, "NumExpr of unknown derived class");
    }
}

string
ConstraintProcessor::process(const NodeIdExpr* e) {
    if (const ANodeIdExpr* de = dynamic_cast<const ANodeIdExpr*>(e)) {
        const Id& id = de->getId();
        if (p_.isActionNode(id)) {
            return Utils::toString(p_.getIndexOfActionNode(id));
        }
        else if (p_.isEntityNode(id)) {
            return Utils::toString(p_.getIndexOfEntityNode(id));
        }
        else if (p_.isLabelNode(id)) {
            return Utils::toString(p_.getIndexOfLabelNode(id));
        }
        else {
            THROW(Exception, "Node ID not found");
        }
    }
    else {
        THROW(Exception, "NodeIdExpr of unknown derived class");
    }
}

string
ConstraintProcessor::process(const InstanceIdExpr* e) {
    if (const AnInstanceIdExpr* de = dynamic_cast<const AnInstanceIdExpr*>(e)) {
        return Utils::toString(de->getId());
    }
    else if (dynamic_cast<const ThisInstanceIdExpr*>(e)) {
        return Utils::toString(p_.getIndexOfInstance(instance_id_));
    }
    else if (const CovererOfActionNodeExpr* de =
             dynamic_cast<const CovererOfActionNodeExpr*>(e))
    {
        return getActionCovererArrayString()
            + "[" + process(de->getExpr()) + "]";
    }
    else if (const DefinerOfEntityNodeExpr* de =
             dynamic_cast<const DefinerOfEntityNodeExpr*>(e))
    {
        return getEntityDefinerArrayString()
            + "[" + process(de->getExpr()) + "]";
    }
    else {
        THROW(Exception, "InstanceIdExpr of unknown derived class");
    }
}

string
ConstraintProcessor::process(const InstructionIdExpr* e) {
    if (dynamic_cast<const AnInstructionIdExpr*>(e)) {
        // TODO: fix implementation
        return "?";
    }
    else if (dynamic_cast<const InstructionIdOfPatternExpr*>(e)) {
        // TODO: fix implementation
        return "?";
    }
    else {
        THROW(Exception, "InstructionIdExpr of unknown derived class");
    }
}

string
ConstraintProcessor::process(const PatternIdExpr* e) {
    if (dynamic_cast<const APatternIdExpr*>(e)) {
        // TODO: fix implementation
        return "?";
    }
    else if (dynamic_cast<const PatternIdOfInstanceExpr*>(e)) {
        // TODO: fix implementation
        return "?";
    }
    else {
        THROW(Exception, "PatternIdExpr of unknown derived class");
    }
}

string
ConstraintProcessor::process(const LabelIdExpr* e) {
    if (const LabelIdAllocatedToInstanceExpr* de =
        dynamic_cast<const LabelIdAllocatedToInstanceExpr*>(e))
    {
        return getBBAllocationArrayString()
            + "[" + process(de->getExpr()) + "]";
    }
    else if (const LabelIdOfLabelNodeExpr* de =
             dynamic_cast<const LabelIdOfLabelNodeExpr*>(e))
    {
        return process(de->getExpr());
    }
    else {
        THROW(Exception, "LabelIdExpr of unknown derived class");
    }
}

string
ConstraintProcessor::process(const RegisterIdExpr* e) {
    if (dynamic_cast<const ARegisterIdExpr*>(e)) {
        // TODO: fix implementation
        return "?";
    }
    else if (dynamic_cast<const RegisterIdAllocatedToDataNodeExpr*>(e)) {
        // TODO: fix implementation
        return "?";
    }
    else {
        THROW(Exception, "PatternIdExpr of unknown derived class");
    }
}

string
ConstraintProcessor::process(const SetExpr* e) {
    if (const UnionSetExpr* de = dynamic_cast<const UnionSetExpr*>(e)) {
        return string("(") + process(de->getLhs()) + " union "
            + process(de->getRhs()) + ")";
    }
    else if (const IntersectSetExpr* de =
             dynamic_cast<const IntersectSetExpr*>(e))
    {
        return string("(") + process(de->getLhs()) + " intersect "
            + process(de->getRhs()) + ")";
    }
    else if (const DiffSetExpr* de = dynamic_cast<const DiffSetExpr*>(e)) {
        return string("(") + process(de->getLhs()) + " diff "
            + process(de->getRhs()) + ")";
    }
    else if (const DomSetOfLabelIdExpr* de =
             dynamic_cast<const DomSetOfLabelIdExpr*>(e))
    {
        return getDomSetString()
            + "[" + process(de->getExpr()) + "]";
    }
    else {
        THROW(Exception, "PatternIdExpr of unknown derived class");
    }
}

string
ConstraintProcessor::process(const SetElemExpr* e) {
    if (const LabelIdToSetElemExpr* de =
        dynamic_cast<const LabelIdToSetElemExpr*>(e))
    {
        return process(de->getExpr());
    }
    else if (const RegisterIdToSetElemExpr* de =
        dynamic_cast<const RegisterIdToSetElemExpr*>(e))
    {
        return process(de->getExpr());
    }
    else {
        THROW(Exception, "PatternIdExpr of unknown derived class");
    }
}

string
ConstraintProcessor::getActionCovererArrayString(void) const {
    return "an_cov";
}

string
ConstraintProcessor::getEntityDefinerArrayString(void) const {
    return "en_def";
}

string
ConstraintProcessor::getBBAllocationArrayString(void) const {
    return "pi_bb";
}

string
ConstraintProcessor::getDomSetString(void) const {
    return "funcLabelDomsets";
}

string
ConstraintProcessor::getInstanceSelectedArrayString(void) const {
    return "pi_sel";
}
