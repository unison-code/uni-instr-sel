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

ConstraintProcessor::ConstraintProcessor(const Model::Params& p)
    : p_(p),
      is_processing_pi_constraint_(false)
{}

ConstraintProcessor::~ConstraintProcessor(void) {}

string
ConstraintProcessor::processConstraintForPI(const Constraint* c, const ID& id) {
    is_processing_pi_constraint_ = true;
    piid_ = id;
    string constraint_str =
        string("constraint\n")
        + getInstanceSelectedVariableArrayName() + "["
        + Utils::toString(p_.getIndexForPI(piid_))
        + "] -> " + process(c) + ";";
    is_processing_pi_constraint_ = false;
    return constraint_str;
}

string
ConstraintProcessor::processConstraintForF(const Constraint* c) {
    return string("constraint\n") + process(c) + ";";
}

string
ConstraintProcessor::process(const Constraint* c) {
    if (const BoolExprConstraint* dc =
        dynamic_cast<const BoolExprConstraint*>(c))
    {
        return process(dc->getExpr());
    }
    else if (dynamic_cast<const DataNodeIsIntConstantConstraint*>(c))
    {
        // TODO: fix implementation
        return "?";
    }
    else {
        THROW(Exception, "Constraint is of unknown derived class");
    }
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
        THROW(Exception, "BoolExpr is of unknown derived class");
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
    else if (const IntToNumExpr* de = dynamic_cast<const IntToNumExpr*>(e)) {
        return process(de->getExpr());
    }
    else if (const BoolToNumExpr* de = dynamic_cast<const BoolToNumExpr*>(e)) {
        return string("bool2int(") + process(de->getExpr()) + ")";
    }
    else if (const NodeIDToNumExpr* de =
             dynamic_cast<const NodeIDToNumExpr*>(e))
    {
        return process(de->getExpr());
    }
    else if (const PatternInstanceIDToNumExpr* de =
             dynamic_cast<const PatternInstanceIDToNumExpr*>(e))
    {
        return process(de->getExpr());
    }
    else if (const InstructionIDToNumExpr* de =
             dynamic_cast<const InstructionIDToNumExpr*>(e))
    {
        return process(de->getExpr());
    }
    else if (const PatternIDToNumExpr* de =
             dynamic_cast<const PatternIDToNumExpr*>(e))
    {
        return process(de->getExpr());
    }
    else if (const LabelIDToNumExpr* de =
             dynamic_cast<const LabelIDToNumExpr*>(e))
    {
        return process(de->getExpr());
    }
    else if (const RegisterIDToNumExpr* de =
             dynamic_cast<const RegisterIDToNumExpr*>(e))
    {
        return process(de->getExpr());
    }
    else if (const DistanceBetweenInstanceAndLabelExpr* de =
             dynamic_cast<const DistanceBetweenInstanceAndLabelExpr*>(e))
    {
        return getInstanceLabelDistsVariableArrayName()
            + "[" + getInstanceAndLabelMappingsMatrixName() + "["
            + process(de->getLhs()) + "," + process(de->getRhs()) + "]]";
    }
    else {
        THROW(Exception, "NumExpr is of unknown derived class");
    }
}

string
ConstraintProcessor::process(const NodeIDExpr* e) {
    if (const ANodeIDExpr* de = dynamic_cast<const ANodeIDExpr*>(e)) {
        const ID& id = de->getID();
        if (p_.isActionNodeInF(id)) {
            return Utils::toString(p_.getIndexForActionNodeInF(id));
        }
        else if (p_.isDataNodeInF(id)) {
            return Utils::toString(p_.getIndexForDataNodeInF(id));
        }
        else if (p_.isStateNodeInF(id)) {
            return Utils::toString(p_.getIndexForStateNodeInF(id));
        }
        else if (p_.isLabelNodeInF(id)) {
            return Utils::toString(p_.getIndexForLabelNodeInF(id));
        }
        else {
            THROW(Exception, "Node ID not found");
        }
    }
    else {
        THROW(Exception, "NodeIDExpr is of unknown derived class");
    }
}

string
ConstraintProcessor::process(const IntExpr* e) {
    if (const AnIntegerExpr* de = dynamic_cast<const AnIntegerExpr*>(e)) {
        return Utils::toString(de->getValue());
    }
    else if (dynamic_cast<const IntConstValueOfDataNodeExpr*>(e)) {
        // TODO: fix implementation
        return "?";
    }
    else {
        THROW(Exception, "NodeIDExpr is of unknown derived class");
    }
}

string
ConstraintProcessor::process(const PatternInstanceIDExpr* e) {
    if (const APatternInstanceIDExpr* de =
        dynamic_cast<const APatternInstanceIDExpr*>(e))
    {
        return Utils::toString(p_.getIndexForPI(de->getID()));
    }
    else if (dynamic_cast<const ThisPatternInstanceIDExpr*>(e)) {
        if (!is_processing_pi_constraint_) {
            THROW(Exception, "ThisPatternInstanceIDExpr is only allowed to be "
                             "used within pattern instance constraints");
        }
        return Utils::toString(p_.getIndexForPI(piid_));
    }
    else if (const CovererOfActionNodeExpr* de =
             dynamic_cast<const CovererOfActionNodeExpr*>(e))
    {
        return getActionCovererVariableArrayName()
            + "[" + process(de->getExpr()) + "]";
    }
    else if (const DefinerOfDataNodeExpr* de =
             dynamic_cast<const DefinerOfDataNodeExpr*>(e))
    {
        return getDataDefinerVariableArrayName()
            + "[" + process(de->getExpr()) + "]";
    }
    else if (const DefinerOfStateNodeExpr* de =
             dynamic_cast<const DefinerOfStateNodeExpr*>(e))
    {
        return getStateDefinerVariableArrayName()
            + "[" + process(de->getExpr()) + "]";
    }
    else {
        THROW(Exception, "PatternInstanceIDExpr is of unknown derived class");
    }
}

string
ConstraintProcessor::process(const InstructionIDExpr* e) {
    if (dynamic_cast<const AnInstructionIDExpr*>(e)) {
        // TODO: fix implementation
        return "?";
    }
    else if (dynamic_cast<const InstructionIDOfPatternExpr*>(e)) {
        // TODO: fix implementation
        return "?";
    }
    else {
        THROW(Exception, "InstructionIDExpr is of unknown derived class");
    }
}

string
ConstraintProcessor::process(const PatternIDExpr* e) {
    if (dynamic_cast<const APatternIDExpr*>(e)) {
        // TODO: fix implementation
        return "?";
    }
    else if (dynamic_cast<const PatternIDOfInstanceExpr*>(e)) {
        // TODO: fix implementation
        return "?";
    }
    else {
        THROW(Exception, "PatternIDExpr is of unknown derived class");
    }
}

string
ConstraintProcessor::process(const LabelIDExpr* e) {
    if (const LabelIDAllocatedToInstanceExpr* de =
        dynamic_cast<const LabelIDAllocatedToInstanceExpr*>(e))
    {
        return getBBAllocationVariableArrayName()
            + "[" + process(de->getExpr()) + "]";
    }
    else if (const LabelIDOfLabelNodeExpr* de =
             dynamic_cast<const LabelIDOfLabelNodeExpr*>(e))
    {
        return process(de->getExpr());
    }
    else {
        THROW(Exception, "LabelIDExpr is of unknown derived class");
    }
}

string
ConstraintProcessor::process(const RegisterIDExpr* e) {
    if (const ARegisterIDExpr* de = dynamic_cast<const ARegisterIDExpr*>(e)) {
        return Utils::toString(p_.getIndexForRegisterInM(de->getID()));
    }
    else if (const RegisterIDAllocatedToDataNodeExpr* de =
             dynamic_cast<const RegisterIDAllocatedToDataNodeExpr*>(e))
    {
        return getDataRegisterVariableArrayName()
            + "[" + process(de->getExpr()) + "]";
    }
    else {
        THROW(Exception, "PatternIDExpr is of unknown derived class");
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
    else if (const DomSetOfLabelIDExpr* de =
             dynamic_cast<const DomSetOfLabelIDExpr*>(e))
    {
        return getDomSetParameterArrayName()
            + "[" + process(de->getExpr()) + "]";
    }
    else {
        THROW(Exception, "PatternIDExpr is of unknown derived class");
    }
}

string
ConstraintProcessor::process(const SetElemExpr* e) {
    if (const LabelIDToSetElemExpr* de =
        dynamic_cast<const LabelIDToSetElemExpr*>(e))
    {
        return process(de->getExpr());
    }
    else if (const RegisterIDToSetElemExpr* de =
        dynamic_cast<const RegisterIDToSetElemExpr*>(e))
    {
        return process(de->getExpr());
    }
    else {
        THROW(Exception, "PatternIDExpr is of unknown derived class");
    }
}

string
ConstraintProcessor::getActionCovererVariableArrayName(void) const {
    return "an_cov";
}

string
ConstraintProcessor::getDataDefinerVariableArrayName(void) const {
    return "dn_def";
}

string
ConstraintProcessor::getDataRegisterVariableArrayName(void) const {
    return "dn_reg";
}

string
ConstraintProcessor::getStateDefinerVariableArrayName(void) const {
    return "sn_def";
}

string
ConstraintProcessor::getBBAllocationVariableArrayName(void) const {
    return "pi_bb";
}

string
ConstraintProcessor::getDomSetParameterArrayName(void) const {
    return "funcLabelDomsets";
}

string
ConstraintProcessor::getInstanceSelectedVariableArrayName(void) const {
    return "pi_sel";
}

string
ConstraintProcessor::getInstanceLabelDistsVariableArrayName(void) const {
    return "br_bb_dists";
}

string
ConstraintProcessor::getInstanceAndLabelMappingsMatrixName(void) const {
    return "patInstAndLabelMappings";
}
