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
#include "common/utils/string.h"
#include <list>

using namespace Model;
using std::list;
using std::string;

ConstraintProcessor::ConstraintProcessor(void) {}

ConstraintProcessor::~ConstraintProcessor(void) {}

string
ConstraintProcessor::processConstraintForF(const Constraint* c) {
    return string("constraint\n") + process(c) + ";";
}

string
ConstraintProcessor::processConstraintForMatch(
    const Constraint* c,
    const ArrayIndex& i
) {
    return string("constraint\n") + getMatchSelectedVariableArrayName() + "["
        + Utils::toString(i) + "] -> " + process(c) + ";";
}

string
ConstraintProcessor::process(const Constraint* c) {
    if (const BoolExprConstraint* dc =
        dynamic_cast<const BoolExprConstraint*>(c))
    {
        return process(dc->getExpr());
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
    else if (const DataNodeIsAnIntConstantExpr* de =
             dynamic_cast<const DataNodeIsAnIntConstantExpr*>(e))
    {
        return getDataRegisterVariableArrayName() + "[" + process(de->getExpr())
            + "] == " + getRegValueForImm();
    }
    else if (const DataNodeIsIntermediateExpr* de =
             dynamic_cast<const DataNodeIsIntermediateExpr*>(e))
    {
        return getDataRegisterVariableArrayName() + "[" + process(de->getExpr())
            + "] == " + getRegValueForNoReuse();
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
    else if (const NodeToNumExpr* de =
             dynamic_cast<const NodeToNumExpr*>(e))
    {
        return process(de->getExpr());
    }
    else if (const MatchToNumExpr* de =
             dynamic_cast<const MatchToNumExpr*>(e))
    {
        return process(de->getExpr());
    }
    else if (const InstructionToNumExpr* de =
             dynamic_cast<const InstructionToNumExpr*>(e))
    {
        return process(de->getExpr());
    }
    else if (const LabelToNumExpr* de =
             dynamic_cast<const LabelToNumExpr*>(e))
    {
        return process(de->getExpr());
    }
    else if (const RegisterToNumExpr* de =
             dynamic_cast<const RegisterToNumExpr*>(e))
    {
        return process(de->getExpr());
    }
    else if (const DistanceBetweenMatchAndLabelExpr* de =
             dynamic_cast<const DistanceBetweenMatchAndLabelExpr*>(e))
    {
        return getMatchLabelDistsVariableArrayName()
            + "[" + getMatchAndLabelMappingsMatrixName() + "["
            + process(de->getLhs()) + "," + process(de->getRhs()) + "]]";
    }
    else {
        THROW(Exception, "NumExpr is of unknown derived class");
    }
}

string
ConstraintProcessor::process(const NodeExpr* e) {
    if (const ANodeArrayIndexExpr* de =
        dynamic_cast<const ANodeArrayIndexExpr*>(e))
    {
        return Utils::toString(de->getArrayIndex());
    }
    else if (dynamic_cast<const ANodeIDExpr*>(e)) {
        THROW(Exception, "ANodeIDExpr is not allowed here");
    }
    else {
        THROW(Exception, "NodeExpr is of unknown derived class");
    }
}

string
ConstraintProcessor::process(const IntExpr* e) {
    if (const AnIntegerExpr* de = dynamic_cast<const AnIntegerExpr*>(e)) {
        return Utils::toString(de->getValue());
    }
    else if (const IntConstValueOfDataNodeExpr* de =
             dynamic_cast<const IntConstValueOfDataNodeExpr*>(e))
    {
        return getDataImmediateValuesVariableArrayName() + "["
            + process(de->getExpr()) + "]";
    }
    else {
        THROW(Exception, "IntExpr is of unknown derived class");
    }
}

string
ConstraintProcessor::process(const MatchExpr* e) {
    if (const AMatchArrayIndexExpr* de =
        dynamic_cast<const AMatchArrayIndexExpr*>(e))
    {
        return Utils::toString(de->getArrayIndex());
    }
    else if (dynamic_cast<const AMatchIDExpr*>(e)) {
        THROW(Exception, "AMatchIDExpr is not allowed here");
    }
    else if (dynamic_cast<const ThisMatchExpr*>(e)) {
        THROW(Exception, "ThisMatchExpr is not allowed here");
    }
    else if (const CovererOfOperationNodeExpr* de =
             dynamic_cast<const CovererOfOperationNodeExpr*>(e))
    {
        return getOperationCovererVariableArrayName()
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
        THROW(Exception, "MatchExpr is of unknown derived class");
    }
}

string
ConstraintProcessor::process(const InstructionExpr* e) {
    if (const AnInstructionArrayIndexExpr* de =
        dynamic_cast<const AnInstructionArrayIndexExpr*>(e))
    {
        return Utils::toString(de->getArrayIndex());
    }
    else if (dynamic_cast<const AnInstructionIDExpr*>(e)) {
        THROW(Exception, "AnInstructionIDExpr is not allowed here");
    }
    else if (dynamic_cast<const InstructionOfMatchExpr*>(e)) {
        // TODO: implement
        THROW(Exception, "Support for InstructionOfMatchExpr not implemented");
    }
    else {
        THROW(Exception, "InstructionExpr is of unknown derived class");
    }
}

string
ConstraintProcessor::process(const LabelExpr* e) {
    if (const LabelAllocatedToMatchExpr* de =
        dynamic_cast<const LabelAllocatedToMatchExpr*>(e))
    {
        return getBBAllocationVariableArrayName()
            + "[" + process(de->getExpr()) + "]";
    }
    else if (const LabelOfLabelNodeExpr* de =
             dynamic_cast<const LabelOfLabelNodeExpr*>(e))
    {
        return process(de->getExpr());
    }
    else {
        THROW(Exception, "LabelExpr is of unknown derived class");
    }
}

string
ConstraintProcessor::process(const RegisterExpr* e) {
    if (const ARegisterArrayIndexExpr* de =
        dynamic_cast<const ARegisterArrayIndexExpr*>(e))
    {
        return Utils::toString(de->getArrayIndex());
    }
    else if (dynamic_cast<const ARegisterIDExpr*>(e)) {
        THROW(Exception, "ARegisterIDExpr is not allowed here");
    }
    else if (const RegisterAllocatedToDataNodeExpr* de =
             dynamic_cast<const RegisterAllocatedToDataNodeExpr*>(e))
    {
        return getDataRegisterVariableArrayName()
            + "[" + process(de->getExpr()) + "]";
    }
    else {
        THROW(Exception, "RegisterExpr is of unknown derived class");
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
    else if (const DomSetOfLabelExpr* de =
             dynamic_cast<const DomSetOfLabelExpr*>(e))
    {
        return getDomSetParameterArrayName()
            + "[" + process(de->getExpr()) + "]";
    }
    else if (const RegisterClassExpr* de =
             dynamic_cast<const RegisterClassExpr*>(e))
    {
        string result("{");
        list<string> subresults;
        for (auto& expr : de->getExprList()) {
            subresults.push_back(process(expr));
        }
        result += Utils::join(subresults, ", ");
        result += "}";
        return result;
    }
    else {
        THROW(Exception, "SetExpr is of unknown derived class");
    }
}

string
ConstraintProcessor::process(const SetElemExpr* e) {
    if (const LabelToSetElemExpr* de =
        dynamic_cast<const LabelToSetElemExpr*>(e))
    {
        return process(de->getExpr());
    }
    else if (const RegisterToSetElemExpr* de =
        dynamic_cast<const RegisterToSetElemExpr*>(e))
    {
        return process(de->getExpr());
    }
    else {
        THROW(Exception, "SetElemExpr is of unknown derived class");
    }
}

string
ConstraintProcessor::getOperationCovererVariableArrayName(void) const {
    return "cov";
}

string
ConstraintProcessor::getDataDefinerVariableArrayName(void) const {
    return "ddefm";
}

string
ConstraintProcessor::getDataRegisterVariableArrayName(void) const {
    return "reg";
}

string
ConstraintProcessor::getDataImmediateValuesVariableArrayName(void) const {
    return "cnst";
}

string
ConstraintProcessor::getStateDefinerVariableArrayName(void) const {
    return "sdefm";
}

string
ConstraintProcessor::getBBAllocationVariableArrayName(void) const {
    return "bb";
}

string
ConstraintProcessor::getDomSetParameterArrayName(void) const {
    return "domsetOfLabelInFunction";
}

string
ConstraintProcessor::getMatchSelectedVariableArrayName(void) const {
    return "sel";
}

string
ConstraintProcessor::getMatchLabelDistsVariableArrayName(void) const {
    return "dist";
}

string
ConstraintProcessor::getMatchAndLabelMappingsMatrixName(void) const {
    return "indexOfMatchLabelMapping";
}

string
ConstraintProcessor::getRegValueForImm(void) const {
    return "regValueForImm";
}

string
ConstraintProcessor::getRegValueForNoReuse(void) const {
    return "regValueForNoReuse";
}
