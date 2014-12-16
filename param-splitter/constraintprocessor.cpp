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
#include <list>

using namespace Model;
using std::list;

ConstraintProcessor::ConstraintProcessor(const Params& p)
    : p_(p),
      is_processing_constraint_(false)
{}

ConstraintProcessor::~ConstraintProcessor(void) {}

Constraint*
ConstraintProcessor::processConstraintForMatch(
    const Constraint* c,
    const ID& id
) {
    is_processing_constraint_ = true;
    mid_ = id;

    if (const BoolExprConstraint* dc =
        dynamic_cast<const BoolExprConstraint*>(c))
    {
        return new BoolExprConstraint(processBoolExpr(dc->getExpr()));
    }
    else {
        THROW(Exception, "Constraint is of unknown derived class");
    }
}

Constraint*
ConstraintProcessor::processConstraintForF(const Constraint* c) {
    if (const BoolExprConstraint* dc =
        dynamic_cast<const BoolExprConstraint*>(c))
    {
        return new BoolExprConstraint(processBoolExpr(dc->getExpr()));
    }
    else {
        THROW(Exception, "Constraint is of unknown derived class");
    }
}

BoolExpr*
ConstraintProcessor::processBoolExpr(const BoolExpr* e) {
    if (const EqExpr* de = dynamic_cast<const EqExpr*>(e)) {
        return new EqExpr(processNumExpr(de->getLhs()),
                          processNumExpr(de->getRhs()));
    }
    else if (const NeqExpr* de = dynamic_cast<const NeqExpr*>(e)) {
        return new NeqExpr(processNumExpr(de->getLhs()),
                           processNumExpr(de->getRhs()));
    }
    else if (const GTExpr* de = dynamic_cast<const GTExpr*>(e)) {
        return new GTExpr(processNumExpr(de->getLhs()),
                          processNumExpr(de->getRhs()));
    }
    else if (const GEExpr* de = dynamic_cast<const GEExpr*>(e)) {
        return new GEExpr(processNumExpr(de->getLhs()),
                          processNumExpr(de->getRhs()));
    }
    else if (const LTExpr* de = dynamic_cast<const LTExpr*>(e)) {
        return new LTExpr(processNumExpr(de->getLhs()),
                          processNumExpr(de->getRhs()));
    }
    else if (const LEExpr* de = dynamic_cast<const LEExpr*>(e)) {
        return new LEExpr(processNumExpr(de->getLhs()),
                          processNumExpr(de->getRhs()));
    }
    else if (const AndExpr* de = dynamic_cast<const AndExpr*>(e)) {
        return new AndExpr(processBoolExpr(de->getLhs()),
                           processBoolExpr(de->getRhs()));
    }
    else if (const OrExpr* de = dynamic_cast<const OrExpr*>(e)) {
        return new OrExpr(processBoolExpr(de->getLhs()),
                          processBoolExpr(de->getRhs()));
    }
    else if (const ImpExpr* de = dynamic_cast<const ImpExpr*>(e)) {
        return new ImpExpr(processBoolExpr(de->getLhs()),
                           processBoolExpr(de->getRhs()));
    }
    else if (const EqvExpr* de = dynamic_cast<const EqvExpr*>(e)) {
        return new EqvExpr(processBoolExpr(de->getLhs()),
                           processBoolExpr(de->getRhs()));
    }
    else if (const NotExpr* de = dynamic_cast<const NotExpr*>(e)) {
        return new NotExpr(processBoolExpr(de->getExpr()));
    }
    else if (const InSetExpr* de = dynamic_cast<const InSetExpr*>(e)) {
        return new InSetExpr(processSetElemExpr(de->getLhs()),
                             processSetExpr(de->getRhs()));
    }
    else if (const DataNodeIsAnIntConstantExpr* de =
             dynamic_cast<const DataNodeIsAnIntConstantExpr*>(e)) {
        return new DataNodeIsAnIntConstantExpr(processNodeExpr(de->getExpr()));
    }
    else if (const MatchIsSelectedExpr* de =
             dynamic_cast<const MatchIsSelectedExpr*>(e))
    {
        return new MatchIsSelectedExpr(processMatchExpr(de->getExpr()));
    }
    else {
        THROW(Exception, "BoolExpr is of unknown derived class");
    }
}

NumExpr*
ConstraintProcessor::processNumExpr(const NumExpr* e) {
    if (const PlusExpr* de = dynamic_cast<const PlusExpr*>(e)) {
        return new PlusExpr(processNumExpr(de->getLhs()),
                            processNumExpr(de->getRhs()));
    }
    else if (const MinusExpr* de = dynamic_cast<const MinusExpr*>(e)) {
        return new MinusExpr(processNumExpr(de->getLhs()),
                             processNumExpr(de->getRhs()));
    }
    else if (const IntToNumExpr* de = dynamic_cast<const IntToNumExpr*>(e)) {
        return new IntToNumExpr(processIntExpr(de->getExpr()));
    }
    else if (const BoolToNumExpr* de = dynamic_cast<const BoolToNumExpr*>(e)) {
        return new BoolToNumExpr(processBoolExpr(de->getExpr()));
    }
    else if (const NodeToNumExpr* de =
             dynamic_cast<const NodeToNumExpr*>(e))
    {
        return new NodeToNumExpr(processNodeExpr(de->getExpr()));
    }
    else if (const MatchToNumExpr* de =
             dynamic_cast<const MatchToNumExpr*>(e))
    {
        return new MatchToNumExpr(processMatchExpr(de->getExpr()));
    }
    else if (const InstructionToNumExpr* de =
             dynamic_cast<const InstructionToNumExpr*>(e))
    {
        return new InstructionToNumExpr(processInstructionExpr(de->getExpr()));
    }
    else if (const PatternToNumExpr* de =
             dynamic_cast<const PatternToNumExpr*>(e))
    {
        return new PatternToNumExpr(processPatternExpr(de->getExpr()));
    }
    else if (const LabelToNumExpr* de =
             dynamic_cast<const LabelToNumExpr*>(e))
    {
        return new LabelToNumExpr(processLabelExpr(de->getExpr()));
    }
    else if (const RegisterToNumExpr* de =
             dynamic_cast<const RegisterToNumExpr*>(e))
    {
        return new RegisterToNumExpr(processRegisterExpr(de->getExpr()));
    }
    else if (const DistanceBetweenMatchAndLabelExpr* de =
             dynamic_cast<const DistanceBetweenMatchAndLabelExpr*>(e))
    {
        return new DistanceBetweenMatchAndLabelExpr(
            processMatchExpr(de->getLhs()),
            processLabelExpr(de->getRhs()));
    }
    else {
        THROW(Exception, "NumExpr is of unknown derived class");
    }
}

NodeExpr*
ConstraintProcessor::processNodeExpr(const NodeExpr* e) {
    if (const ANodeIDExpr* de = dynamic_cast<const ANodeIDExpr*>(e)) {
        const ID& id = de->getID();
        ArrayIndex i;
        if (p_.isOperationNodeInF(id)) {
            i = p_.getIndexForOperationNodeInF(id);
        }
        else if (p_.isDataNodeInF(id)) {
            i = p_.getIndexForDataNodeInF(id);
        }
        else if (p_.isStateNodeInF(id)) {
            i = p_.getIndexForStateNodeInF(id);
        }
        else if (p_.isLabelNodeInF(id)) {
            i = p_.getIndexForLabelNodeInF(id);
        }
        else {
            THROW(Exception, "Node ID not found");
        }
        return new ANodeArrayIndexExpr(i);
    }
    else {
        THROW(Exception, "NodeExpr is of unknown derived class");
    }
}

IntExpr*
ConstraintProcessor::processIntExpr(const IntExpr* e) {
    if (const AnIntegerExpr* de = dynamic_cast<const AnIntegerExpr*>(e)) {
        return new AnIntegerExpr(de->getValue());
    }
    else if (const IntConstValueOfDataNodeExpr* de =
             dynamic_cast<const IntConstValueOfDataNodeExpr*>(e))
    {
        return new IntConstValueOfDataNodeExpr(processNodeExpr(de->getExpr()));
    }
    else {
        THROW(Exception, "IntExpr is of unknown derived class");
    }
}

MatchExpr*
ConstraintProcessor::processMatchExpr(const MatchExpr* e) {
    if (const AMatchIDExpr* de =
        dynamic_cast<const AMatchIDExpr*>(e))
    {
        return new AMatchArrayIndexExpr(
            p_.getIndexForMatch(de->getID()));
    }
    else if (dynamic_cast<const ThisMatchExpr*>(e)) {
        if (!is_processing_constraint_) {
            THROW(Exception, "ThisMatchExpr is only allowed to be "
                             "used within pattern instance constraints");
        }
        return new AMatchArrayIndexExpr(p_.getIndexForMatch(mid_));
    }
    else if (const CovererOfOperationNodeExpr* de =
             dynamic_cast<const CovererOfOperationNodeExpr*>(e))
    {
        return new CovererOfOperationNodeExpr(processNodeExpr(de->getExpr()));
    }
    else if (const DefinerOfDataNodeExpr* de =
             dynamic_cast<const DefinerOfDataNodeExpr*>(e))
    {
        return new DefinerOfDataNodeExpr(processNodeExpr(de->getExpr()));
    }
    else if (const DefinerOfStateNodeExpr* de =
             dynamic_cast<const DefinerOfStateNodeExpr*>(e))
    {
        return new DefinerOfStateNodeExpr(processNodeExpr(de->getExpr()));
    }
    else {
        THROW(Exception, "MatchExpr is of unknown derived class");
    }
}

InstructionExpr*
ConstraintProcessor::processInstructionExpr(const InstructionExpr* e) {
    if (dynamic_cast<const AnInstructionIDExpr*>(e)) {
        // TODO: fix implementation
        return NULL;
    }
    else if (const InstructionOfPatternExpr* de =
             dynamic_cast<const InstructionOfPatternExpr*>(e))
    {
        return new InstructionOfPatternExpr(processPatternExpr(de->getExpr()));
    }
    else {
        THROW(Exception, "InstructionExpr is of unknown derived class");
    }
}

PatternExpr*
ConstraintProcessor::processPatternExpr(const PatternExpr* e) {
    if (dynamic_cast<const APatternIDExpr*>(e)) {
        // TODO: fix implementation
        return NULL;
    }
    else if (const PatternOfMatchExpr* de =
             dynamic_cast<const PatternOfMatchExpr*>(e))
    {
        return new PatternOfMatchExpr(processMatchExpr(de->getExpr()));
    }
    else {
        THROW(Exception, "PatternExpr is of unknown derived class");
    }
}

LabelExpr*
ConstraintProcessor::processLabelExpr(const LabelExpr* e) {
    if (const LabelAllocatedToMatchExpr* de =
        dynamic_cast<const LabelAllocatedToMatchExpr*>(e))
    {
        return new LabelAllocatedToMatchExpr(processMatchExpr(de->getExpr()));
    }
    else if (const LabelOfLabelNodeExpr* de =
             dynamic_cast<const LabelOfLabelNodeExpr*>(e))
    {
        return new LabelOfLabelNodeExpr(processNodeExpr(de->getExpr()));
    }
    else {
        THROW(Exception, "LabelExpr is of unknown derived class");
    }
}

RegisterExpr*
ConstraintProcessor::processRegisterExpr(const RegisterExpr* e) {
    if (const ARegisterIDExpr* de = dynamic_cast<const ARegisterIDExpr*>(e)) {
        return new ARegisterArrayIndexExpr(
            p_.getIndexForRegisterInM(de->getID()));
    }
    else if (const RegisterAllocatedToDataNodeExpr* de =
             dynamic_cast<const RegisterAllocatedToDataNodeExpr*>(e))
    {
        return new RegisterAllocatedToDataNodeExpr(
            processNodeExpr(de->getExpr()));
    }
    else {
        THROW(Exception, "PatternExpr is of unknown derived class");
    }
}

SetExpr*
ConstraintProcessor::processSetExpr(const SetExpr* e) {
    if (const UnionSetExpr* de = dynamic_cast<const UnionSetExpr*>(e)) {
        return new UnionSetExpr(processSetExpr(de->getLhs()),
                                processSetExpr(de->getRhs()));
    }
    else if (const IntersectSetExpr* de =
             dynamic_cast<const IntersectSetExpr*>(e))
    {
        return new IntersectSetExpr(processSetExpr(de->getLhs()),
                                    processSetExpr(de->getRhs()));
    }
    else if (const DiffSetExpr* de = dynamic_cast<const DiffSetExpr*>(e)) {
        return new DiffSetExpr(processSetExpr(de->getLhs()),
                               processSetExpr(de->getRhs()));
    }
    else if (const DomSetOfLabelExpr* de =
             dynamic_cast<const DomSetOfLabelExpr*>(e))
    {
        return new DomSetOfLabelExpr(processLabelExpr(de->getExpr()));
    }
    else if (const RegisterClassExpr* de =
             dynamic_cast<const RegisterClassExpr*>(e))
    {
        list<const RegisterExpr*> new_exprs;
        for (auto expr : de->getExprList()) {
            new_exprs.push_back(processRegisterExpr(expr));
        }
        return new RegisterClassExpr(new_exprs);
    }
    else {
        THROW(Exception, "SetExpr is of unknown derived class");
    }
}

SetElemExpr*
ConstraintProcessor::processSetElemExpr(const SetElemExpr* e) {
    if (const LabelToSetElemExpr* de =
        dynamic_cast<const LabelToSetElemExpr*>(e))
    {
        return new LabelToSetElemExpr(processLabelExpr(de->getExpr()));
    }
    else if (const RegisterToSetElemExpr* de =
        dynamic_cast<const RegisterToSetElemExpr*>(e))
    {
        return new RegisterToSetElemExpr(processRegisterExpr(de->getExpr()));
    }
    else {
        THROW(Exception, "SetElemExpr is of unknown derived class");
    }
}
