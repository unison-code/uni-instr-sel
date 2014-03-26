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

#include "constraints.h"
#include "constraintvisitor.h"
#include "../exceptions/exception.h"

using namespace Model;

ConstraintVisitor::ConstraintVisitor(void) {}

ConstraintVisitor::~ConstraintVisitor(void) {}

void
ConstraintVisitor::beforeVisit(const Expr& e) {
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const Expr& e) {
    atVisit(e);
}

void
ConstraintVisitor::betweenChildrenVisits(const Expr& e) {
    atBetweenChildrenVisits(e);
}

void
ConstraintVisitor::afterVisit(const Expr& e) {
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const BoolExpr& e) {
    beforeVisit(static_cast<const Expr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const BoolExpr& e) {
    visit(static_cast<const Expr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::betweenChildrenVisits(const BoolExpr& e) {
    betweenChildrenVisits(static_cast<const Expr&>(e));
    atBetweenChildrenVisits(e);
}

void
ConstraintVisitor::afterVisit(const BoolExpr& e) {
    afterVisit(static_cast<const Expr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const NumExpr& e) {
    beforeVisit(static_cast<const Expr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const NumExpr& e) {
    visit(static_cast<const Expr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::betweenChildrenVisits(const NumExpr& e) {
    betweenChildrenVisits(static_cast<const Expr&>(e));
    atBetweenChildrenVisits(e);
}

void
ConstraintVisitor::afterVisit(const NumExpr& e) {
    afterVisit(static_cast<const Expr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const EqExpr& e) {
    beforeVisit(static_cast<const BoolExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const EqExpr& e) {
    visit(static_cast<const BoolExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::betweenChildrenVisits(const EqExpr& e) {
    betweenChildrenVisits(static_cast<const BoolExpr&>(e));
    atBetweenChildrenVisits(e);
}

void
ConstraintVisitor::afterVisit(const EqExpr& e) {
    afterVisit(static_cast<const BoolExpr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const NeqExpr& e) {
    beforeVisit(static_cast<const BoolExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const NeqExpr& e) {
    visit(static_cast<const BoolExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::betweenChildrenVisits(const NeqExpr& e) {
    betweenChildrenVisits(static_cast<const BoolExpr&>(e));
    atBetweenChildrenVisits(e);
}

void
ConstraintVisitor::afterVisit(const NeqExpr& e) {
    afterVisit(static_cast<const BoolExpr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const GTExpr& e) {
    beforeVisit(static_cast<const BoolExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const GTExpr& e) {
    visit(static_cast<const BoolExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::betweenChildrenVisits(const GTExpr& e) {
    betweenChildrenVisits(static_cast<const BoolExpr&>(e));
    atBetweenChildrenVisits(e);
}

void
ConstraintVisitor::afterVisit(const GTExpr& e) {
    afterVisit(static_cast<const BoolExpr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const GEExpr& e) {
    beforeVisit(static_cast<const BoolExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const GEExpr& e) {
    visit(static_cast<const BoolExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::betweenChildrenVisits(const GEExpr& e) {
    betweenChildrenVisits(static_cast<const BoolExpr&>(e));
    atBetweenChildrenVisits(e);
}

void
ConstraintVisitor::afterVisit(const GEExpr& e) {
    afterVisit(static_cast<const BoolExpr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const LTExpr& e) {
    beforeVisit(static_cast<const BoolExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const LTExpr& e) {
    visit(static_cast<const BoolExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::betweenChildrenVisits(const LTExpr& e) {
    betweenChildrenVisits(static_cast<const BoolExpr&>(e));
    atBetweenChildrenVisits(e);
}

void
ConstraintVisitor::afterVisit(const LTExpr& e) {
    afterVisit(static_cast<const BoolExpr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const LEExpr& e) {
    beforeVisit(static_cast<const BoolExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const LEExpr& e) {
    visit(static_cast<const BoolExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::betweenChildrenVisits(const LEExpr& e) {
    betweenChildrenVisits(static_cast<const BoolExpr&>(e));
    atBetweenChildrenVisits(e);
}

void
ConstraintVisitor::afterVisit(const LEExpr& e) {
    afterVisit(static_cast<const BoolExpr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const EqvExpr& e) {
    beforeVisit(static_cast<const BoolExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const EqvExpr& e) {
    visit(static_cast<const BoolExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::betweenChildrenVisits(const EqvExpr& e) {
    betweenChildrenVisits(static_cast<const BoolExpr&>(e));
    atBetweenChildrenVisits(e);
}

void
ConstraintVisitor::afterVisit(const EqvExpr& e) {
    afterVisit(static_cast<const BoolExpr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const ImpExpr& e) {
    beforeVisit(static_cast<const BoolExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const ImpExpr& e) {
    visit(static_cast<const BoolExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::betweenChildrenVisits(const ImpExpr& e) {
    betweenChildrenVisits(static_cast<const BoolExpr&>(e));
    atBetweenChildrenVisits(e);
}

void
ConstraintVisitor::afterVisit(const ImpExpr& e) {
    afterVisit(static_cast<const BoolExpr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const AndExpr& e) {
    beforeVisit(static_cast<const BoolExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const AndExpr& e) {
    visit(static_cast<const BoolExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::betweenChildrenVisits(const AndExpr& e) {
    betweenChildrenVisits(static_cast<const BoolExpr&>(e));
    atBetweenChildrenVisits(e);
}

void
ConstraintVisitor::afterVisit(const AndExpr& e) {
    afterVisit(static_cast<const BoolExpr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const OrExpr& e) {
    beforeVisit(static_cast<const BoolExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const OrExpr& e) {
    visit(static_cast<const BoolExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::betweenChildrenVisits(const OrExpr& e) {
    betweenChildrenVisits(static_cast<const BoolExpr&>(e));
    atBetweenChildrenVisits(e);
}

void
ConstraintVisitor::afterVisit(const OrExpr& e) {
    afterVisit(static_cast<const BoolExpr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const NotExpr& e) {
    beforeVisit(static_cast<const BoolExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const NotExpr& e) {
    visit(static_cast<const BoolExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::afterVisit(const NotExpr& e) {
    afterVisit(static_cast<const BoolExpr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const PlusExpr& e) {
    beforeVisit(static_cast<const NumExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const PlusExpr& e) {
    visit(static_cast<const NumExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::betweenChildrenVisits(const PlusExpr& e) {
    betweenChildrenVisits(static_cast<const NumExpr&>(e));
    atBetweenChildrenVisits(e);
}

void
ConstraintVisitor::afterVisit(const PlusExpr& e) {
    afterVisit(static_cast<const NumExpr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const MinusExpr& e) {
    beforeVisit(static_cast<const NumExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const MinusExpr& e) {
    visit(static_cast<const NumExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::betweenChildrenVisits(const MinusExpr& e) {
    betweenChildrenVisits(static_cast<const NumExpr&>(e));
    atBetweenChildrenVisits(e);
}

void
ConstraintVisitor::afterVisit(const MinusExpr& e) {
    afterVisit(static_cast<const NumExpr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const AnIntegerExpr& e) {
    beforeVisit(static_cast<const NumExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const AnIntegerExpr& e) {
    visit(static_cast<const NumExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::afterVisit(const AnIntegerExpr& e) {
    afterVisit(static_cast<const NumExpr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const NodeIdToNumExpr& e) {
    beforeVisit(static_cast<const NumExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const NodeIdToNumExpr& e) {
    visit(static_cast<const NumExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::afterVisit(const NodeIdToNumExpr& e) {
    afterVisit(static_cast<const NumExpr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const InstanceIdToNumExpr& e) {
    beforeVisit(static_cast<const NumExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const InstanceIdToNumExpr& e) {
    visit(static_cast<const NumExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::afterVisit(const InstanceIdToNumExpr& e) {
    afterVisit(static_cast<const NumExpr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const InstructionIdToNumExpr& e) {
    beforeVisit(static_cast<const NumExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const InstructionIdToNumExpr& e) {
    visit(static_cast<const NumExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::afterVisit(const InstructionIdToNumExpr& e) {
    afterVisit(static_cast<const NumExpr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const PatternIdToNumExpr& e) {
    beforeVisit(static_cast<const NumExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const PatternIdToNumExpr& e) {
    visit(static_cast<const NumExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::afterVisit(const PatternIdToNumExpr& e) {
    afterVisit(static_cast<const NumExpr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const LabelIdToNumExpr& e) {
    beforeVisit(static_cast<const NumExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const LabelIdToNumExpr& e) {
    visit(static_cast<const NumExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::afterVisit(const LabelIdToNumExpr& e) {
    afterVisit(static_cast<const NumExpr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const RegisterIdToNumExpr& e) {
    beforeVisit(static_cast<const NumExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const RegisterIdToNumExpr& e) {
    visit(static_cast<const NumExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::afterVisit(const RegisterIdToNumExpr& e) {
    afterVisit(static_cast<const NumExpr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const AnInstanceIdExpr& e) {
    beforeVisit(static_cast<const InstanceIdExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const AnInstanceIdExpr& e) {
    visit(static_cast<const InstanceIdExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::afterVisit(const AnInstanceIdExpr& e) {}

void
ConstraintVisitor::beforeVisit(const AnInstructionIdExpr& e) {
    beforeVisit(static_cast<const InstructionIdExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const AnInstructionIdExpr& e) {
    visit(static_cast<const InstructionIdExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::afterVisit(const AnInstructionIdExpr& e) {
    afterVisit(static_cast<const InstructionIdExpr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const ANodeIdExpr& e) {
    beforeVisit(static_cast<const NodeIdExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const ANodeIdExpr& e) {
    visit(static_cast<const NodeIdExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::afterVisit(const ANodeIdExpr& e) {
    afterVisit(static_cast<const NodeIdExpr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const ALabelIdExpr& e) {
    beforeVisit(static_cast<const LabelIdExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const ALabelIdExpr& e) {
    visit(static_cast<const LabelIdExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::afterVisit(const ALabelIdExpr& e) {
    afterVisit(static_cast<const LabelIdExpr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const ARegisterIdExpr& e) {
    beforeVisit(static_cast<const RegisterIdExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const ARegisterIdExpr& e) {
    visit(static_cast<const RegisterIdExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::afterVisit(const ARegisterIdExpr& e) {
    afterVisit(static_cast<const RegisterIdExpr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const APatternIdExpr& e) {
    beforeVisit(static_cast<const PatternIdExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const APatternIdExpr& e) {
    visit(static_cast<const PatternIdExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::afterVisit(const APatternIdExpr& e) {
    afterVisit(static_cast<const PatternIdExpr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const ThisInstanceIdExpr& e) {
    beforeVisit(static_cast<const InstanceIdExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const ThisInstanceIdExpr& e) {
    visit(static_cast<const InstanceIdExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::afterVisit(const ThisInstanceIdExpr& e) {
    afterVisit(static_cast<const InstanceIdExpr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const CovererOfActionNodeExpr& e) {
    beforeVisit(static_cast<const InstanceIdExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const CovererOfActionNodeExpr& e) {
    visit(static_cast<const InstanceIdExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::afterVisit(const CovererOfActionNodeExpr& e) {
    afterVisit(static_cast<const InstanceIdExpr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const DefinerOfEntityNodeExpr& e) {
    beforeVisit(static_cast<const InstanceIdExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const DefinerOfEntityNodeExpr& e) {
    visit(static_cast<const InstanceIdExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::afterVisit(const DefinerOfEntityNodeExpr& e) {
    afterVisit(static_cast<const InstanceIdExpr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const InstructionIdOfPatternExpr& e) {
    beforeVisit(static_cast<const InstructionIdExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const InstructionIdOfPatternExpr& e) {
    visit(static_cast<const InstructionIdExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::afterVisit(const InstructionIdOfPatternExpr& e) {
    afterVisit(static_cast<const InstructionIdExpr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const PatternIdOfInstanceExpr& e) {
    beforeVisit(static_cast<const PatternIdExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const PatternIdOfInstanceExpr& e) {
    visit(static_cast<const PatternIdExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::afterVisit(const PatternIdOfInstanceExpr& e) {
    afterVisit(static_cast<const PatternIdExpr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const LabelIdAllocatedToInstanceExpr& e) {
    beforeVisit(static_cast<const LabelIdExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const LabelIdAllocatedToInstanceExpr& e) {
    visit(static_cast<const LabelIdExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::afterVisit(const LabelIdAllocatedToInstanceExpr& e) {
    afterVisit(static_cast<const LabelIdExpr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const LabelIdOfLabelNodeExpr& e) {
    beforeVisit(static_cast<const LabelIdExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const LabelIdOfLabelNodeExpr& e) {
    visit(static_cast<const LabelIdExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::afterVisit(const LabelIdOfLabelNodeExpr& e) {
    afterVisit(static_cast<const LabelIdExpr&>(e));
    atAfterVisit(e);
}

void
ConstraintVisitor::beforeVisit(const RegisterIdAllocatedToDataNodeExpr& e) {
    beforeVisit(static_cast<const RegisterIdExpr&>(e));
    atBeforeVisit(e);
}

void
ConstraintVisitor::visit(const RegisterIdAllocatedToDataNodeExpr& e) {
    visit(static_cast<const RegisterIdExpr&>(e));
    atVisit(e);
}

void
ConstraintVisitor::afterVisit(const RegisterIdAllocatedToDataNodeExpr& e) {
    afterVisit(static_cast<const RegisterIdExpr&>(e));
    atAfterVisit(e);
}

// TODO: remove comment
//...

void
ConstraintVisitor::atBeforeVisit(const Expr& e) {}

void
ConstraintVisitor::atVisit(const Expr& e) {}

void
ConstraintVisitor::atBetweenChildrenVisits(const Expr& e) {}

void
ConstraintVisitor::atAfterVisit(const Expr& e) {}

void
ConstraintVisitor::atBeforeVisit(const BoolExpr& e) {}

void
ConstraintVisitor::atVisit(const BoolExpr& e) {}

void
ConstraintVisitor::atBetweenChildrenVisits(const BoolExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const BoolExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const NumExpr& e) {}

void
ConstraintVisitor::atVisit(const NumExpr& e) {}

void
ConstraintVisitor::atBetweenChildrenVisits(const NumExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const NumExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const EqExpr& e) {}

void
ConstraintVisitor::atVisit(const EqExpr& e) {}

void
ConstraintVisitor::atBetweenChildrenVisits(const EqExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const EqExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const NeqExpr& e) {}

void
ConstraintVisitor::atVisit(const NeqExpr& e) {}

void
ConstraintVisitor::atBetweenChildrenVisits(const NeqExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const NeqExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const GTExpr& e) {}

void
ConstraintVisitor::atVisit(const GTExpr& e) {}

void
ConstraintVisitor::atBetweenChildrenVisits(const GTExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const GTExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const GEExpr& e) {}

void
ConstraintVisitor::atVisit(const GEExpr& e) {}

void
ConstraintVisitor::atBetweenChildrenVisits(const GEExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const GEExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const LTExpr& e) {}

void
ConstraintVisitor::atVisit(const LTExpr& e) {}

void
ConstraintVisitor::atBetweenChildrenVisits(const LTExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const LTExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const LEExpr& e) {}

void
ConstraintVisitor::atVisit(const LEExpr& e) {}

void
ConstraintVisitor::atBetweenChildrenVisits(const LEExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const LEExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const EqvExpr& e) {}

void
ConstraintVisitor::atVisit(const EqvExpr& e) {}

void
ConstraintVisitor::atBetweenChildrenVisits(const EqvExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const EqvExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const ImpExpr& e) {}

void
ConstraintVisitor::atVisit(const ImpExpr& e) {}

void
ConstraintVisitor::atBetweenChildrenVisits(const ImpExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const ImpExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const AndExpr& e) {}

void
ConstraintVisitor::atVisit(const AndExpr& e) {}

void
ConstraintVisitor::atBetweenChildrenVisits(const AndExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const AndExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const OrExpr& e) {}

void
ConstraintVisitor::atVisit(const OrExpr& e) {}

void
ConstraintVisitor::atBetweenChildrenVisits(const OrExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const OrExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const NotExpr& e) {}

void
ConstraintVisitor::atVisit(const NotExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const NotExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const PlusExpr& e) {}

void
ConstraintVisitor::atVisit(const PlusExpr& e) {}

void
ConstraintVisitor::atBetweenChildrenVisits(const PlusExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const PlusExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const MinusExpr& e) {}

void
ConstraintVisitor::atVisit(const MinusExpr& e) {}

void
ConstraintVisitor::atBetweenChildrenVisits(const MinusExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const MinusExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const AnIntegerExpr& e) {}

void
ConstraintVisitor::atVisit(const AnIntegerExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const AnIntegerExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const NodeIdToNumExpr& e) {}

void
ConstraintVisitor::atVisit(const NodeIdToNumExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const NodeIdToNumExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const InstanceIdToNumExpr& e) {}

void
ConstraintVisitor::atVisit(const InstanceIdToNumExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const InstanceIdToNumExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const InstructionIdToNumExpr& e) {}

void
ConstraintVisitor::atVisit(const InstructionIdToNumExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const InstructionIdToNumExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const PatternIdToNumExpr& e) {}

void
ConstraintVisitor::atVisit(const PatternIdToNumExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const PatternIdToNumExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const LabelIdToNumExpr& e) {}

void
ConstraintVisitor::atVisit(const LabelIdToNumExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const LabelIdToNumExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const RegisterIdToNumExpr& e) {}

void
ConstraintVisitor::atVisit(const RegisterIdToNumExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const RegisterIdToNumExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const AnInstanceIdExpr& e) {}

void
ConstraintVisitor::atVisit(const AnInstanceIdExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const AnInstanceIdExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const AnInstructionIdExpr& e) {}

void
ConstraintVisitor::atVisit(const AnInstructionIdExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const AnInstructionIdExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const ANodeIdExpr& e) {}

void
ConstraintVisitor::atVisit(const ANodeIdExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const ANodeIdExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const ALabelIdExpr& e) {}

void
ConstraintVisitor::atVisit(const ALabelIdExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const ALabelIdExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const ARegisterIdExpr& e) {}

void
ConstraintVisitor::atVisit(const ARegisterIdExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const ARegisterIdExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const APatternIdExpr& e) {}

void
ConstraintVisitor::atVisit(const APatternIdExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const APatternIdExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const ThisInstanceIdExpr& e) {}

void
ConstraintVisitor::atVisit(const ThisInstanceIdExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const ThisInstanceIdExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const CovererOfActionNodeExpr& e) {}

void
ConstraintVisitor::atVisit(const CovererOfActionNodeExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const CovererOfActionNodeExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const DefinerOfEntityNodeExpr& e) {}

void
ConstraintVisitor::atVisit(const DefinerOfEntityNodeExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const DefinerOfEntityNodeExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const InstructionIdOfPatternExpr& e) {}

void
ConstraintVisitor::atVisit(const InstructionIdOfPatternExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const InstructionIdOfPatternExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const PatternIdOfInstanceExpr& e) {}

void
ConstraintVisitor::atVisit(const PatternIdOfInstanceExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const PatternIdOfInstanceExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const LabelIdAllocatedToInstanceExpr& e) {}

void
ConstraintVisitor::atVisit(const LabelIdAllocatedToInstanceExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const LabelIdAllocatedToInstanceExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const LabelIdOfLabelNodeExpr& e) {}

void
ConstraintVisitor::atVisit(const LabelIdOfLabelNodeExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const LabelIdOfLabelNodeExpr& e) {}

void
ConstraintVisitor::atBeforeVisit(const RegisterIdAllocatedToDataNodeExpr& e) {}

void
ConstraintVisitor::atVisit(const RegisterIdAllocatedToDataNodeExpr& e) {}

void
ConstraintVisitor::atAfterVisit(const RegisterIdAllocatedToDataNodeExpr& e) {}
