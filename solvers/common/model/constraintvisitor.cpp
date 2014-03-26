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
ConstraintVisitor::beforeVisit(const Expr& e) {}

void
ConstraintVisitor::visit(const Expr& e) {}

void
ConstraintVisitor::betweenChildrenVisits(const Expr& e) {}

void
ConstraintVisitor::afterVisit(const Expr& e) {}

void
ConstraintVisitor::beforeVisit(const BoolExpr& e) {}

void
ConstraintVisitor::visit(const BoolExpr& e) {}

void
ConstraintVisitor::betweenChildrenVisits(const BoolExpr& e) {}

void
ConstraintVisitor::afterVisit(const BoolExpr& e) {}

void
ConstraintVisitor::beforeVisit(const NumExpr& e) {}

void
ConstraintVisitor::visit(const NumExpr& e) {}

void
ConstraintVisitor::betweenChildrenVisits(const NumExpr& e) {}

void
ConstraintVisitor::afterVisit(const NumExpr& e) {}

void
ConstraintVisitor::beforeVisit(const EqExpr& e) {}

void
ConstraintVisitor::visit(const EqExpr& e) {}

void
ConstraintVisitor::betweenChildrenVisits(const EqExpr& e) {}

void
ConstraintVisitor::afterVisit(const EqExpr& e) {}

void
ConstraintVisitor::beforeVisit(const NeqExpr& e) {}

void
ConstraintVisitor::visit(const NeqExpr& e) {}

void
ConstraintVisitor::betweenChildrenVisits(const NeqExpr& e) {}

void
ConstraintVisitor::afterVisit(const NeqExpr& e) {}

void
ConstraintVisitor::beforeVisit(const GTExpr& e) {}

void
ConstraintVisitor::visit(const GTExpr& e) {}

void
ConstraintVisitor::betweenChildrenVisits(const GTExpr& e) {}

void
ConstraintVisitor::afterVisit(const GTExpr& e) {}

void
ConstraintVisitor::beforeVisit(const GEExpr& e) {}

void
ConstraintVisitor::visit(const GEExpr& e) {}

void
ConstraintVisitor::betweenChildrenVisits(const GEExpr& e) {}

void
ConstraintVisitor::afterVisit(const GEExpr& e) {}

void
ConstraintVisitor::beforeVisit(const LTExpr& e) {}

void
ConstraintVisitor::visit(const LTExpr& e) {}

void
ConstraintVisitor::betweenChildrenVisits(const LTExpr& e) {}

void
ConstraintVisitor::afterVisit(const LTExpr& e) {}

void
ConstraintVisitor::beforeVisit(const LEExpr& e) {}

void
ConstraintVisitor::visit(const LEExpr& e) {}

void
ConstraintVisitor::betweenChildrenVisits(const LEExpr& e) {}

void
ConstraintVisitor::afterVisit(const LEExpr& e) {}

void
ConstraintVisitor::beforeVisit(const EqvExpr& e) {}

void
ConstraintVisitor::visit(const EqvExpr& e) {}

void
ConstraintVisitor::betweenChildrenVisits(const EqvExpr& e) {}

void
ConstraintVisitor::afterVisit(const EqvExpr& e) {}

void
ConstraintVisitor::beforeVisit(const ImpExpr& e) {}

void
ConstraintVisitor::visit(const ImpExpr& e) {}

void
ConstraintVisitor::betweenChildrenVisits(const ImpExpr& e) {}

void
ConstraintVisitor::afterVisit(const ImpExpr& e) {}

void
ConstraintVisitor::beforeVisit(const AndExpr& e) {}

void
ConstraintVisitor::visit(const AndExpr& e) {}

void
ConstraintVisitor::betweenChildrenVisits(const AndExpr& e) {}

void
ConstraintVisitor::afterVisit(const AndExpr& e) {}

void
ConstraintVisitor::beforeVisit(const OrExpr& e) {}

void
ConstraintVisitor::visit(const OrExpr& e) {}

void
ConstraintVisitor::betweenChildrenVisits(const OrExpr& e) {}

void
ConstraintVisitor::afterVisit(const OrExpr& e) {}

void
ConstraintVisitor::beforeVisit(const NotExpr& e) {}

void
ConstraintVisitor::visit(const NotExpr& e) {}

void
ConstraintVisitor::afterVisit(const NotExpr& e) {}

void
ConstraintVisitor::beforeVisit(const PlusExpr& e) {}

void
ConstraintVisitor::visit(const PlusExpr& e) {}

void
ConstraintVisitor::betweenChildrenVisits(const PlusExpr& e) {}

void
ConstraintVisitor::afterVisit(const PlusExpr& e) {}

void
ConstraintVisitor::beforeVisit(const MinusExpr& e) {}

void
ConstraintVisitor::visit(const MinusExpr& e) {}

void
ConstraintVisitor::betweenChildrenVisits(const MinusExpr& e) {}

void
ConstraintVisitor::afterVisit(const MinusExpr& e) {}

void
ConstraintVisitor::beforeVisit(const AnIntegerExpr& e) {}

void
ConstraintVisitor::visit(const AnIntegerExpr& e) {}

void
ConstraintVisitor::afterVisit(const AnIntegerExpr& e) {}

void
ConstraintVisitor::beforeVisit(const NodeIdToNumExpr& e) {}

void
ConstraintVisitor::visit(const NodeIdToNumExpr& e) {}

void
ConstraintVisitor::afterVisit(const NodeIdToNumExpr& e) {}

void
ConstraintVisitor::beforeVisit(const InstanceIdToNumExpr& e) {}

void
ConstraintVisitor::visit(const InstanceIdToNumExpr& e) {}

void
ConstraintVisitor::afterVisit(const InstanceIdToNumExpr& e) {}

void
ConstraintVisitor::beforeVisit(const InstructionIdToNumExpr& e) {}

void
ConstraintVisitor::visit(const InstructionIdToNumExpr& e) {}

void
ConstraintVisitor::afterVisit(const InstructionIdToNumExpr& e) {}

void
ConstraintVisitor::beforeVisit(const PatternIdToNumExpr& e) {}

void
ConstraintVisitor::visit(const PatternIdToNumExpr& e) {}

void
ConstraintVisitor::afterVisit(const PatternIdToNumExpr& e) {}

void
ConstraintVisitor::beforeVisit(const LabelIdToNumExpr& e) {}

void
ConstraintVisitor::visit(const LabelIdToNumExpr& e) {}

void
ConstraintVisitor::afterVisit(const LabelIdToNumExpr& e) {}

void
ConstraintVisitor::beforeVisit(const RegisterIdToNumExpr& e) {}

void
ConstraintVisitor::visit(const RegisterIdToNumExpr& e) {}

void
ConstraintVisitor::afterVisit(const RegisterIdToNumExpr& e) {}

void
ConstraintVisitor::beforeVisit(const AnInstanceIdExpr& e) {}

void
ConstraintVisitor::visit(const AnInstanceIdExpr& e) {}

void
ConstraintVisitor::afterVisit(const AnInstanceIdExpr& e) {}

void
ConstraintVisitor::beforeVisit(const AnInstructionIdExpr& e) {}

void
ConstraintVisitor::visit(const AnInstructionIdExpr& e) {}

void
ConstraintVisitor::afterVisit(const AnInstructionIdExpr& e) {}

void
ConstraintVisitor::beforeVisit(const ANodeIdExpr& e) {}

void
ConstraintVisitor::visit(const ANodeIdExpr& e) {}

void
ConstraintVisitor::afterVisit(const ANodeIdExpr& e) {}

void
ConstraintVisitor::beforeVisit(const ALabelIdExpr& e) {}

void
ConstraintVisitor::visit(const ALabelIdExpr& e) {}

void
ConstraintVisitor::afterVisit(const ALabelIdExpr& e) {}

void
ConstraintVisitor::beforeVisit(const ARegisterIdExpr& e) {}

void
ConstraintVisitor::visit(const ARegisterIdExpr& e) {}

void
ConstraintVisitor::afterVisit(const ARegisterIdExpr& e) {}

void
ConstraintVisitor::beforeVisit(const APatternIdExpr& e) {}

void
ConstraintVisitor::visit(const APatternIdExpr& e) {}

void
ConstraintVisitor::afterVisit(const APatternIdExpr& e) {}

void
ConstraintVisitor::beforeVisit(const ThisInstanceIdExpr& e) {}

void
ConstraintVisitor::visit(const ThisInstanceIdExpr& e) {}

void
ConstraintVisitor::afterVisit(const ThisInstanceIdExpr& e) {}

void
ConstraintVisitor::beforeVisit(const CovererOfActionNodeExpr& e) {}

void
ConstraintVisitor::visit(const CovererOfActionNodeExpr& e) {}

void
ConstraintVisitor::afterVisit(const CovererOfActionNodeExpr& e) {}

void
ConstraintVisitor::beforeVisit(const DefinerOfEntityNodeExpr& e) {}

void
ConstraintVisitor::visit(const DefinerOfEntityNodeExpr& e) {}

void
ConstraintVisitor::afterVisit(const DefinerOfEntityNodeExpr& e) {}

void
ConstraintVisitor::beforeVisit(const InstructionIdOfPatternExpr& e) {}

void
ConstraintVisitor::visit(const InstructionIdOfPatternExpr& e) {}

void
ConstraintVisitor::afterVisit(const InstructionIdOfPatternExpr& e) {}

void
ConstraintVisitor::beforeVisit(const PatternIdOfInstanceExpr& e) {}

void
ConstraintVisitor::visit(const PatternIdOfInstanceExpr& e) {}

void
ConstraintVisitor::afterVisit(const PatternIdOfInstanceExpr& e) {}

void
ConstraintVisitor::beforeVisit(const LabelIdAllocatedToInstanceExpr& e) {}

void
ConstraintVisitor::visit(const LabelIdAllocatedToInstanceExpr& e) {}

void
ConstraintVisitor::afterVisit(const LabelIdAllocatedToInstanceExpr& e) {}

void
ConstraintVisitor::beforeVisit(const LabelIdOfLabelNodeExpr& e) {}

void
ConstraintVisitor::visit(const LabelIdOfLabelNodeExpr& e) {}

void
ConstraintVisitor::afterVisit(const LabelIdOfLabelNodeExpr& e) {}

void
ConstraintVisitor::beforeVisit(const RegisterIdAllocatedToDataNodeExpr& e) {}

void
ConstraintVisitor::visit(const RegisterIdAllocatedToDataNodeExpr& e) {}

void
ConstraintVisitor::afterVisit(const RegisterIdAllocatedToDataNodeExpr& e) {}
