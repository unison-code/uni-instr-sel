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
#include "../exceptions/exception.h"

using namespace Model;
using std::list;

Constraint::~Constraint(void) {}

DataNodeIsIntConstantConstraint::DataNodeIsIntConstantConstraint(const ID& id)
    : id_(id)
{}

DataNodeIsIntConstantConstraint::~DataNodeIsIntConstantConstraint(void) {}

ID
DataNodeIsIntConstantConstraint::getNodeID(void) const {
    return id_;
}

BoolExprConstraint::BoolExprConstraint(BoolExpr* expr)
    : expr_(expr)
{
    if (!expr_) THROW(Exception, "expr cannot be NULL");
}

BoolExprConstraint::~BoolExprConstraint(void) {
    delete expr_;
}

const BoolExpr*
BoolExprConstraint::getExpr(void) const {
    return expr_;
}

Expr::Expr(void) {}

Expr::~Expr(void) {}

IntExpr::IntExpr(void) {}

IntExpr::~IntExpr(void) {}

BoolExpr::BoolExpr(void) {}

BoolExpr::~BoolExpr(void) {}

NumExpr::NumExpr(void) {}

NumExpr::~NumExpr(void) {}

NodeIDExpr::NodeIDExpr(void) {}

NodeIDExpr::~NodeIDExpr(void) {}

PatternInstanceIDExpr::PatternInstanceIDExpr(void) {}

PatternInstanceIDExpr::~PatternInstanceIDExpr(void) {}

InstructionIDExpr::InstructionIDExpr(void) {}

InstructionIDExpr::~InstructionIDExpr(void) {}

PatternIDExpr::PatternIDExpr(void) {}

PatternIDExpr::~PatternIDExpr(void) {}

LabelIDExpr::LabelIDExpr(void) {}

LabelIDExpr::~LabelIDExpr(void) {}

RegisterIDExpr::RegisterIDExpr(void) {}

RegisterIDExpr::~RegisterIDExpr(void) {}

SetElemExpr::SetElemExpr(void) {}

SetElemExpr::~SetElemExpr(void) {}

SetExpr::SetExpr(void) {}

SetExpr::~SetExpr(void) {}

EqExpr::EqExpr(const NumExpr* lhs, const NumExpr* rhs)
    : BinaryExpr(lhs, rhs)
{}

EqExpr::~EqExpr(void) {}

NeqExpr::NeqExpr(const NumExpr* lhs, const NumExpr* rhs)
    : BinaryExpr(lhs, rhs)
{}

NeqExpr::~NeqExpr(void) {}

GTExpr::GTExpr(const NumExpr* lhs, const NumExpr* rhs)
    : BinaryExpr(lhs, rhs)
{}

GTExpr::~GTExpr(void) {}

GEExpr::GEExpr(const NumExpr* lhs, const NumExpr* rhs)
    : BinaryExpr(lhs, rhs)
{}

GEExpr::~GEExpr(void) {}

LTExpr::LTExpr(const NumExpr* lhs, const NumExpr* rhs)
    : BinaryExpr(lhs, rhs)
{}

LTExpr::~LTExpr(void) {}

LEExpr::LEExpr(const NumExpr* lhs, const NumExpr* rhs)
    : BinaryExpr(lhs, rhs)
{}

LEExpr::~LEExpr(void) {}

EqvExpr::EqvExpr(const BoolExpr* lhs, const BoolExpr* rhs)
    : BinaryExpr(lhs, rhs)
{}

EqvExpr::~EqvExpr(void) {}

ImpExpr::ImpExpr(const BoolExpr* lhs, const BoolExpr* rhs)
    : BinaryExpr(lhs, rhs)
{}

ImpExpr::~ImpExpr(void) {}

AndExpr::AndExpr(const BoolExpr* lhs, const BoolExpr* rhs)
    : BinaryExpr(lhs, rhs)
{}

AndExpr::~AndExpr(void) {}

OrExpr::OrExpr(const BoolExpr* lhs, const BoolExpr* rhs)
    : BinaryExpr(lhs, rhs)
{}

OrExpr::~OrExpr(void) {}

NotExpr::NotExpr(const BoolExpr* expr)
    : UnaryExpr(expr)
{}

NotExpr::~NotExpr(void) {}

PlusExpr::PlusExpr(const NumExpr* lhs, const NumExpr* rhs)
    : BinaryExpr(lhs, rhs)
{}

InSetExpr::InSetExpr(const SetElemExpr* lhs, const SetExpr* rhs)
    : BinaryExpr(lhs, rhs)
{}

InSetExpr::~InSetExpr(void) {}

PlusExpr::~PlusExpr(void) {}

MinusExpr::MinusExpr(const NumExpr* lhs, const NumExpr* rhs)
    : BinaryExpr(lhs, rhs)
{}

MinusExpr::~MinusExpr(void) {}

AnIntegerExpr::AnIntegerExpr(int i)
    : i_(i)
{}

AnIntegerExpr::~AnIntegerExpr(void) {}

int
AnIntegerExpr::getValue(void) const {
    return i_;
}

IntConstValueOfDataNodeExpr::IntConstValueOfDataNodeExpr(
    const NodeIDExpr* expr
) : UnaryExpr(expr)
{}

IntConstValueOfDataNodeExpr::~IntConstValueOfDataNodeExpr(void) {}

IntToNumExpr::IntToNumExpr(const IntExpr* expr)
    : UnaryExpr(expr)
{}

IntToNumExpr::~IntToNumExpr(void) {}

BoolToNumExpr::BoolToNumExpr(const BoolExpr* expr)
    : UnaryExpr(expr)
{}

BoolToNumExpr::~BoolToNumExpr(void) {}

NodeIDToNumExpr::NodeIDToNumExpr(const NodeIDExpr* expr)
    : UnaryExpr(expr)
{}

NodeIDToNumExpr::~NodeIDToNumExpr(void) {}

PatternInstanceIDToNumExpr::PatternInstanceIDToNumExpr(
    const PatternInstanceIDExpr* expr
) : UnaryExpr(expr)
{}

PatternInstanceIDToNumExpr::~PatternInstanceIDToNumExpr(void) {}

InstructionIDToNumExpr::InstructionIDToNumExpr(const InstructionIDExpr* expr)
    : UnaryExpr(expr)
{}

InstructionIDToNumExpr::~InstructionIDToNumExpr(void) {}

PatternIDToNumExpr::PatternIDToNumExpr(const PatternIDExpr* expr)
    : UnaryExpr(expr)
{}

PatternIDToNumExpr::~PatternIDToNumExpr(void) {}

LabelIDToNumExpr::LabelIDToNumExpr(const LabelIDExpr* expr)
    : UnaryExpr(expr)
{}

LabelIDToNumExpr::~LabelIDToNumExpr(void) {}

RegisterIDToNumExpr::RegisterIDToNumExpr(const RegisterIDExpr* expr)
    : UnaryExpr(expr)
{}

RegisterIDToNumExpr::~RegisterIDToNumExpr(void) {}

ANodeIDExpr::ANodeIDExpr(const ID& id)
    : id_(id)
{}

ANodeIDExpr::~ANodeIDExpr(void) {}

ID
ANodeIDExpr::getID(void) const {
    return id_;
}

APatternInstanceIDExpr::APatternInstanceIDExpr(const ID& id)
    : id_(id)
{}

APatternInstanceIDExpr::~APatternInstanceIDExpr(void) {}

ID
APatternInstanceIDExpr::getID(void) const {
    return id_;
}

AnInstructionIDExpr::AnInstructionIDExpr(const ID& id)
    : id_(id)
{}

AnInstructionIDExpr::~AnInstructionIDExpr(void) {}

ID
AnInstructionIDExpr::getID(void) const {
    return id_;
}

APatternIDExpr::APatternIDExpr(const ID& id)
    : id_(id)
{}

APatternIDExpr::~APatternIDExpr(void) {}

ID
APatternIDExpr::getID(void) const {
    return id_;
}

ALabelIDExpr::ALabelIDExpr(const ID& id)
    : id_(id)
{}

ALabelIDExpr::~ALabelIDExpr(void) {}

ID
ALabelIDExpr::getID(void) const {
    return id_;
}

ARegisterIDExpr::ARegisterIDExpr(const ID& id)
    : id_(id)
{}

ARegisterIDExpr::~ARegisterIDExpr(void) {}

ID
ARegisterIDExpr::getID(void) const {
    return id_;
}

ThisPatternInstanceIDExpr::ThisPatternInstanceIDExpr(void) {}

ThisPatternInstanceIDExpr::~ThisPatternInstanceIDExpr(void) {}

CovererOfActionNodeExpr::CovererOfActionNodeExpr(const NodeIDExpr* expr)
    : UnaryExpr(expr)
{}

CovererOfActionNodeExpr::~CovererOfActionNodeExpr(void) {}

DefinerOfDataNodeExpr::DefinerOfDataNodeExpr(const NodeIDExpr* expr)
    : UnaryExpr(expr)
{}

DefinerOfDataNodeExpr::~DefinerOfDataNodeExpr(void) {}

DefinerOfStateNodeExpr::DefinerOfStateNodeExpr(const NodeIDExpr* expr)
    : UnaryExpr(expr)
{}

DefinerOfStateNodeExpr::~DefinerOfStateNodeExpr(void) {}

InstructionIDOfPatternExpr::InstructionIDOfPatternExpr(
    const PatternIDExpr* expr
) : UnaryExpr(expr)
{}

InstructionIDOfPatternExpr::~InstructionIDOfPatternExpr(void) {}

PatternIDOfInstanceExpr::PatternIDOfInstanceExpr(
    const PatternInstanceIDExpr* expr
) : UnaryExpr(expr)
{}

PatternIDOfInstanceExpr::~PatternIDOfInstanceExpr(void) {}

LabelIDAllocatedToInstanceExpr::LabelIDAllocatedToInstanceExpr(
    const PatternInstanceIDExpr* expr
) : UnaryExpr(expr)
{}

LabelIDAllocatedToInstanceExpr::~LabelIDAllocatedToInstanceExpr(void) {}

LabelIDOfLabelNodeExpr::LabelIDOfLabelNodeExpr(const NodeIDExpr* expr)
    : UnaryExpr(expr)
{}

LabelIDOfLabelNodeExpr::~LabelIDOfLabelNodeExpr(void) {}

RegisterIDAllocatedToDataNodeExpr::RegisterIDAllocatedToDataNodeExpr(
    const NodeIDExpr* expr
) : UnaryExpr(expr)
{}

RegisterIDAllocatedToDataNodeExpr::~RegisterIDAllocatedToDataNodeExpr(void) {}

UnionSetExpr::UnionSetExpr(const SetExpr* lhs, const SetExpr* rhs)
    : BinaryExpr(lhs, rhs)
{}

UnionSetExpr::~UnionSetExpr(void) {}

IntersectSetExpr::IntersectSetExpr(const SetExpr* lhs, const SetExpr* rhs)
    : BinaryExpr(lhs, rhs)
{}

IntersectSetExpr::~IntersectSetExpr(void) {}

DiffSetExpr::DiffSetExpr(const SetExpr* lhs, const SetExpr* rhs)
    : BinaryExpr(lhs, rhs)
{}

DiffSetExpr::~DiffSetExpr(void) {}

DomSetOfLabelIDExpr::DomSetOfLabelIDExpr(const LabelIDExpr* expr)
    : UnaryExpr(expr)
{}

DomSetOfLabelIDExpr::~DomSetOfLabelIDExpr(void) {}

RegisterClassExpr::RegisterClassExpr(
    const std::list<const RegisterIDExpr*>& expr
) : expr_(expr)
{}

RegisterClassExpr::~RegisterClassExpr(void) {
    for (auto e : expr_) {
        delete e;
    }
}

const list<const RegisterIDExpr*>&
RegisterClassExpr::getExprList(void) const {
    return expr_;
}

LabelIDToSetElemExpr::LabelIDToSetElemExpr(const LabelIDExpr* expr)
    : UnaryExpr(expr)
{}

LabelIDToSetElemExpr::~LabelIDToSetElemExpr(void) {}

RegisterIDToSetElemExpr::RegisterIDToSetElemExpr(const RegisterIDExpr* expr)
    : UnaryExpr(expr)
{}

RegisterIDToSetElemExpr::~RegisterIDToSetElemExpr(void) {}

DistanceBetweenInstanceAndLabelExpr::DistanceBetweenInstanceAndLabelExpr(
    const PatternInstanceIDExpr* lhs,
    const LabelIDExpr* rhs
) : BinaryExpr(lhs, rhs)
{}

DistanceBetweenInstanceAndLabelExpr::~DistanceBetweenInstanceAndLabelExpr(void)
{}
