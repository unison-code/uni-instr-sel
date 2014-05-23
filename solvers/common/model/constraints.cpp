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
#include "../utils/string.h"

using namespace Model;
using std::list;
using std::string;

Constraint::~Constraint(void) {}

DataNodeIsIntConstantConstraint::DataNodeIsIntConstantConstraint(const ID& id)
    : id_(id)
{}

DataNodeIsIntConstantConstraint::~DataNodeIsIntConstantConstraint(void) {}

ID
DataNodeIsIntConstantConstraint::getNodeID(void) const {
    return id_;
}

string
DataNodeIsIntConstantConstraint::toLisp(void) const {
    return string("(") + getStrName() + " " + Utils::toString(id_) + ")";
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

string
BoolExprConstraint::toLisp(void) const {
    return expr_->toLisp();
}

Expr::~Expr(void) {}

IntExpr::~IntExpr(void) {}

BoolExpr::~BoolExpr(void) {}

NumExpr::~NumExpr(void) {}

NodeIDExpr::~NodeIDExpr(void) {}

PatternInstanceIDExpr::~PatternInstanceIDExpr(void) {}

InstructionIDExpr::~InstructionIDExpr(void) {}

PatternIDExpr::~PatternIDExpr(void) {}

LabelIDExpr::~LabelIDExpr(void) {}

RegisterIDExpr::~RegisterIDExpr(void) {}

SetElemExpr::~SetElemExpr(void) {}

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

string
AnIntegerExpr::toLisp(void) const {
    return string("(") + getStrName() + " " + Utils::toString(i_) + ")";
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

string
ANodeIDExpr::toLisp(void) const {
    return string("(") + getStrName() + " " + Utils::toString(id_) + ")";
}

APatternInstanceIDExpr::APatternInstanceIDExpr(const ID& id)
    : id_(id)
{}

APatternInstanceIDExpr::~APatternInstanceIDExpr(void) {}

ID
APatternInstanceIDExpr::getID(void) const {
    return id_;
}

string
APatternInstanceIDExpr::toLisp(void) const {
    return string("(") + getStrName() + " " + Utils::toString(id_) + ")";
}

AnInstructionIDExpr::AnInstructionIDExpr(const ID& id)
    : id_(id)
{}

AnInstructionIDExpr::~AnInstructionIDExpr(void) {}

ID
AnInstructionIDExpr::getID(void) const {
    return id_;
}

string
AnInstructionIDExpr::toLisp(void) const {
    return string("(") + getStrName() + " " + Utils::toString(id_) + ")";
}

APatternIDExpr::APatternIDExpr(const ID& id)
    : id_(id)
{}

APatternIDExpr::~APatternIDExpr(void) {}

ID
APatternIDExpr::getID(void) const {
    return id_;
}

string
APatternIDExpr::toLisp(void) const {
    return string("(") + getStrName() + " " + Utils::toString(id_) + ")";
}

ALabelIDExpr::ALabelIDExpr(const ID& id)
    : id_(id)
{}

ALabelIDExpr::~ALabelIDExpr(void) {}

ID
ALabelIDExpr::getID(void) const {
    return id_;
}

string
ALabelIDExpr::toLisp(void) const {
    return string("(") + getStrName() + " " + Utils::toString(id_) + ")";
}

ARegisterIDExpr::ARegisterIDExpr(const ID& id)
    : id_(id)
{}

ARegisterIDExpr::~ARegisterIDExpr(void) {}

ID
ARegisterIDExpr::getID(void) const {
    return id_;
}

string
ARegisterIDExpr::toLisp(void) const {
    return string("(") + getStrName() + " " + Utils::toString(id_) + ")";
}

ThisPatternInstanceIDExpr::ThisPatternInstanceIDExpr(void) {}

ThisPatternInstanceIDExpr::~ThisPatternInstanceIDExpr(void) {}

string
ThisPatternInstanceIDExpr::toLisp(void) const {
    return getStrName();
}

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

PatternIDOfPatternInstanceExpr::PatternIDOfPatternInstanceExpr(
    const PatternInstanceIDExpr* expr
) : UnaryExpr(expr)
{}

PatternIDOfPatternInstanceExpr::~PatternIDOfPatternInstanceExpr(void) {}

LabelIDAllocatedToPatternInstanceExpr::LabelIDAllocatedToPatternInstanceExpr(
    const PatternInstanceIDExpr* expr
) : UnaryExpr(expr)
{}

LabelIDAllocatedToPatternInstanceExpr
::~LabelIDAllocatedToPatternInstanceExpr(void) {}

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

string
RegisterClassExpr::toLisp(void) const {
    string str;
    str += "(" + getStrName();
    for (auto& expr : getExprList()) {
        str += " " + expr->toLisp();
    }
    str += ")";
    return str;
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

const string BoolExprConstraint::STRNAME = "";
const string DataNodeIsIntConstantConstraint::STRNAME = "dnode-is-int-const";
const string EqExpr::STRNAME = "==";
const string NeqExpr::STRNAME = "!=";
const string GTExpr::STRNAME = ">";
const string GEExpr::STRNAME = ">=";
const string LTExpr::STRNAME = "<";
const string LEExpr::STRNAME = "<=";
const string EqvExpr::STRNAME = "<->";
const string ImpExpr::STRNAME = "->";
const string AndExpr::STRNAME = "&&";
const string OrExpr::STRNAME = "||";
const string NotExpr::STRNAME = "!";
const string InSetExpr::STRNAME = "in-set";
const string PlusExpr::STRNAME = "+";
const string MinusExpr::STRNAME = "-";
const string IntToNumExpr::STRNAME = "int-to-num";
const string AnIntegerExpr::STRNAME = "";
const string IntConstValueOfDataNodeExpr::STRNAME = "int-const-val-of-dnode";
const string BoolToNumExpr::STRNAME = "bool-to-num";
const string NodeIDToNumExpr::STRNAME = "node-id-to-num";
const string PatternIDToNumExpr::STRNAME = "pat-id-to-num";
const string PatternInstanceIDToNumExpr::STRNAME = "pat-inst-id-to-num";
const string InstructionIDToNumExpr::STRNAME = "instr-id-to-num";
const string LabelIDToNumExpr::STRNAME = "label-id-to-num";
const string RegisterIDToNumExpr::STRNAME = "reg-id-to-num";
const string DistanceBetweenInstanceAndLabelExpr::STRNAME = "dist-pat-to-lab";
const string ANodeIDExpr::STRNAME = "";
const string APatternInstanceIDExpr::STRNAME = "";
const string AnInstructionIDExpr::STRNAME = "";
const string APatternIDExpr::STRNAME = "";
const string ALabelIDExpr::STRNAME = "";
const string ThisPatternInstanceIDExpr::STRNAME = "this";
const string CovererOfActionNodeExpr::STRNAME = "cov-of-anode";
const string DefinerOfDataNodeExpr::STRNAME = "def-of-dnode";
const string DefinerOfStateNodeExpr::STRNAME = "def-of-snode";
const string InstructionIDOfPatternExpr::STRNAME = "instr-of-pat";
const string PatternIDOfPatternInstanceExpr::STRNAME = "pat-of-pat-inst";
const string LabelIDAllocatedToPatternInstanceExpr::STRNAME =
    "lab-alloc-to-pat-inst";
const string LabelIDOfLabelNodeExpr::STRNAME = "lab-of-lnode";
const string ARegisterIDExpr::STRNAME = "";
const string RegisterIDAllocatedToDataNodeExpr::STRNAME = "reg-alloc-to-dnode";
const string UnionSetExpr::STRNAME = "union";
const string IntersectSetExpr::STRNAME = "intersect";
const string DiffSetExpr::STRNAME = "diff";
const string DomSetOfLabelIDExpr::STRNAME = "domset-of";
const string RegisterClassExpr::STRNAME = "reg-class";
const string LabelIDToSetElemExpr::STRNAME = "lab-id-to-set-elem";
const string RegisterIDToSetElemExpr::STRNAME = "reg-id-to-set-elem";
