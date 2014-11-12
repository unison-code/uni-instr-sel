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
using std::ostream;
using std::string;

Constraint::~Constraint(void) {}

namespace Model {

ostream&
operator<<(ostream& os, const Constraint& c) {
    os << c.toLisp();
    return os;
}

ostream&
operator<<(ostream& os, const Constraint* c) {
    os << *c;
    return os;
}

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

NodeExpr::~NodeExpr(void) {}

MatchExpr::~MatchExpr(void) {}

InstructionExpr::~InstructionExpr(void) {}

PatternExpr::~PatternExpr(void) {}

LabelExpr::~LabelExpr(void) {}

RegisterExpr::~RegisterExpr(void) {}

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

DataNodeIsAnIntConstantExpr::DataNodeIsAnIntConstantExpr(const NodeExpr* expr)
    : UnaryExpr(expr)
{}

DataNodeIsAnIntConstantExpr::~DataNodeIsAnIntConstantExpr(void) {}

DataNodeIsIntermediateExpr::DataNodeIsIntermediateExpr(const NodeExpr* expr)
    : UnaryExpr(expr)
{}

DataNodeIsIntermediateExpr::~DataNodeIsIntermediateExpr(void) {}

MatchIsSelectedExpr::MatchIsSelectedExpr(const MatchExpr* expr)
    : UnaryExpr(expr)
{}

MatchIsSelectedExpr::~MatchIsSelectedExpr(void) {}

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

IntConstValueOfDataNodeExpr::IntConstValueOfDataNodeExpr(const NodeExpr* expr)
    : UnaryExpr(expr)
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

NodeToNumExpr::NodeToNumExpr(const NodeExpr* expr)
    : UnaryExpr(expr)
{}

NodeToNumExpr::~NodeToNumExpr(void) {}

MatchToNumExpr::MatchToNumExpr(const MatchExpr* expr)
    : UnaryExpr(expr)
{}

MatchToNumExpr::~MatchToNumExpr(void) {}

InstructionToNumExpr::InstructionToNumExpr(const InstructionExpr* expr)
    : UnaryExpr(expr)
{}

InstructionToNumExpr::~InstructionToNumExpr(void) {}

PatternToNumExpr::PatternToNumExpr(const PatternExpr* expr)
    : UnaryExpr(expr)
{}

PatternToNumExpr::~PatternToNumExpr(void) {}

LabelToNumExpr::LabelToNumExpr(const LabelExpr* expr)
    : UnaryExpr(expr)
{}

LabelToNumExpr::~LabelToNumExpr(void) {}

RegisterToNumExpr::RegisterToNumExpr(const RegisterExpr* expr)
    : UnaryExpr(expr)
{}

RegisterToNumExpr::~RegisterToNumExpr(void) {}

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

AMatchIDExpr::AMatchIDExpr(const ID& id)
    : id_(id)
{}

AMatchIDExpr::~AMatchIDExpr(void) {}

ID
AMatchIDExpr::getID(void) const {
    return id_;
}

string
AMatchIDExpr::toLisp(void) const {
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

ANodeArrayIndexExpr::ANodeArrayIndexExpr(const ArrayIndex& i)
    : i_(i)
{}

ANodeArrayIndexExpr::~ANodeArrayIndexExpr(void) {}

ArrayIndex
ANodeArrayIndexExpr::getArrayIndex(void) const {
    return i_;
}

string
ANodeArrayIndexExpr::toLisp(void) const {
    return string("(") + getStrName() + " " + Utils::toString(i_) + ")";
}

AMatchArrayIndexExpr::AMatchArrayIndexExpr(const ArrayIndex& i)
    : i_(i)
{}

AMatchArrayIndexExpr::~AMatchArrayIndexExpr(void) {}

ArrayIndex
AMatchArrayIndexExpr::getArrayIndex(void) const {
    return i_;
}

string
AMatchArrayIndexExpr::toLisp(void) const {
    return string("(") + getStrName() + " " + Utils::toString(i_) + ")";
}

ARegisterArrayIndexExpr::ARegisterArrayIndexExpr(const ArrayIndex& i)
    : i_(i)
{}

ARegisterArrayIndexExpr::~ARegisterArrayIndexExpr(void) {}

ArrayIndex
ARegisterArrayIndexExpr::getArrayIndex(void) const {
    return i_;
}

string
ARegisterArrayIndexExpr::toLisp(void) const {
    return string("(") + getStrName() + " " + Utils::toString(i_) + ")";
}

ThisMatchExpr::ThisMatchExpr(void) {}

ThisMatchExpr::~ThisMatchExpr(void) {}

string
ThisMatchExpr::toLisp(void) const {
    return getStrName();
}

CovererOfOperationNodeExpr::CovererOfOperationNodeExpr(const NodeExpr* expr)
    : UnaryExpr(expr)
{}

CovererOfOperationNodeExpr::~CovererOfOperationNodeExpr(void) {}

DefinerOfDataNodeExpr::DefinerOfDataNodeExpr(const NodeExpr* expr)
    : UnaryExpr(expr)
{}

DefinerOfDataNodeExpr::~DefinerOfDataNodeExpr(void) {}

DefinerOfStateNodeExpr::DefinerOfStateNodeExpr(const NodeExpr* expr)
    : UnaryExpr(expr)
{}

DefinerOfStateNodeExpr::~DefinerOfStateNodeExpr(void) {}

InstructionOfPatternExpr::InstructionOfPatternExpr(const PatternExpr* expr)
    : UnaryExpr(expr)
{}

InstructionOfPatternExpr::~InstructionOfPatternExpr(void) {}

PatternOfMatchExpr::PatternOfMatchExpr(const MatchExpr* expr)
    : UnaryExpr(expr)
{}

PatternOfMatchExpr::~PatternOfMatchExpr(void) {}

LabelAllocatedToMatchExpr::LabelAllocatedToMatchExpr(const MatchExpr* expr)
    : UnaryExpr(expr)
{}

LabelAllocatedToMatchExpr
::~LabelAllocatedToMatchExpr(void) {}

LabelOfLabelNodeExpr::LabelOfLabelNodeExpr(const NodeExpr* expr)
    : UnaryExpr(expr)
{}

LabelOfLabelNodeExpr::~LabelOfLabelNodeExpr(void) {}

RegisterAllocatedToDataNodeExpr::RegisterAllocatedToDataNodeExpr(
    const NodeExpr* expr
) : UnaryExpr(expr)
{}

RegisterAllocatedToDataNodeExpr::~RegisterAllocatedToDataNodeExpr(void) {}

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

DomSetOfLabelExpr::DomSetOfLabelExpr(const LabelExpr* expr)
    : UnaryExpr(expr)
{}

DomSetOfLabelExpr::~DomSetOfLabelExpr(void) {}

RegisterClassExpr::RegisterClassExpr(const std::list<const RegisterExpr*>& expr)
    : expr_(expr)
{}

RegisterClassExpr::~RegisterClassExpr(void) {
    for (auto e : expr_) {
        delete e;
    }
}

const list<const RegisterExpr*>&
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

LabelToSetElemExpr::LabelToSetElemExpr(const LabelExpr* expr)
    : UnaryExpr(expr)
{}

LabelToSetElemExpr::~LabelToSetElemExpr(void) {}

RegisterToSetElemExpr::RegisterToSetElemExpr(const RegisterExpr* expr)
    : UnaryExpr(expr)
{}

RegisterToSetElemExpr::~RegisterToSetElemExpr(void) {}

DistanceBetweenMatchAndLabelExpr
::DistanceBetweenMatchAndLabelExpr(
    const MatchExpr* lhs,
    const LabelExpr* rhs
) : BinaryExpr(lhs, rhs)
{}

DistanceBetweenMatchAndLabelExpr
::~DistanceBetweenMatchAndLabelExpr(void)
{}

const string BoolExprConstraint::STRNAME = "";
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
const string DataNodeIsAnIntConstantExpr::STRNAME = "dnode-is-int-const";
const string DataNodeIsIntermediateExpr::STRNAME = "dnode-is-intermediate";
const string MatchIsSelectedExpr::STRNAME = "match-is-selected";
const string PlusExpr::STRNAME = "+";
const string MinusExpr::STRNAME = "-";
const string IntToNumExpr::STRNAME = "int-to-num";
const string AnIntegerExpr::STRNAME = "int";
const string IntConstValueOfDataNodeExpr::STRNAME = "int-const-val-of-dnode";
const string BoolToNumExpr::STRNAME = "bool-to-num";
const string NodeToNumExpr::STRNAME = "node-to-num";
const string PatternToNumExpr::STRNAME = "pat-to-num";
const string MatchToNumExpr::STRNAME = "match-to-num";
const string InstructionToNumExpr::STRNAME = "instr-to-num";
const string LabelToNumExpr::STRNAME = "lab-to-num";
const string RegisterToNumExpr::STRNAME = "reg-to-num";
const string DistanceBetweenMatchAndLabelExpr::STRNAME = "dist-match-to-lab";
const string ANodeIDExpr::STRNAME = "id";
const string AMatchIDExpr::STRNAME = "id";
const string AnInstructionIDExpr::STRNAME = "id";
const string APatternIDExpr::STRNAME = "id";
const string ALabelIDExpr::STRNAME = "id";
const string ARegisterIDExpr::STRNAME = "id";
const string ANodeArrayIndexExpr::STRNAME = "array-index";
const string AMatchArrayIndexExpr::STRNAME = "array-index";
const string ARegisterArrayIndexExpr::STRNAME = "array-index";
const string ThisMatchExpr::STRNAME = "this";
const string CovererOfOperationNodeExpr::STRNAME = "cov-of-onode";
const string DefinerOfDataNodeExpr::STRNAME = "def-of-dnode";
const string DefinerOfStateNodeExpr::STRNAME = "def-of-snode";
const string InstructionOfPatternExpr::STRNAME = "instr-of-pat";
const string PatternOfMatchExpr::STRNAME = "pat-of-match";
const string LabelAllocatedToMatchExpr::STRNAME = "lab-alloc-to-match";
const string LabelOfLabelNodeExpr::STRNAME = "lab-of-lnode";
const string RegisterAllocatedToDataNodeExpr::STRNAME = "reg-alloc-to-dnode";
const string UnionSetExpr::STRNAME = "union";
const string IntersectSetExpr::STRNAME = "intersect";
const string DiffSetExpr::STRNAME = "diff";
const string DomSetOfLabelExpr::STRNAME = "domset-of-lab";
const string RegisterClassExpr::STRNAME = "reg-class";
const string LabelToSetElemExpr::STRNAME = "lab-to-set-elem";
const string RegisterToSetElemExpr::STRNAME = "reg-to-set-elem";
