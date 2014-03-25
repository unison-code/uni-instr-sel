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

Constraint::Constraint(BoolExpr* e)
    : expr_(e)
{
    if (!expr_) THROW(Exception, "e cannot be NULL");
}

Constraint::~Constraint(void) {
    delete expr_;
}

void
Constraint::walk(ConstraintVisitor& v) const {
    expr_->accept(v);
}

Expr::Expr(void) {}

Expr::~Expr(void) {}

BoolExpr::BoolExpr(void) {}

BoolExpr::~BoolExpr(void) {}

NumExpr::NumExpr(void) {}

NumExpr::~NumExpr(void) {}

NodeIdExpr::NodeIdExpr(void) {}

NodeIdExpr::~NodeIdExpr(void) {}

InstanceIdExpr::InstanceIdExpr(void) {}

InstanceIdExpr::~InstanceIdExpr(void) {}

InstructionIdExpr::InstructionIdExpr(void) {}

InstructionIdExpr::~InstructionIdExpr(void) {}

PatternIdExpr::PatternIdExpr(void) {}

PatternIdExpr::~PatternIdExpr(void) {}

LabelIdExpr::LabelIdExpr(void) {}

LabelIdExpr::~LabelIdExpr(void) {}

RegisterIdExpr::RegisterIdExpr(void) {}

RegisterIdExpr::~RegisterIdExpr(void) {}

BinaryBoolToBoolExpr::BinaryBoolToBoolExpr(BoolExpr* lhs, BoolExpr* rhs)
    : lhs_(lhs),
      rhs_(rhs)
{
    if (!lhs_) THROW(Exception, "lhs cannot be NULL");
    if (!rhs_) THROW(Exception, "rhs cannot be NULL");
}

BinaryBoolToBoolExpr::~BinaryBoolToBoolExpr(void) {
    delete lhs_;
    delete rhs_;
}

void
BinaryBoolToBoolExpr::accept(ConstraintVisitor& v) const {
    v.beforeVisit(*this);
    v.visit(*this);
    lhs_->accept(v);
    v.betweenChildren(*this);
    rhs_->accept(v);
    v.afterVisit(*this);
}

BinaryNumToBoolExpr::BinaryNumToBoolExpr(NumExpr* lhs, NumExpr* rhs)
    : lhs_(lhs),
      rhs_(rhs)
{
    if (!lhs_) THROW(Exception, "lhs cannot be NULL");
    if (!rhs_) THROW(Exception, "rhs cannot be NULL");
}

BinaryNumToBoolExpr::~BinaryNumToBoolExpr(void) {
    delete lhs_;
    delete rhs_;
}

void
BinaryNumToBoolExpr::accept(ConstraintVisitor& v) const {
    v.beforeVisit(*this);
    v.visit(*this);
    lhs_->accept(v);
    v.betweenChildren(*this);
    rhs_->accept(v);
    v.afterVisit(*this);
}


EqExpr::EqExpr(NumExpr* lhs, NumExpr* rhs)
    : BinaryNumToBoolExpr(lhs, rhs)
{}

EqExpr::~EqExpr(void) {}

NeqExpr::NeqExpr(NumExpr* lhs, NumExpr* rhs)
    : BinaryNumToBoolExpr(lhs, rhs)
{}

NeqExpr::~NeqExpr(void) {}

GTExpr::GTExpr(NumExpr* lhs, NumExpr* rhs)
    : BinaryNumToBoolExpr(lhs, rhs)
{}

GTExpr::~GTExpr(void) {}

GEExpr::GEExpr(NumExpr* lhs, NumExpr* rhs)
    : BinaryNumToBoolExpr(lhs, rhs)
{}

GEExpr::~GEExpr(void) {}

LTExpr::LTExpr(NumExpr* lhs, NumExpr* rhs)
    : BinaryNumToBoolExpr(lhs, rhs)
{}

LTExpr::~LTExpr(void) {}

LEExpr::LEExpr(NumExpr* lhs, NumExpr* rhs)
    : BinaryNumToBoolExpr(lhs, rhs)
{}

LEExpr::~LEExpr(void) {}

EqvExpr::EqvExpr(BoolExpr* lhs, BoolExpr* rhs)
    : BinaryBoolToBoolExpr(lhs, rhs)
{}

EqvExpr::~EqvExpr(void) {}

ImpExpr::ImpExpr(BoolExpr* lhs, BoolExpr* rhs)
    : BinaryBoolToBoolExpr(lhs, rhs)
{}

ImpExpr::~ImpExpr(void) {}

AndExpr::AndExpr(BoolExpr* lhs, BoolExpr* rhs)
    : BinaryBoolToBoolExpr(lhs, rhs)
{}

AndExpr::~AndExpr(void) {}

OrExpr::OrExpr(BoolExpr* lhs, BoolExpr* rhs)
    : BinaryBoolToBoolExpr(lhs, rhs)
{}

OrExpr::~OrExpr(void) {}

NotExpr::NotExpr(BoolExpr* e)
    : expr_(e)
{
    if (!expr_) THROW(Exception, "e cannot be NULL");
}

NotExpr::~NotExpr(void) {
    delete expr_;
}

void
NotExpr::accept(ConstraintVisitor& v) const {
    v.beforeVisit(*this);
    v.visit(*this);
    expr_->accept(v);
    v.afterVisit(*this);
}

BinaryNumToNumExpr::BinaryNumToNumExpr(NumExpr* lhs, NumExpr* rhs)
    : lhs_(lhs),
      rhs_(rhs)
{
    if (!lhs_) THROW(Exception, "lhs cannot be NULL");
    if (!rhs_) THROW(Exception, "rhs cannot be NULL");
}

BinaryNumToNumExpr::~BinaryNumToNumExpr(void) {
    delete lhs_;
    delete rhs_;
}

void
BinaryNumToNumExpr::accept(ConstraintVisitor& v) const {
    v.beforeVisit(*this);
    v.visit(*this);
    lhs_->accept(v);
    v.betweenChildren(*this);
    rhs_->accept(v);
    v.afterVisit(*this);
}

PlusExpr::PlusExpr(NumExpr* lhs, NumExpr* rhs)
    : BinaryNumToNumExpr(lhs, rhs)
{}

PlusExpr::~PlusExpr(void) {}

MinusExpr::MinusExpr(NumExpr* lhs, NumExpr* rhs)
    : BinaryNumToNumExpr(lhs, rhs)
{}

MinusExpr::~MinusExpr(void) {}

AnIntegerExpr::AnIntegerExpr(int i)
    : i_(i)
{}

AnIntegerExpr::~AnIntegerExpr(void) {}

void
AnIntegerExpr::accept(ConstraintVisitor& v) const {
    v.beforeVisit(*this);
    v.visit(*this);
    v.visit(*this);
    v.afterVisit(*this);
}

int
AnIntegerExpr::getValue(void) const {
    return i_;
}

NodeIdToNumExpr::NodeIdToNumExpr(NodeIdExpr* e)
    : expr_(e)
{
    if (!expr_) THROW(Exception, "e cannot be NULL");
}

NodeIdToNumExpr::~NodeIdToNumExpr(void) {
    delete expr_;
}

void
NodeIdToNumExpr::accept(ConstraintVisitor& v) const {
    v.beforeVisit(*this);
    v.visit(*this);
    expr_->accept(v);
    v.afterVisit(*this);
}
