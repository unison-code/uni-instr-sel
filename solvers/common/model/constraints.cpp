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

EqExpr::EqExpr(NumExpr* lhs, NumExpr* rhs)
    : BinaryBoolExpr(lhs, rhs)
{}

EqExpr::~EqExpr(void) {}

NeqExpr::NeqExpr(NumExpr* lhs, NumExpr* rhs)
    : BinaryBoolExpr(lhs, rhs)
{}

NeqExpr::~NeqExpr(void) {}

GTExpr::GTExpr(NumExpr* lhs, NumExpr* rhs)
    : BinaryBoolExpr(lhs, rhs)
{}

GTExpr::~GTExpr(void) {}

GEExpr::GEExpr(NumExpr* lhs, NumExpr* rhs)
    : BinaryBoolExpr(lhs, rhs)
{}

GEExpr::~GEExpr(void) {}

LTExpr::LTExpr(NumExpr* lhs, NumExpr* rhs)
    : BinaryBoolExpr(lhs, rhs)
{}

LTExpr::~LTExpr(void) {}

LEExpr::LEExpr(NumExpr* lhs, NumExpr* rhs)
    : BinaryBoolExpr(lhs, rhs)
{}

LEExpr::~LEExpr(void) {}

EqvExpr::EqvExpr(BoolExpr* lhs, BoolExpr* rhs)
    : BinaryBoolExpr(lhs, rhs)
{}

EqvExpr::~EqvExpr(void) {}

ImpExpr::ImpExpr(BoolExpr* lhs, BoolExpr* rhs)
    : BinaryBoolExpr(lhs, rhs)
{}

ImpExpr::~ImpExpr(void) {}

AndExpr::AndExpr(BoolExpr* lhs, BoolExpr* rhs)
    : BinaryBoolExpr(lhs, rhs)
{}

AndExpr::~AndExpr(void) {}

OrExpr::OrExpr(BoolExpr* lhs, BoolExpr* rhs)
    : BinaryBoolExpr(lhs, rhs)
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

PlusExpr::PlusExpr(NumExpr* lhs, NumExpr* rhs)
    : BinaryNumExpr(lhs, rhs)
{}

PlusExpr::~PlusExpr(void) {}

MinusExpr::MinusExpr(NumExpr* lhs, NumExpr* rhs)
    : BinaryNumExpr(lhs, rhs)
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

InstanceIdToNumExpr::InstanceIdToNumExpr(InstanceIdExpr* e)
    : expr_(e)
{
    if (!expr_) THROW(Exception, "e cannot be NULL");
}

InstanceIdToNumExpr::~InstanceIdToNumExpr(void) {
    delete expr_;
}

void
InstanceIdToNumExpr::accept(ConstraintVisitor& v) const {
    v.beforeVisit(*this);
    v.visit(*this);
    expr_->accept(v);
    v.afterVisit(*this);
}

InstructionIdToNumExpr::InstructionIdToNumExpr(InstructionIdExpr* e)
    : expr_(e)
{
    if (!expr_) THROW(Exception, "e cannot be NULL");
}

InstructionIdToNumExpr::~InstructionIdToNumExpr(void) {
    delete expr_;
}

void
InstructionIdToNumExpr::accept(ConstraintVisitor& v) const {
    v.beforeVisit(*this);
    v.visit(*this);
    expr_->accept(v);
    v.afterVisit(*this);
}

PatternIdToNumExpr::PatternIdToNumExpr(PatternIdExpr* e)
    : expr_(e)
{
    if (!expr_) THROW(Exception, "e cannot be NULL");
}

PatternIdToNumExpr::~PatternIdToNumExpr(void) {
    delete expr_;
}

void
PatternIdToNumExpr::accept(ConstraintVisitor& v) const {
    v.beforeVisit(*this);
    v.visit(*this);
    expr_->accept(v);
    v.afterVisit(*this);
}

LabelIdToNumExpr::LabelIdToNumExpr(LabelIdExpr* e)
    : expr_(e)
{
    if (!expr_) THROW(Exception, "e cannot be NULL");
}

LabelIdToNumExpr::~LabelIdToNumExpr(void) {
    delete expr_;
}

void
LabelIdToNumExpr::accept(ConstraintVisitor& v) const {
    v.beforeVisit(*this);
    v.visit(*this);
    expr_->accept(v);
    v.afterVisit(*this);
}

RegisterIdToNumExpr::RegisterIdToNumExpr(RegisterIdExpr* e)
    : expr_(e)
{
    if (!expr_) THROW(Exception, "e cannot be NULL");
}

RegisterIdToNumExpr::~RegisterIdToNumExpr(void) {
    delete expr_;
}

void
RegisterIdToNumExpr::accept(ConstraintVisitor& v) const {
    v.beforeVisit(*this);
    v.visit(*this);
    expr_->accept(v);
    v.afterVisit(*this);
}

ANodeIdExpr::ANodeIdExpr(const Id& id)
    : id_(id)
{}

ANodeIdExpr::~ANodeIdExpr(void) {}

void
ANodeIdExpr::accept(ConstraintVisitor& v) const {
    v.beforeVisit(*this);
    v.visit(*this);
    v.afterVisit(*this);
}

Id
ANodeIdExpr::getId(void) const {
    return id_;
}

AnInstanceIdExpr::AnInstanceIdExpr(const Id& id)
    : id_(id)
{}

AnInstanceIdExpr::~AnInstanceIdExpr(void) {}

void
AnInstanceIdExpr::accept(ConstraintVisitor& v) const {
    v.beforeVisit(*this);
    v.visit(*this);
    v.afterVisit(*this);
}

Id
AnInstanceIdExpr::getId(void) const {
    return id_;
}

AnInstructionIdExpr::AnInstructionIdExpr(const Id& id)
    : id_(id)
{}

AnInstructionIdExpr::~AnInstructionIdExpr(void) {}

void
AnInstructionIdExpr::accept(ConstraintVisitor& v) const {
    v.beforeVisit(*this);
    v.visit(*this);
    v.afterVisit(*this);
}

Id
AnInstructionIdExpr::getId(void) const {
    return id_;
}

APatternIdExpr::APatternIdExpr(const Id& id)
    : id_(id)
{}

APatternIdExpr::~APatternIdExpr(void) {}

void
APatternIdExpr::accept(ConstraintVisitor& v) const {
    v.beforeVisit(*this);
    v.visit(*this);
    v.afterVisit(*this);
}

Id
APatternIdExpr::getId(void) const {
    return id_;
}

ALabelIdExpr::ALabelIdExpr(const Id& id)
    : id_(id)
{}

ALabelIdExpr::~ALabelIdExpr(void) {}

void
ALabelIdExpr::accept(ConstraintVisitor& v) const {
    v.beforeVisit(*this);
    v.visit(*this);
    v.afterVisit(*this);
}

Id
ALabelIdExpr::getId(void) const {
    return id_;
}

ARegisterIdExpr::ARegisterIdExpr(const Id& id)
    : id_(id)
{}

ARegisterIdExpr::~ARegisterIdExpr(void) {}

void
ARegisterIdExpr::accept(ConstraintVisitor& v) const {
    v.beforeVisit(*this);
    v.visit(*this);
    v.afterVisit(*this);
}

Id
ARegisterIdExpr::getId(void) const {
    return id_;
}

ThisInstanceIdExpr::ThisInstanceIdExpr(void) {}

ThisInstanceIdExpr::~ThisInstanceIdExpr(void) {}

void
ThisInstanceIdExpr::accept(ConstraintVisitor& v) const {
    v.beforeVisit(*this);
    v.visit(*this);
    v.afterVisit(*this);
}

CovererOfActionNodeExpr::CovererOfActionNodeExpr(NodeIdExpr* e)
    : expr_(e)
{
    if (!expr_) THROW(Exception, "e cannot be NULL");
}

CovererOfActionNodeExpr::~CovererOfActionNodeExpr(void) {
    delete expr_;
}

void
CovererOfActionNodeExpr::accept(ConstraintVisitor& v) const {
    v.beforeVisit(*this);
    v.visit(*this);
    expr_->accept(v);
    v.afterVisit(*this);
}

DefinerOfEntityNodeExpr::DefinerOfEntityNodeExpr(NodeIdExpr* e)
    : expr_(e)
{
    if (!expr_) THROW(Exception, "e cannot be NULL");
}

DefinerOfEntityNodeExpr::~DefinerOfEntityNodeExpr(void) {
    delete expr_;
}

void
DefinerOfEntityNodeExpr::accept(ConstraintVisitor& v) const {
    v.beforeVisit(*this);
    v.visit(*this);
    expr_->accept(v);
    v.afterVisit(*this);
}

InstructionIdOfPatternExpr::InstructionIdOfPatternExpr(PatternIdExpr* e)
    : expr_(e)
{
    if (!expr_) THROW(Exception, "e cannot be NULL");
}

InstructionIdOfPatternExpr::~InstructionIdOfPatternExpr(void) {
    delete expr_;
}

void
InstructionIdOfPatternExpr::accept(ConstraintVisitor& v) const {
    v.beforeVisit(*this);
    v.visit(*this);
    expr_->accept(v);
    v.afterVisit(*this);
}

PatternIdOfInstanceExpr::PatternIdOfInstanceExpr(InstanceIdExpr* e)
    : expr_(e)
{
    if (!expr_) THROW(Exception, "e cannot be NULL");
}

PatternIdOfInstanceExpr::~PatternIdOfInstanceExpr(void) {
    delete expr_;
}

void
PatternIdOfInstanceExpr::accept(ConstraintVisitor& v) const {
    v.beforeVisit(*this);
    v.visit(*this);
    expr_->accept(v);
    v.afterVisit(*this);
}

LabelIdAllocatedToInstanceExpr::LabelIdAllocatedToInstanceExpr(
    InstanceIdExpr* e
)
    : expr_(e)
{
    if (!expr_) THROW(Exception, "e cannot be NULL");
}

LabelIdAllocatedToInstanceExpr::~LabelIdAllocatedToInstanceExpr(void) {
    delete expr_;
}

void
LabelIdAllocatedToInstanceExpr::accept(ConstraintVisitor& v) const {
    v.beforeVisit(*this);
    v.visit(*this);
    expr_->accept(v);
    v.afterVisit(*this);
}

LabelIdOfLabelNodeExpr::LabelIdOfLabelNodeExpr(NodeIdExpr* e)
    : expr_(e)
{
    if (!expr_) THROW(Exception, "e cannot be NULL");
}

LabelIdOfLabelNodeExpr::~LabelIdOfLabelNodeExpr(void) {
    delete expr_;
}

void
LabelIdOfLabelNodeExpr::accept(ConstraintVisitor& v) const {
    v.beforeVisit(*this);
    v.visit(*this);
    expr_->accept(v);
    v.afterVisit(*this);
}

RegisterIdAllocatedToDataNodeExpr::RegisterIdAllocatedToDataNodeExpr(
    NodeIdExpr* e
)
    : expr_(e)
{
    if (!expr_) THROW(Exception, "e cannot be NULL");
}

RegisterIdAllocatedToDataNodeExpr::~RegisterIdAllocatedToDataNodeExpr(void) {
    delete expr_;
}

void
RegisterIdAllocatedToDataNodeExpr::accept(ConstraintVisitor& v) const {
    v.beforeVisit(*this);
    v.visit(*this);
    expr_->accept(v);
    v.afterVisit(*this);
}
