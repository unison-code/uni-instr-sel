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

#ifndef SOLVERS_COMMON_MODEL_CONSTRAINTS__
#define SOLVERS_COMMON_MODEL_CONSTRAINTS__

#include "constraintvisitor.h"
#include "types.h"
#include "../exceptions/exception.h"

namespace Model {

/**
 * Forward declarations
 */
class BoolExpr;
class ConstraintVisitor;
class NumExpr;
class InstanceIdExpr;
class InstructionIdExpr;
class NodeIdExpr;
class PatternIdExpr;

/**
 * Defines a class for representing a constraint.
 */
class Constraint {
  public:
    /**
     * Creates a constraint from a Boolean expression. The new object assumes
     * ownership of the expression object.
     *
     * @param e
     *        The expression.
     * @throws Exception
     *         When \c e is \c NULL.
     */
    Constraint(BoolExpr* e);

    /**
     * Destroys this object.
     */
    ~Constraint(void);

    /**
     * Performs a walk through the expression tree which constitute this
     * constraint. The walk is implemented using the \c Visitor design pattern.
     *
     * @param v
     *        The visitor.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    void
    walk(ConstraintVisitor& v) const;

  protected:
    BoolExpr* expr_;
};

/**
 * Base class for all expressions.
 */
class Expr {
  public:
    /**
     * Creates an expression.
     */
    Expr(void);

    /**
     * Destroys this expression.
     */
    virtual
    ~Expr(void);

    /**
     * Accepts a constraint visitor.
     *
     * @param v
     *        The visitor.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    accept(ConstraintVisitor& v) const
    =0;
};

/**
 * Base class for a Boolean expression.
 */
class BoolExpr : public Expr {
  public:
    /**
     * \copydoc Expr::Expr()
     */
    BoolExpr(void);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~BoolExpr(void);
};

/**
 * Base class for a numerical expression.
 */
class NumExpr : public Expr {
  public:
    /**
     * \copydoc Expr::Expr()
     */
    NumExpr(void);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~NumExpr(void);
};

/**
 * Base class for a node ID expression.
 */
class NodeIdExpr : public Expr {
  public:
    /**
     * \copydoc Expr::Expr()
     */
    NodeIdExpr(void);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~NodeIdExpr(void);
};

/**
 * Base class for a pattern instance ID expression.
 */
class InstanceIdExpr : public Expr {
  public:
    /**
     * \copydoc Expr::Expr()
     */
    InstanceIdExpr(void);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~InstanceIdExpr(void);
};

/**
 * Base class for a instruction ID expression.
 */
class InstructionIdExpr : public Expr {
  public:
    /**
     * \copydoc Expr::Expr()
     */
    InstructionIdExpr(void);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~InstructionIdExpr(void);
};

/**
 * Base class for a pattern ID expression.
 */
class PatternIdExpr : public Expr {
  public:
    /**
     * \copydoc Expr::Expr()
     */
    PatternIdExpr(void);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~PatternIdExpr(void);
};

/**
 * Base class for a label ID expression.
 */
class LabelIdExpr : public Expr {
  public:
    /**
     * \copydoc Expr::Expr()
     */
    LabelIdExpr(void);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~LabelIdExpr(void);
};

/**
 * Base class for a register ID expression.
 */
class RegisterIdExpr : public Expr {
  public:
    /**
     * \copydoc Expr::Expr()
     */
    RegisterIdExpr(void);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~RegisterIdExpr(void);
};

/**
 * Base class for a binary Boolean expression.
 *
 * @tparam Derived
 *         The derived expression class.
 */
template <typename Derived>
class BinaryBoolExpr : public BoolExpr {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param lhs
     *        The left-hand side expression.
     * @param rhs
     *        The right-hand side expression.
     * @throws Exception
     *         When either \c lhs or \c rhs is \c NULL.
     */
    BinaryBoolExpr(Expr* lhs, Expr* rhs)
        : lhs_(lhs),
          rhs_(rhs)
    {
        if (!lhs_) THROW(Exception, "lhs cannot be NULL");
        if (!rhs_) THROW(Exception, "rhs cannot be NULL");
    }


    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~BinaryBoolExpr(void) {
        delete lhs_;
        delete rhs_;
    }

    /**
     * \copydoc Expr::accept(ConstraintVisitor&)
     */
    virtual void
    accept(ConstraintVisitor& v) const {
        v.beforeVisit(getDerived());
        v.visit(getDerived());
        lhs_->accept(v);
        v.betweenChildrenVisits(getDerived());
        rhs_->accept(v);
        v.afterVisit(getDerived());
    }

  private:
    /**
     * Gets the derived object.
     *
     * @return Derived object.
     */
    const Derived&
    getDerived(void) const {
        return *static_cast<const Derived*>(this);
    }

  private:
    Expr* lhs_;
    Expr* rhs_;
};

/**
 * Base class for a binary numerical expression.
 *
 * @tparam Derived
 *         The derived expression class.
 */
template <typename Derived>
class BinaryNumExpr : public NumExpr {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param lhs
     *        The left-hand side expression.
     * @param rhs
     *        The right-hand side expression.
     * @throws Exception
     *         When either \c lhs or \c rhs is \c NULL.
     */
    BinaryNumExpr(Expr* lhs, Expr* rhs)
        : lhs_(lhs),
          rhs_(rhs)
    {
        if (!lhs_) THROW(Exception, "lhs cannot be NULL");
        if (!rhs_) THROW(Exception, "rhs cannot be NULL");
    }


    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~BinaryNumExpr(void) {
        delete lhs_;
        delete rhs_;
    }

    /**
     * \copydoc Expr::accept(ConstraintVisitor&)
     */
    virtual void
    accept(ConstraintVisitor& v) const {
        v.beforeVisit(getDerived());
        v.visit(getDerived());
        lhs_->accept(v);
        v.betweenChildrenVisits(getDerived());
        rhs_->accept(v);
        v.afterVisit(getDerived());
    }

  private:
    /**
     * Gets the derived object.
     *
     * @return Derived object.
     */
    const Derived&
    getDerived(void) const {
        return *static_cast<const Derived*>(this);
    }

  private:
    Expr* lhs_;
    Expr* rhs_;
};

/**
 * Equality expression.
 */
class EqExpr : public BinaryBoolExpr<EqExpr> {
  public:
    /**
     * \copydoc BinaryBoolExpr::BinaryBoolExpr(NumExpr*, NumExpr*)
     */
    EqExpr(NumExpr* lhs, NumExpr* rhs);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~EqExpr(void);
};

/**
 * Disequality expression.
 */
class NeqExpr : public BinaryBoolExpr<NeqExpr> {
  public:
    /**
     * \copydoc BinaryBoolExpr::BinaryBoolExpr(NumExpr*, NumExpr*)
     */
    NeqExpr(NumExpr* lhs, NumExpr* rhs);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~NeqExpr(void);
};

/**
 * Greater-than expression.
 */
class GTExpr : public BinaryBoolExpr<GTExpr> {
  public:
    /**
     * \copydoc BinaryBoolExpr::BinaryBoolExpr(NumExpr*, NumExpr*)
     */
    GTExpr(NumExpr* lhs, NumExpr* rhs);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~GTExpr(void);
};

/**
 * Greater-than-or-equals-to expression.
 */
class GEExpr : public BinaryBoolExpr<GEExpr> {
  public:
    /**
     * \copydoc BinaryBoolExpr::BinaryBoolExpr(NumExpr*, NumExpr*)
     */
    GEExpr(NumExpr* lhs, NumExpr* rhs);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~GEExpr(void);
};

/**
 * Less-than expression.
 */
class LTExpr : public BinaryBoolExpr<LTExpr> {
  public:
    /**
     * \copydoc BinaryBoolExpr::BinaryBoolExpr(NumExpr*, NumExpr*)
     */
    LTExpr(NumExpr* lhs, NumExpr* rhs);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~LTExpr(void);
};

/**
 * Less-than-or-equals-to expression.
 */
class LEExpr : public BinaryBoolExpr<LEExpr> {
  public:
    /**
     * \copydoc BinaryBoolExpr::BinaryBoolExpr(NumExpr*, NumExpr*)
     */
    LEExpr(NumExpr* lhs, NumExpr* rhs);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~LEExpr(void);
};

/**
 * Equivalence expression.
 */
class EqvExpr : public BinaryBoolExpr<EqvExpr> {
  public:
    /**
     * \copydoc BinaryBoolExpr::BinaryBoolExpr(Expr*, Expr*)
     */
    EqvExpr(BoolExpr* lhs, BoolExpr* rhs);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~EqvExpr(void);
};

/**
 * Implication expression.
 */
class ImpExpr : public BinaryBoolExpr<ImpExpr> {
  public:
    /**
     * \copydoc BinaryBoolExpr::BinaryBoolExpr(Expr*, Expr*)
     */
    ImpExpr(BoolExpr* lhs, BoolExpr* rhs);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~ImpExpr(void);
};

/**
 * And expression.
 */
class AndExpr : public BinaryBoolExpr<AndExpr> {
  public:
    /**
     * \copydoc BinaryBoolExpr::BinaryBoolExpr(Expr*, Expr*)
     */
    AndExpr(BoolExpr* lhs, BoolExpr* rhs);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~AndExpr(void);
};

/**
 * Or expression.
 */
class OrExpr : public BinaryBoolExpr<OrExpr> {
  public:
    /**
     * \copydoc BinaryBoolExpr::BinaryBoolExpr(Expr*, Expr*)
     */
    OrExpr(BoolExpr* lhs, BoolExpr* rhs);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~OrExpr(void);
};

/**
 * Not expression.
 */
class NotExpr : public BoolExpr {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param e
     *        The expression.
     * @throws Exception
     *         When \c e is \c NULL.
     */
    NotExpr(BoolExpr* e);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~NotExpr(void);

    /**
     * \copydoc Expr::accept(ConstraintVisitor&)
     */
    virtual void
    accept(ConstraintVisitor& v) const;

  private:
    BoolExpr* expr_;
};

/**
 * Plus expression.
 */
class PlusExpr : public BinaryNumExpr<PlusExpr> {
  public:
    /**
     * \copydoc BinaryNumExpr::BinaryNumExpr(Expr*, Expr*)
     */
    PlusExpr(NumExpr* lhs, NumExpr* rhs);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~PlusExpr(void);
};

/**
 * Minus expression.
 */
class MinusExpr : public BinaryNumExpr<MinusExpr> {
  public:
    /**
     * \copydoc BinaryNumExpr::BinaryNumExpr(Expr*, Expr*)
     */
    MinusExpr(NumExpr* lhs, NumExpr* rhs);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~MinusExpr(void);
};

/**
 * Introduces an integer to be part of an expression.
 */
class AnIntegerExpr : public NumExpr {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param i
     *        The integer.
     * @throws Exception
     *         When \c e is \c NULL.
     */
    AnIntegerExpr(int i);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~AnIntegerExpr(void);

    /**
     * \copydoc Expr::accept(ConstraintVisitor&)
     */
    virtual void
    accept(ConstraintVisitor& v) const;

    /**
     * Gets the integer value.
     *
     * @returns The value.
     */
    int
    getValue(void) const;

  private:
    int i_;
};

/**
 * Converts a node ID expression to a numerical expression.
 */
class NodeIdToNumExpr : public NumExpr {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param e
     *        The expression.
     * @throws Exception
     *         When \c e is \c NULL.
     */
    NodeIdToNumExpr(NodeIdExpr* e);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~NodeIdToNumExpr(void);

    /**
     * \copydoc Expr::accept(ConstraintVisitor&)
     */
    virtual void
    accept(ConstraintVisitor& v) const;

  private:
    NodeIdExpr* expr_;
};

/**
 * Converts a pattern instance ID expression to a numerical expression.
 */
class InstanceIdToNumExpr : public NumExpr {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param e
     *        The expression.
     * @throws Exception
     *         When \c e is \c NULL.
     */
    InstanceIdToNumExpr(InstanceIdExpr* e);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~InstanceIdToNumExpr(void);

    /**
     * \copydoc Expr::accept(ConstraintVisitor&)
     */
    virtual void
    accept(ConstraintVisitor& v) const;

  private:
    InstanceIdExpr* expr_;
};

/**
 * Converts an instruction ID expression to a numerical expression.
 */
class InstructionIdToNumExpr : public NumExpr {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param e
     *        The expression.
     * @throws Exception
     *         When \c e is \c NULL.

     */
    InstructionIdToNumExpr(InstructionIdExpr* e);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~InstructionIdToNumExpr(void);

    /**
     * \copydoc Expr::accept(ConstraintVisitor&)
     */
    virtual void
    accept(ConstraintVisitor& v) const;

  private:
    InstructionIdExpr* expr_;
};

/**
 * Converts a pattern ID expression to a numerical expression.
 */
class PatternIdToNumExpr : public NumExpr {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param e
     *        The expression.
     * @throws Exception
     *         When \c e is \c NULL.
     */
    PatternIdToNumExpr(PatternIdExpr* e);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~PatternIdToNumExpr(void);

    /**
     * \copydoc Expr::accept(ConstraintVisitor&)
     */
    virtual void
    accept(ConstraintVisitor& v) const;

  private:
    PatternIdExpr* expr_;
};

/**
 * Converts a label ID expression to a numerical expression.
 */
class LabelIdToNumExpr : public NumExpr {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param e
     *        The expression.
     * @throws Exception
     *         When \c e is \c NULL.
     */
    LabelIdToNumExpr(LabelIdExpr* e);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~LabelIdToNumExpr(void);

    /**
     * \copydoc Expr::accept(ConstraintVisitor&)
     */
    virtual void
    accept(ConstraintVisitor& v) const;

  private:
    LabelIdExpr* expr_;
};

/**
 * Converts a register ID expression to a numerical expression.
 */
class RegisterIdToNumExpr : public NumExpr {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param e
     *        The expression.
     * @throws Exception
     *         When \c e is \c NULL.
     */
    RegisterIdToNumExpr(RegisterIdExpr* e);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~RegisterIdToNumExpr(void);

    /**
     * \copydoc Expr::accept(ConstraintVisitor&)
     */
    virtual void
    accept(ConstraintVisitor& v) const;

  private:
    RegisterIdExpr* expr_;
};

/**
 * Introduces a node ID to be part of an expression.
 */
class ANodeIdExpr : public NodeIdExpr {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param i
     *        The node ID.
     */
    ANodeIdExpr(const Id& i);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~ANodeIdExpr(void);

    /**
     * \copydoc Expr::accept(ConstraintVisitor&)
     */
    virtual void
    accept(ConstraintVisitor& v) const;

    /**
     * Gets the node ID.
     *
     * @returns The ID.
     */
    Id
    getId(void) const;

  private:
    Id id_;
};

/**
 * Introduces a pattern instance ID to be part of an expression.
 */
class AnInstanceIdExpr : public InstanceIdExpr {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param i
     *        The instance ID.
     */
    AnInstanceIdExpr(const Id& i);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~AnInstanceIdExpr(void);

    /**
     * \copydoc Expr::accept(ConstraintVisitor&)
     */
    virtual void
    accept(ConstraintVisitor& v) const;

    /**
     * Gets the instance ID.
     *
     * @returns The ID.
     */
    Id
    getId(void) const;

  private:
    Id id_;
};

/**
 * Introduces an instruction ID to be part of an expression.
 */
class AnInstructionIdExpr : public InstructionIdExpr {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param i
     *        The instruction ID.
     */
    AnInstructionIdExpr(const Id& i);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~AnInstructionIdExpr(void);

    /**
     * \copydoc Expr::accept(ConstraintVisitor&)
     */
    virtual void
    accept(ConstraintVisitor& v) const;

    /**
     * Gets the instruction ID.
     *
     * @returns The ID.
     */
    Id
    getId(void) const;

  private:
    Id id_;
};

/**
 * Introduces a label ID to be part of an expression.
 */
class APatternIdExpr : public PatternIdExpr {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param i
     *        The pattern ID.
     */
    APatternIdExpr(const Id& i);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~APatternIdExpr(void);

    /**
     * \copydoc Expr::accept(ConstraintVisitor&)
     */
    virtual void
    accept(ConstraintVisitor& v) const;

    /**
     * Gets the pattern ID.
     *
     * @returns The ID.
     */
    Id
    getId(void) const;

  private:
    Id id_;
};

/**
 * Introduces a label ID to be part of an expression.
 */
class ALabelIdExpr : public LabelIdExpr {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param i
     *        The label ID.
     */
    ALabelIdExpr(const Id& i);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~ALabelIdExpr(void);

    /**
     * \copydoc Expr::accept(ConstraintVisitor&)
     */
    virtual void
    accept(ConstraintVisitor& v) const;

    /**
     * Gets the label ID.
     *
     * @returns The ID.
     */
    Id
    getId(void) const;

  private:
    Id id_;
};

/**
 * Introduces a register ID to be part of an expression.
 */
class ARegisterIdExpr : public RegisterIdExpr {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param i
     *        The register ID.
     */
    ARegisterIdExpr(const Id& i);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~ARegisterIdExpr(void);

    /**
     * \copydoc Expr::accept(ConstraintVisitor&)
     */
    virtual void
    accept(ConstraintVisitor& v) const;

    /**
     * Gets the register ID.
     *
     * @returns The ID.
     */
    Id
    getId(void) const;

  private:
    Id id_;
};

/**
 * Represents the ID of the pattern instance where this is declared.
 */
class ThisInstanceIdExpr : public InstanceIdExpr {
  public:
    /**
     * \copydoc Expr::Expr()
     */
    ThisInstanceIdExpr(void);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~ThisInstanceIdExpr(void);

    /**
     * \copydoc Expr::accept(ConstraintVisitor&)
     */
    virtual void
    accept(ConstraintVisitor& v) const;
};

/**
 * Represents the pattern instance ID which covers a certain action node.
 */
class CovererOfActionNodeExpr : public InstanceIdExpr {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param e
     *        The expression.
     * @throws Exception
     *         When \c e is \c NULL.
     */
    CovererOfActionNodeExpr(NodeIdExpr* e);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~CovererOfActionNodeExpr(void);

    /**
     * \copydoc Expr::accept(ConstraintVisitor&)
     */
    virtual void
    accept(ConstraintVisitor& v) const;

  private:
    NodeIdExpr* expr_;
};

/**
 * Represents the pattern instance ID which defines a certain entity node.
 */
class DefinerOfEntityNodeExpr : public InstanceIdExpr {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param e
     *        The expression.
     * @throws Exception
     *         When \c e is \c NULL.
     */
    DefinerOfEntityNodeExpr(NodeIdExpr* e);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~DefinerOfEntityNodeExpr(void);

    /**
     * \copydoc Expr::accept(ConstraintVisitor&)
     */
    virtual void
    accept(ConstraintVisitor& v) const;

  private:
    NodeIdExpr* expr_;
};

/**
 * Represents the instruction ID to which a pattern belongs.
 */
class InstructionIdOfPatternExpr : public InstructionIdExpr {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param e
     *        The expression.
     * @throws Exception
     *         When \c e is \c NULL.
     */
    InstructionIdOfPatternExpr(PatternIdExpr* e);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~InstructionIdOfPatternExpr(void);

    /**
     * \copydoc Expr::accept(ConstraintVisitor&)
     */
    virtual void
    accept(ConstraintVisitor& v) const;

  private:
    PatternIdExpr* expr_;
};

/**
 * Represents the pattern ID to which a pattern instance is derived from.
 */
class PatternIdOfInstanceExpr : public PatternIdExpr {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param e
     *        The expression.
     * @throws Exception
     *         When \c e is \c NULL.
     */
    PatternIdOfInstanceExpr(InstanceIdExpr* e);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~PatternIdOfInstanceExpr(void);

    /**
     * \copydoc Expr::accept(ConstraintVisitor&)
     */
    virtual void
    accept(ConstraintVisitor& v) const;

  private:
    InstanceIdExpr* expr_;
};

/**
 * Represents the ID of the label to which a pattern instance has been
 * allocated.
 */
class LabelIdAllocatedToInstanceExpr : public LabelIdExpr {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param e
     *        The expression.
     * @throws Exception
     *         When \c e is \c NULL.
     */
    LabelIdAllocatedToInstanceExpr(InstanceIdExpr* e);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~LabelIdAllocatedToInstanceExpr(void);

    /**
     * \copydoc Expr::accept(ConstraintVisitor&)
     */
    virtual void
    accept(ConstraintVisitor& v) const;

  private:
    InstanceIdExpr* expr_;
};

/**
 * Represents the label ID associated with a label node.
 */
class LabelIdOfLabelNodeExpr : public LabelIdExpr {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param e
     *        The expression.
     * @throws Exception
     *         When \c e is \c NULL.
     */
    LabelIdOfLabelNodeExpr(NodeIdExpr* e);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~LabelIdOfLabelNodeExpr(void);

    /**
     * \copydoc Expr::accept(ConstraintVisitor&)
     */
    virtual void
    accept(ConstraintVisitor& v) const;

  private:
    NodeIdExpr* expr_;
};

/**
 * Represents the label ID associated with a label node.
 */
class RegisterIdAllocatedToDataNodeExpr : public RegisterIdExpr {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param e
     *        The expression.
     * @throws Exception
     *         When \c e is \c NULL.
     */
    RegisterIdAllocatedToDataNodeExpr(NodeIdExpr* e);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~RegisterIdAllocatedToDataNodeExpr(void);

    /**
     * \copydoc Expr::accept(ConstraintVisitor&)
     */
    virtual void
    accept(ConstraintVisitor& v) const;

  private:
    NodeIdExpr* expr_;
};

}

#endif
