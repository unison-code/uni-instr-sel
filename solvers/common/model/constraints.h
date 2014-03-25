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
 * Base class for a binary Boolean expression which takes two Boolean
 * expressions as arguments.
 */
class BinaryBoolToBoolExpr : public BoolExpr {
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
    BinaryBoolToBoolExpr(BoolExpr* lhs, BoolExpr* rhs);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~BinaryBoolToBoolExpr(void);

    /**
     * \copydoc Expr::accept(ConstraintVisitor&)
     */
    virtual void
    accept(ConstraintVisitor& v) const;

  private:
    BoolExpr* lhs_;
    BoolExpr* rhs_;
};

/**
 * Base class for a binary Boolean expression which takes two numerical
 * expressions as arguments.
 */
class BinaryNumToBoolExpr : public BoolExpr {
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
    BinaryNumToBoolExpr(NumExpr* lhs, NumExpr* rhs);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~BinaryNumToBoolExpr(void);

    /**
     * \copydoc Expr::accept(ConstraintVisitor&)
     */
    virtual void
    accept(ConstraintVisitor& v) const;

  private:
    NumExpr* lhs_;
    NumExpr* rhs_;
};

/**
 * Equality expression.
 */
class EqExpr : public BinaryNumToBoolExpr {
  public:
    /**
     * \copydoc BinaryNumToBoolExpr::BinaryNumToBoolExpr(NumExpr*, NumExpr*)
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
class NeqExpr : public BinaryNumToBoolExpr {
  public:
    /**
     * \copydoc BinaryNumToBoolExpr::BinaryNumToBoolExpr(NumExpr*, NumExpr*)
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
class GTExpr : public BinaryNumToBoolExpr {
  public:
    /**
     * \copydoc BinaryNumToBoolExpr::BinaryNumToBoolExpr(NumExpr*, NumExpr*)
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
class GEExpr : public BinaryNumToBoolExpr {
  public:
    /**
     * \copydoc BinaryNumToBoolExpr::BinaryNumToBoolExpr(NumExpr*, NumExpr*)
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
class LTExpr : public BinaryNumToBoolExpr {
  public:
    /**
     * \copydoc BinaryNumToBoolExpr::BinaryNumToBoolExpr(NumExpr*, NumExpr*)
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
class LEExpr : public BinaryNumToBoolExpr {
  public:
    /**
     * \copydoc BinaryNumToBoolExpr::BinaryNumToBoolExpr(NumExpr*, NumExpr*)
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
class EqvExpr : public BinaryBoolToBoolExpr {
  public:
    /**
     * \copydoc BinaryBoolToBoolExpr::BinaryBoolToBoolExpr(BoolExpr*, BoolExpr*)
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
class ImpExpr : public BinaryBoolToBoolExpr {
  public:
    /**
     * \copydoc BinaryBoolToBoolExpr::BinaryBoolToBoolExpr(BoolExpr*, BoolExpr*)
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
class AndExpr : public BinaryBoolToBoolExpr {
  public:
    /**
     * \copydoc BinaryBoolToBoolExpr::BinaryBoolToBoolExpr(BoolExpr*, BoolExpr*)
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
class OrExpr : public BinaryBoolToBoolExpr {
  public:
    /**
     * \copydoc BinaryBoolToBoolExpr::BinaryBoolToBoolExpr(BoolExpr*, BoolExpr*)
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
 * Base class for a binary numerical expression which takes two numerical
 * expressions as arguments.
 */
class BinaryNumToNumExpr : public NumExpr {
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
    BinaryNumToNumExpr(NumExpr* lhs, NumExpr* rhs);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~BinaryNumToNumExpr(void);

    /**
     * \copydoc Expr::accept(ConstraintVisitor&)
     */
    virtual void
    accept(ConstraintVisitor& v) const;

  private:
    NumExpr* lhs_;
    NumExpr* rhs_;
};

/**
 * Plus expression.
 */
class PlusExpr : public BinaryNumToNumExpr {
  public:
    /**
     * \copydoc BinaryNumToNumExpr::BinaryNumToNumExpr(NumExpr*, NumExpr*)
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
class MinusExpr : public BinaryNumToNumExpr {
  public:
    /**
     * \copydoc BinaryNumToNumExpr::BinaryNumToNumExpr(NumExpr*, NumExpr*)
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

}

#endif
