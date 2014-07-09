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
#include <list>
#include <ostream>

namespace Model {

/**
 * Forward declarations
 */
class BoolExpr;
class ConstraintVisitor;
class NumExpr;
class PatternInstanceExpr;
class InstructionExpr;
class IntExpr;
class NodeExpr;
class PatternExpr;
class SetElemExpr;
class SetExpr;

/**
 * Extends a base class with an additional static method getStrName(), which is
 * done using the the curiously recurring template pattern (see
 * http://en.wikipedia.org/wiki/Curiously_recurring_template_pattern).
 *
 * @tparam Base
 *         The base class.
 * @tparam Derived
 *         The class deriving the base class.
 */
template <typename Base, typename Derived>
class WithStrName : public Base {
  public:
    /**
     * Gets the string name of this class. It is expected that a public static
     * variable #STRNAME is provided by the deriving class.
     *
     * @return String name.
     */
    static std::string
    getStrName(void) {
        return Derived::STRNAME;
    }
};

/**
 * Defines a base class for representing a constraint.
 */
class Constraint {
  public:
    /**
     * Destroys this object.
     */
    virtual
    ~Constraint(void)
    =0;

    /**
     * Converts this constraint into a the lispian string.
     *
     * @return Lisp string.
     */
    virtual std::string
    toLisp(void) const
    =0;

    /**
     * Prints this constraint as a the lispian string.
     *
     * @param os
     *        The stream.
     * @param c
     *        The constraint.
     * @returns The stream.
     */
    friend std::ostream&
    operator<<(std::ostream& os, const Constraint& c);

    /**
     * \copydef operator<<(std::ostream&, const Constraint&)
     */
    friend std::ostream&
    operator<<(std::ostream& os, const Constraint* c);
};

/**
 * Defines a class for representing a Boolean expression constraint.
 */
class BoolExprConstraint : public WithStrName<Constraint, BoolExprConstraint> {
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
    BoolExprConstraint(BoolExpr* expr);

    /**
     * Destroys this object.
     */
    ~BoolExprConstraint(void);

    /**
     * Gets the expression.
     *
     * @return The expression.
     */
    const BoolExpr*
    getExpr(void) const;

    /**
     * \copydoc Constraint::toLisp() const
     */
    virtual std::string
    toLisp(void) const;

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;

  protected:
    const BoolExpr* expr_;
};

/**
 * Base class for all expressions.
 */
class Expr {
  public:
    /**
     * Destroys this expression.
     */
    virtual
    ~Expr(void)
    =0;

    /**
     * Converts this constraint into a the lispian string.
     *
     * @return Lisp string.
     */
    virtual std::string
    toLisp(void) const
    =0;
};

/**
 * Base class for a Boolean expression.
 */
class BoolExpr : public Expr {
  public:
    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~BoolExpr(void)
    =0;
};

/**
 * Base class for a numerical expression.
 */
class NumExpr : public Expr {
  public:
    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~NumExpr(void)
    =0;
};

/**
 * Base class for an integer expression.
 */
class IntExpr : public Expr {
  public:
    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~IntExpr(void)
    =0;
};

/**
 * Base class for a node expression.
 */
class NodeExpr : public Expr {
  public:
    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~NodeExpr(void)
    =0;
};

/**
 * Base class for a pattern instance expression.
 */
class PatternInstanceExpr : public Expr {
  public:
    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~PatternInstanceExpr(void)
    =0;
};

/**
 * Base class for a instruction expression.
 */
class InstructionExpr : public Expr {
  public:
    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~InstructionExpr(void)
    =0;
};

/**
 * Base class for a pattern expression.
 */
class PatternExpr : public Expr {
  public:
    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~PatternExpr(void)
    =0;
};

/**
 * Base class for a label expression.
 */
class LabelExpr : public Expr {
  public:
    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~LabelExpr(void)
    =0;
};

/**
 * Base class for a register expression.
 */
class RegisterExpr : public Expr {
  public:
    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~RegisterExpr(void)
    =0;
};

/**
 * Base class for a set element expression.
 */
class SetElemExpr : public Expr {
  public:
    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~SetElemExpr(void)
    =0;
};

/**
 * Base class for a set expression.
 */
class SetExpr : public Expr {
  public:
    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~SetExpr(void)
    =0;
};

/**
 * Base class for expressions which take one expression as argument.
 *
 * @tparam Base
 *         Expression base class.
 * @tparam Derived
 *         The deriving class.
 * @tparam Arg
 *         Type of operand.
 */
template <typename Base, typename Derived, typename Arg>
class UnaryExpr : public WithStrName<Base, Derived> {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param expr
     *        The expression argument.
     * @throws Exception
     *         When \c expr is \c NULL.
     */
    UnaryExpr(const Arg* expr)
        : expr_(expr)
    {
        if (!expr_) THROW(Exception, "expr cannot be NULL");
    }

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~UnaryExpr(void) {
        delete expr_;
    }

    /**
     * Gets the expression argument.
     *
     * @returns The expression.
     */
    const Arg*
    getExpr(void) const {
        return expr_;
    }

    /**
     * \copydoc Expr::toLisp() const
     */
    virtual std::string
    toLisp(void) const {
        return std::string("(") + Derived::getStrName()
            + " " + expr_->toLisp()
            + ")";
    }

  private:
    const Arg* expr_;
};

/**
 * Base class for expressions which take two expressions as arguments.
 *
 * @tparam Base
 *         Expression base class.
 * @tparam Derived
 *         The deriving class.
 * @tparam Arg1
 *         Type of first operand.
 * @tparam Arg2
 *         Type of second operand.
 */
template <typename Base, typename Derived, typename Arg1, typename Arg2 = Arg1>
class BinaryExpr : public WithStrName<Base, Derived> {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param lhs
     *        The left-hand side expression argument.
     * @param rhs
     *        The right-hand side expression argument.
     * @throws Exception
     *         When either \c lhs or \c rhs is \c NULL.
     */
    BinaryExpr(const Arg1* lhs, const Arg2* rhs)
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
    ~BinaryExpr(void) {
        delete lhs_;
        delete rhs_;
    }

    /**
     * Gets the left-hand side expression argument.
     *
     * @returns The expression.
     */
    const Arg1*
    getLhs(void) const {
        return lhs_;
    }

    /**
     * Gets the right-hand side expression argument.
     *
     * @returns The expression.
     */
    const Arg2*
    getRhs(void) const {
        return rhs_;
    }

    /**
     * \copydoc Expr::toLisp() const
     */
    virtual std::string
    toLisp(void) const {
        return std::string("(") + Derived::getStrName()
            + " " + lhs_->toLisp()
            + " " + rhs_->toLisp()
            + ")";
    }

  private:
    const Arg1* lhs_;
    const Arg2* rhs_;
};

/**
 * Equality expression.
 */
class EqExpr : public BinaryExpr<BoolExpr, EqExpr, NumExpr> {
  public:
    /**
     * \copydoc BinaryExpr::BinaryExpr(const Arg1*, const Arg2*)
     */
    EqExpr(const NumExpr* lhs, const NumExpr* rhs);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~EqExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Disequality expression.
 */
class NeqExpr : public BinaryExpr<BoolExpr, NeqExpr, NumExpr> {
  public:
    /**
     * \copydoc BinaryExpr::BinaryExpr(const Arg1*, const Arg2*)
     */
    NeqExpr(const NumExpr* lhs, const NumExpr* rhs);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~NeqExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Greater-than expression.
 */
class GTExpr : public BinaryExpr<BoolExpr, GTExpr, NumExpr> {
  public:
    /**
     * \copydoc BinaryExpr::BinaryExpr(const Arg1*, const Arg2*)
     */
    GTExpr(const NumExpr* lhs, const NumExpr* rhs);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~GTExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Greater-than-or-equals-to expression.
 */
class GEExpr : public BinaryExpr<BoolExpr, GEExpr, NumExpr> {
  public:
    /**
     * \copydoc BinaryExpr::BinaryExpr(const Arg1*, const Arg2*)
     */
    GEExpr(const NumExpr* lhs, const NumExpr* rhs);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~GEExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Less-than expression.
 */
class LTExpr : public BinaryExpr<BoolExpr, LTExpr, NumExpr> {
  public:
    /**
     * \copydoc BinaryExpr::BinaryExpr(const Arg1*, const Arg2*)
     */
    LTExpr(const NumExpr* lhs, const NumExpr* rhs);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~LTExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Less-than-or-equals-to expression.
 */
class LEExpr : public BinaryExpr<BoolExpr, LEExpr, NumExpr> {
  public:
    /**
     * \copydoc BinaryExpr::BinaryExpr(const Arg1*, const Arg2*)
     */
    LEExpr(const NumExpr* lhs, const NumExpr* rhs);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~LEExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Equivalence expression.
 */
class EqvExpr : public BinaryExpr<BoolExpr, EqvExpr, BoolExpr> {
  public:
    /**
     * \copydoc BinaryExpr::BinaryExpr(const Arg1*, const Arg2*)
     */
    EqvExpr(const BoolExpr* lhs, const BoolExpr* rhs);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~EqvExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Implication expression.
 */
class ImpExpr : public BinaryExpr<BoolExpr, ImpExpr, BoolExpr> {
  public:
    /**
     * \copydoc BinaryExpr::BinaryExpr(const Arg1*, const Arg2*)
     */
    ImpExpr(const BoolExpr* lhs, const BoolExpr* rhs);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~ImpExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * And expression.
 */
class AndExpr : public BinaryExpr<BoolExpr, AndExpr, BoolExpr> {
  public:
    /**
     * \copydoc BinaryExpr::BinaryExpr(const Arg1*, const Arg2*)
     */
    AndExpr(const BoolExpr* lhs, const BoolExpr* rhs);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~AndExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Or expression.
 */
class OrExpr : public BinaryExpr<BoolExpr, OrExpr, BoolExpr> {
  public:
    /**
     * \copydoc BinaryExpr::BinaryExpr(const Arg1*, const Arg2*)
     */
    OrExpr(const BoolExpr* lhs, const BoolExpr* rhs);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~OrExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Not expression.
 */
class NotExpr : public UnaryExpr<BoolExpr, NotExpr, BoolExpr> {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param e
     *        The expression.
     * @throws Exception
     *         When \c e is \c NULL.
     */
    NotExpr(const BoolExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~NotExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Exists-in-set expression.
 */
class InSetExpr : public BinaryExpr<BoolExpr, InSetExpr, SetElemExpr, SetExpr> {
  public:
    /**
     * \copydoc BinaryExpr::BinaryExpr(const Arg1*, const Arg2*)
     */
    InSetExpr(const SetElemExpr* lhs, const SetExpr* rhs);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~InSetExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Expression so that a data node is an integer constant.
 */
class DataNodeIsAnIntConstantExpr
    : public UnaryExpr<BoolExpr, DataNodeIsAnIntConstantExpr, NodeExpr>
{
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param e
     *        The expression.
     * @throws Exception
     *         When \c e is \c NULL.
     */
    DataNodeIsAnIntConstantExpr(const NodeExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~DataNodeIsAnIntConstantExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Expression so that a data node represents an intermediate data value, which
 * cannot be reused by other pattern instances.
 */
class DataNodeIsIntermediateExpr
    : public UnaryExpr<BoolExpr, DataNodeIsIntermediateExpr, NodeExpr>
{
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param e
     *        The expression.
     * @throws Exception
     *         When \c e is \c NULL.
     */
    DataNodeIsIntermediateExpr(const NodeExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~DataNodeIsIntermediateExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Expression to say that a particular pattern instance is selected.
 */
class PatternInstanceIsSelectedExpr
    : public UnaryExpr<BoolExpr,
                       PatternInstanceIsSelectedExpr,
                       PatternInstanceExpr>
{
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param e
     *        The expression.
     * @throws Exception
     *         When \c e is \c NULL.
     */
    PatternInstanceIsSelectedExpr(const PatternInstanceExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~PatternInstanceIsSelectedExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Plus expression.
 */
class PlusExpr : public BinaryExpr<NumExpr, PlusExpr, NumExpr> {
  public:
    /**
     * \copydoc BinaryExpr::BinaryExpr(Expr*, Expr*)
     */
    PlusExpr(const NumExpr* lhs, const NumExpr* rhs);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~PlusExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Minus expression.
 */
class MinusExpr : public BinaryExpr<NumExpr, MinusExpr, NumExpr> {
  public:
    /**
     * \copydoc BinaryExpr::BinaryExpr(Expr*, Expr*)
     */
    MinusExpr(const NumExpr* lhs, const NumExpr* rhs);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~MinusExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Converts an integer into a numerical expression.
 */
class IntToNumExpr : public UnaryExpr<NumExpr, IntToNumExpr, IntExpr> {
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    IntToNumExpr(const IntExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~IntToNumExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Introduces an integer to be part of an expression.
 */
class AnIntegerExpr : public WithStrName<IntExpr, AnIntegerExpr> {
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
     * Gets the integer value.
     *
     * @returns The value.
     */
    int
    getValue(void) const;

    /**
     * \copydoc Constraint::toLisp() const
     */
    virtual std::string
    toLisp(void) const;

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;

  private:
    int i_;
};

/**
 * Retrieves the value of a data node which represents an integer constant. This
 * expression *must* be used together with #DataNodeIsIntConstantConstraint!
 */
class IntConstValueOfDataNodeExpr
    : public UnaryExpr<IntExpr, IntConstValueOfDataNodeExpr, NodeExpr>
{
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    IntConstValueOfDataNodeExpr(const NodeExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~IntConstValueOfDataNodeExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Converts a Boolean into a numerical expression.
 */
class BoolToNumExpr : public UnaryExpr<NumExpr, BoolToNumExpr, BoolExpr> {
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    BoolToNumExpr(const BoolExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~BoolToNumExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Converts a node into a numerical expression.
 */
class NodeToNumExpr : public UnaryExpr<NumExpr, NodeToNumExpr, NodeExpr> {
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    NodeToNumExpr(const NodeExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~NodeToNumExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Converts a pattern instance into a numerical expression.
 */
class PatternToNumExpr
    : public UnaryExpr<NumExpr, PatternToNumExpr, PatternExpr>
{
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    PatternToNumExpr(const PatternExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~PatternToNumExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Converts a instance into a numerical expression.
 */
class PatternInstanceToNumExpr
    : public UnaryExpr<NumExpr, PatternInstanceToNumExpr, PatternInstanceExpr>
{
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    PatternInstanceToNumExpr(const PatternInstanceExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~PatternInstanceToNumExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Converts a instruction into a numerical expression.
 */
class InstructionToNumExpr
    : public UnaryExpr<NumExpr, InstructionToNumExpr, InstructionExpr>
{
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    InstructionToNumExpr(const InstructionExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~InstructionToNumExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Converts a label into a numerical expression.
 */
class LabelToNumExpr
    : public UnaryExpr<NumExpr, LabelToNumExpr, LabelExpr>
{
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    LabelToNumExpr(const LabelExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~LabelToNumExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Converts a register into a numerical expression.
 */
class RegisterToNumExpr
    : public UnaryExpr<NumExpr, RegisterToNumExpr, RegisterExpr>
{
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    RegisterToNumExpr(const RegisterExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~RegisterToNumExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Represents the distance between a pattern instance and a label. The distance
 * starts from the end of the instruction represented by the pattern and stops
 * at the beginning of the first instruction within the basic block represented
 * by the label. The distance is negative if the label appears before the
 * pattern.
 */
class DistanceBetweenPatternInstanceAndLabelExpr
    : public BinaryExpr<NumExpr,
                        DistanceBetweenPatternInstanceAndLabelExpr,
                        PatternInstanceExpr,
                        LabelExpr>
{
  public:
    /**
     * \copydoc BinaryExpr::BinaryExpr(const Arg1*, const Arg2*)
     */
    DistanceBetweenPatternInstanceAndLabelExpr(const PatternInstanceExpr* lhs,
                                               const LabelExpr* rhs);


    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~DistanceBetweenPatternInstanceAndLabelExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Introduces a node ID to be part of an expression.
 */
class ANodeIDExpr : public WithStrName<NodeExpr, ANodeIDExpr> {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param id
     *        The node ID.
     */
    ANodeIDExpr(const ID& id);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~ANodeIDExpr(void);

    /**
     * Gets the node ID.
     *
     * @returns The ID.
     */
    ID
    getID(void) const;

    /**
     * \copydoc Constraint::toLisp() const
     */
    virtual std::string
    toLisp(void) const;

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;

  private:
    ID id_;
};

/**
 * Introduces a node array index to be part of an expression.
 */
class ANodeArrayIndexExpr
    : public WithStrName<NodeExpr, ANodeArrayIndexExpr>
{
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param i
     *        The node array index.
     */
    ANodeArrayIndexExpr(const ArrayIndex& i);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~ANodeArrayIndexExpr(void);

    /**
     * Gets the node array index.
     *
     * @returns The array index.
     */
    ArrayIndex
    getArrayIndex(void) const;

    /**
     * \copydoc Constraint::toLisp() const
     */
    virtual std::string
    toLisp(void) const;

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;

  private:
    ArrayIndex i_;
};

/**
 * Introduces a pattern instance ID to be part of an expression.
 */
class APatternInstanceIDExpr
    : public WithStrName<PatternInstanceExpr, APatternInstanceIDExpr>
{
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param id
     *        The instance ID.
     */
    APatternInstanceIDExpr(const ID& id);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~APatternInstanceIDExpr(void);

    /**
     * Gets the instance ID.
     *
     * @returns The ID.
     */
    ID
    getID(void) const;

    /**
     * \copydoc Constraint::toLisp() const
     */
    virtual std::string
    toLisp(void) const;

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;

  private:
    ID id_;
};

/**
 * Introduces a node array index to be part of an expression.
 */
class APatternInstanceArrayIndexExpr
    : public WithStrName<PatternInstanceExpr, APatternInstanceArrayIndexExpr>
{
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param i
     *        The node array index.
     */
    APatternInstanceArrayIndexExpr(const ArrayIndex& i);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~APatternInstanceArrayIndexExpr(void);

    /**
     * Gets the node array index.
     *
     * @returns The array index.
     */
    ArrayIndex
    getArrayIndex(void) const;

    /**
     * \copydoc Constraint::toLisp() const
     */
    virtual std::string
    toLisp(void) const;

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;

  private:
    ArrayIndex i_;
};

/**
 * Introduces an instruction ID to be part of an expression.
 */
class AnInstructionIDExpr
    : public WithStrName<InstructionExpr, AnInstructionIDExpr>
{
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param id
     *        The instruction ID.
     */
    AnInstructionIDExpr(const ID& id);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~AnInstructionIDExpr(void);

    /**
     * Gets the instruction ID.
     *
     * @returns The ID.
     */
    ID
    getID(void) const;

    /**
     * \copydoc Constraint::toLisp() const
     */
    virtual std::string
    toLisp(void) const;

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;

  private:
    ID id_;
};

/**
 * Introduces a label ID to be part of an expression.
 */
class APatternIDExpr : public WithStrName<PatternExpr, APatternIDExpr> {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param id
     *        The pattern ID.
     */
    APatternIDExpr(const ID& id);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~APatternIDExpr(void);

    /**
     * Gets the pattern ID.
     *
     * @returns The ID.
     */
    ID
    getID(void) const;

    /**
     * \copydoc Constraint::toLisp() const
     */
    virtual std::string
    toLisp(void) const;

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;

  private:
    ID id_;
};

/**
 * Introduces a label ID to be part of an expression.
 */
class ALabelIDExpr : public WithStrName<LabelExpr, ALabelIDExpr> {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param id
     *        The label ID.
     */
    ALabelIDExpr(const ID& id);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~ALabelIDExpr(void);

    /**
     * Gets the label ID.
     *
     * @returns The ID.
     */
    ID
    getID(void) const;

    /**
     * \copydoc Constraint::toLisp() const
     */
    virtual std::string
    toLisp(void) const;

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;

  private:
    ID id_;
};

/**
 * Introduces a register ID to be part of an expression.
 */
class ARegisterIDExpr : public WithStrName<RegisterExpr, ARegisterIDExpr> {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param id
     *        The register ID.
     */
    ARegisterIDExpr(const ID& id);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~ARegisterIDExpr(void);

    /**
     * Gets the register ID.
     *
     * @returns The ID.
     */
    ID
    getID(void) const;

    /**
     * \copydoc Constraint::toLisp() const
     */
    virtual std::string
    toLisp(void) const;

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;

  private:
    ID id_;
};

/**
 * Introduces a register array index to be part of an expression.
 */
class ARegisterArrayIndexExpr
    : public WithStrName<RegisterExpr, ARegisterArrayIndexExpr>
{
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param i
     *        The register array index.
     */
    ARegisterArrayIndexExpr(const ArrayIndex& i);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~ARegisterArrayIndexExpr(void);

    /**
     * Gets the register array index.
     *
     * @returns The array index.
     */
    ArrayIndex
    getArrayIndex(void) const;

    /**
     * \copydoc Constraint::toLisp() const
     */
    virtual std::string
    toLisp(void) const;

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;

  private:
    ArrayIndex i_;
};

/**
 * Represents the pattern instance where this is declared.
 */
class ThisPatternInstanceExpr
    : public WithStrName<PatternInstanceExpr, ThisPatternInstanceExpr>
{
  public:
    /**
     * \copydoc Expr::Expr()
     */
    ThisPatternInstanceExpr(void);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~ThisPatternInstanceExpr(void);

    /**
     * \copydoc Constraint::toLisp() const
     */
    virtual std::string
    toLisp(void) const;

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Represents the pattern instance which covers a certain action node.
 */
class CovererOfActionNodeExpr
    : public UnaryExpr<PatternInstanceExpr, CovererOfActionNodeExpr, NodeExpr>
{
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    CovererOfActionNodeExpr(const NodeExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~CovererOfActionNodeExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Represents the pattern instance which defines a certain data node.
 */
class DefinerOfDataNodeExpr
    : public UnaryExpr<PatternInstanceExpr, DefinerOfDataNodeExpr, NodeExpr>
{
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    DefinerOfDataNodeExpr(const NodeExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~DefinerOfDataNodeExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Represents the pattern instance which defines a certain state node.
 */
class DefinerOfStateNodeExpr
    : public UnaryExpr<PatternInstanceExpr, DefinerOfStateNodeExpr, NodeExpr>
{
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    DefinerOfStateNodeExpr(const NodeExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~DefinerOfStateNodeExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Represents the instruction to which a pattern belongs.
 */
class InstructionOfPatternExpr
    : public UnaryExpr<InstructionExpr, InstructionOfPatternExpr, PatternExpr>
{
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    InstructionOfPatternExpr(const PatternExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~InstructionOfPatternExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Represents the pattern to which a pattern instance is derived from.
 */
class PatternOfPatternInstanceExpr
    : public UnaryExpr<PatternExpr,
                       PatternOfPatternInstanceExpr,
                       PatternInstanceExpr>
{
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    PatternOfPatternInstanceExpr(const PatternInstanceExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~PatternOfPatternInstanceExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Represents the label to which a pattern instance has been allocated.
 */
class LabelAllocatedToPatternInstanceExpr
    : public UnaryExpr<LabelExpr,
                       LabelAllocatedToPatternInstanceExpr,
                       PatternInstanceExpr>
{
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    LabelAllocatedToPatternInstanceExpr(const PatternInstanceExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~LabelAllocatedToPatternInstanceExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Represents the label associated with a label node.
 */
class LabelOfLabelNodeExpr
    : public UnaryExpr<LabelExpr, LabelOfLabelNodeExpr, NodeExpr>
{
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    LabelOfLabelNodeExpr(const NodeExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~LabelOfLabelNodeExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Represents the register to which a data node has been allocated.
 */
class RegisterAllocatedToDataNodeExpr
    : public UnaryExpr<RegisterExpr,
                       RegisterAllocatedToDataNodeExpr,
                       NodeExpr>
{
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    RegisterAllocatedToDataNodeExpr(const NodeExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~RegisterAllocatedToDataNodeExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Union set expression.
 */
class UnionSetExpr : public BinaryExpr<SetExpr, UnionSetExpr, SetExpr> {
  public:
    /**
     * \copydoc BinaryExpr::BinaryExpr(const Arg1*, const Arg2*)
     */
    UnionSetExpr(const SetExpr* lhs, const SetExpr* rhs);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~UnionSetExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Intersect set expression.
 */
class IntersectSetExpr : public BinaryExpr<SetExpr, IntersectSetExpr, SetExpr> {
  public:
    /**
     * \copydoc BinaryExpr::BinaryExpr(const Arg1*, const Arg2*)
     */
    IntersectSetExpr(const SetExpr* lhs, const SetExpr* rhs);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~IntersectSetExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Difference set expression.
 */
class DiffSetExpr : public BinaryExpr<SetExpr, DiffSetExpr, SetExpr> {
  public:
    /**
     * \copydoc BinaryExpr::BinaryExpr(const Arg1*, const Arg2*)
     */
    DiffSetExpr(const SetExpr* lhs, const SetExpr* rhs);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~DiffSetExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Label dominator set expression.
 */
class DomSetOfLabelExpr
    : public UnaryExpr<SetExpr, DomSetOfLabelExpr, LabelExpr>
{
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    DomSetOfLabelExpr(const LabelExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~DomSetOfLabelExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Register class expression.
 */
class RegisterClassExpr : public WithStrName<SetExpr, RegisterClassExpr> {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param expr
     *        The list of expressions.
     */
    RegisterClassExpr(const std::list<const RegisterExpr*>& expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~RegisterClassExpr(void);

    /**
     * Gets the list of expressions.
     *
     * @return The list of expressions.
     */
    const std::list<const RegisterExpr*>&
    getExprList(void) const;

    /**
     * \copydoc Constraint::toLisp() const
     */
    virtual std::string
    toLisp(void) const;

  private:
    std::list<const RegisterExpr*> expr_;

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Converts a label into a set element expression.
 */
class LabelToSetElemExpr
    : public UnaryExpr<SetElemExpr, LabelToSetElemExpr, LabelExpr>
{
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    LabelToSetElemExpr(const LabelExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~LabelToSetElemExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

/**
 * Converts a register into a set element expression.
 */
class RegisterToSetElemExpr
    : public UnaryExpr<SetElemExpr, RegisterToSetElemExpr, RegisterExpr>
{
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    RegisterToSetElemExpr(const RegisterExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~RegisterToSetElemExpr(void);

  public:
    /**
     * @see WithStrName::getStrName() const
     */
    static const std::string STRNAME;
};

}

#endif
