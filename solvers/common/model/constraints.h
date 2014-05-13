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

namespace Model {

/**
 * Forward declarations
 */
class BoolExpr;
class ConstraintVisitor;
class NumExpr;
class PatternInstanceIDExpr;
class InstructionIDExpr;
class IntExpr;
class NodeIDExpr;
class PatternIDExpr;
class SetElemExpr;
class SetExpr;

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
};

/**
 * Defines a class for representing a Boolean expression constraint.
 */
class BoolExprConstraint : public Constraint {
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

  protected:
    const BoolExpr* expr_;
};

/**
 * Defines a class for representing a constraint that a data node is an integer
 * constant.
 */
class DataNodeIsIntConstantConstraint : public Constraint {
  public:
    /**
     * Creates a new constraint.
     *
     * @param id
     *        The ID of the data node.
     * @throws Exception
     *         When \c e is \c NULL.
     */
    DataNodeIsIntConstantConstraint(const ID& id);

    /**
     * Destroys this object.
     */
    ~DataNodeIsIntConstantConstraint(void);

    /**
     * Gets the node ID of the data node.
     *
     * @return The node ID.
     */
    ID
    getNodeID(void) const;

  protected:
    const ID id_;
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
    ~Expr(void)
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
    ~BoolExpr(void)
    =0;
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
    ~NumExpr(void)
    =0;
};

/**
 * Base class for an integer expression.
 */
class IntExpr : public Expr {
  public:
    /**
     * \copydoc Expr::Expr()
     */
    IntExpr(void);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~IntExpr(void)
    =0;
};

/**
 * Base class for a node ID expression.
 */
class NodeIDExpr : public Expr {
  public:
    /**
     * \copydoc Expr::Expr()
     */
    NodeIDExpr(void);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~NodeIDExpr(void)
    =0;
};

/**
 * Base class for a pattern instance ID expression.
 */
class PatternInstanceIDExpr : public Expr {
  public:
    /**
     * \copydoc Expr::Expr()
     */
    PatternInstanceIDExpr(void);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~PatternInstanceIDExpr(void)
    =0;
};

/**
 * Base class for a instruction ID expression.
 */
class InstructionIDExpr : public Expr {
  public:
    /**
     * \copydoc Expr::Expr()
     */
    InstructionIDExpr(void);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~InstructionIDExpr(void)
    =0;
};

/**
 * Base class for a pattern ID expression.
 */
class PatternIDExpr : public Expr {
  public:
    /**
     * \copydoc Expr::Expr()
     */
    PatternIDExpr(void);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~PatternIDExpr(void)
    =0;
};

/**
 * Base class for a label ID expression.
 */
class LabelIDExpr : public Expr {
  public:
    /**
     * \copydoc Expr::Expr()
     */
    LabelIDExpr(void);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~LabelIDExpr(void)
    =0;
};

/**
 * Base class for a register ID expression.
 */
class RegisterIDExpr : public Expr {
  public:
    /**
     * \copydoc Expr::Expr()
     */
    RegisterIDExpr(void);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~RegisterIDExpr(void)
    =0;
};

/**
 * Base class for a set element expression.
 */
class SetElemExpr : public Expr {
  public:
    /**
     * \copydoc Expr::Expr()
     */
    SetElemExpr(void);

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
     * \copydoc Expr::Expr()
     */
    SetExpr(void);

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
 * @tparam Arg
 *         Type of operand.
 */
template <typename Base, typename Arg>
class UnaryExpr : public Base {
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

  private:
    const Arg* expr_;
};

/**
 * Base class for expressions which take two expressions as arguments.
 *
 * @tparam Base
 *         Expression base class.
 * @tparam Arg1
 *         Type of first operand.
 * @tparam Arg2
 *         Type of second operand.
 */
template <typename Base, typename Arg1, typename Arg2 = Arg1>
class BinaryExpr : public Base {
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

  private:
    const Arg1* lhs_;
    const Arg2* rhs_;
};

/**
 * Equality expression.
 */
class EqExpr : public BinaryExpr<BoolExpr, NumExpr> {
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
};

/**
 * Disequality expression.
 */
class NeqExpr : public BinaryExpr<BoolExpr, NumExpr> {
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
};

/**
 * Greater-than expression.
 */
class GTExpr : public BinaryExpr<BoolExpr, NumExpr> {
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
};

/**
 * Greater-than-or-equals-to expression.
 */
class GEExpr : public BinaryExpr<BoolExpr, NumExpr> {
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
};

/**
 * Less-than expression.
 */
class LTExpr : public BinaryExpr<BoolExpr, NumExpr> {
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
};

/**
 * Less-than-or-equals-to expression.
 */
class LEExpr : public BinaryExpr<BoolExpr, NumExpr> {
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
};

/**
 * Equivalence expression.
 */
class EqvExpr : public BinaryExpr<BoolExpr, BoolExpr> {
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
};

/**
 * Implication expression.
 */
class ImpExpr : public BinaryExpr<BoolExpr, BoolExpr> {
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
};

/**
 * And expression.
 */
class AndExpr : public BinaryExpr<BoolExpr, BoolExpr> {
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
};

/**
 * Or expression.
 */
class OrExpr : public BinaryExpr<BoolExpr, BoolExpr> {
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
};

/**
 * Not expression.
 */
class NotExpr : public UnaryExpr<BoolExpr, BoolExpr> {
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
};

/**
 * Exists-in-set expression.
 */
class InSetExpr : public BinaryExpr<BoolExpr, SetElemExpr, SetExpr> {
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
};

/**
 * Plus expression.
 */
class PlusExpr : public BinaryExpr<NumExpr, NumExpr> {
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
};

/**
 * Minus expression.
 */
class MinusExpr : public BinaryExpr<NumExpr, NumExpr> {
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
};

/**
 * Converts an integer into a numerical expression.
 */
class IntToNumExpr : public UnaryExpr<NumExpr, IntExpr> {
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
};

/**
 * Introduces an integer to be part of an expression.
 */
class AnIntegerExpr : public IntExpr {
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

  private:
    int i_;
};

/**
 * Retrieves the value of a data node which represents an integer constant. This
 * expression *must* be used together with #DataNodeIsIntConstantConstraint!
 */
class IntConstValueOfDataNodeExpr : public UnaryExpr<IntExpr, NodeIDExpr> {
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    IntConstValueOfDataNodeExpr(const NodeIDExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~IntConstValueOfDataNodeExpr(void);
};

/**
 * Converts a Boolean into a numerical expression.
 */
class BoolToNumExpr : public UnaryExpr<NumExpr, BoolExpr> {
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
};

/**
 * Converts a node ID into a numerical expression.
 */
class NodeIDToNumExpr : public UnaryExpr<NumExpr, NodeIDExpr> {
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    NodeIDToNumExpr(const NodeIDExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~NodeIDToNumExpr(void);
};

/**
 * Converts a pattern instance ID into a numerical expression.
 */
class PatternIDToNumExpr : public UnaryExpr<NumExpr, PatternIDExpr> {
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    PatternIDToNumExpr(const PatternIDExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~PatternIDToNumExpr(void);
};

/**
 * Converts a instance ID into a numerical expression.
 */
class PatternInstanceIDToNumExpr : public UnaryExpr<NumExpr, PatternInstanceIDExpr> {
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    PatternInstanceIDToNumExpr(const PatternInstanceIDExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~PatternInstanceIDToNumExpr(void);
};

/**
 * Converts a instruction ID into a numerical expression.
 */
class InstructionIDToNumExpr : public UnaryExpr<NumExpr, InstructionIDExpr> {
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    InstructionIDToNumExpr(const InstructionIDExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~InstructionIDToNumExpr(void);
};

/**
 * Converts a label ID into a numerical expression.
 */
class LabelIDToNumExpr : public UnaryExpr<NumExpr, LabelIDExpr> {
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    LabelIDToNumExpr(const LabelIDExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~LabelIDToNumExpr(void);
};

/**
 * Converts a register ID into a numerical expression.
 */
class RegisterIDToNumExpr : public UnaryExpr<NumExpr, RegisterIDExpr> {
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    RegisterIDToNumExpr(const RegisterIDExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~RegisterIDToNumExpr(void);
};

/**
 * Represents the distance between a pattern instance and a label. The distance
 * starts from the end of the instruction represented by the pattern and stops
 * at the beginning of the first instruction within the basic block represented
 * by the label. The distance is negative if the label appears before the
 * pattern.
 */
class DistanceBetweenInstanceAndLabelExpr
    : public BinaryExpr<NumExpr, PatternInstanceIDExpr, LabelIDExpr>
{
  public:
    /**
     * \copydoc BinaryExpr::BinaryExpr(const Arg1*, const Arg2*)
     */
    DistanceBetweenInstanceAndLabelExpr(const PatternInstanceIDExpr* lhs,
                                        const LabelIDExpr* rhs);


    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~DistanceBetweenInstanceAndLabelExpr(void);
};

/**
 * Introduces a node ID to be part of an expression.
 */
class ANodeIDExpr : public NodeIDExpr {
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

  private:
    ID id_;
};

/**
 * Introduces a pattern instance ID to be part of an expression.
 */
class APatternInstanceIDExpr : public PatternInstanceIDExpr {
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

  private:
    ID id_;
};

/**
 * Introduces an instruction ID to be part of an expression.
 */
class AnInstructionIDExpr : public InstructionIDExpr {
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

  private:
    ID id_;
};

/**
 * Introduces a label ID to be part of an expression.
 */
class APatternIDExpr : public PatternIDExpr {
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

  private:
    ID id_;
};

/**
 * Introduces a label ID to be part of an expression.
 */
class ALabelIDExpr : public LabelIDExpr {
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

  private:
    ID id_;
};

/**
 * Introduces a register ID to be part of an expression.
 */
class ARegisterIDExpr : public RegisterIDExpr {
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

  private:
    ID id_;
};

/**
 * Represents the ID of the pattern instance where this is declared.
 */
class ThisPatternInstanceIDExpr : public PatternInstanceIDExpr {
  public:
    /**
     * \copydoc Expr::Expr()
     */
    ThisPatternInstanceIDExpr(void);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~ThisPatternInstanceIDExpr(void);
};

/**
 * Represents the pattern instance ID which covers a certain action node.
 */
class CovererOfActionNodeExpr : public UnaryExpr<PatternInstanceIDExpr,
                                                 NodeIDExpr> {
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    CovererOfActionNodeExpr(const NodeIDExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~CovererOfActionNodeExpr(void);
};

/**
 * Represents the pattern instance ID which defines a certain data node.
 */
class DefinerOfDataNodeExpr : public UnaryExpr<PatternInstanceIDExpr,
                                               NodeIDExpr> {
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    DefinerOfDataNodeExpr(const NodeIDExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~DefinerOfDataNodeExpr(void);
};

/**
 * Represents the pattern instance ID which defines a certain state node.
 */
class DefinerOfStateNodeExpr : public UnaryExpr<PatternInstanceIDExpr,
                                                NodeIDExpr> {
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    DefinerOfStateNodeExpr(const NodeIDExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~DefinerOfStateNodeExpr(void);
};

/**
 * Represents the instruction ID to which a pattern belongs.
 */
class InstructionIDOfPatternExpr : public UnaryExpr<InstructionIDExpr,
                                                    PatternIDExpr>
{
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    InstructionIDOfPatternExpr(const PatternIDExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~InstructionIDOfPatternExpr(void);
};

/**
 * Represents the pattern ID to which a pattern instance is derived from.
 */
class PatternIDOfInstanceExpr : public UnaryExpr<PatternIDExpr,
                                                 PatternInstanceIDExpr>
{
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    PatternIDOfInstanceExpr(const PatternInstanceIDExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~PatternIDOfInstanceExpr(void);
};

/**
 * Represents the ID of the label to which a pattern instance has been
 * allocated.
 */
class LabelIDAllocatedToInstanceExpr : public UnaryExpr<LabelIDExpr,
                                                        PatternInstanceIDExpr>
{
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    LabelIDAllocatedToInstanceExpr(const PatternInstanceIDExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~LabelIDAllocatedToInstanceExpr(void);
};

/**
 * Represents the label ID associated with a label node.
 */
class LabelIDOfLabelNodeExpr : public UnaryExpr<LabelIDExpr, NodeIDExpr> {
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    LabelIDOfLabelNodeExpr(const NodeIDExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~LabelIDOfLabelNodeExpr(void);
};

/**
 * Represents the label ID associated with a label node.
 */
class RegisterIDAllocatedToDataNodeExpr : public UnaryExpr<RegisterIDExpr,
                                                           NodeIDExpr>
{
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    RegisterIDAllocatedToDataNodeExpr(const NodeIDExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~RegisterIDAllocatedToDataNodeExpr(void);
};

/**
 * Union set expression.
 */
class UnionSetExpr : public BinaryExpr<SetExpr, SetExpr> {
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
};

/**
 * Intersect set expression.
 */
class IntersectSetExpr : public BinaryExpr<SetExpr, SetExpr> {
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
};

/**
 * Difference set expression.
 */
class DiffSetExpr : public BinaryExpr<SetExpr, SetExpr> {
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
};

/**
 * Label dominator set expression.
 */
class DomSetOfLabelIDExpr : public UnaryExpr<SetExpr, LabelIDExpr> {
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    DomSetOfLabelIDExpr(const LabelIDExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~DomSetOfLabelIDExpr(void);
};

/**
 * Register class expression.
 */
class RegisterClassExpr : public SetExpr {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param expr
     *        The list of expressions.
     */
    RegisterClassExpr(const std::list<const RegisterIDExpr*>& expr);

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
    const std::list<const RegisterIDExpr*>&
    getExprList(void) const;

  private:
    std::list<const RegisterIDExpr*> expr_;
};

/**
 * Converts a label ID into a set element expression.
 */
class LabelIDToSetElemExpr : public UnaryExpr<SetElemExpr, LabelIDExpr> {
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    LabelIDToSetElemExpr(const LabelIDExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~LabelIDToSetElemExpr(void);
};

/**
 * Converts a register ID into a set element expression.
 */
class RegisterIDToSetElemExpr : public UnaryExpr<SetElemExpr, RegisterIDExpr> {
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(const Arg*)
     */
    RegisterIDToSetElemExpr(const RegisterIDExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~RegisterIDToSetElemExpr(void);
};

}

#endif
