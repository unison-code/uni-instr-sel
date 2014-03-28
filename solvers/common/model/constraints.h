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
class InstanceIdExpr;
class InstructionIdExpr;
class NodeIdExpr;
class PatternIdExpr;
class SetElemExpr;
class SetExpr;

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
    Constraint(BoolExpr* expr);

    /**
     * Destroys this object.
     */
    ~Constraint(void);

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
    ~NodeIdExpr(void)
    =0;
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
    ~InstanceIdExpr(void)
    =0;
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
    ~InstructionIdExpr(void)
    =0;
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
    ~PatternIdExpr(void)
    =0;
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
    ~LabelIdExpr(void)
    =0;
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
    ~RegisterIdExpr(void)
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
    UnaryExpr(Arg* expr)
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
    BinaryExpr(Arg1* lhs, Arg2* rhs)
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
     * \copydoc BinaryExpr::BinaryExpr(Arg1*, Arg2*)
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
class NeqExpr : public BinaryExpr<BoolExpr, NumExpr> {
  public:
    /**
     * \copydoc BinaryExpr::BinaryExpr(Arg1*, Arg2*)
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
class GTExpr : public BinaryExpr<BoolExpr, NumExpr> {
  public:
    /**
     * \copydoc BinaryExpr::BinaryExpr(Arg1*, Arg2*)
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
class GEExpr : public BinaryExpr<BoolExpr, NumExpr> {
  public:
    /**
     * \copydoc BinaryExpr::BinaryExpr(Arg1*, Arg2*)
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
class LTExpr : public BinaryExpr<BoolExpr, NumExpr> {
  public:
    /**
     * \copydoc BinaryExpr::BinaryExpr(Arg1*, Arg2*)
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
class LEExpr : public BinaryExpr<BoolExpr, NumExpr> {
  public:
    /**
     * \copydoc BinaryExpr::BinaryExpr(Arg1*, Arg2*)
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
class EqvExpr : public BinaryExpr<BoolExpr, BoolExpr> {
  public:
    /**
     * \copydoc BinaryExpr::BinaryExpr(Arg1*, Arg2*)
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
class ImpExpr : public BinaryExpr<BoolExpr, BoolExpr> {
  public:
    /**
     * \copydoc BinaryExpr::BinaryExpr(Arg1*, Arg2*)
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
class AndExpr : public BinaryExpr<BoolExpr, BoolExpr> {
  public:
    /**
     * \copydoc BinaryExpr::BinaryExpr(Arg1*, Arg2*)
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
class OrExpr : public BinaryExpr<BoolExpr, BoolExpr> {
  public:
    /**
     * \copydoc BinaryExpr::BinaryExpr(Arg1*, Arg2*)
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
    NotExpr(BoolExpr* expr);

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
     * \copydoc BinaryExpr::BinaryExpr(Arg1*, Arg2*)
     */
    InSetExpr(SetElemExpr* lhs, SetExpr* rhs);

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
class MinusExpr : public BinaryExpr<NumExpr, NumExpr> {
  public:
    /**
     * \copydoc BinaryExpr::BinaryExpr(Expr*, Expr*)
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
class NodeIdToNumExpr : public UnaryExpr<NumExpr, NodeIdExpr> {
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(Arg*)
     */
    NodeIdToNumExpr(NodeIdExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~NodeIdToNumExpr(void);
};

/**
 * Converts a pattern instance ID expression to a numerical expression.
 */
class PatternIdToNumExpr : public UnaryExpr<NumExpr, PatternIdExpr> {
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(Arg*)
     */
    PatternIdToNumExpr(PatternIdExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~PatternIdToNumExpr(void);
};

/**
 * Converts a instance ID expression to a numerical expression.
 */
class InstanceIdToNumExpr : public UnaryExpr<NumExpr, InstanceIdExpr> {
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(Arg*)
     */
    InstanceIdToNumExpr(InstanceIdExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~InstanceIdToNumExpr(void);
};

/**
 * Converts a instruction ID expression to a numerical expression.
 */
class InstructionIdToNumExpr : public UnaryExpr<NumExpr, InstructionIdExpr> {
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(Arg*)
     */
    InstructionIdToNumExpr(InstructionIdExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~InstructionIdToNumExpr(void);
};

/**
 * Converts a label ID expression to a numerical expression.
 */
class LabelIdToNumExpr : public UnaryExpr<NumExpr, LabelIdExpr> {
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(Arg*)
     */
    LabelIdToNumExpr(LabelIdExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~LabelIdToNumExpr(void);
};

/**
 * Converts a register ID expression to a numerical expression.
 */
class RegisterIdToNumExpr : public UnaryExpr<NumExpr, RegisterIdExpr> {
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(Arg*)
     */
    RegisterIdToNumExpr(RegisterIdExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~RegisterIdToNumExpr(void);
};

/**
 * Introduces a node ID to be part of an expression.
 */
class ANodeIdExpr : public NodeIdExpr {
  public:
    /**
     * \copydoc Expr::Expr()
     *
     * @param id
     *        The node ID.
     */
    ANodeIdExpr(const Id& id);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~ANodeIdExpr(void);

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
     * @param id
     *        The instance ID.
     */
    AnInstanceIdExpr(const Id& id);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~AnInstanceIdExpr(void);

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
     * @param id
     *        The instruction ID.
     */
    AnInstructionIdExpr(const Id& id);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~AnInstructionIdExpr(void);

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
     * @param id
     *        The pattern ID.
     */
    APatternIdExpr(const Id& id);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~APatternIdExpr(void);

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
     * @param id
     *        The label ID.
     */
    ALabelIdExpr(const Id& id);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~ALabelIdExpr(void);

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
     * @param id
     *        The register ID.
     */
    ARegisterIdExpr(const Id& id);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~ARegisterIdExpr(void);

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
};

/**
 * Represents the pattern instance ID which covers a certain action node.
 */
class CovererOfActionNodeExpr : public UnaryExpr<InstanceIdExpr, NodeIdExpr> {
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(Arg*)
     */
    CovererOfActionNodeExpr(NodeIdExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~CovererOfActionNodeExpr(void);
};

/**
 * Represents the pattern instance ID which defines a certain entity node.
 */
class DefinerOfEntityNodeExpr : public UnaryExpr<InstanceIdExpr, NodeIdExpr> {
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(Arg*)
     */
    DefinerOfEntityNodeExpr(NodeIdExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~DefinerOfEntityNodeExpr(void);
};

/**
 * Represents the instruction ID to which a pattern belongs.
 */
class InstructionIdOfPatternExpr : public UnaryExpr<InstructionIdExpr,
                                                    PatternIdExpr>
{
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(Arg*)
     */
    InstructionIdOfPatternExpr(PatternIdExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~InstructionIdOfPatternExpr(void);
};

/**
 * Represents the pattern ID to which a pattern instance is derived from.
 */
class PatternIdOfInstanceExpr : public UnaryExpr<PatternIdExpr, InstanceIdExpr>
{
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(Arg*)
     */
    PatternIdOfInstanceExpr(InstanceIdExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~PatternIdOfInstanceExpr(void);
};

/**
 * Represents the ID of the label to which a pattern instance has been
 * allocated.
 */
class LabelIdAllocatedToInstanceExpr : public UnaryExpr<LabelIdExpr,
                                                        InstanceIdExpr>
{
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(Arg*)
     */
    LabelIdAllocatedToInstanceExpr(InstanceIdExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~LabelIdAllocatedToInstanceExpr(void);
};

/**
 * Represents the label ID associated with a label node.
 */
class LabelIdOfLabelNodeExpr : public UnaryExpr<LabelIdExpr, NodeIdExpr> {
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(Arg*)
     */
    LabelIdOfLabelNodeExpr(NodeIdExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~LabelIdOfLabelNodeExpr(void);
};

/**
 * Represents the label ID associated with a label node.
 */
class RegisterIdAllocatedToDataNodeExpr : public UnaryExpr<RegisterIdExpr,
                                                           NodeIdExpr>
{
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(Arg*)
     */
    RegisterIdAllocatedToDataNodeExpr(NodeIdExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~RegisterIdAllocatedToDataNodeExpr(void);
};

/**
 * Union set expression.
 */
class UnionSetExpr : public BinaryExpr<SetExpr, SetExpr> {
  public:
    /**
     * \copydoc BinaryExpr::BinaryExpr(Arg1*, Arg2*)
     */
    UnionSetExpr(SetExpr* lhs, SetExpr* rhs);

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
     * \copydoc BinaryExpr::BinaryExpr(Arg1*, Arg2*)
     */
    IntersectSetExpr(SetExpr* lhs, SetExpr* rhs);

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
     * \copydoc BinaryExpr::BinaryExpr(Arg1*, Arg2*)
     */
    DiffSetExpr(SetExpr* lhs, SetExpr* rhs);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~DiffSetExpr(void);
};

/**
 * Label domset expression.
 */
class DomsetOfLabelIdExpr : public UnaryExpr<SetExpr, LabelIdExpr> {
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(Arg*)
     */
    DomsetOfLabelIdExpr(LabelIdExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~DomsetOfLabelIdExpr(void);
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
    RegisterClassExpr(std::list<const RegisterIdExpr*> expr);

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
    const std::list<const RegisterIdExpr*>&
    getExprList(void) const;

  private:
    std::list<const RegisterIdExpr*> expr_;
};

/**
 * Converts a label ID to a set element expression.
 */
class LabelIdToSetElemExpr : public UnaryExpr<SetExpr, LabelIdExpr> {
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(Arg*)
     */
    LabelIdToSetElemExpr(LabelIdExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~LabelIdToSetElemExpr(void);
};

/**
 * Converts a register ID to a set element expression.
 */
class RegisterIdToSetElemExpr : public UnaryExpr<SetExpr, RegisterIdExpr> {
  public:
    /**
     * \copydoc UnaryExpr::UnaryExpr(Arg*)
     */
    RegisterIdToSetElemExpr(RegisterIdExpr* expr);

    /**
     * \copydoc ~Expr::Expr()
     */
    virtual
    ~RegisterIdToSetElemExpr(void);
};

}

#endif
