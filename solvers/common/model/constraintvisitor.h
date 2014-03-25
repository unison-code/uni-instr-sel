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

#ifndef SOLVERS_COMMON_MODEL_CONSTRAINTVISITOR__
#define SOLVERS_COMMON_MODEL_CONSTRAINTVISITOR__

#include "constraints.h"

namespace Model {

/**
 * Defines a base class for the constraint visitor.
 *
 * The order of callbacks are as follows:
 *     1. 'beforeVisit(...)' (just before visiting a node)
 *     2. 'visit(...)' (right after 'preVisit(...)')
 *     3. visit next child
 *     4. if there are more children, 'betweenChildren(...)' and go back to step
 *        3, otherwise proceed to step 5
 *     5. 'afterVisit(...)'
 *
 * If there are more than one callback method which could be matched during a
 * walk, the closest match (i.e. the most derived argument) will be invoked and
 * all other matches will not be called. By default all callbacks do nothing.
 */
class ConstraintVisitor {
  public:
    /**
     * Creates a constraint visitor.
     */
    ConstraintVisitor(void);

    /**
     * Destroys this visitor.
     */
    virtual
    ~ConstraintVisitor(void);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const Expr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const Expr& e);

    /**
     * Called between having visited the children. Only called for expressions
     * which has more than one argument.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    betweenChildren(const Expr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const Expr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const BoolExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const BoolExpr& e);

    /**
     * Called between having visited the children. Only called for expressions
     * which has more than one argument.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    betweenChildren(const BoolExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const BoolExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const NumExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const NumExpr& e);

    /**
     * Called between having visited the children. Only called for expressions
     * which has more than one argument.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    betweenChildren(const NumExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const NumExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const BinaryBoolToBoolExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const BinaryBoolToBoolExpr& e);

    /**
     * Called between having visited the children. Only called for expressions
     * which has more than one argument.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    betweenChildren(const BinaryBoolToBoolExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const BinaryBoolToBoolExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const BinaryNumToBoolExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const BinaryNumToBoolExpr& e);

    /**
     * Called between having visited the children. Only called for expressions
     * which has more than one argument.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    betweenChildren(const BinaryNumToBoolExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const BinaryNumToBoolExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const BinaryNumToNumExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const BinaryNumToNumExpr& e);

    /**
     * Called between having visited the children. Only called for expressions
     * which has more than one argument.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    betweenChildren(const BinaryNumToNumExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const BinaryNumToNumExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const EqExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const EqExpr& e);

    /**
     * Called between having visited the children. Only called for expressions
     * which has more than one argument.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    betweenChildren(const EqExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const EqExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const NeqExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const NeqExpr& e);

    /**
     * Called between having visited the children. Only called for expressions
     * which has more than one argument.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    betweenChildren(const NeqExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const NeqExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const GTExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const GTExpr& e);

    /**
     * Called between having visited the children. Only called for expressions
     * which has more than one argument.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    betweenChildren(const GTExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const GTExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const GEExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const GEExpr& e);

    /**
     * Called between having visited the children. Only called for expressions
     * which has more than one argument.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    betweenChildren(const GEExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const GEExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const LTExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const LTExpr& e);

    /**
     * Called between having visited the children. Only called for expressions
     * which has more than one argument.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    betweenChildren(const LTExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const LTExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const LEExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const LEExpr& e);

    /**
     * Called between having visited the children. Only called for expressions
     * which has more than one argument.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    betweenChildren(const LEExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const LEExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const EqvExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const EqvExpr& e);

    /**
     * Called between having visited the children. Only called for expressions
     * which has more than one argument.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    betweenChildren(const EqvExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const EqvExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const ImpExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const ImpExpr& e);

    /**
     * Called between having visited the children. Only called for expressions
     * which has more than one argument.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    betweenChildren(const ImpExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const ImpExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const AndExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const AndExpr& e);

    /**
     * Called between having visited the children. Only called for expressions
     * which has more than one argument.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    betweenChildren(const AndExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const AndExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const OrExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const OrExpr& e);

    /**
     * Called between having visited the children. Only called for expressions
     * which has more than one argument.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    betweenChildren(const OrExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const OrExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const NotExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const NotExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const NotExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const PlusExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const PlusExpr& e);

    /**
     * Called between having visited the children. Only called for expressions
     * which has more than one argument.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    betweenChildren(const PlusExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const PlusExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const MinusExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const MinusExpr& e);

    /**
     * Called between having visited the children. Only called for expressions
     * which has more than one argument.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    betweenChildren(const MinusExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const MinusExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const AnIntegerExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const AnIntegerExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const AnIntegerExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const NodeIdToNumExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const NodeIdToNumExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const NodeIdToNumExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const InstanceIdToNumExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const InstanceIdToNumExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const InstanceIdToNumExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const InstructionIdToNumExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const InstructionIdToNumExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const InstructionIdToNumExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const PatternIdToNumExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const PatternIdToNumExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const PatternIdToNumExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const LabelIdToNumExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const LabelIdToNumExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const LabelIdToNumExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const RegisterIdToNumExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const RegisterIdToNumExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const RegisterIdToNumExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const AnInstanceIdExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const AnInstanceIdExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const AnInstanceIdExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const AnInstructionIdExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const AnInstructionIdExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const AnInstructionIdExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const ANodeIdExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const ANodeIdExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const ANodeIdExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const ALabelIdExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const ALabelIdExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const ALabelIdExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const ARegisterIdExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const ARegisterIdExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const ARegisterIdExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const APatternIdExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const APatternIdExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const APatternIdExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const ThisInstanceIdExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const ThisInstanceIdExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const ThisInstanceIdExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const CovererOfActionNodeExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const CovererOfActionNodeExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const CovererOfActionNodeExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const DefinerOfEntityNodeExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const DefinerOfEntityNodeExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const DefinerOfEntityNodeExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const InstructionIdOfPatternExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const InstructionIdOfPatternExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const InstructionIdOfPatternExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const PatternIdOfInstanceExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const PatternIdOfInstanceExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const PatternIdOfInstanceExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const LabelAllocatedToInstanceExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const LabelAllocatedToInstanceExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const LabelAllocatedToInstanceExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const LabelIdOfLabelNodeExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const LabelIdOfLabelNodeExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const LabelIdOfLabelNodeExpr& e);

    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    beforeVisit(const RegisterAllocatedToDataNodeExpr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    visit(const RegisterAllocatedToDataNodeExpr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    afterVisit(const RegisterAllocatedToDataNodeExpr& e);

};

}

#endif
