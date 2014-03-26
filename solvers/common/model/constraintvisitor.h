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
 *     4. if there are more children, 'betweenChildrenVisits(...)' and go back
 *        to step 3, otherwise proceed to step 5
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
    betweenChildrenVisits(const Expr& e);

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
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const BoolExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const BoolExpr& e);

    /**
     * \copydoc ConstraintVisitor::betweenChildrenVisits(const Expr&)
     */
    virtual void
    betweenChildrenVisits(const BoolExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const BoolExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const NumExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const NumExpr& e);

    /**
     * \copydoc ConstraintVisitor::betweenChildrenVisits(const Expr&)
     */
    virtual void
    betweenChildrenVisits(const NumExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const NumExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const BinaryBoolToBoolExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const BinaryBoolToBoolExpr& e);

    /**
     * \copydoc ConstraintVisitor::betweenChildrenVisits(const Expr&)
     */
    virtual void
    betweenChildrenVisits(const BinaryBoolToBoolExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const BinaryBoolToBoolExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const BinaryNumToBoolExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const BinaryNumToBoolExpr& e);

    /**
     * \copydoc ConstraintVisitor::betweenChildrenVisits(const Expr&)
     */
    virtual void
    betweenChildrenVisits(const BinaryNumToBoolExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const BinaryNumToBoolExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const BinaryNumToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const BinaryNumToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::betweenChildrenVisits(const Expr&)
     */
    virtual void
    betweenChildrenVisits(const BinaryNumToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const BinaryNumToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const EqExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const EqExpr& e);

    /**
     * \copydoc ConstraintVisitor::betweenChildrenVisits(const Expr&)
     */
    virtual void
    betweenChildrenVisits(const EqExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const EqExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const NeqExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const NeqExpr& e);

    /**
     * \copydoc ConstraintVisitor::betweenChildrenVisits(const Expr&)
     */
    virtual void
    betweenChildrenVisits(const NeqExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const NeqExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const GTExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const GTExpr& e);

    /**
     * \copydoc ConstraintVisitor::betweenChildrenVisits(const Expr&)
     */
    virtual void
    betweenChildrenVisits(const GTExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const GTExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const GEExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const GEExpr& e);

    /**
     * \copydoc ConstraintVisitor::betweenChildrenVisits(const Expr&)
     */
    virtual void
    betweenChildrenVisits(const GEExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const GEExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const LTExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const LTExpr& e);

    /**
     * \copydoc ConstraintVisitor::betweenChildrenVisits(const Expr&)
     */
    virtual void
    betweenChildrenVisits(const LTExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const LTExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const LEExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const LEExpr& e);

    /**
     * \copydoc ConstraintVisitor::betweenChildrenVisits(const Expr&)
     */
    virtual void
    betweenChildrenVisits(const LEExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const LEExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const EqvExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const EqvExpr& e);

    /**
     * \copydoc ConstraintVisitor::betweenChildrenVisits(const Expr&)
     */
    virtual void
    betweenChildrenVisits(const EqvExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const EqvExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const ImpExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const ImpExpr& e);

    /**
     * \copydoc ConstraintVisitor::betweenChildrenVisits(const Expr&)
     */
    virtual void
    betweenChildrenVisits(const ImpExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const ImpExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const AndExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const AndExpr& e);

    /**
     * \copydoc ConstraintVisitor::betweenChildrenVisits(const Expr&)
     */
    virtual void
    betweenChildrenVisits(const AndExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const AndExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const OrExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const OrExpr& e);

    /**
     * \copydoc ConstraintVisitor::betweenChildrenVisits(const Expr&)
     */
    virtual void
    betweenChildrenVisits(const OrExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const OrExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const NotExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const NotExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const NotExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const PlusExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const PlusExpr& e);

    /**
     * \copydoc ConstraintVisitor::betweenChildrenVisits(const Expr&)
     */
    virtual void
    betweenChildrenVisits(const PlusExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const PlusExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const MinusExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const MinusExpr& e);

    /**
     * \copydoc ConstraintVisitor::betweenChildrenVisits(const Expr&)
     */
    virtual void
    betweenChildrenVisits(const MinusExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const MinusExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const AnIntegerExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const AnIntegerExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const AnIntegerExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const NodeIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const NodeIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const NodeIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const InstanceIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const InstanceIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const InstanceIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const InstructionIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const InstructionIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const InstructionIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const PatternIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const PatternIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const PatternIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const LabelIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const LabelIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const LabelIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const RegisterIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const RegisterIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const RegisterIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const AnInstanceIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const AnInstanceIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const AnInstanceIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const AnInstructionIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const AnInstructionIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const AnInstructionIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const ANodeIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const ANodeIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const ANodeIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const ALabelIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const ALabelIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const ALabelIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const ARegisterIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const ARegisterIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const ARegisterIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const APatternIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const APatternIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const APatternIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const ThisInstanceIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const ThisInstanceIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const ThisInstanceIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const CovererOfActionNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const CovererOfActionNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const CovererOfActionNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const DefinerOfEntityNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const DefinerOfEntityNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const DefinerOfEntityNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const InstructionIdOfPatternExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const InstructionIdOfPatternExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const InstructionIdOfPatternExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const PatternIdOfInstanceExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const PatternIdOfInstanceExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const PatternIdOfInstanceExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const LabelIdAllocatedToInstanceExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const LabelIdAllocatedToInstanceExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const LabelIdAllocatedToInstanceExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const LabelIdOfLabelNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const LabelIdOfLabelNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const LabelIdOfLabelNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    virtual void
    beforeVisit(const RegisterIdAllocatedToDataNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    visit(const RegisterIdAllocatedToDataNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    virtual void
    afterVisit(const RegisterIdAllocatedToDataNodeExpr& e);
};

}

#endif
