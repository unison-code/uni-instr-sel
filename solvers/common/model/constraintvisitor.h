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

namespace Model {

/**
 * Forward declarations
 */
class AndExpr;
class ALabelIdExpr;
class AnIntegerExpr;
class AnInstanceIdExpr;
class AnInstructionIdExpr;
class ANodeIdExpr;
class APatternIdExpr;
class ARegisterIdExpr;
class BoolExpr;
class CovererOfActionNodeExpr;
class DefinerOfEntityNodeExpr;
class EqExpr;
class EqvExpr;
class Expr;
class GEExpr;
class GTExpr;
class ImpExpr;
class InstructionIdOfPatternExpr;
class LabelIdAllocatedToInstanceExpr;
class LabelIdExpr;
class LabelIdOfLabelNodeExpr;
class LabelIdToNumExpr;
class LEExpr;
class LTExpr;
class MinusExpr;
class NeqExpr;
class NodeIdExpr;
class NodeIdToNumExpr;
class NotExpr;
class NumExpr;
class InstanceIdExpr;
class InstanceIdToNumExpr;
class InstructionIdExpr;
class InstructionIdToNumExpr;
class NodeIdExpr;
class OrExpr;
class PatternIdExpr;
class PatternIdOfInstanceExpr;
class PatternIdToNumExpr;
class PlusExpr;
class RegisterIdAllocatedToDataNodeExpr;
class RegisterIdExpr;
class RegisterIdToNumExpr;
class ThisInstanceIdExpr;

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
 * For each callback a corresponding 'at...(...)' method will be called, and
 * there is one such method for every expression class which will be invoked in
 * descending order of the class hierarchy. The point of this is to allow
 * derived visitor instances to respond with minimum implementation effort: for
 * instance, if the same action is to be performed for every visited expression
 * then it is enough to override the atVisit(const Expr&) method instead of
 * overriding the 'visit(...)' callback for every expression subclass. By
 * default the 'at...(...)' callbacks do nothing.
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
    void
    beforeVisit(const Expr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    void
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
    void
    betweenChildrenVisits(const Expr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    void
    afterVisit(const Expr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const BoolExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const BoolExpr& e);

    /**
     * \copydoc ConstraintVisitor::betweenChildrenVisits(const Expr&)
     */
    void
    betweenChildrenVisits(const BoolExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const BoolExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const NumExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const NumExpr& e);

    /**
     * \copydoc ConstraintVisitor::betweenChildrenVisits(const Expr&)
     */
    void
    betweenChildrenVisits(const NumExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const NumExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const EqExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const EqExpr& e);

    /**
     * \copydoc ConstraintVisitor::betweenChildrenVisits(const Expr&)
     */
    void
    betweenChildrenVisits(const EqExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const EqExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const NeqExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const NeqExpr& e);

    /**
     * \copydoc ConstraintVisitor::betweenChildrenVisits(const Expr&)
     */
    void
    betweenChildrenVisits(const NeqExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const NeqExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const GTExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const GTExpr& e);

    /**
     * \copydoc ConstraintVisitor::betweenChildrenVisits(const Expr&)
     */
    void
    betweenChildrenVisits(const GTExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const GTExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const GEExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const GEExpr& e);

    /**
     * \copydoc ConstraintVisitor::betweenChildrenVisits(const Expr&)
     */
    void
    betweenChildrenVisits(const GEExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const GEExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const LTExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const LTExpr& e);

    /**
     * \copydoc ConstraintVisitor::betweenChildrenVisits(const Expr&)
     */
    void
    betweenChildrenVisits(const LTExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const LTExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const LEExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const LEExpr& e);

    /**
     * \copydoc ConstraintVisitor::betweenChildrenVisits(const Expr&)
     */
    void
    betweenChildrenVisits(const LEExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const LEExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const EqvExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const EqvExpr& e);

    /**
     * \copydoc ConstraintVisitor::betweenChildrenVisits(const Expr&)
     */
    void
    betweenChildrenVisits(const EqvExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const EqvExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const ImpExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const ImpExpr& e);

    /**
     * \copydoc ConstraintVisitor::betweenChildrenVisits(const Expr&)
     */
    void
    betweenChildrenVisits(const ImpExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const ImpExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const AndExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const AndExpr& e);

    /**
     * \copydoc ConstraintVisitor::betweenChildrenVisits(const Expr&)
     */
    void
    betweenChildrenVisits(const AndExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const AndExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const OrExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const OrExpr& e);

    /**
     * \copydoc ConstraintVisitor::betweenChildrenVisits(const Expr&)
     */
    void
    betweenChildrenVisits(const OrExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const OrExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const NotExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const NotExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const NotExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const PlusExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const PlusExpr& e);

    /**
     * \copydoc ConstraintVisitor::betweenChildrenVisits(const Expr&)
     */
    void
    betweenChildrenVisits(const PlusExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const PlusExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const MinusExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const MinusExpr& e);

    /**
     * \copydoc ConstraintVisitor::betweenChildrenVisits(const Expr&)
     */
    void
    betweenChildrenVisits(const MinusExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const MinusExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const AnIntegerExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const AnIntegerExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const AnIntegerExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const NodeIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const NodeIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const NodeIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const InstanceIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const InstanceIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const InstanceIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const InstructionIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const InstructionIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const InstructionIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const PatternIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const PatternIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const PatternIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const LabelIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const LabelIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const LabelIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const RegisterIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const RegisterIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const RegisterIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const AnInstanceIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const AnInstanceIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const AnInstanceIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const AnInstructionIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const AnInstructionIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const AnInstructionIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const ANodeIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const ANodeIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const ANodeIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const ALabelIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const ALabelIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const ALabelIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const ARegisterIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const ARegisterIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const ARegisterIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const APatternIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const APatternIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const APatternIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const ThisInstanceIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const ThisInstanceIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const ThisInstanceIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const CovererOfActionNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const CovererOfActionNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const CovererOfActionNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const DefinerOfEntityNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const DefinerOfEntityNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const DefinerOfEntityNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const InstructionIdOfPatternExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const InstructionIdOfPatternExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const InstructionIdOfPatternExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const PatternIdOfInstanceExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const PatternIdOfInstanceExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const PatternIdOfInstanceExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const LabelIdAllocatedToInstanceExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const LabelIdAllocatedToInstanceExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const LabelIdAllocatedToInstanceExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const LabelIdOfLabelNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const LabelIdOfLabelNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const LabelIdOfLabelNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::beforeVisit(const Expr&)
     */
    void
    beforeVisit(const RegisterIdAllocatedToDataNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    void
    visit(const RegisterIdAllocatedToDataNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::afterVisit(const Expr&)
     */
    void
    afterVisit(const RegisterIdAllocatedToDataNodeExpr& e);

  protected:
    /**
     * Called just before visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    atBeforeVisit(const Expr& e);

    /**
     * Called when visiting the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    atVisit(const Expr& e);

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
    atBetweenChildrenVisits(const Expr& e);

    /**
     * Called just after having visited the expression.
     *
     * @param e
     *        The expression to visit.
     * @throws Exception
     *         When something went wrong during the walk.
     */
    virtual void
    atAfterVisit(const Expr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const BoolExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const BoolExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBetweenChildrenVisits(const Expr&)
     */
    virtual void
    atBetweenChildrenVisits(const BoolExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const BoolExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const NumExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const NumExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBetweenChildrenVisits(const Expr&)
     */
    virtual void
    atBetweenChildrenVisits(const NumExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const NumExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const EqExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const EqExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBetweenChildrenVisits(const Expr&)
     */
    virtual void
    atBetweenChildrenVisits(const EqExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const EqExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const NeqExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const NeqExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBetweenChildrenVisits(const Expr&)
     */
    virtual void
    atBetweenChildrenVisits(const NeqExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const NeqExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const GTExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const GTExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBetweenChildrenVisits(const Expr&)
     */
    virtual void
    atBetweenChildrenVisits(const GTExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const GTExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const GEExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const GEExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBetweenChildrenVisits(const Expr&)
     */
    virtual void
    atBetweenChildrenVisits(const GEExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const GEExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const LTExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const LTExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBetweenChildrenVisits(const Expr&)
     */
    virtual void
    atBetweenChildrenVisits(const LTExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const LTExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const LEExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const LEExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBetweenChildrenVisits(const Expr&)
     */
    virtual void
    atBetweenChildrenVisits(const LEExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const LEExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const EqvExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const EqvExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBetweenChildrenVisits(const Expr&)
     */
    virtual void
    atBetweenChildrenVisits(const EqvExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const EqvExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const ImpExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const ImpExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBetweenChildrenVisits(const Expr&)
     */
    virtual void
    atBetweenChildrenVisits(const ImpExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const ImpExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const AndExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const AndExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBetweenChildrenVisits(const Expr&)
     */
    virtual void
    atBetweenChildrenVisits(const AndExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const AndExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const OrExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const OrExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBetweenChildrenVisits(const Expr&)
     */
    virtual void
    atBetweenChildrenVisits(const OrExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const OrExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const NotExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const NotExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const NotExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const PlusExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const PlusExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBetweenChildrenVisits(const Expr&)
     */
    virtual void
    atBetweenChildrenVisits(const PlusExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const PlusExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const MinusExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const MinusExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBetweenChildrenVisits(const Expr&)
     */
    virtual void
    atBetweenChildrenVisits(const MinusExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const MinusExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const AnIntegerExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const AnIntegerExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const AnIntegerExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const NodeIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const NodeIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const NodeIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const InstanceIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const InstanceIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const InstanceIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const InstructionIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const InstructionIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const InstructionIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const PatternIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const PatternIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const PatternIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const LabelIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const LabelIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const LabelIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const RegisterIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const RegisterIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const RegisterIdToNumExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const AnInstanceIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const AnInstanceIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const AnInstanceIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const AnInstructionIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const AnInstructionIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const AnInstructionIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const ANodeIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const ANodeIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const ANodeIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const ALabelIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const ALabelIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const ALabelIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const ARegisterIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const ARegisterIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const ARegisterIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const APatternIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const APatternIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const APatternIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const ThisInstanceIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const ThisInstanceIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const ThisInstanceIdExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const CovererOfActionNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const CovererOfActionNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const CovererOfActionNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const DefinerOfEntityNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const DefinerOfEntityNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const DefinerOfEntityNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const InstructionIdOfPatternExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const InstructionIdOfPatternExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const InstructionIdOfPatternExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const PatternIdOfInstanceExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const PatternIdOfInstanceExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const PatternIdOfInstanceExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const LabelIdAllocatedToInstanceExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const LabelIdAllocatedToInstanceExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const LabelIdAllocatedToInstanceExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const LabelIdOfLabelNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const LabelIdOfLabelNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const LabelIdOfLabelNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::atBeforeVisit(const Expr&)
     */
    virtual void
    atBeforeVisit(const RegisterIdAllocatedToDataNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::visit(const Expr&)
     */
    virtual void
    atVisit(const RegisterIdAllocatedToDataNodeExpr& e);

    /**
     * \copydoc ConstraintVisitor::atAfterVisit(const Expr&)
     */
    virtual void
    atAfterVisit(const RegisterIdAllocatedToDataNodeExpr& e);
};

}

#endif
