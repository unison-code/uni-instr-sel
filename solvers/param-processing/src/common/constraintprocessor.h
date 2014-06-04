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

#ifndef SOLVERS_PARAM_PROCESSING_COMMON_CONSTRAINTPROCESSOR__
#define SOLVERS_PARAM_PROCESSING_COMMON_CONSTRAINTPROCESSOR__

#include "preparams.h"
#include "../../../common/model/constraints.h"

/**
 * Walks a constraint and replaces all occurrences of IDs with array indices.
 * Also, a constraint relating to a pattern instance will be augmented such that
 * the constraint will only be activated if the pattern instance is selected.
 *
 * An object of this class should not be used by multiple threads as its methods
 * are not thread-safe.
 */
class ConstraintProcessor {
  public:
    /**
     * Creates a constraint processor.
     *
     * @param p
     *        The parameter object wherein the constraints belong.
     */
    ConstraintProcessor(const Preparams& p);

    /**
     * Destroys this processor.
     */
    virtual
    ~ConstraintProcessor(void);

    /**
     * Walks a given constraint and replaces all occurrences of IDs with array
     * indices, and augments the constraints such that it will only be activated
     * if the pattern instance is selected.
     *
     * @param c
     *        The constraint. This can be destroyed upon return.
     * @param id
     *        ID of the pattern instance to which the constraint belongs.
     * @returns The new constraint.
     * @throws Exception
     *         When something went wrong.
     */
    Model::Constraint*
    processConstraintForPI(const Model::Constraint* c, const Model::ID& id);

    /**
     * Walks a given constraint and replaces all occurrences of IDs with array
     * indices.
     *
     * @param c
     *        The constraint. This can be destroyed upon return.
     * @returns The new constraint.
     * @throws Exception
     *         When something went wrong.
     */
    Model::Constraint*
    processConstraintForF(const Model::Constraint* c);

  protected:
    /**
     * Walks a given expression and replaces all occurrences of IDs with array
     * indices.
     *
     * @param e
     *        The expression. This can be destroyed upon returned.
     * @returns The new expression.
     * @throws Exception
     *         When something went wrong.
     */
    Model::BoolExpr*
    processBoolExpr(const Model::BoolExpr* e);

    /**
     * \copydoc process(const Model::IntExpr*)
     */
    Model::IntExpr*
    processIntExpr(const Model::IntExpr* e);

    /**
     * \copydoc process(const Model::BoolExpr*)
     */
    Model::NumExpr*
    processNumExpr(const Model::NumExpr* e);

    /**
     * \copydoc process(const Model::BoolExpr*)
     */
    Model::NodeExpr*
    processNodeExpr(const Model::NodeExpr* e);

    /**
     * \copydoc process(const Model::BoolExpr*)
     */
    Model::PatternInstanceExpr*
    processPatternInstanceExpr(const Model::PatternInstanceExpr* e);

    /**
     * \copydoc process(const Model::BoolExpr*)
     */
    Model::InstructionExpr*
    processInstructionExpr(const Model::InstructionExpr* e);

    /**
     * \copydoc process(const Model::BoolExpr*)
     */
    Model::PatternExpr*
    processPatternExpr(const Model::PatternExpr* e);

    /**
     * \copydoc process(const Model::BoolExpr*)
     */
    Model::LabelExpr*
    processLabelExpr(const Model::LabelExpr* e);

    /**
     * \copydoc process(const Model::BoolExpr*)
     */
    Model::RegisterExpr*
    processRegisterExpr(const Model::RegisterExpr* e);

    /**
     * \copydoc process(const Model::BoolExpr*)
     */
    Model::SetExpr*
    processSetExpr(const Model::SetExpr* e);

    /**
     * \copydoc process(const Model::BoolExpr*)
     */
    Model::SetElemExpr*
    processSetElemExpr(const Model::SetElemExpr* e);

  protected:
    /**
     * The preparams object.
     */
    const Preparams& p_;

    /**
     * Determines whether a constraint for a pattern instance is currently being
     * processed.
     */
    bool is_processing_pi_constraint_;

    /**
     * ID of the pattern instance to which the constraint which is currently
     * being processed belongs to.
     */
    Model::ID piid_;
};

#endif
