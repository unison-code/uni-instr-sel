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

#ifndef SOLVERS_MINIZINC_CONSTRAINTPROCESSOR__
#define SOLVERS_MINIZINC_CONSTRAINTPROCESSOR__

#include "../common/model/constraints.h"
#include "../common/model/params.h"
#include "../common/model/types.h"
#include <string>

/**
 * Walks a constraint and outputs the Minizinc version of it.
 */
class ConstraintProcessor {
  public:
    /**
     * Creates a constraint processor.
     *
     * @param p
     *        The parameter object wherein the constraints belong.
     */
    ConstraintProcessor(const Model::Params& p);

    /**
     * Destroys this processor.
     */
    virtual
    ~ConstraintProcessor(void);

    /**
     * Converts a constraint to a Minizinc equivalent.
     *
     * @param id
     *        ID of the pattern instance to which the constraint belongs.
     * @param c
     *        The constraint.
     * @returns Minizinc constraint as a string.
     * @throws Exception
     *         When something went wrong.
     */
    std::string
    process(const Model::Id& id, const Model::Constraint* c);

  protected:
    /**
     * Gets the variable array for the action node coverers.
     *
     * @returns Array name.
     */
    std::string
    getActionCovererArrayString(void) const;

    /**
     * Gets the variable array for the entity node definers.
     *
     * @returns Array name.
     */
    std::string
    getEntityDefinerArrayString(void) const;

    /**
     * Gets the variable array for the pattern instance-to-basic block
     * allocations.
     *
     * @returns Array name.
     */
    std::string
    getBBAllocationArrayString(void) const;

    /**
     * Converts an expression into a string to be used in a Minizinc constraint.
     *
     * @param e
     *        Expression
     * @returns The corresponding string.
     * @throws Exception
     *         When something went wrong.
     */
    std::string
    process(const Model::BoolExpr* e);

    /**
     * Converts an expression into a string to be used in a Minizinc constraint.
     *
     * @param e
     *        Expression
     * @returns The corresponding string.
     * @throws Exception
     *         When something went wrong.
     */
    std::string
    process(const Model::NumExpr* e);

    /**
     * Converts an expression into a string to be used in a Minizinc constraint.
     *
     * @param e
     *        Expression
     * @returns The corresponding string.
     * @throws Exception
     *         When something went wrong.
     */
    std::string
    process(const Model::NodeIdExpr* e);

    /**
     * Converts an expression into a string to be used in a Minizinc constraint.
     *
     * @param e
     *        Expression
     * @returns The corresponding string.
     * @throws Exception
     *         When something went wrong.
     */
    std::string
    process(const Model::InstanceIdExpr* e);

    /**
     * Converts an expression into a string to be used in a Minizinc constraint.
     *
     * @param e
     *        Expression
     * @returns The corresponding string.
     * @throws Exception
     *         When something went wrong.
     */
    std::string
    process(const Model::InstructionIdExpr* e);

    /**
     * Converts an expression into a string to be used in a Minizinc constraint.
     *
     * @param e
     *        Expression
     * @returns The corresponding string.
     * @throws Exception
     *         When something went wrong.
     */
    std::string
    process(const Model::PatternIdExpr* e);

    /**
     * Converts an expression into a string to be used in a Minizinc constraint.
     *
     * @param e
     *        Expression
     * @returns The corresponding string.
     * @throws Exception
     *         When something went wrong.
     */
    std::string
    process(const Model::LabelIdExpr* e);

    /**
     * Converts an expression into a string to be used in a Minizinc constraint.
     *
     * @param e
     *        Expression
     * @returns The corresponding string.
     * @throws Exception
     *         When something went wrong.
     */
    std::string
    process(const Model::RegisterIdExpr* e);

  protected:
    /**
     * The params object.
     */
    const Model::Params& p_;

    /**
     * ID of the pattern instance to which the constraint which is currently
     * being processed belongs to.
     */
    Model::Id instance_id_;
};

#endif
