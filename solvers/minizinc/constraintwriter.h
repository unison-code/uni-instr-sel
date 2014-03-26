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

#ifndef SOLVERS_MINIZINC_CONSTRAINTWRITER__
#define SOLVERS_MINIZINC_CONSTRAINTWRITER__

#include "../common/model/constraintvisitor.h"
#include "../common/model/params.h"
#include <string>

/**
 * Walks a constraint and outputs the Minizinc version of it.
 */
class ConstraintWriter : public Model::ConstraintVisitor {
  public:
    /**
     * Creates a constraint writer.
     *
     * @param p
     *        The parameter object wherein the constraints belong.
     */
    ConstraintWriter(const Model::Params& p);

    /**
     * Destroys this writer.
     */
    virtual
    ~ConstraintWriter(void);

    /**
     * Converts a constraint to a Minizinc equivalent.
     *
     * @param c
     *        The constraint.
     * @returns Minizinc constraint as a string.
     * @throws Exception
     *         When something went wrong.
     */
    std::string
    toString(const Model::Constraint* c);

  protected:
    // TODO: override visitor functions

  protected:
    /**
     * The params object.
     */
    const Model::Params& p_;

    /**
     * The string to which the output will be accumulated as the constraints is
     * being traversed.
     */
    std::string output_;

};

#endif
