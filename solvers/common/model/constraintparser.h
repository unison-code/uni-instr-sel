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

#ifndef SOLVERS_COMMON_MODEL_CONSTRAINTPARSER__
#define SOLVERS_COMMON_MODEL_CONSTRAINTPARSER__

#include "constraints.h"
#include <list>
#include <string>

namespace Model {

/**
 * Defines a constraint parser that parses a lispian string into internal
 * constraints.
 */
class ConstraintParser {
  public:
    /**
     * Creates a constraint parser.
     */
    ConstraintParser(void);

    /**
     * Destroys this constraint parser.
     */
    virtual
    ~ConstraintParser(void);

    /**
     * Parses a lispian string containing a constraint.
     *
     * @param str
     *        String to parse.
     * @returns Parsed constraint as a newly allocated object.
     * @throws Exception
     *         When the parsing fails.
     */
    Constraint*
    parseConstraint(const std::string& str);

  protected:
    /**
     * Parses a Boolean expression.
     *
     * @param str
     *        String to parse. This will be modified as part of parsing.
     * @returns Parsed expression.
     */
    BoolExpr*
    parseBoolExpr(std::string& str);

    /**
     * Parses a numerical expression.
     *
     * @param str
     *        String to parse. This will be modified as part of parsing.
     * @returns Parsed expression.
     */
    NumExpr*
    parseNumExpr(std::string& str);

    /**
     * Parses an integer expression.
     *
     * @param str
     *        String to parse. This will be modified as part of parsing.
     * @returns Parsed expression.
     */
    IntExpr*
    parseIntExpr(std::string& str);

    /**
     * Parses a node ID expression.
     *
     * @param str
     *        String to parse. This will be modified as part of parsing.
     * @returns Parsed expression.
     */
    NodeIDExpr*
    parseNodeIDExpr(std::string& str);

    /**
     * Parses a pattern instance ID expression.
     *
     * @param str
     *        String to parse. This will be modified as part of parsing.
     * @returns Parsed expression.
     */
    PatternInstanceIDExpr*
    parsePatternInstanceIDExpr(std::string& str);

    /**
     * Parses an instruction ID expression.
     *
     * @param str
     *        String to parse. This will be modified as part of parsing.
     * @returns Parsed expression.
     */
    InstructionIDExpr*
    parseInstructionIDExpr(std::string& str);

    /**
     * Parses a pattern ID expression.
     *
     * @param str
     *        String to parse. This will be modified as part of parsing.
     * @returns Parsed expression.
     */
    PatternIDExpr*
    parsePatternIDExpr(std::string& str);

    /**
     * Parses a label ID expression.
     *
     * @param str
     *        String to parse. This will be modified as part of parsing.
     * @returns Parsed expression.
     */
    LabelIDExpr*
    parseLabelIDExpr(std::string& str);

    /**
     * Parses a register ID expression.
     *
     * @param str
     *        String to parse. This will be modified as part of parsing.
     * @returns Parsed expression.
     */
    RegisterIDExpr*
    parseRegisterIDExpr(std::string& str);

    /**
     * Parses a list of register ID expression.
     *
     * @param str
     *        String to parse. This will be modified as part of parsing.
     * @returns Parsed list of expression.
     */
    std::list<const RegisterIDExpr*>
    parseListOfRegisterIDExpr(std::string& str);

    /**
     * Parses a set expression.
     *
     * @param str
     *        String to parse. This will be modified as part of parsing.
     * @returns Parsed expression.
     */
    SetExpr*
    parseSetExpr(std::string& str);

    /**
     * Parses a set element expression.
     *
     * @param str
     *        String to parse. This will be modified as part of parsing.
     * @returns Parsed expression.
     */
    SetElemExpr*
    parseSetElemExpr(std::string& str);

    /**
     * Parses a node ID.
     *
     * @param str
     *        String to parse. This will be modified as part of parsing.
     * @returns Parsed expression.
     */
    ID
    parseNodeID(std::string& str);

    /**
     * Checks if the current part of the string matches the string name of a
     * given class. If it is, that part of the string is eaten.
     *
     * @tparam Class
     *         The class to get the string name from.
     * @param str
     *        String to check and modify.
     * @returns \c true if there was a match.
     */
    template <typename Class>
    bool
    eatType(std::string& str) {
        return eat(Class::getStrName() + " ", str);
    }


    /**
     * Removes initial whitespace from a string.
     *
     * @param str
     *        String to modify.
     */
    void
    eatWhitespace(std::string& str);

    /**
     * Removes an initial part that matches another string.
     *
     * @param search
     *        Initial part to match.
     * @param str
     *        String to search in and modify (but only if there is match).
     * @returns \c true if there was a match.
     */
    bool
    eat(const std::string& search, std::string& str);

    /**
     * Removes an initial integer value from a string.
     *
     * @param str
     *        String to modify.
     * @returns An integer.
     * @throws Exception
     *         When the string does not begin with an integer.
     */
    int
    eatInt(std::string& str);
};

}

#endif
