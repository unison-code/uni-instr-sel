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

#include "constraintparser.h"
#include "../exceptions/exception.h"
#include "../utils/string.h"

using namespace Model;
using std::list;
using std::string;

ConstraintParser::ConstraintParser(void) {}

ConstraintParser::~ConstraintParser(void) {}

Constraint*
ConstraintParser::parseConstraint(const string& str) {
    // Turn string into a single-line string
    string sl_str(Utils::searchReplace(str, "\n", " "));
    sl_str = Utils::searchReplace(sl_str, "\r", " ");
    sl_str = Utils::searchReplace(sl_str, "\t", " ");

    Constraint* c;
    string sl_str_copy(sl_str);
    // Check if it's an 'Data-Node-Is-Integer-Constant' constraint
    bool was_parse_successful = false;
    if (eat("(", sl_str_copy)) {
        eatWhitespace(sl_str_copy);
        if (eatType<DataNodeIsIntConstantConstraint>(sl_str_copy)) {
            c = new DataNodeIsIntConstantConstraint(parseNodeID(sl_str_copy));
            if (!eat(")", sl_str_copy)) {
                THROW(Exception,
                      "Invalid constraint expression (missing ')' char)");
            }
            sl_str = sl_str_copy;
            was_parse_successful = true;
        }
    }
    if (!was_parse_successful) {
        // Assume it's a Boolean expression constraint
        c = new BoolExprConstraint(parseBoolExpr(sl_str));
    }

    eatWhitespace(sl_str);
    if (sl_str.length() != 0) {
        delete c;
        THROW(Exception, "Invalid constraint expression (has trailing part)");
    }
    return c;
}

BoolExpr*
ConstraintParser::parseBoolExpr(string& str) {
    BoolExpr* expr = NULL;

    eatWhitespace(str);
    if (eat("(", str)) {
        eatWhitespace(str);
        if (eatType<EqExpr>(str)) {
            auto lhs = parseNumExpr(str);
            auto rhs = parseNumExpr(str);
            expr = new EqExpr(lhs, rhs);
        }
        else if (eatType<NeqExpr>(str)) {
            auto lhs = parseNumExpr(str);
            auto rhs = parseNumExpr(str);
            expr = new NeqExpr(lhs, rhs);
        }
        else if (eatType<GTExpr>(str)) {
            auto lhs = parseNumExpr(str);
            auto rhs = parseNumExpr(str);
            expr = new GTExpr(lhs, rhs);
        }
        else if (eatType<GEExpr>(str)) {
            auto lhs = parseNumExpr(str);
            auto rhs = parseNumExpr(str);
            expr = new GEExpr(lhs, rhs);
        }
        else if (eatType<LTExpr>(str)) {
            auto lhs = parseNumExpr(str);
            auto rhs = parseNumExpr(str);
            expr = new LTExpr(lhs, rhs);
        }
        else if (eatType<LEExpr>(str)) {
            auto lhs = parseNumExpr(str);
            auto rhs = parseNumExpr(str);
            expr = new LEExpr(lhs, rhs);
        }
        else if (eatType<AndExpr>(str)) {
            auto lhs = parseBoolExpr(str);
            auto rhs = parseBoolExpr(str);
            expr = new AndExpr(lhs, rhs);
        }
        else if (eatType<OrExpr>(str)) {
            auto lhs = parseBoolExpr(str);
            auto rhs = parseBoolExpr(str);
            expr = new OrExpr(lhs, rhs);
        }
        else if (eatType<ImpExpr>(str)) {
            auto lhs = parseBoolExpr(str);
            auto rhs = parseBoolExpr(str);
            expr = new ImpExpr(lhs, rhs);
        }
        else if (eatType<EqvExpr>(str)) {
            auto lhs = parseBoolExpr(str);
            auto rhs = parseBoolExpr(str);
            expr = new EqvExpr(lhs, rhs);
        }
        else if (eatType<NotExpr>(str)) {
            auto e = parseBoolExpr(str);
            expr = new NotExpr(e);
        }
        else if (eatType<InSetExpr>(str)) {
            auto lhs = parseSetElemExpr(str);
            auto rhs = parseSetExpr(str);
            expr = new InSetExpr(lhs, rhs);
        }
        else {
            THROW(Exception, "Invalid constraint expression (unknown keyword)");
        }

        eatWhitespace(str);
        if (!eat(")", str)) {
            THROW(Exception,
                  "Invalid constraint expression (missing ')' char)");
        }
    }
    else {
        THROW(Exception, "Invalid constraint expression (missing '(' char)");
    }

    return expr;
}

NumExpr*
ConstraintParser::parseNumExpr(string& str) {
    NumExpr* expr = NULL;

    eatWhitespace(str);
    if (eat("(", str)) {
        eatWhitespace(str);
        if (eatType<PlusExpr>(str)) {
            auto lhs = parseNumExpr(str);
            auto rhs = parseNumExpr(str);
            expr = new PlusExpr(lhs, rhs);
        }
        else if (eatType<MinusExpr>(str)) {
            auto lhs = parseNumExpr(str);
            auto rhs = parseNumExpr(str);
            expr = new MinusExpr(lhs, rhs);
        }
        else if (eatType<IntToNumExpr>(str)) {
            auto e = parseIntExpr(str);
            expr = new IntToNumExpr(e);
        }
        else if (eatType<BoolToNumExpr>(str)) {
            auto e = parseBoolExpr(str);
            expr = new BoolToNumExpr(e);
        }
        else if (eatType<NodeIDToNumExpr>(str)) {
            auto e = parseNodeIDExpr(str);
            expr = new NodeIDToNumExpr(e);
        }
        else if (eatType<PatternInstanceIDToNumExpr>(str)) {
            auto e = parsePatternInstanceIDExpr(str);
            expr = new PatternInstanceIDToNumExpr(e);
        }
        else if (eatType<InstructionIDToNumExpr>(str)) {
            auto e = parseInstructionIDExpr(str);
            expr = new InstructionIDToNumExpr(e);
        }
        else if (eatType<PatternIDToNumExpr>(str)) {
            auto e = parsePatternIDExpr(str);
            expr = new PatternIDToNumExpr(e);
        }
        else if (eatType<LabelIDToNumExpr>(str)) {
            auto e = parseLabelIDExpr(str);
            expr = new LabelIDToNumExpr(e);
        }
        else if (eatType<RegisterIDToNumExpr>(str)) {
            auto e = parseRegisterIDExpr(str);
            expr = new RegisterIDToNumExpr(e);
        }
        else if (eatType<DistanceBetweenInstanceAndLabelExpr>(str)) {
            auto lhs = parsePatternInstanceIDExpr(str);
            auto rhs = parseLabelIDExpr(str);
            expr = new DistanceBetweenInstanceAndLabelExpr(lhs, rhs);
        }
        else {
            THROW(Exception, "Invalid constraint expression (unknown keyword)");
        }

        eatWhitespace(str);
        if (!eat(")", str)) {
            THROW(Exception,
                  "Invalid constraint expression (missing ')' char)");
        }
    }
    else {
        THROW(Exception, "Invalid constraint expression (missing '(' char)");
    }

    return expr;
}

IntExpr*
ConstraintParser::parseIntExpr(string& str) {
    IntExpr* expr = NULL;

    eatWhitespace(str);
    if (eat("(", str)) {
        eatWhitespace(str);
        if (eatType<IntConstValueOfDataNodeExpr>(str)) {
            auto e = parseNodeIDExpr(str);
            expr = new IntConstValueOfDataNodeExpr(e);
        }
        else {
            THROW(Exception, "Invalid constraint expression (unknown keyword)");
        }

        eatWhitespace(str);
        if (!eat(")", str)) {
            THROW(Exception,
                  "Invalid constraint expression (missing ')' char)");
        }
    }
    else {
        int num = eatInt(str);
        expr = new AnIntegerExpr(num);
    }

    return expr;
}

NodeIDExpr*
ConstraintParser::parseNodeIDExpr(string& str) {
    eatWhitespace(str);
    int num = eatInt(str);
    return new ANodeIDExpr(num);
}

PatternInstanceIDExpr*
ConstraintParser::parsePatternInstanceIDExpr(string& str) {
    PatternInstanceIDExpr* expr = NULL;

    eatWhitespace(str);
    if (eat("(", str)) {
        eatWhitespace(str);
        if (eatType<CovererOfActionNodeExpr>(str)) {
            auto e = parseNodeIDExpr(str);
            expr = new CovererOfActionNodeExpr(e);
        }
        else if (eatType<DefinerOfDataNodeExpr>(str)) {
            auto e = parseNodeIDExpr(str);
            expr = new DefinerOfDataNodeExpr(e);
        }
        else if (eatType<DefinerOfStateNodeExpr>(str)) {
            auto e = parseNodeIDExpr(str);
            expr = new DefinerOfStateNodeExpr(e);
        }
        else {
            THROW(Exception, "Invalid constraint expression (unknown keyword)");
        }

        eatWhitespace(str);
        if (!eat(")", str)) {
            THROW(Exception,
                  "Invalid constraint expression (missing ')' char)");
        }
    }
    else {
        if (eatType<ThisPatternInstanceIDExpr>(str)) {
            expr = new ThisPatternInstanceIDExpr;
        }
        else {
            int num = eatInt(str);
            expr = new APatternInstanceIDExpr(num);
        }
    }

    return expr;
}

InstructionIDExpr*
ConstraintParser::parseInstructionIDExpr(string& str) {
    InstructionIDExpr* expr = NULL;

    eatWhitespace(str);
    if (eat("(", str)) {
        eatWhitespace(str);
        if (eatType<InstructionIDOfPatternExpr>(str)) {
            auto e = parsePatternIDExpr(str);
            expr = new InstructionIDOfPatternExpr(e);
        }
        else {
            THROW(Exception, "Invalid constraint expression (unknown keyword)");
        }

        eatWhitespace(str);
        if (!eat(")", str)) {
            THROW(Exception,
                  "Invalid constraint expression (missing ')' char)");
        }
    }
    else {
        int num = eatInt(str);
        expr = new AnInstructionIDExpr(num);
    }

    return expr;
}

PatternIDExpr*
ConstraintParser::parsePatternIDExpr(string& str) {
    PatternIDExpr* expr = NULL;

    eatWhitespace(str);
    if (eat("(", str)) {
        eatWhitespace(str);
        if (eatType<PatternIDOfPatternInstanceExpr>(str)) {
            auto e = parsePatternInstanceIDExpr(str);
            expr = new PatternIDOfPatternInstanceExpr(e);
        }
        else {
            THROW(Exception, "Invalid constraint expression (unknown keyword)");
        }

        eatWhitespace(str);
        if (!eat(")", str)) {
            THROW(Exception,
                  "Invalid constraint expression (missing ')' char)");
        }
    }
    else {
        int num = eatInt(str);
        expr = new APatternIDExpr(num);
    }

    return expr;
}

LabelIDExpr*
ConstraintParser::parseLabelIDExpr(string& str) {
    LabelIDExpr* expr = NULL;

    eatWhitespace(str);
    if (eat("(", str)) {
        eatWhitespace(str);
        if (eatType<LabelIDAllocatedToPatternInstanceExpr>(str)) {
            auto e = parsePatternInstanceIDExpr(str);
            expr = new LabelIDAllocatedToPatternInstanceExpr(e);
        }
        else if (eatType<LabelIDOfLabelNodeExpr>(str)) {
            auto e = parseNodeIDExpr(str);
            expr = new LabelIDOfLabelNodeExpr(e);
        }
        else {
            THROW(Exception, "Invalid constraint expression (unknown keyword)");
        }

        eatWhitespace(str);
        if (!eat(")", str)) {
            THROW(Exception,
                  "Invalid constraint expression (missing ')' char)");
        }
    }
    else {
        THROW(Exception, "Invalid constraint expression (missing '(' char)");
    }

    return expr;
}

RegisterIDExpr*
ConstraintParser::parseRegisterIDExpr(string& str) {
    RegisterIDExpr* expr = NULL;

    eatWhitespace(str);
    if (eat("(", str)) {
        eatWhitespace(str);
        if (eatType<RegisterIDAllocatedToDataNodeExpr>(str)) {
            auto e = parseNodeIDExpr(str);
            expr = new RegisterIDAllocatedToDataNodeExpr(e);
        }
        else {
            THROW(Exception, "Invalid constraint expression (unknown keyword)");
        }

        eatWhitespace(str);
        if (!eat(")", str)) {
            THROW(Exception,
                  "Invalid constraint expression (missing ')' char)");
        }
    }
    else {
        int num = eatInt(str);
        expr = new ARegisterIDExpr(num);
    }

    return expr;
}

list<const RegisterIDExpr*>
ConstraintParser::parseListOfRegisterIDExpr(string& str) {
    list<const RegisterIDExpr*> expr;

    eatWhitespace(str);
    if (eat("(", str)) {
        while (true) {
            eatWhitespace(str);
            expr.push_back(parseRegisterIDExpr(str));
            if (eat(" ", str)) continue;
            if (eat(")", str)) break;
        }
    }

    return expr;
}

SetExpr*
ConstraintParser::parseSetExpr(string& str) {
    SetExpr* expr = NULL;

    eatWhitespace(str);
    if (eat("(", str)) {
        eatWhitespace(str);
        if (eatType<UnionSetExpr>(str)) {
            auto lhs = parseSetExpr(str);
            auto rhs = parseSetExpr(str);
            expr = new UnionSetExpr(lhs, rhs);
        }
        else if (eatType<IntersectSetExpr>(str)) {
            auto lhs = parseSetExpr(str);
            auto rhs = parseSetExpr(str);
            expr = new IntersectSetExpr(lhs, rhs);
        }
        else if (eatType<DiffSetExpr>(str)) {
            auto lhs = parseSetExpr(str);
            auto rhs = parseSetExpr(str);
            expr = new DiffSetExpr(lhs, rhs);
        }
        else if (eatType<DomSetOfLabelIDExpr>(str)) {
            auto e = parseLabelIDExpr(str);
            expr = new DomSetOfLabelIDExpr(e);
        }
        else if (eatType<RegisterClassExpr>(str)) {
            auto es = parseListOfRegisterIDExpr(str);
            expr = new RegisterClassExpr(es);
        }
        else {
            THROW(Exception, "Invalid constraint expression (unknown keyword)");
        }

        eatWhitespace(str);
        if (!eat(")", str)) {
            THROW(Exception,
                  "Invalid constraint expression (missing ')' char)");
        }
    }
    else {
        THROW(Exception, "Invalid constraint expression (missing '(' char)");
    }

    return expr;
}

SetElemExpr*
ConstraintParser::parseSetElemExpr(string& str) {
    SetElemExpr* expr = NULL;

    eatWhitespace(str);
    if (eat("(", str)) {
        eatWhitespace(str);
        if (eatType<LabelIDToSetElemExpr>(str)) {
            auto e = parseLabelIDExpr(str);
            expr = new LabelIDToSetElemExpr(e);
        }
        else if (eatType<RegisterIDToSetElemExpr>(str)) {
            auto e = parseRegisterIDExpr(str);
            expr = new RegisterIDToSetElemExpr(e);
        }
        else {
            THROW(Exception, "Invalid constraint expression (unknown keyword)");
        }

        eatWhitespace(str);
        if (!eat(")", str)) {
            THROW(Exception,
                  "Invalid constraint expression (missing ')' char)");
        }
    }
    else {
        THROW(Exception, "Invalid constraint expression (missing '(' char)");
    }

    return expr;
}

ID
ConstraintParser::parseNodeID(string& str) {
    eatWhitespace(str);
    return eatInt(str);
}

void
ConstraintParser::eatWhitespace(string& str) {
    while (str.length() > 0 && Utils::isWhitespace(str[0])) {
        str = str.substr(1);
    }
}

bool
ConstraintParser::eat(const string& search, string& str) {
    if (search.length() <= str.length()) {
        if (search == str.substr(0, search.length())) {
            str = str.substr(search.length());
            return true;
        }
    }
    return false;
}

int
ConstraintParser::eatInt(string& str) {
    string int_str;
    if (str[0] == '-') {
        int_str += str[0];
        str = str.substr(1);
    }
    while (str.length() > 0 && Utils::isNumeric(str[0])) {
        int_str += str[0];
        str = str.substr(1);
    }
    if (!Utils::isNumeric(int_str)) {
        THROW(Exception, "Invalid constraint expression (not an integer)");
    }
    return Utils::toInt(int_str);
}
