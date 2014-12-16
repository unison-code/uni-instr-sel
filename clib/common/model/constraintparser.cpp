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

    Constraint* c = new BoolExprConstraint(parseBoolExpr(sl_str));

    // Check trailing part
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
        else if (eatType<DataNodeIsAnIntConstantExpr>(str)) {
            auto e = parseNodeExpr(str);
            expr = new DataNodeIsAnIntConstantExpr(e);
        }
        else if (eatType<DataNodeIsIntermediateExpr>(str)) {
            auto e = parseNodeExpr(str);
            expr = new DataNodeIsIntermediateExpr(e);
        }
        else if (eatType<MatchIsSelectedExpr>(str)) {
            auto e = parseMatchExpr(str);
            expr = new MatchIsSelectedExpr(e);
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
        else if (eatType<NodeToNumExpr>(str)) {
            auto e = parseNodeExpr(str);
            expr = new NodeToNumExpr(e);
        }
        else if (eatType<MatchToNumExpr>(str)) {
            auto e = parseMatchExpr(str);
            expr = new MatchToNumExpr(e);
        }
        else if (eatType<InstructionToNumExpr>(str)) {
            auto e = parseInstructionExpr(str);
            expr = new InstructionToNumExpr(e);
        }
        else if (eatType<PatternToNumExpr>(str)) {
            auto e = parsePatternExpr(str);
            expr = new PatternToNumExpr(e);
        }
        else if (eatType<LabelToNumExpr>(str)) {
            auto e = parseLabelExpr(str);
            expr = new LabelToNumExpr(e);
        }
        else if (eatType<RegisterToNumExpr>(str)) {
            auto e = parseRegisterExpr(str);
            expr = new RegisterToNumExpr(e);
        }
        else if (eatType<DistanceBetweenMatchAndLabelExpr>(str)) {
            auto lhs = parseMatchExpr(str);
            auto rhs = parseLabelExpr(str);
            expr = new DistanceBetweenMatchAndLabelExpr(lhs, rhs);
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
        if (eatType<AnIntegerExpr>(str)) {
            int num = eatInt(str);
            expr = new AnIntegerExpr(num);
        }
        else if (eatType<IntConstValueOfDataNodeExpr>(str)) {
            auto e = parseNodeExpr(str);
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
        THROW(Exception, "Invalid constraint expression (missing '(' char)");
    }

    return expr;
}

NodeExpr*
ConstraintParser::parseNodeExpr(string& str) {
    NodeExpr* expr = NULL;

    eatWhitespace(str);
    if (eat("(", str)) {
        eatWhitespace(str);
        if (eatType<ANodeIDExpr>(str)) {
            ID id = eatID(str);
            expr = new ANodeIDExpr(id);
        }
        else if (eatType<ANodeArrayIndexExpr>(str)) {
            ArrayIndex i = eatArrayIndex(str);
            expr = new ANodeArrayIndexExpr(i);
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

MatchExpr*
ConstraintParser::parseMatchExpr(string& str) {
    MatchExpr* expr = NULL;

    eatWhitespace(str);
    if (eat("(", str)) {
        eatWhitespace(str);
        if (eatType<AMatchIDExpr>(str)) {
            ID id = eatID(str);
            expr = new AMatchIDExpr(id);
        }
        else if (eatType<AMatchArrayIndexExpr>(str)) {
            ArrayIndex i = eatArrayIndex(str);
            expr = new AMatchArrayIndexExpr(i);
        }
        else if (eatType<CovererOfOperationNodeExpr>(str)) {
            auto e = parseNodeExpr(str);
            expr = new CovererOfOperationNodeExpr(e);
        }
        else if (eatType<DefinerOfDataNodeExpr>(str)) {
            auto e = parseNodeExpr(str);
            expr = new DefinerOfDataNodeExpr(e);
        }
        else if (eatType<DefinerOfStateNodeExpr>(str)) {
            auto e = parseNodeExpr(str);
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
        if (eatType<ThisMatchExpr>(str)) {
            expr = new ThisMatchExpr;
        }
        else {
            THROW(Exception, "Invalid constraint expression (unknown keyword)");
        }
    }

    return expr;
}

InstructionExpr*
ConstraintParser::parseInstructionExpr(string& str) {
    InstructionExpr* expr = NULL;

    eatWhitespace(str);
    if (eat("(", str)) {
        eatWhitespace(str);
        if (eatType<InstructionOfPatternExpr>(str)) {
            auto e = parsePatternExpr(str);
            expr = new InstructionOfPatternExpr(e);
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
        ID id = eatID(str);
        expr = new AnInstructionIDExpr(id);
    }

    return expr;
}

PatternExpr*
ConstraintParser::parsePatternExpr(string& str) {
    PatternExpr* expr = NULL;

    eatWhitespace(str);
    if (eat("(", str)) {
        eatWhitespace(str);
        if (eatType<APatternIDExpr>(str)) {
            ID e = eatID(str);
            expr = new APatternIDExpr(e);
        }
        else if (eatType<PatternOfMatchExpr>(str)) {
            auto e = parseMatchExpr(str);
            expr = new PatternOfMatchExpr(e);
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

LabelExpr*
ConstraintParser::parseLabelExpr(string& str) {
    LabelExpr* expr = NULL;

    eatWhitespace(str);
    if (eat("(", str)) {
        eatWhitespace(str);
        if (eatType<LabelAllocatedToMatchExpr>(str)) {
            auto e = parseMatchExpr(str);
            expr = new LabelAllocatedToMatchExpr(e);
        }
        else if (eatType<LabelOfLabelNodeExpr>(str)) {
            auto e = parseNodeExpr(str);
            expr = new LabelOfLabelNodeExpr(e);
        }
        else {
            THROW(Exception, str + " Invalid constraint expression (unknown keyword)");
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

RegisterExpr*
ConstraintParser::parseRegisterExpr(string& str) {
    RegisterExpr* expr = NULL;

    eatWhitespace(str);
    if (eat("(", str)) {
        eatWhitespace(str);
        if (eatType<ARegisterIDExpr>(str)) {
            ID id = eatID(str);
            expr = new ARegisterIDExpr(id);
        }
        else if (eatType<ARegisterArrayIndexExpr>(str)) {
            ArrayIndex i = eatArrayIndex(str);
            expr = new ARegisterArrayIndexExpr(i);
        }
        else if (eatType<RegisterAllocatedToDataNodeExpr>(str)) {
            auto e = parseNodeExpr(str);
            expr = new RegisterAllocatedToDataNodeExpr(e);
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

list<const RegisterExpr*>
ConstraintParser::parseListOfRegisterExpr(string& str) {
    list<const RegisterExpr*> expr;

    eatWhitespace(str);
    if (eat("(", str)) {
        while (true) {
            eatWhitespace(str);
            expr.push_back(parseRegisterExpr(str));
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
        else if (eatType<DomSetOfLabelExpr>(str)) {
            auto e = parseLabelExpr(str);
            expr = new DomSetOfLabelExpr(e);
        }
        else if (eatType<RegisterClassExpr>(str)) {
            auto es = parseListOfRegisterExpr(str);
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
        if (eatType<LabelToSetElemExpr>(str)) {
            auto e = parseLabelExpr(str);
            expr = new LabelToSetElemExpr(e);
        }
        else if (eatType<RegisterToSetElemExpr>(str)) {
            auto e = parseRegisterExpr(str);
            expr = new RegisterToSetElemExpr(e);
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

ID
ConstraintParser::eatID(std::string& str) {
    return eatInt(str);
}

ArrayIndex
ConstraintParser::eatArrayIndex(std::string& str) {
    return eatInt(str);
}
