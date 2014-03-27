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

#include "constraintprocessor.h"

using namespace Model;
using std::string;

// TODO: remove
#include <iostream>
using std::cout;
using std::endl;

ConstraintProcessor::ConstraintProcessor(const Model::Params& p)
    : p_(p)
{}

ConstraintProcessor::~ConstraintProcessor(void) {}

string
ConstraintProcessor::toString(const Constraint* c) {
    return string("constraint\n") + process(c->getExpr()) + ";";
}

string
ConstraintProcessor::process(const BoolExpr* e) {
    // TODO: implement
    return "";
}

string
ConstraintProcessor::process(const NumExpr* e) {
    // TODO: implement
    return "";
}

string
ConstraintProcessor::process(const NodeIdExpr* e) {
    // TODO: implement
    return "";
}

string
ConstraintProcessor::process(const InstanceIdExpr* e) {
    // TODO: implement
    return "";
}

string
ConstraintProcessor::process(const InstructionIdExpr* e) {
    // TODO: implement
    return "";
}

string
ConstraintProcessor::process(const PatternIdExpr* e) {
    // TODO: implement
    return "";
}

string
ConstraintProcessor::process(const LabelIdExpr* e) {
    // TODO: implement
    return "";
}

string
ConstraintProcessor::process(const RegisterIdExpr* e) {
    // TODO: implement
    return "";
}
