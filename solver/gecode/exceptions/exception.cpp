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

#include "exception.h"

using std::string;

Exception::Exception(const string& source_file, int source_line)
    : exception(),
      source_file_(source_file),
      source_line_(source_line),
      message_("")
{}

Exception::Exception(const string& source_file,
                     int source_line,
                     const string& message)
    : exception(),
      source_file_(source_file),
      source_line_(source_line),
      message_(message)
{}

Exception::~Exception(void) throw() {}

string
Exception::getSourceFile(void) const {
    return source_file_;
}

int
Exception::getSourceLine(void) const {
    return source_line_;
}

string
Exception::getMessage(void) const {
    return message_;
}

string
Exception::toString(void) const {
    string str;
    str += type();
    str += " thrown in \"";
    str += getSourceFile();
    str += "\" at line ";
    str += toString(getSourceLine());
    str += ": ";
    str += getMessage();
    return str;
}

string
Exception::type(void) const {
    return "Exception";
}
