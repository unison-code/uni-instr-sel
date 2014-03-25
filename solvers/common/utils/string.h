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

#ifndef SOLVERS_COMMON_UTILS_STRING__
#define SOLVERS_COMMON_UTILS_STRING__

#include <sstream>
#include <string>

namespace Utils {

/**
 * Converts an element of any type (or at least most) into a string.
 *
 * @tparam T
 *         Element type.
 * @param e
 *        Element to convert.
 * @returns String representation.
 */
template <typename T>
std::string
toString(const T& e) {
    std::stringstream ss;
    ss << e;
    return ss.str();
}

/**
 * Checks if a given character is a whitespace.
 *
 * @param c
 *        Character
 * @return \c true if whitespace.
 */
bool
isWhitespace(char c);

/**
 * Search a string for a substring and replaces it with another substring.
 *
 * @param str
 *        String to search in.
 * @param search
 *        String to search for.
 * @param replace
 *        String to replace with.
 */
std::string
searchReplace(const std::string& str,
              const std::string& search,
              const std::string& replace);

}

#endif
