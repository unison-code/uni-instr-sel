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

#ifndef SOLVERS_PARAM_PROCESSING_COMMON_JSONPRINTING__
#define SOLVERS_PARAM_PROCESSING_COMMON_JSONPRINTING__

#include <list>
#include <ostream>
#include <vector>

// Forward declaration
template <typename T>
void
printJsonList(std::ostream&, const T&);

/**
 * Prints a list as a JSON list. It is expected that T implements the stream
 * operator (\c <<).
 *
 * @tparam T
 *         The data type contained in the list.
 * @param out
 *        The output stream.
 * @param l
 *        The list to print.
 */
template <typename T>
void
printJsonValue(std::ostream& out, const std::list<T>& l) {
    printJsonList(out, l);
}

/**
 * Prints a vector as a JSON list. It is expected that T implements the stream
 * operator (\c <<).
 *
 * @tparam T
 *         The data type contained in the vector.
 * @param out
 *        The output stream.
 * @param l
 *        The vector to print.
 */
template <typename T>
void
printJsonValue(std::ostream& out, const std::vector<T>& v) {
    printJsonList(out, v);
}

/**
 * Prints a data type as a JSON value. It is expected that the data type
 * implements the stream operator (\c <<).
 *
 * @tparam T
 *         The data type parameter.
 * @param out
 *        The output stream.
 * @param v
 *        The entity to print.
 */
template <typename T>
void
printJsonValue(std::ostream& out, const T& v) {
    out << v;
}

/**
 * Prints a boolean value as a JSON boolean.
 *
 * @param out
 *        The output stream.
 * @param v
 *        The value to print.
 */
template <>
void
printJsonValue(std::ostream& out, const bool& v) {
    out << (v ? "true" : "false");
}

/**
 * Prints a container as a JSON list. It is expected that the container is
 * iterable, and the elements within the container implement the stream operator
 * (\c <<).
 *
 * @tparam T
 *         The container type parameter.
 * @param out
 *        The output stream.
 * @param l
 *        The list to print.
 */
template <typename T>
void
printJsonList(std::ostream& out, const T& l) {
    out << "[";
    bool isFirst = true;
    for (const auto& e : l) {
        if (isFirst) isFirst = false;
        else out << ",";
        printJsonValue(out, e);
    }
    out << "]";
}

#endif
