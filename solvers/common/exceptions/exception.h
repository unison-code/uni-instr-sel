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

#ifndef SOLVER_GECODE_EXCEPTIONS_EXCEPTION__
#define SOLVER_GECODE_EXCEPTIONS_EXCEPTION__

#include <exception>
#include <sstream>
#include <string>

/**
 * @brief Base class for exceptions.
 *
 * This class serves as base from which other exceptions derive. The \c
 * Exception class itself derives from \c std::exception in order to be
 * throwable, but hides the methods provided by the standard exception class
 * since it provides equivalent methods of its own.
 *
 * The macro #THROW(exception_class, ...) can and should be used to simplify
 * exception throwing.
 */
class Exception : private std::exception {
  public:
    /**
     * Creates an exception with no message.
     *
     * @param source_file
     *        Name of source file from where the exception was thrown.
     * @param source_line
     *        Line from where the exception was thrown.
     */
    Exception(const std::string& source_file, int source_line);

    /**
     * Same as Exception() but with an error message.
     *
     * @param source_file
     *        Name of source file from where the exception was thrown.
     * @param source_line
     *        Line in the source file from where the exception was thrown.
     * @param message
     *        Message of what caused this exception.
     */
    Exception(const std::string& source_file,
              int source_line,
              const std::string& message);

    /**
     * Destroys this exception.
     */
    virtual
    ~Exception(void) throw();

    /**
     * Gets the name of the source file from where the exception was thrown.
     *
     * @returns Source file.
     */
    std::string
    getSourceFile(void) const;

    /**
     * Gets the line number from where the exception was thrown.
     *
     * @returns Line number.
     */
    int getSourceLine(void) const;

    /**
     * Gets the error message of this exception.
     *
     * @returns Error message.
     */
    virtual std::string
    getMessage(void) const;

    /**
     * Gets a string representation of this exception.
     *
     * @returns This exception as a string.
     */
    std::string
    toString(void) const;

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
    toString(const T& e) const {
        std::stringstream ss;
        ss << e;
        return ss.str();
    }

  protected:
    /**
     * Gets the name of this exception as a string (needed for toString()).
     *
     * @returns Exception name.
     */
    virtual std::string
    type(void) const;

  private:
    /**
     * Name of source file from where the exception was thrown.
     */
    const std::string source_file_;

    /**
     * Line from where the exception was thrown.
     */
    const int source_line_;

  protected:
    /**
     * Error message.
     */
    const std::string message_;

};

/**
 * @file
 * @def THROW(exception_class, ...)
 *
 * @brief Throws an exception.
 *
 * Throws an exception.
 *
 * All exceptions require that the name of the source file and the line from
 * which the exception is thrown. Although there are preprocessor macros for
 * getting the file name and line (\c __FILE__ and \c __LINE__), this would be
 * tedious if they would have to be inserted manually into the constructor
 * arguments of the exception (using the macros inside the constructor doesn't
 * work). By using this macro, these required values are automatically added as
 * arguments to the exception so the programmer needn't bother.
 *
 * The macro takes an exception class as mandatory argument, along with a
 * variable list of additional arguments. Thus, an exception of type \c
 * ExampleException, which requires an additional \c int argument, can be thrown
 * as follows:
 * @code
 *    THROW(ExampleException, 10);
 * @endcode
 * This is equivalent to writing:
 * @code
 *    throw ExampleException(__FILE__, __LINE__, 10);
 * @endcode
 * If the exception has no additional arguments, the variable list of arguments
 * must be removed entirely, like so:
 * @code
 *    THROW(ExampleException);
 * @endcode
 * The following is an \em incorrect use of the macro and causes an syntax error
 * when compiled:
 * @code
 *    THROW(ExampleException, );
 * @endcode
 *
 * @param exception_class
 *        Name of the exception class to throw.
 * @param ...
 *        A variable list of additional arguments needed by the exception
 *        constructor. If there are no such arguments, remove this parameter
 *        entirely in the macro call (see description).
 */

#define THROW(exception_class, ...) \
    throw exception_class(__FILE__, __LINE__, ##__VA_ARGS__)

#endif
