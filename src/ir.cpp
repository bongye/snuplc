//------------------------------------------------------------------------------
/// @brief SnuPL intermediate representation
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/03/07 Bernhard Egger adapted to SnuPL/0
/// 2013/04/01 Bernhard Egger adapted to SnuPL/-1 (assignment 1)
///
/// @section license_section License
/// Copyright (c) 2012, Bernhard Egger
/// All rights reserved.
///
/// Redistribution and use in source and binary forms,  with or without modifi-
/// cation, are permitted provided that the following conditions are met:
///
/// - Redistributions of source code must retain the above copyright notice,
///   this list of conditions and the following disclaimer.
/// - Redistributions in binary form must reproduce the above copyright notice,
///   this list of conditions and the following disclaimer in the documentation
///   and/or other materials provided with the distribution.
///
/// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
/// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,  BUT NOT LIMITED TO,  THE
/// IMPLIED WARRANTIES OF MERCHANTABILITY  AND FITNESS FOR A PARTICULAR PURPOSE
/// ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDER  OR CONTRIBUTORS BE
/// LIABLE FOR ANY DIRECT,  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSE-
/// QUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF  SUBSTITUTE
/// GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
/// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN  CONTRACT, STRICT
/// LIABILITY, OR TORT  (INCLUDING NEGLIGENCE OR OTHERWISE)  ARISING IN ANY WAY
/// OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
/// DAMAGE.
//------------------------------------------------------------------------------

#include <iomanip>
#include <cassert>

#include "ir.h"
#include "ast.h"
using namespace std;


//------------------------------------------------------------------------------
// operation names
//
#define OPERATION_STRLEN 8

char EOperationName[][OPERATION_STRLEN] = {
  // binary operators
  "add",                            ///< addition
  "sub",                            ///< subtraction
  "mul",                            ///< multiplication
  "div",                            ///< division
  "and",                            ///< binary and
  "or",                             ///< binary or

  // unary operators
  "neg",                            ///< negation
  "not",                            ///< binary not

  // binary comparison operators
  "eq",                             ///< =  equal
  "neq",                            ///< #  not equal
  "lt",                             ///< <  less than
  "le",                             ///< <= less or equal
  "gt",                             ///< >  bigger than
  "ge",                             ///< >= bigger or equal

  // nop
  "nop",                            ///< no operation

  // memory operations
  "assign",                         ///< (HL) assign: dst = src1
  "load",                           ///< load:  dst = mem[src1]
  "store",                          ///< store: mem[dst] = src1

  // branching
  "goto",                           ///< uncond. branch: dst = target
  "if",                             ///< cond. branch: src1 = cond, dst = target
  "if not",                         ///< cond. branch: src1 = cond, dst = target

  // function calls
  "call",                           ///< call:  dst = call src1
  "return",                         ///< return: [src1]
  "param",                          ///< argument: src1 = index, src2 = argument
};

ostream& operator<<(ostream &out, EOperation t)
{
  out << EOperationName[t];
  return out;
}
