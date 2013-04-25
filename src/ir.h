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

#ifndef __SnuPL0_IR_H__
#define __SnuPL0_IR_H__

#include <iostream>
#include <vector>

#include "symtab.h"

using namespace std;

//------------------------------------------------------------------------------
/// @brief operations
///
enum EOperation {
  // binary operators
  // dst = src1 op src2
  opAdd=0,                          ///< +  addition
  opSub,                            ///< -  subtraction
  opMul,                            ///< *  multiplication
  opDiv,                            ///< /  division
  opAnd,                            ///< && binary and
  opOr,                             ///< || binary or

  // unary operators
  // dst = src1
  opNeg,                            ///< -  negation
  opNot,                            ///< !  binary not

  // binary comparison operators
  // src1 op src2
  opEqual,                          ///< =  equal
  opNotEqual,                       ///< #  not equal
  opLessThan,                       ///< <  less than
  opLessEqual,                      ///< <= less or equal
  opBiggerThan,                     ///< >  bigger than
  opBiggerEqual,                    ///< >= bigger or equal

  // nop
  opNop,                            ///< no operation

  // memory operations
  opAssign,                         ///< (HL) assign: dst = src1
  opLoad,                           ///< load:  dst = mem[src1]
  opStore,                          ///< store: mem[dst] = src1

  // branching
  opGoto,                           ///< uncond. branch: dst = target
  opIf,                             ///< cond. branch: src1 = cond, dst = target
  opIfNot,                          ///< cond. branch: src1 = cond, dst = target

  // function calls
  opCall,                           ///< call:  dst = call src1
  opReturn,                         ///< return: [src1]
  opParam,                          ///< argument: src1 = index, src2 = argument
};

/// @brief EOperation output operator
///
/// @param out output stream
/// @param t EOperation
/// @retval output stream
ostream& operator<<(ostream &out, EOperation t);


//------------------------------------------------------------------------------
/// @brief address class
///
/// base class for addresses (variables, constants, temporaries)
///
class CTacAddr {
};


//------------------------------------------------------------------------------
/// @brief code block
///
/// base node class for code blocks
///

class CCodeBlock {
};


/// @}


#endif // __SnuPL0_IR_H__
