//------------------------------------------------------------------------------
/// @brief SnuPL backend
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/11/28 Bernhard Egger created
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

#ifndef __SnuPL_BACKEND_H__
#define __SnuPL_BACKEND_H__

#include <iostream>
#include <vector>

#include "symtab.h"
#include "ir.h"

using namespace std;

//------------------------------------------------------------------------------
/// @brief backend base class
///
/// base class for backends
///

class CBackend {
  public:
    /// @name constructors/destructors
    /// @{

    CBackend(void);
    virtual ~CBackend(void);

    /// @}

    /// @name output method
    /// @{

    virtual bool Emit(CModule *m) = 0;

    /// @}

  protected:
    CModule *_m;                    ///< module
};


//------------------------------------------------------------------------------
/// @brief x86 backend
///
/// backend for Intel IA32
///
class CBackendx86 : public CBackend {
  public:
    /// @name constructors/destructors
    /// @{

    CBackendx86(const string fn);
    virtual ~CBackendx86(void);

    /// @}

    /// @name output method
    /// @{

    virtual bool Emit(CModule *m);

    /// @}

  protected:

    /// @name 
    /// @[

    /// @brief emit a scope
    bool EmitScope(CScope *s);

    /// @brief emit global data
    bool EmitGlobals(CScope *s);

    /// @brief emit a code block
    bool EmitCodeBlock(CCodeBlock *cb, CSymtab *symtab);

    /// @brief emit an instruction
    /// @param label controls if the instruction needs a target label or not
    bool EmitInstruction(CTacInstr *i, CSymtab *symtab, bool label);

    /// @brief emit an instruction
    bool EmitInstruction(string mnemonic, string args="", string comment="",
                         string label="");

    /// @brief return an operand string for @a op
    string Operand(CTac *op) const;

    /// @brief return a local label for @a id
    string Label(int id) const;

    /// @brief return an immediate for @a value
    string Imm(int value) const;

    /// @brief return the inverse operation for a binary comparison operation
    EOperation Inverse(EOperation cond) const;

    /// @brief return the condition suffix for a binary comparison operation
    string Condition(EOperation cond) const;

    /// @brief compute the location of local variables, temporaries and
    ///        arguments on the stack. Returns the total size occupied on
    ///        the stack as well as the the number of arguments for this 
    ///        scope (if @a nargs is not NULL)
    /// @param symtab symbol table
    /// @param param_ofs offset to parameters from base pointer after epilogue
    /// @param local_ofs offset to local vars from base pointer after epilogue
    /// @param nargs [output] returns the number of arguments for this scope
    size_t ComputeStackOffsets(CSymtab *symtab, int param_ofs, int local_ofs,
                               unsigned int *nargs);

    /// @}

  private:
    CModule *_m;                    ///< module
    ostream *out;                   ///< output stream

};




#endif // __SnuPL_BACKEND_H__
