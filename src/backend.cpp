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

#include <fstream>
#include <sstream>
#include <iomanip>
#include <cassert>

#include "backend.h"
using namespace std;


//------------------------------------------------------------------------------
// CBackend
//
CBackend::CBackend(void)
{
}

CBackend::~CBackend(void)
{
}


//------------------------------------------------------------------------------
// CBackendx86
//
CBackendx86::CBackendx86(const string fn)
{
  out = new ofstream(fn);
}

CBackendx86::~CBackendx86(void)
{
  delete out;
}

bool CBackendx86::Emit(CModule *m)
{
  assert(m != NULL);
  assert(out != NULL);

  _m = m;

  if (!out->good()) return false;

  *out << "# " << m->GetName() << endl
       << "        .text" << endl
       << "        .align 4" << endl
       << endl
       << "        .global main" << endl
       << "        .extern Input" << endl
       << "        .extern Output" << endl
       << endl;

  EmitScope(m);


  *out << "        .data" << endl
       << "        .align 4" << endl
       << endl;

  EmitGlobals(m);

  *out << "        .end" << endl;

  return out->good();
}


bool CBackendx86::EmitScope(CScope *s)
{
  assert(s != NULL);

  string label;

  if (s->GetParent() == NULL) label = "main";
  else label = s->GetName();

  // label
  *out << "    # scope " << s->GetName() << endl
       << label << ":" << endl;

  // prologue
  EmitInstruction("push", "%ebp");
  EmitInstruction("movl", "%esp, %ebp");
  EmitInstruction("push", "%ebx");
  EmitInstruction("push", "%esi");
  EmitInstruction("push", "%edi");


  // compute the size of locals
  CSymtab *st = s->GetSymbolTable();
  assert(st != NULL);

  unsigned int nargs = 0;
  size_t size = ComputeStackOffsets(st, 8, -12, &nargs);

  // align at 32-byte boundaries
  // after the prologue, the following data is on the stack
  // - arguments (4 bytes each)
  // - return address (4 bytes)
  // - 4 registers (ebp, ebx, esi, edi, 4 bytes each)
  // = 20 + 4*#args

  // add the required size and round up to the next multiple of 32:
  size = (((size + 20 + nargs*4) + 31) / 32) * 32 - (20 + nargs*4);

  if (size != 0) {
    EmitInstruction("subl", Imm(size) + ", %esp", "align at 32-byte boundary");
  }
  *out << endl;

  CCodeBlock *cb = s->GetCodeBlock();

  while (cb != NULL) {
    EmitCodeBlock(cb, st);
    cb = cb->GetNext();
  }


  // epilogue
  *out << endl;
  if (size != 0) EmitInstruction("addl", Imm(size) + ", %esp");
  EmitInstruction("pop", "%edi");
  EmitInstruction("pop", "%esi");
  EmitInstruction("pop", "%ebx");
  EmitInstruction("pop", "%ebp");
  EmitInstruction("ret");
  *out << endl;

  *out << endl;

  const vector<CScope*> &proc = s->GetSubscopes();
  for (size_t p=0; p<proc.size(); p++) EmitScope(proc[p]);

  return out->good();
}

bool CBackendx86::EmitGlobals(CScope *s)
{
  assert(s != NULL);

  CSymtab *st = s->GetSymbolTable();
  assert(st != NULL);

  vector<CSymbol*> slist = st->GetSymbols();

  for (size_t i=0; i<slist.size(); i++) {
    CSymbol *s = slist[i];
    const CType *t = s->GetDataType();

    if (s->GetSymbolType() == stGlobal) {
      *out << left << setw(7) << s->GetName() + ":" << " "
           << ".skip " << dec << right << setw(4) << t->GetSize()
           << "                  # " << t
           << endl;
    }
  }

  *out << endl;

  return out->good();
}

bool CBackendx86::EmitCodeBlock(CCodeBlock *cb, CSymtab *symtab)
{
  assert(cb != NULL);
  assert(symtab != NULL);

  EmitInstruction(string("# " + cb->GetName()));

  const vector<CTacInstr*> &instr = cb->GetInstr();
  vector<CTacInstr*>::const_iterator it = instr.begin();
  bool first = true;
  while (it != instr.end()) {
    EmitInstruction(*it++, symtab, first);
    first = false;
  }

  return out->good();
}

bool CBackendx86::EmitInstruction(CTacInstr *i, CSymtab *symtab, bool label)
{
  assert(i != NULL);
  assert(symtab != NULL);

  string lbl = (label ? Label(i->GetId())+":" : "");

  CTacTemp *t;
  CTacName *n;
  static EOperation lastCmp;

  EOperation op = i->GetOperation();
  switch (op) {
    case opAssign:
      EmitInstruction("movl", Operand(i->GetSrc(1)) + ", %eax", "", lbl);
      EmitInstruction("movl", "%eax," + Operand(i->GetDest()));
      break;

    case opAdd:
    case opSub:
      EmitInstruction("movl", Operand(i->GetSrc(1)) + ", %eax", "", lbl);
      EmitInstruction(op == opAdd ? "addl" : "subl",
                      Operand(i->GetSrc(2)) + ", %eax");
      t = dynamic_cast<CTacTemp*>(i->GetDest());
      assert(t != NULL);
      t->SetRegister("%eax");
      break;

    case opParam:
      EmitInstruction("movl", Operand(i->GetSrc(2)) + ", %eax", "", lbl);
      EmitInstruction("push", "%eax");
      break;

    case opCall:
      EmitInstruction("call", Operand(i->GetSrc(1)), "", lbl);
      n = dynamic_cast<CTacName*>(i->GetSrc(1));
      t = dynamic_cast<CTacTemp*>(i->GetDest());
      assert(n != NULL);
      EmitInstruction("addl", 
          Imm(dynamic_cast<const CSymProc*>(n->GetSymbol())->GetNParams()*4) +
              ", %esp");
      if (t != NULL) t->SetRegister("%eax");
      break;

    case opGoto:
      EmitInstruction("jmp", Operand(i->GetDest()), "", lbl);
      break;

    case opEqual:
    case opNotEqual:
    case opLessThan:
    case opLessEqual:
    case opBiggerThan:
    case opBiggerEqual:
      EmitInstruction("movl", Operand(i->GetSrc(1)) + ", %eax", "", lbl);
      EmitInstruction("cmpl", Operand(i->GetSrc(2)) + ", %eax");

      lastCmp = i->GetOperation();
      break;

    case opIfNot:
      lastCmp = Inverse(lastCmp);
    case opIf:
      EmitInstruction("j"+Condition(lastCmp), Operand(i->GetDest()), "", lbl);
      break;

    default:
      EmitInstruction("# ???", "", "", lbl);
  }


  return out->good();
}

bool CBackendx86::EmitInstruction(string mnemonic, string args, string comment,
                                  string label)
{
  *out << left
       << setw(7) << label << " "
       << setw(7) << mnemonic << " "
       << setw(23) << args;
  if (comment != "") *out << " # " << comment;
  *out << endl;

  return out->good();
}

string CBackendx86::Operand(CTac *op) const
{
  CTacName *n;
  CTacConst *c;
  CTacTemp *t;
  CTacInstr *i;

  if ((c = dynamic_cast<CTacConst*>(op)) != NULL) {
    return Imm(c->GetValue());
  } else
  if ((n = dynamic_cast<CTacName*>(op)) != NULL) {
    const CSymbol *s = n->GetSymbol();
    return s->GetName();
  } else
  if ((t = dynamic_cast<CTacTemp*>(op)) != NULL) {
    return t->GetRegister();
  } else
  if ((i = dynamic_cast<CTacInstr*>(op)) != NULL) {
    return Label(i->GetId());
  } else
    return "?";
}

string CBackendx86::Label(int id) const
{
  ostringstream o;
  o << ".L" << dec << id;
  return o.str();
}

string CBackendx86::Imm(int value) const
{
  ostringstream o;
  o << "$" << dec << value;
  return o.str();
}

EOperation CBackendx86::Inverse(EOperation cond) const
{
  switch (cond) {
    case opEqual:       return opNotEqual;
    case opNotEqual:    return opEqual;
    case opLessThan:    return opBiggerEqual;
    case opLessEqual:   return opBiggerThan;
    case opBiggerThan:  return opLessEqual;
    case opBiggerEqual: return opLessThan;
    default:            assert(false); break;
  }
}

string CBackendx86::Condition(EOperation cond) const
{
  switch (cond) {
    case opEqual:       return "e";
    case opNotEqual:    return "ne";
    case opLessThan:    return "l";
    case opLessEqual:   return "le";
    case opBiggerThan:  return "g";
    case opBiggerEqual: return "ge";
    default:            assert(false); break;
  }
}


size_t CBackendx86::ComputeStackOffsets(CSymtab *symtab,
                                        int param_ofs,int local_ofs,
                                        unsigned int *nargs)
{
  assert(symtab != NULL);
  vector<CSymbol*> slist = symtab->GetSymbols();

  size_t size = 0;

  if (nargs != NULL) *nargs = 0;

  //cout << "  ComputeStackOffsets()" << endl;
  for (size_t i=0; i<slist.size(); i++) {
    CSymbol *s = slist[i];

    //cout << "    " << left << setw(32) << s;

    ESymbolType st = s->GetSymbolType();

    if (st == stLocal) {
      int ssize = s->GetDataType()->GetSize();
      size += ssize;
      local_ofs -= ssize;

      s->SetBaseRegister("%ebp");
      s->SetOffset(local_ofs);

    } else if (st == stParam) {
      CSymParam *p = dynamic_cast<CSymParam*>(s);
      assert(p != NULL);

      if (nargs != NULL) (*nargs)++;

      p->SetBaseRegister("%ebp");
      p->SetOffset(param_ofs + p->GetIndex()*4);
    }

    //cout << " location: " << Imm(s->GetOffset())
    //     << "(" << s->GetBaseRegister() << ")" << endl;
  }
  //cout << endl
  //     << "    size = " << size;
  //if (nargs != NULL) cout << ", #args: " << *nargs;
  //cout << endl;

  return size;
}
