//------------------------------------------------------------------------------
/// @brief SnuPL backend
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/11/28 Bernhard Egger created
/// 2013/06/09 Bernhard Egger adapted to SnuPL/0
///
/// @section license_section License
/// Copyright (c) 2012,2013 Bernhard Egger
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
CBackend::CBackend(ostream &out)
  : _out(out)
{
}

CBackend::~CBackend(void)
{
}

bool CBackend::Emit(CModule *m)
{
  assert(m != NULL);
  _m = m;

  if (!_out.good()) return false;

  bool res = true;

  try {
    EmitHeader();
    EmitCode();
    EmitData();
    EmitFooter();

    res = _out.good();
  } catch (...) {
    res = false;
  }

  return res;
}

void CBackend::EmitHeader(void)
{
}

void CBackend::EmitCode(void)
{
}

void CBackend::EmitData(void)
{
}

void CBackend::EmitFooter(void)
{
}


//------------------------------------------------------------------------------
// CBackendx86
//
CBackendx86::CBackendx86(ostream &out)
  : CBackend(out), _curr_scope(NULL)
{
  _ind = string(4, ' ');
}

CBackendx86::~CBackendx86(void)
{
}

void CBackendx86::EmitHeader(void)
{
  _out << "##################################################" << endl
       << "# " << _m->GetName() << endl
       << "#" << endl
       << endl;
}

void CBackendx86::EmitCode(void)
{
  _out << _ind << "#-----------------------------------------" << endl
       << _ind << "# text section" << endl
       << _ind << "#" << endl
       << _ind << ".text" << endl
       << _ind << ".align 4" << endl
       << endl
       << _ind << "# entry point and pre-defined functions" << endl
       << _ind << ".global main" << endl
       << _ind << ".extern Input" << endl
       << _ind << ".extern Output" << endl
       << endl;

  // TODO
	vector<CScope *>::const_iterator sit = _m->GetSubscopes().begin();
	while (sit != _m->GetSubscopes().end()) EmitScope(*sit++);
	EmitScope(_m);

  _out << _ind << "# end of text section" << endl
       << _ind << "#-----------------------------------------" << endl
       << endl;
}

void CBackendx86::EmitData(void)
{
  _out << _ind << "#-----------------------------------------" << endl
       << _ind << "# global data section" << endl
       << _ind << "#" << endl
       << _ind << ".data" << endl
       << _ind << ".align 4" << endl
       << endl;

  // TODO
	EmitGlobalData(_m);

  _out << _ind << "# end of global data section" << endl
       << _ind << "#-----------------------------------------" << endl
       << endl;
}

void CBackendx86::EmitFooter(void)
{
  _out << _ind << ".end" << endl
       << "##################################################" << endl;
}

void CBackendx86::SetScope(CScope *scope)
{
  _curr_scope = scope;
}

CScope* CBackendx86::GetScope(void) const
{
  return _curr_scope;
}

void CBackendx86::EmitScope(CScope *scope)
{
  assert(scope != NULL);

  string label;

  if (scope->GetParent() == NULL) label = "main";
  else label = scope->GetName();

  // label
  _out << _ind << "# scope " << scope->GetName() << endl
       << label << ":" << endl;

  // TODO
	SetScope(scope);

	// prologue
	EmitInstruction("pushl", "%ebp");
	EmitInstruction("movl", "%esp, %ebp");
	EmitInstruction("pushl", "%ebx");
	EmitInstruction("pushl", "%esi");
	EmitInstruction("pushl", "%edi");

	// compute the size of locals
	CSymtab *symbolTable = scope->GetSymbolTable();
	assert(symbolTable != NULL);

	size_t size = ComputeStackOffsets(symbolTable, 8, -12);
	EmitInstruction("subl", Imm(size) + ", %esp", "make room for locals");
	_out << endl;
	
	CCodeBlock *cb = scope->GetCodeBlock();
	EmitCodeBlock(cb, symbolTable);
	
	// epilogue
	_out << endl;
	_out << Label("exit") << ":" << endl;

	EmitInstruction("addl", Imm(size) + ", %esp", "remove locals");
	EmitInstruction("popl", "%edi");
	EmitInstruction("popl", "%esi");
	EmitInstruction("popl", "%ebx");
	EmitInstruction("popl", "%ebp");
	EmitInstruction("ret");
  
	_out << endl;
}

void CBackendx86::EmitGlobalData(CScope *scope)
{
  assert(scope != NULL);

  _out << _ind << "# scope: " << scope->GetName() << endl;

  // emit the globals for the current scope
  // TODO
	CSymtab *symbolTable = scope->GetSymbolTable();
	assert(symbolTable != NULL);

	vector<CSymbol *> symbols = symbolTable->GetSymbols();
	for (size_t i=0; i<symbols.size(); i++) {
		CSymbol *symbol = symbols[i];
		const CType *t = symbol->GetDataType();

		if (symbol->GetSymbolType() == stGlobal) {
			_out << left << setw(7) << symbol->GetName() + ":" << " " 
					 << setw(7) << ".skip" << " " 
					 << setw(16) << (t->GetSize() + 3) / 4 * 4;
			_out << " # " << t;
			_out << endl;
		}
	}
	_out << endl;

  // emit globals in subscopes (necessary if we support static local variables)
  vector<CScope*>::const_iterator sit = scope->GetSubscopes().begin();
  while (sit != scope->GetSubscopes().end()) EmitGlobalData(*sit++);
}

void CBackendx86::EmitCodeBlock(CCodeBlock *cb, CSymtab *symtab)
{
  assert(cb != NULL);
  assert(symtab != NULL);

  const list<CTacInstr*> &instr = cb->GetInstr();
  list<CTacInstr*>::const_iterator it = instr.begin();

  while (it != instr.end()) EmitInstruction(*it++, symtab);
}

void CBackendx86::EmitInstruction(CTacInstr *i, CSymtab *symtab)
{
  assert(i != NULL);
  assert(symtab != NULL);

  ostringstream cmt;
  cmt << i;

	CTacTemp *tmp;
	CTacName *name;
	int nParams;

  EOperation op = i->GetOperation();
  switch (op) {
    // binary operators
    // dst = src1 op src2
    // TODO

		case opAdd:
		case opSub:
			EmitInstruction("movl", Operand(i->GetSrc(1)) + ", %eax", cmt.str());
			EmitInstruction((op == opAdd) ? "addl" : "subl", Operand(i->GetSrc(2)) + ", %eax");
			tmp = dynamic_cast<CTacTemp *>(i->GetDest());
			assert(tmp != NULL);
			EmitInstruction("movl", "%eax, " + Operand(tmp));
			break;

		case opMul:
			EmitInstruction("movl", Operand(i->GetSrc(1)) + ", %eax", cmt.str());
			EmitInstruction("imull", Operand(i->GetSrc(2)));
			tmp = dynamic_cast<CTacTemp *>(i->GetDest());
			assert(tmp != NULL);
			EmitInstruction("movl", "%eax, " + Operand(tmp));
			break;

		case opDiv:
			EmitInstruction("movl", Operand(i->GetSrc(1)) + ", %eax", cmt.str());
			EmitInstruction("cdq", "");
			EmitInstruction("idivl", Operand(i->GetSrc(2)));
			tmp = dynamic_cast<CTacTemp *>(i->GetDest());
			assert(tmp != NULL);
			EmitInstruction("movl", "%eax, " + Operand(tmp));
			break;

    // unary operators
    // dst = op src1
    // TODO

		case opNeg:
			EmitInstruction("movl", Operand(i->GetSrc(1)) + ", %eax", cmt.str());
			EmitInstruction("negl", "%eax");
			tmp = dynamic_cast<CTacTemp *>(i->GetDest());
			assert(tmp != NULL);
			EmitInstruction("movl", "%eax, " + Operand(tmp));
			break;

    // memory operations
    // dst = src1
    // TODO
		case opAssign:
			EmitInstruction("movl", Operand(i->GetSrc(1)) + ", %eax", cmt.str());
			EmitInstruction("movl", "%eax, " + Operand(i->GetDest()));
			break;

    // unconditional branching
    // goto dst
    // TODO
		case opGoto:
			EmitInstruction("jmp", Operand(i->GetDest()), cmt.str());
			break;

    // conditional branching
    // if src1 relOp src2 then goto dst
    // TODO
		case opEqual:
		case opNotEqual:
		case opLessThan:
		case opLessEqual:
		case opBiggerThan:
		case opBiggerEqual:
			EmitInstruction("movl", Operand(i->GetSrc(1)) + ", %eax", cmt.str());
			EmitInstruction("cmpl", Operand(i->GetSrc(2)) + ", %eax" );
			EmitInstruction("j" + Condition(op), Operand(i->GetDest()));
			break;

    // function call-related operations
    // TODO
		case opParam:
			EmitInstruction("movl", Operand(i->GetSrc(1)) + ", %eax", cmt.str());
			EmitInstruction("pushl", "%eax");
			break;

		case opCall:
			EmitInstruction("call", Operand(i->GetSrc(1)), cmt.str());

			name = dynamic_cast<CTacName *>(i->GetSrc(1));
			tmp = dynamic_cast<CTacTemp *>(i->GetDest());
			assert(name != NULL);
			nParams = dynamic_cast<const CSymProc *>(name->GetSymbol())->GetNParams();
			if (nParams > 0 ) EmitInstruction("addl", Imm(nParams * 4) + ", %esp", "");
			if (tmp != NULL) EmitInstruction("movl", "%eax, " + Operand(tmp));
			break;

    // special
    case opLabel:
      _out << Label(dynamic_cast<CTacLabel*>(i)) << ":" << endl;
      break;

    case opNop:
      EmitInstruction("nop", "", cmt.str());
      break;
		
		case opReturn:
			if (i->GetSrc(1) != NULL) EmitInstruction("movl", Operand(i->GetSrc(1)) + ", %eax", cmt.str());
			EmitInstruction("jmp", Label("exit"));
			break;

    default:
      EmitInstruction("# ???", "not implemented", cmt.str());
	}
}

void CBackendx86::EmitInstruction(string mnemonic, string args, string comment)
{
  _out << left
       << _ind
       << setw(7) << mnemonic << " "
       << setw(23) << args;
  if (comment != "") _out << " # " << comment;
  _out << endl;
}

string CBackendx86::Operand(CTac *op) const
{
  string operand;

	CTacName *n;
	CTacConst *c;
	CTacTemp *t;
	CTacLabel *l;

  // TODO
	if ((c = dynamic_cast<CTacConst *>(op)) != NULL) {
		return Imm(c->GetValue());
	}
	if ((t = dynamic_cast<CTacTemp *>(op)) != NULL) {
		const CSymbol *s = t->GetSymbol();
		ESymbolType st = s->GetSymbolType();
		ostringstream ostr;
		if (st == stLocal) {
			ostr << s->GetOffset() << "(" << s->GetBaseRegister() << ")";
			operand = ostr.str();
		}
	}
	if ((n = dynamic_cast<CTacName *>(op)) != NULL) {
		const CSymbol *s = n->GetSymbol();
		ESymbolType st = s->GetSymbolType();
		ostringstream ostr;
		if (st == stLocal || st == stParam) {
			ostr << s->GetOffset() << "(" << s->GetBaseRegister() << ")";
			operand = ostr.str();
		} else {
			operand = s->GetName();
		}
	}
	if ((l = dynamic_cast<CTacLabel *>(op)) != NULL) {
		return Label(l);
	}

  return operand;
}

string CBackendx86::Imm(int value) const
{
  ostringstream o;
  o << "$" << dec << value;
  return o.str();
}

string CBackendx86::Label(CTacLabel* label) const
{
  CScope *cs = GetScope();
  assert(cs != NULL);

  ostringstream o;
  o << "l_" << cs->GetName() << "_" << label->GetLabel();
  return o.str();
}

string CBackendx86::Label(string label) const
{
  CScope *cs = GetScope();
  assert(cs != NULL);

  return "l_" + cs->GetName() + "_" + label;
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
                                        int param_ofs, int local_ofs)
{
  // TODO
	size_t size = 0;
  assert(symtab != NULL);
  vector<CSymbol*> symbols = symtab->GetSymbols();
	int nargs = 0;
	for (size_t i=0; i<symbols.size(); i++) {
		CSymbol *symbol = symbols[i];
		ESymbolType st = symbol->GetSymbolType();
		if (st == stLocal) {
			int symbolSize = symbol->GetDataType()->GetSize();

			symbol->SetBaseRegister("%ebp");
			symbol->SetOffset(local_ofs - symbolSize);

			size += (symbolSize + 3) / 4 * 4;
			local_ofs -= (symbolSize + 3) / 4 * 4;
		} else if (st == stParam) {
			nargs ++;

			CSymParam *param = dynamic_cast<CSymParam *>(symbol);
			assert(param != NULL);

			param->SetBaseRegister("%ebp");
			param->SetOffset(param_ofs + param->GetIndex() * 4);
		}
	}

  //size = (((size + 20 + nargs*4) + 31) / 32) * 32 - (20 + nargs*4);
  return size;
}
