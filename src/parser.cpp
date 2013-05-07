//------------------------------------------------------------------------------
/// @brief SnuPL/0 parser
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/03/07 Bernhard Egger adapted to SnuPL/0
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

#include <limits.h>
#include <cassert>
#include <vector>
#include <iostream>
#include <exception>
#include <stdlib.h>
#include <errno.h>


#include "parser.h"
using namespace std;


//------------------------------------------------------------------------------
// CParser
//
CParser::CParser(CScanner *scanner)
{
  _scanner = scanner;
  _module = NULL;
}

CAstNode* CParser::Parse(void)
{
  _abort = false;

  if (_module != NULL) { delete _module; _module = NULL; }

  try {
    if (_scanner != NULL) _module = module();

    if (_module != NULL) {
      CToken t;
      string msg;
      if (!_module->TypeCheck(&t, &msg)) SetError(t, msg);
    }
  } catch (...) {
    _module = NULL;
  }

  return _module;
}

const CToken* CParser::GetErrorToken(void) const
{
  if (_abort) return &_error_token;
  else return NULL;
}

string CParser::GetErrorMessage(void) const
{
  if (_abort) return _message;
  else return "";
}

void CParser::SetError(CToken t, const string message)
{
  _error_token = t;
  _message = message;
  _abort = true;
  throw message;
}

bool CParser::Consume(EToken type, CToken *token)
{
  if (_abort) return false;

  CToken t = _scanner->Get();

  if (t.GetType() != type) {
    SetError(t, "expected '" + CToken::Name(type) + "', got '" +
             t.GetName() + "'");
  }

  if (token != NULL) *token = t;

  return t.GetType() == type;
}

void CParser::InitSymbolTable(CSymtab *s)
{
  CTypeManager *tm = CTypeManager::Get();

  CSymProc *input = new CSymProc("Input", tm->GetInt());

  CSymProc *output = new CSymProc("Output", tm->GetNull());
  output->AddParam(new CSymParam(0, "x", tm->GetInt()));

  s->AddSymbol(input);
  s->AddSymbol(output);
}

CAstModule* CParser::module(void)
{
  //
  // module = statSequence ".".
  //
  CToken dummy;
  CAstModule *m = new CAstModule(dummy, "placeholder");
  CAstStatement *statseq = NULL;

  statseq = statSequence(m);
  Consume(tDot);

  m->SetStatementSequence(statseq);

  return m;
}

CAstDesignator* CParser::ident(void)
{
	//
	// ident = letter { letter | digit } .
	//
	// "letter { letter | digit }" is scanned as one token (tIdent) 
	//

	CToken t;
	
	Consume(tIdent, &t);
	CSymtab *symtab = _module->GetSymbolTable();
	const CSymbol *symbol = symtab->FindSymbol(t.GetName());

	return new CAstDesignator(t, symbol);
}

CAstConstant* CParser::number(void)
{
  //
  // number = digit { digit }.
  //
  // "digit { digit }" is scanned as one token (tNumber)
  //

  CToken t;

  Consume(tNumber, &t);

  errno = 0;
  long long v = strtoll(t.GetValue().c_str(), NULL, 10);
  if (errno != 0) SetError(t, "invalid number.");

  return new CAstConstant(t, CTypeManager::Get()->GetInt(), v);
}

CAstConstant* CParser::boolean(void)
{
	//
	// boolean = "true" | "false"
	//
	// '"true" | "false"' is scanned as one token(tBoolean)
	//

	CToken t;
	
	Consume(tBoolean, &t);
	bool v = t.GetValue().compare("true") == 0;

	return new CAstConstant(t, CTypeManager::Get()->GetBool(), v);
}

CAstType* CParser::type(void)
{
	//
	// type = "integer" | "boolean"
	//
	// '"integer" | "boolean"' is scanned as one token(tType)
	//

	CToken t;
	CAstType *n;
	
	Consume(tType, &t);
	
	if (t.GetValue() == "integer") n = new CAstType(t, CTypeManager::Get()->GetInt());
	else n = new CAstType(t, CTypeManager::Get()->GetBool());

	return n;
}

CAstExpression* CParser::factor(CAstScope *s)
{
  //
  // factor = ident | number | boolean | "(" expression ")" | subroutineCall | "!" factor
  //
  // FIRST(factor) = { tIdent, tNumber, tBoolean, tLBrak }
  //
  CAstExpression *n = NULL;
	CToken t = _scanner->Peek();
	CSymtab *symtab = _module->GetSymbolTable();

  switch (t.GetType()) {
    case tNumber:
      n = number();
      break;
		case tIdent:
			if (symtab->FindSymbol(t.GetValue())->GetSymbolType() == stProcedure) {
				subroutineCall(s);
			} else {
				assignment(s);
			}
			break;
		case tBoolean:
			n = boolean();
			break;
    case tLBrak:
      Consume(tLBrak);
      n = expression(s);
      Consume(tRBrak);
      break;
		case tUnaryOp:
			Consume(tUnaryOp);
			n = factor(s);
			break;

    default:
      SetError(_scanner->Peek(), "factor expected.");
      break;
  }

  return n;
}

CAstExpression* CParser::term(CAstScope *s)
{
  //
  // term = factor { factorOp factor }.
  //
  CAstExpression *n = NULL;

  n = factor(s);

  while (_scanner->Peek().GetType() == tFactOp) {
    CToken t;
    CAstExpression *l = n, *r;

    Consume(tFactOp, &t);

    r = factor(s);
		EOperation op;
		if(t.GetValue() == "*") op = opMul;
		else if(t.GetValue() == "/") op = opDiv;
		else if(t.GetValue() == "&&") op = opAnd;
		else SetError(t, "invalid operator.");

    n = new CAstBinaryOp(t, op, l, r);
  }

  return n;
}

CAstExpression* CParser::simpleexpr(CAstScope *s)
{
  //
  // simpleexpr = ['+'|'-'] term { termOp term }.
	//
  CAstExpression *n = NULL;
	CToken tt;
	tt = _scanner->Peek();
	if(tt.GetValue() == "+" || tt.GetValue() == "-"){
		Consume(tTermOp, &tt);
	}

  n = term(s);

  while (_scanner->Peek().GetType() == tTermOp){
    CToken t;
    CAstExpression *l = n, *r;

    Consume(tTermOp, &t);

    r = term(s);
		EOperation op;
		if(t.GetValue() == "+") op = opAdd;
		else if(t.GetValue() == "-") op = opSub;
		else if(t.GetValue() == "||") op = opOr;
		else SetError(t, "invalid operator.");

    n = new CAstBinaryOp(t, op, l, r);
  }


  return n;
}

CAstExpression* CParser::expression(CAstScope* s)
{
  //
  // expression = simpleexpr [ relOp simpleexpr ].
  //
  CToken t;
  EOperation relop;
  CAstExpression *left = NULL, *right = NULL;

  left = simpleexpr(s);

  if (_scanner->Peek().GetType() == tRelOp) {
    Consume(tRelOp, &t);
    right = simpleexpr(s);

    if (t.GetValue() == "=")       relop = opEqual;
    else if (t.GetValue() == "#")  relop = opNotEqual;
		else if (t.GetValue() == "<")  relop = opLessThan;
		else if (t.GetValue() == "<=") relop = opLessEqual;
		else if (t.GetValue() == ">")  relop = opBiggerThan;
		else if (t.GetValue() == ">=") relop = opBiggerEqual;
    else SetError(t, "invalid relation.");

    return new CAstBinaryOp(t, relop, left, right);
  } else {
    return left;
  }
}

CAstStatAssign* CParser::assignment(CAstScope *s)
{
  //
  // assignment = ident ":=" expression.
  //
  CToken t;

  CAstDesignator *lhs = ident();

  Consume(tAssign, &t);

  CAstExpression *rhs = expression(s);

  return new CAstStatAssign(t, lhs, rhs);
}

CAstStatCall* CParser::subroutineCall(CAstScope *s)
{
	//
	// subroutineCall = ident "(" [expression { "," expression } ] ")" .
	//
	// FIRST(expression) = { +, -, tIdent, tBoolean, tNumber, tLBrak, tUnary}
	//
	CToken t;
	CAstDesignator *identifier = NULL;
	CAstExpression *right = NULL;
	CAstFunctionCall *call = NULL;

	identifier = ident();
	
	Consume(tLBrak, &t);
	
	t = _scanner->Peek();
	if(t.GetType() != tRBrak){
		right = expression(s);
		while(_scanner->Peek().GetType() == tComma){
			Consume(tComma, &t);
			right = expression(s);
		}
	} else {
		Consume(tRBrak, &t);
	}
	call = new CAstFunctionCall(t, (CSymProc *)identifier->GetSymbol());
	return new CAstStatCall(t, call);
}

CAstStatIf* CParser::ifStatement(CAstScope *s)
{
	CToken t;
	CAstExpression *cond = NULL;
	CAstStatement *ifBody = NULL;
	CAstStatement *elseBody = NULL;

	Consume(tIf, &t);
	Consume(tLBrak, &t);
	cond = expression(s);
	Consume(tRBrak, &t);
	Consume(tThen, &t);
	ifBody = statSequence(s);
	if(_scanner->Peek().GetType() == tElse) {
		Consume(tElse, &t);
		elseBody = statSequence(s);
	}
	Consume(tEnd, &t);
	return new CAstStatIf(t, cond, ifBody, elseBody);
}

CAstStatWhile* CParser::whileStatement(CAstScope *s)
{
	CToken t;
	CAstExpression *cond = NULL;
	CAstStatement *body = NULL;

	Consume(tWhile, &t);
	Consume(tLBrak, &t);
	cond = expression(s);
	Consume(tRBrak, &t);
	Consume(tDo, &t);
	body = statSequence(s);
	Consume(tEnd, &t);
	return new CAstStatWhile(t, cond, body);
}

CAstStatReturn* CParser::returnStatement(CAstScope *s)
{
	// returnStatement = "return" [ expression ]
	// FIRST(expression) = { '+', '-', tIdent, tNumber, tBoolean, tLBrak, '!' }
	CToken t;
	CAstExpression *expr = NULL;

	Consume(tReturn, &t);
	t = _scanner->Peek();
	if(t.GetValue() == '+' || t.GetValue() == '-' || t.GetValue() == '!' || t.GetType() == tIdent || t.GetType() == tBoolean || t.GetType() == tLBrak){
		expr = expression(s);
	}
	return new CAstStatReturn(t, s, expr);
}

CAstStatement* CParser::statement(CAstScope *s)
{
	// statement    = assignment | subroutineCall | ifStatement | whileStatement | returnStatement.
  // FIRST(statement) = { tIdent, tIf, tWhile, tReturn }
	CToken t;
	CAstStatement *statement = NULL;
	
	t = _scanner->Peek();
	switch (t.GetType()) {
		case tIdent:
			break;
		case tIf:
			break;
		case tWhile:
			break;
		case tReturn:
			break;
	}
}

CAstStatement* CParser::statSequence(CAstScope *s)
{
  //
  // statSequence = [ statement { ";" statement } ].
  // FOLLOW(statSequence) = { tEnd }
  //
  CAstStatement *head = NULL;


  EToken tt = _scanner->Peek().GetType();
  if (!(tt == tDot)) {
    CAstStatement *tail = NULL;

    do {
      CToken t;
      EToken tt = _scanner->Peek().GetType();
      CAstStatement *st = NULL;

      switch (tt) {
        // statement = assignment | subroutineCall
        case tIdent:
          st = assignment(s);
          break;

				case tIf:
					break;
				
				case tWhile:
					break;



        default:
          SetError(_scanner->Peek(), "statement expected.");
          break;
      }

      assert(st != NULL);
      if (head == NULL) head = st;
      else tail->SetNext(st);
      tail = st;

      tt = _scanner->Peek().GetType();
      if (tt == tDot) break;

      Consume(tSemicolon);
    } while (!_abort);
  }

  return head;
}