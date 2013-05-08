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
  // module = "module" ident ";" varDeclaration { subroutineDecl } "begin" statSequence "end" ident "."
  //

  CToken t;  
  CAstStatement *statseq = NULL;
  String name = NULL;
  CAstModule *m = NULL;

  Consume(tModule, &t);
  Consume(tIdentifier, &t)
  name = t.GetValue();
  m = new CAstModule(t, name);
  Consume(tSemicolon, &t);  
  varDeclaration(m);  
  while(_scanner->Peek().GetType() == tProcedure || _scanner->Peek().GetType() == tFunction){
    subroutineDecl(m);
  }
  Consume(tBegin, &t);
  statseq = statSequence(m);
  Consume(tEnd, &t);
  Consume(tIdent, &t);
  if(t.GetValue() != name){
    SetError(t, "expected module '" + name + "', got '" + t.GetValue() + "'");
  }
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
	CSymtab *symTab = _module->GetSymbolTable();
	const CSymbol *symbol = symTab->FindSymbol(t.GetValue());

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
	bool v = t.GetValue() == "true";

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
	CToken t;
	CSymtab *symTab = _module->GetSymbolTable();
  CAstStatCall* call = NULL;
  CAstStatAssign* assign = NULL;

  switch (_scanner->Peek().GetType()) {
    case tNumber:
      n = number();
      break;
		case tIdent:
			if (symTab->FindSymbol(t.GetValue())->GetSymbolType() == stProcedure) {
        call = subroutineCall(s);
        n = call->GetCall();
			} else {
        assign = assignment(s);
				n = new CAstBinaryOp(_token, opAssign, assign->GetLHS(), assign->GetRHS());
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
			n = new CAstUnaryOp(_token, opNot, factor(s));
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
  EOperation op;
	CToken t;
	t = _scanner->Peek();
	if(t.GetValue() == "+" || t.GetValue() == "-"){
		Consume(tTermOp, &t);    
    if (t.GetValue() == "+") op = opAdd;
    else op = opSub;
    n = new CAstUnaryOp(t, op, term(s));
	} else {
    n = term(s);
  }

  while (_scanner->Peek().GetType() == tTermOp){    
    CAstExpression *l = n, *r;

    Consume(tTermOp, &t);
    r = term(s);
		
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
  EOperation op;
  CAstExpression *left = NULL, *right = NULL;

  left = simpleexpr(s);

  if (_scanner->Peek().GetType() == tRelOp) {
    Consume(tRelOp, &t);
    right = simpleexpr(s);

    if (t.GetValue() == "=")       op = opEqual;
    else if (t.GetValue() == "#")  op = opNotEqual;
		else if (t.GetValue() == "<")  op = opLessThan;
		else if (t.GetValue() == "<=") op = opLessEqual;
		else if (t.GetValue() == ">")  op = opBiggerThan;
		else if (t.GetValue() == ">=") op = opBiggerEqual;
    else SetError(t, "invalid relation.");

    return new CAstBinaryOp(t, op, left, right);
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
	CAstExpression *arg = NULL;
	CAstFunctionCall *call = NULL;

	identifier = ident();
	call = new CAstFunctionCall(t, (CSymProc *)identifier->GetSymbol());

	Consume(tLBrak, &t);
	
	if(_scanner->Peek().GetType() != tRBrak){
		arg = expression(s);
    call->AddArg(arg);
		while(_scanner->Peek().GetType() == tComma){
			Consume(tComma, &t);
			arg = expression(s);
      call->AddArg(arg);
		}
	} else {
		Consume(tRBrak, &t);
	}
	
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
  CSymtab *symtab = _module->GetSymbolTable();
	CAstStatement *statement = NULL;
		
	switch (_scanner->Peek().GetType()) {
		case tIdent:
      t = _scanner->Peek();
      if (symtab->FindSymbol(t.GetValue())->GetSymbolType() == stProcedure) {
        statement = subroutineCall(s);
      } else {
        statement = assignment(s);
      }
			break;
		case tIf:
      statement = ifStatement(s);
			break;
		case tWhile:
      statement = whileStatement(s);
			break;
		case tReturn:
      statement = returnStatement(s);
			break;
    default:
      SetError(t, "statement expected");
      break;
	}
  return statement;
}

CAstStatement* CParser::statSequence(CAstScope *s)
{
  //
  // statSequence = [ statement { ";" statement } ].
  // FOLLOW(statSequence) = { tElse, tEnd }
  //
  CAstStatement *head = NULL;


  EToken tt = _scanner->Peek().GetType();
  if (tt != tEnd && tt != tElse) {
    head = statement(s);
    CAstStatement *tail = head;
    while(_scanner->Peek().GetType == tSemicolon){
      Consume(tSemicolon, &t);
      CAstStatement *st = statement(s);
      tail->SetNext(st);
      tail = st;
    }

/*
    do {
      CAstStatement *st = statement(s);

      assert(st != NULL);
      if (head == NULL) head = st;
      else tail->SetNext(st);
      tail = st;

      tt = _scanner->Peek().GetType();
      if (tt == tEnd) break;
      if (tt == tElse) break;

      Consume(tSemicolon);
    } while (!_abort);
*/
  }

  return head;
}

void CParser::varDeclaration(CAstScope *s)
{
  //
  // varDeclaration = [ "var" { ident { "," ident } ":" type ";" } ].
  //
  
  CToken t;
  CAstType *type = NULL;
  std::vector<string> v;
  
  if (_scanner->Peek().GetType() == tVar) {
    Consume(tVar, &t);
    while(_scanner->Peek().GetType() == tIdent){
      Consume(tIdent, &t);
      v.push_back(t.GetValue());
      while(_scanner->Peek().GetType() == tComma){
        Consume(tComma, &t);
        Consume(tIdent, &t);
        v.push_back(t.GetValue());
      }
      Consume(tColon, &t);
      type = type();
      Consume(tSemicolon, &t);
      int size = v.size();
      for(int i=0; i<size; i++){
        s->CreateVar(v[i], type.GetType());
      }
      v.clear();
    }
  }
}

CAstProcedure* subroutineDecl(CAstScope *s)
{
  CToken t;
  EToken tt;
  string name = NULL;

  CAstProcedure *procedure = NULL;
  CAstStatement *statSequence = NULL;
  CAstType *type = NULL;

  CSymProc *symProc = NULL;
  CSymParam *symParam = NULL;
  CSymTab *symTab = s->GetSymbolTable();
  
  std::vector<CSymParam *> v;  
  

  tt = _scanner->Peek().GetType();
  if(tt == tProcedure) Consume(tProcedure, &t);
  else Consume(tFunction, &t);
  Consume(tIdent, &t);
  name = t.GetValue();

  // formalParam
  if(_scanner->Peek().GetType() == tLBrak) {
    Consume(tLBrak, &t);  
    if(_scanner->Peek().GetType() == tIdent){
      Consume(tIdent, &t);
      symParam = new CSymParam(v.size(), t.GetValue(), CTypeManager::Get()->GetInt());
      v.push_back(symParam);
      while (_scanner->Peek().GetType() == tComma){
        Consume(tComma, &t);
        Consume(tIdent, &t);
        symParam = new CSymParam(v.size(), t.GetValue(), CTypeManager::Get()->GetInt());
        v.push_back(symParam);
      }
    }
    Consume(tRBrak, &t);
  }

  if(tt == tFunction) {
    Consume(tColon, &t);
    CAstType* type = type();
  }
  Consume(tSemicolon, &t);
  symProc = new CSymProc(name, type.GetType());
  int size = v.size();
  for(int i=0; i<size; i++){
    symProc->AddParam(v[i]);
  }
  procedure = new CAstProcedure(t, name, s, symProc);
  symTab->AddSymbol(symProc);

  // subroutineBody
  varDeclaration(procedure);
  Consume(tBegin, &t);
  statSequence = statSequence(s);
  procedure->SetStatementSequence(statSequence);
  Consume(tEnd, &t);

  return procedure;
}