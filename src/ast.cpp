//------------------------------------------------------------------------------
/// @brief SnuPL abstract syntax tree
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

#include <iostream>
#include <cassert>

#include <typeinfo>

#include "ast.h"
using namespace std;


//------------------------------------------------------------------------------
// CAstNode
//
int CAstNode::_global_id = 0;

CAstNode::CAstNode(CToken token)
  : _token(token), _addr(NULL)
{
  _id = _global_id++;
}

CAstNode::~CAstNode(void)
{
  if (_addr != NULL) delete _addr;
}

int CAstNode::GetID(void) const
{
  return _id;
}

CToken CAstNode::GetToken(void) const
{
  return _token;
}

const CType* CAstNode::GetType(void) const
{
  return CTypeManager::Get()->GetNull();
}

string CAstNode::dotID(void) const
{
  ostringstream out;
  out << "node" << dec << _id;
  return out.str();
}

string CAstNode::dotAttr(void) const
{
  return " [label=\"" + dotID() + "\"]";
}

void CAstNode::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << dotID() << dotAttr() << ";" << endl;
}

CTacAddr* CAstNode::ToTac(CCodeBlock *cb)
{
  return NULL;
}

CTacAddr* CAstNode::GetTacAddr(void) const
{
  return NULL;
}

ostream& operator<<(ostream &out, const CAstNode &t)
{
  return t.print(out);
}

ostream& operator<<(ostream &out, const CAstNode *t)
{
  return t->print(out);
}

//------------------------------------------------------------------------------
// CAstScope
//
CAstScope::CAstScope(CToken t, const string name, CAstScope *parent)
  : CAstNode(t), _name(name), _symtab(NULL), _parent(parent), _statseq(NULL),
    _cb(NULL)
{
  if (_parent != NULL) _parent->AddChild(this);
}

CAstScope::~CAstScope(void)
{
  delete _symtab;
  delete _statseq;
  delete _cb;
}

const string CAstScope::GetName(void) const
{
  return _name;
}

CAstScope* CAstScope::GetParent(void) const
{
  return _parent;
}

size_t CAstScope::GetNumChildren(void) const
{
  return _children.size();
}

CAstScope* CAstScope::GetChild(size_t i) const
{
  assert(i < _children.size());
  return _children[i];
}

CSymtab* CAstScope::GetSymbolTable(void) const
{
  assert(_symtab != NULL);
  return _symtab;
}

void CAstScope::SetStatementSequence(CAstStatement *statseq)
{
  _statseq = statseq;
}

CAstStatement* CAstScope::GetStatementSequence(void) const
{
  return _statseq;
}

bool CAstScope::TypeCheck(CToken *t, string *msg) const
{
	bool result = true;

	try {
		CAstStatement *s = _statseq;
		while (result && (s != NULL)) {
			result = s->TypeCheck(t, msg);
			s = s->GetNext();
		}

		vector<CAstScope *>::const_iterator it = _children.begin();
		while (result && (it != _children.end())) {
			result = (*it)->TypeCheck(t, msg);
			it++;
		}
	} catch (...) {
		result = false;
	}

  return result;
}

ostream& CAstScope::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "CAstScope: '" << _name << "'" << endl;
  out << ind << "  symbol table:" << endl;
  _symtab->print(out, indent+4);
  out << ind << "  statement list:" << endl;
  CAstStatement *s = GetStatementSequence();
  if (s != NULL) {
    do {
      s->print(out, indent+4);
      s = s->GetNext();
    } while (s != NULL);
  } else {
    out << ind << "    empty." << endl;
  }

  out << ind << "  nested scopes:" << endl;
  if (_children.size() > 0) {
    for (size_t i=0; i<_children.size(); i++) {
      _children[i]->print(out, indent+4);
    }
  } else {
    out << ind << "    empty." << endl;
  }
  out << ind << endl;

  return out;
}

void CAstScope::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  CAstStatement *s = GetStatementSequence();
  if (s != NULL) {
    string prev = dotID();
    do {
      s->toDot(out, indent);
      out << ind << prev << " -> " << s->dotID() << " [style=dotted];" << endl;
      prev = s->dotID();
      s = s->GetNext();
    } while (s != NULL);
  }

  vector<CAstScope*>::const_iterator it = _children.begin();
  while (it != _children.end()) {
    CAstScope *s = *it++;
    s->toDot(out, indent);
    out << ind << dotID() << " -> " << s->dotID() << ";" << endl;
  }

}

CTacAddr* CAstScope::ToTac(CCodeBlock *cb)
{
	assert(cb != NULL);

	CAstStatement *s = GetStatementSequence();
	while (s != NULL) {
		CTacLabel *next = cb->CreateLabel();
		s->ToTac(cb, next);
		cb->AddInstr(next);
		s = s->GetNext();
	}
	cb->CleanupControlFlow();

  return NULL;
}

CCodeBlock* CAstScope::GetCodeBlock(void) const
{
  return _cb;
}

void CAstScope::SetSymbolTable(CSymtab *st)
{
  if (_symtab != NULL) delete _symtab;
  _symtab = st;
}

void CAstScope::AddChild(CAstScope *child)
{
  _children.push_back(child);
}


//------------------------------------------------------------------------------
// CAstModule
//
CAstModule::CAstModule(CToken t, const string name)
  : CAstScope(t, name, NULL)
{
  SetSymbolTable(new CSymtab());
}

CSymbol* CAstModule::CreateVar(const string ident, const CType *type)
{
  return new CSymGlobal(ident, type);
}

string CAstModule::dotAttr(void) const
{
  return " [label=\"m " + GetName() + "\",shape=box]";
}



//------------------------------------------------------------------------------
// CAstProcedure
//
CAstProcedure::CAstProcedure(CToken t, const string name,
                             CAstScope *parent, CSymProc *symbol)
  : CAstScope(t, name, parent), _symbol(symbol)
{
  assert(GetParent() != NULL);
  SetSymbolTable(new CSymtab(GetParent()->GetSymbolTable()));
  assert(_symbol != NULL);
}

CSymProc* CAstProcedure::GetSymbol(void) const
{
  return _symbol;
}

CSymbol* CAstProcedure::CreateVar(const string ident, const CType *type)
{
  return new CSymLocal(ident, type);
}

const CType* CAstProcedure::GetType(void) const
{
  return GetSymbol()->GetDataType();
}

string CAstProcedure::dotAttr(void) const
{
  return " [label=\"p/f " + GetName() + "\",shape=box]";
}


//------------------------------------------------------------------------------
// CAstType
//
CAstType::CAstType(CToken t, const CType *type)
  : CAstNode(t), _type(type)
{
  assert(type != NULL);
}

const CType* CAstType::GetType(void) const
{
  return _type;
}

ostream& CAstType::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "CAstType (" << _type << ")" << endl;
  return out;
}


//------------------------------------------------------------------------------
// CAstStatement
//
CAstStatement::CAstStatement(CToken token)
  : CAstNode(token), _next(NULL)
{
}

CAstStatement::~CAstStatement(void)
{
  delete _next;
}

void CAstStatement::SetNext(CAstStatement *next)
{
  _next = next;
}

CAstStatement* CAstStatement::GetNext(void) const
{
  return _next;
}

CTacAddr* CAstStatement::ToTac(CCodeBlock *cb, CTacLabel *next)
{
	// generate code for the statement
	cb->AddInstr(new CTacInstr(opGoto, next));

  return NULL;
}


//------------------------------------------------------------------------------
// CAstStatAssign
//
CAstStatAssign::CAstStatAssign(CToken t,
                               CAstDesignator *lhs, CAstExpression *rhs)
  : CAstStatement(t), _lhs(lhs), _rhs(rhs)
{
  assert(lhs != NULL);
  assert(rhs != NULL);
}

CAstDesignator* CAstStatAssign::GetLHS(void) const
{
  return _lhs;
}

CAstExpression* CAstStatAssign::GetRHS(void) const
{
  return _rhs;
}

bool CAstStatAssign::TypeCheck(CToken *t, string *msg) const
{
	ostringstream ostr;
	if (!_lhs->TypeCheck(t, msg)) return false;
	if (!_rhs->TypeCheck(t, msg)) return false;
	if (!_lhs->GetType()->Match(_rhs->GetType())) {
		if (t != NULL) *t = GetToken();
		if (msg != NULL) {
			ostr << "type mismatch between lhs(" << _lhs->GetType() << ") and rhs(" << _rhs->GetType() << ").";
			*msg = ostr.str();
		}
		return false;
	}
  return true;
}

const CType* CAstStatAssign::GetType(void) const
{
  return _lhs->GetType();
}

ostream& CAstStatAssign::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << ":=" << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  _lhs->print(out, indent+2);
  _rhs->print(out, indent+2);

  return out;
}

string CAstStatAssign::dotAttr(void) const
{
  return " [label=\":=\",shape=box]";
}

void CAstStatAssign::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _lhs->toDot(out, indent);
  out << ind << dotID() << "->" << _lhs->dotID() << ";" << endl;
  _rhs->toDot(out, indent);
  out << ind << dotID() << "->" << _rhs->dotID() << ";" << endl;
}

CTacAddr* CAstStatAssign::ToTac(CCodeBlock *cb, CTacLabel *next)
{
	cb->AddInstr(new CTacInstr(opAssign, _lhs->ToTac(cb, NULL, NULL), _rhs->ToTac(cb, NULL, NULL)));
	cb->AddInstr(new CTacInstr(opGoto, next));
  return NULL;
}


//------------------------------------------------------------------------------
// CAstStatCall
//
CAstStatCall::CAstStatCall(CToken t, CAstFunctionCall *call)
  : CAstStatement(t), _call(call)
{
  assert(call != NULL);
}

CAstFunctionCall* CAstStatCall::GetCall(void) const
{
  return _call;
}

bool CAstStatCall::TypeCheck(CToken *t, string *msg) const
{
  return GetCall()->TypeCheck(t, msg);
}

ostream& CAstStatCall::print(ostream &out, int indent) const
{
  _call->print(out, indent);

  return out;
}

string CAstStatCall::dotID(void) const
{
  return _call->dotID();
}

string CAstStatCall::dotAttr(void) const
{
  return _call->dotAttr();
}

void CAstStatCall::toDot(ostream &out, int indent) const
{
  _call->toDot(out, indent);
}

CTacAddr* CAstStatCall::ToTac(CCodeBlock *cb, CTacLabel *next)
{
	_call->ToTac(cb, NULL, NULL);
	cb->AddInstr(new CTacInstr(opGoto, next));
  return NULL;
}


//------------------------------------------------------------------------------
// CAstStatReturn
//
CAstStatReturn::CAstStatReturn(CToken t, CAstScope *scope, CAstExpression *expr)
  : CAstStatement(t), _scope(scope), _expr(expr)
{
  assert(scope != NULL);
}

CAstScope* CAstStatReturn::GetScope(void) const
{
  return _scope;
}

CAstExpression* CAstStatReturn::GetExpression(void) const
{
  return _expr;
}

bool CAstStatReturn::TypeCheck(CToken *t, string *msg) const
{
	const CType *st = GetScope()->GetType();
	CAstExpression *e = GetExpression();

	if (st->Match(CTypeManager::Get()->GetNull())) {
		if (e != NULL) {
			if (t != NULL) *t = e->GetToken();
			if (msg != NULL) *msg = "superfluous expression after return.";
			return false;
		}
	} else {
		if (e == NULL) {
			if (t != NULL) *t = GetToken();
			if (msg != NULL) *msg = "expression expected after return.";
			return false;
		}

		if (!e->TypeCheck(t, msg)) return false;

		if (!st->Match(e->GetType())) {
			if (t != NULL) *t = e->GetToken();
			if (msg != NULL) *msg = "return type mismatch.";
			return false;
		}
	}
  return true;
}

const CType* CAstStatReturn::GetType(void) const
{
  const CType *t = NULL;

  if (GetExpression() != NULL) {
    t = GetExpression()->GetType();
  } else {
    t = CTypeManager::Get()->GetNull();
  }

  return t;
}

ostream& CAstStatReturn::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "return" << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  if (_expr != NULL) _expr->print(out, indent+2);

  return out;
}

string CAstStatReturn::dotAttr(void) const
{
  return " [label=\"return\",shape=box]";
}

void CAstStatReturn::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  if (_expr != NULL) {
    _expr->toDot(out, indent);
    out << ind << dotID() << "->" << _expr->dotID() << ";" << endl;
  }
}

CTacAddr* CAstStatReturn::ToTac(CCodeBlock *cb, CTacLabel *next)
{
	if (_expr != NULL) cb->AddInstr(new CTacInstr(opReturn, NULL, _expr->ToTac(cb, NULL, NULL), NULL));
	else cb->AddInstr(new CTacInstr(opReturn, NULL, NULL, NULL));
	cb->AddInstr(new CTacInstr(opGoto, next));
  return NULL;
}


//------------------------------------------------------------------------------
// CAstStatIf
//
CAstStatIf::CAstStatIf(CToken t, CAstExpression *cond,
                       CAstStatement *ifBody, CAstStatement *elseBody)
  : CAstStatement(t), _cond(cond), _ifBody(ifBody), _elseBody(elseBody)
{
  assert(cond != NULL);
}

CAstExpression* CAstStatIf::GetCondition(void) const
{
  return _cond;
}

CAstStatement* CAstStatIf::GetIfBody(void) const
{
  return _ifBody;
}

CAstStatement* CAstStatIf::GetElseBody(void) const
{
  return _elseBody;
}

bool CAstStatIf::TypeCheck(CToken *t, string *msg) const
{
	bool result = true;

	if (!GetCondition()->TypeCheck(t, msg)) return false;
	const CType *st = GetCondition()->GetType();
	if (!st->IsBoolean()) {
		if (t != NULL) *t = GetCondition()->GetToken();
		if (msg != NULL) *msg = "condition should be boolean.";
		return false;
	}
	
	try {
		CAstStatement *s = GetIfBody();
		while (result && (s != NULL)) {
			result = s->TypeCheck(t, msg);
			s = s->GetNext();
		}

		s = GetElseBody();
		while (result && (s != NULL)) {
			result = s->TypeCheck(t, msg);
			s = s->GetNext();
		}
	} catch (...) {
		result = false;
	}
	return result;
}

ostream& CAstStatIf::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "if cond" << endl;
  _cond->print(out, indent+2);
  out << ind << "if-body" << endl;
  if (_ifBody != NULL) {
    CAstStatement *s = _ifBody;
    do {
      s->print(out, indent+2);
      s = s->GetNext();
    } while (s != NULL);
  } else out << ind << "  empty." << endl;
  out << ind << "else-body" << endl;
  if (_elseBody != NULL) {
    CAstStatement *s = _elseBody;
    do {
      s->print(out, indent+2);
      s = s->GetNext();
    } while (s != NULL);
  } else out << ind << "  empty." << endl;

  return out;
}

string CAstStatIf::dotAttr(void) const
{
  return " [label=\"if\",shape=box]";
}

void CAstStatIf::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _cond->toDot(out, indent);
  out << ind << dotID() << "->" << _cond->dotID() << ";" << endl;

  if (_ifBody != NULL) {
    CAstStatement *s = _ifBody;
    if (s != NULL) {
      string prev = dotID();
      do {
        s->toDot(out, indent);
        out << ind << prev << " -> " << s->dotID() << " [style=dotted];"
            << endl;
        prev = s->dotID();
        s = s->GetNext();
      } while (s != NULL);
    }
  }

  if (_elseBody != NULL) {
    CAstStatement *s = _elseBody;
    if (s != NULL) {
      string prev = dotID();
      do {
        s->toDot(out, indent);
        out << ind << prev << " -> " << s->dotID() << " [style=dotted];" 
            << endl;
        prev = s->dotID();
        s = s->GetNext();
      } while (s != NULL);
    }
  }
}

CTacAddr* CAstStatIf::ToTac(CCodeBlock *cb, CTacLabel *next)
{
	CTacLabel *lt = cb->CreateLabel("if_true");
	CTacLabel *lf = cb->CreateLabel("if_false");
	CTacAddr *condResult = NULL;

	condResult = _cond->ToTac(cb, lt, lf);
	if (condResult != NULL) {
		cb->AddInstr(new CTacInstr(opEqual, lt, condResult, new CTacConst(1)));
		cb->AddInstr(new CTacInstr(opGoto, lf));
	}
	
	cb->AddInstr(lt);
	CAstStatement *s = _ifBody;
	while (s != NULL) {
		CTacLabel *ll = cb->CreateLabel();
		s->ToTac(cb, ll);
		cb->AddInstr(ll);
		s = s->GetNext();
	}
	cb->AddInstr(new CTacInstr(opGoto, next));
	cb->AddInstr(lf);
	s = _elseBody;
	while (s != NULL) {
		CTacLabel *ll = cb->CreateLabel();
		s->ToTac(cb, ll);
		cb->AddInstr(ll);
		s = s->GetNext();
	}
	cb->AddInstr(new CTacInstr(opGoto, next));

  return NULL;
}


//------------------------------------------------------------------------------
// CAstStatWhile
//
CAstStatWhile::CAstStatWhile(CToken t,
                             CAstExpression *cond, CAstStatement *body)
  : CAstStatement(t), _cond(cond), _body(body)
{
  assert(cond != NULL);
}

CAstExpression* CAstStatWhile::GetCondition(void) const
{
  return _cond;
}

CAstStatement* CAstStatWhile::GetBody(void) const
{
  return _body;
}

bool CAstStatWhile::TypeCheck(CToken *t, string *msg) const
{
	bool result = true;
	if (!GetCondition()->TypeCheck(t, msg)) return false;
	const CType *st = GetCondition()->GetType();
	if (!st->IsBoolean()) {
		if (t != NULL) *t = GetCondition()->GetToken();
		if (msg != NULL) *msg = "condition should be boolean.";
		return false;
	}

	CAstStatement *s = GetBody();
	while (result && (s != NULL)) {
		result = s->TypeCheck(t, msg);
		s = s->GetNext();
	}
	return result;
}

ostream& CAstStatWhile::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "while cond" << endl;
  _cond->print(out, indent+2);
  out << ind << "while-body" << endl;
  if (_body != NULL) {
    CAstStatement *s = _body;
    do {
      s->print(out, indent+2);
      s = s->GetNext();
    } while (s != NULL);
  }
  else out << ind << "  empty." << endl;

  return out;
}

string CAstStatWhile::dotAttr(void) const
{
  return " [label=\"while\",shape=box]";
}

void CAstStatWhile::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _cond->toDot(out, indent);
  out << ind << dotID() << "->" << _cond->dotID() << ";" << endl;

  if (_body != NULL) {
    CAstStatement *s = _body;
    if (s != NULL) {
      string prev = dotID();
      do {
        s->toDot(out, indent);
        out << ind << prev << " -> " << s->dotID() << " [style=dotted];"
            << endl;
        prev = s->dotID();
        s = s->GetNext();
      } while (s != NULL);
    }
  }
}

CTacAddr* CAstStatWhile::ToTac(CCodeBlock *cb, CTacLabel *next)
{
	CTacLabel *l1 = cb->CreateLabel("while_cond");
	CTacLabel *l2 = cb->CreateLabel("while_body");
	CTacAddr *condResult = NULL;

	cb->AddInstr(l1);
	condResult = _cond->ToTac(cb, l2, next);
	if (condResult != NULL) {
		cb->AddInstr(new CTacInstr(opEqual, l2, condResult, new CTacConst(1)));
		cb->AddInstr(new CTacInstr(opGoto, next));
	}
	cb->AddInstr(l2);
	CAstStatement *s = _body;
	while (s != NULL) {
		CTacLabel *ll = cb->CreateLabel();
		s->ToTac(cb, ll);
		cb->AddInstr(ll);
		s = s->GetNext();
	}
	cb->AddInstr(new CTacInstr(opGoto, l1));
  return NULL;
}


//------------------------------------------------------------------------------
// CAstExpression
//
CAstExpression::CAstExpression(CToken t)
  : CAstNode(t)
{
}

CTacAddr * CAstExpression::ToTac(CCodeBlock *cb, CTacLabel *ltrue, CTacLabel *lfalse)
{
	return NULL;
}

//------------------------------------------------------------------------------
// CAstOperation
//
CAstOperation::CAstOperation(CToken t, EOperation oper)
  : CAstExpression(t), _oper(oper)
{
}

EOperation CAstOperation::GetOperation(void) const
{
  return _oper;
}


//------------------------------------------------------------------------------
// CAstBinaryOp
//
CAstBinaryOp::CAstBinaryOp(CToken t, EOperation oper,
                           CAstExpression *l,CAstExpression *r)
  : CAstOperation(t, oper), _left(l), _right(r)
{
  // these are the only binary operation we support for now
  assert((oper == opAdd)        || (oper == opSub)         ||
         (oper == opMul)        || (oper == opDiv)         ||
         (oper == opAnd)        || (oper == opOr)          ||
         (oper == opEqual)      || (oper == opNotEqual)    ||
         (oper == opLessThan)   || (oper == opLessEqual)   ||
         (oper == opBiggerThan) || (oper == opBiggerEqual)
        );
  assert(l != NULL);
  assert(r != NULL);
}

CAstExpression* CAstBinaryOp::GetLeft(void) const
{
  return _left;
}

CAstExpression* CAstBinaryOp::GetRight(void) const
{
  return _right;
}

bool CAstBinaryOp::TypeCheck(CToken *t, string *msg) const
{
	ostringstream ostr;
	if (!_left->TypeCheck(t, msg)) return false;
	if (!_right->TypeCheck(t, msg)) return false;

	const CType *lt, *rt;
	lt = _left->GetType();
	rt = _right->GetType();

	switch (GetOperation()) {
		case opAdd:
		case opSub:
		case opMul:
		case opDiv:
		case opLessThan:
		case opLessEqual:
		case opBiggerThan:
		case opBiggerEqual:
			if (!lt->Match(CTypeManager::Get()->GetInt())){
				if (t != NULL) *t = _left->GetToken();
				if (msg != NULL) {
					ostr << "expected " << CTypeManager::Get()->GetInt() << ", but " << lt;
				 	*msg = ostr.str();
				}
				return false;
			}	
			if (!rt->Match(CTypeManager::Get()->GetInt())){
				if (t != NULL) *t = _right->GetToken();
				if (msg != NULL) {
					ostr << "expected " << CTypeManager::Get()->GetInt() << ", but " << rt;
				 	*msg = ostr.str();
				}
				return false;
			}
			break;
		case opEqual:
		case opNotEqual:
			if (!lt->IsScalar()) {
				if (t != NULL) *t = _left->GetToken();
				if (msg != NULL) {
					ostr << "expected " << CTypeManager::Get()->GetInt() << " or " << CTypeManager::Get()->GetBool() << ", but " << lt;
				 	*msg = ostr.str();
				}
			}
			if (!lt->Match(rt)) {
				if (t != NULL) *t = _right->GetToken();
				if (msg != NULL) {
					ostr << "expected " << lt << ", but " << rt;
				 	*msg = ostr.str();
				}
				return false;
			}
			break;
		case opAnd:
		case opOr:
			if (!lt->IsBoolean()){
				if (t != NULL) *t = _left->GetToken();
				if (msg != NULL) {
					ostr << "expected " << CTypeManager::Get()->GetBool() <<  ", but " << lt;
				 	*msg = ostr.str();
				}
				return false;
			}
			if (!rt->IsBoolean()){
				if (t != NULL) *t = _right->GetToken();
				if (msg != NULL) {
					ostr << "expected " << CTypeManager::Get()->GetBool() << ", but " << rt;
				 	*msg = ostr.str();
				}
				return false;
			}
			break;
		default:
			break;
	}
	return true;
}

const CType* CAstBinaryOp::GetType(void) const
{
  const CType *t = NULL, *lt, *rt;

  lt = _left->GetType();
  rt = _right->GetType();
  assert((lt != NULL) && (rt != NULL));

  switch (GetOperation()) {
    // arithmetic operator
    case opAdd:
    case opSub:
    case opMul:
    case opDiv:
      if (lt->IsScalar() && !lt->IsBoolean() && lt->Match(rt)) t = lt;
      break;

    // relational operator
    case opEqual:
    case opNotEqual:
			if (lt->IsScalar() && lt->Match(rt)) t = CTypeManager::Get()->GetBool();
			break;

		case opAnd:
    case opOr:
      if (lt->IsBoolean() && rt->IsBoolean()) t = lt;
      break;
      // else fallthrough

    case opLessThan:
    case opLessEqual:
    case opBiggerThan:
    case opBiggerEqual:
      if (lt->IsScalar() && !lt->IsBoolean() && lt->Match(rt)) {
        t = CTypeManager::Get()->GetBool();
      }
      break;

    default:
      break;
  }

  return t;
}

ostream& CAstBinaryOp::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << GetOperation() << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  _left->print(out, indent+2);
  _right->print(out, indent+2);

  return out;
}

string CAstBinaryOp::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << GetOperation() << "\",shape=box]";
  return out.str();
}

void CAstBinaryOp::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _left->toDot(out, indent);
  out << ind << dotID() << "->" << _left->dotID() << ";" << endl;
  _right->toDot(out, indent);
  out << ind << dotID() << "->" << _right->dotID() << ";" << endl;
}

CTacAddr* CAstBinaryOp::ToTac(CCodeBlock *cb, CTacLabel *ltrue, CTacLabel *lfalse)
{
	CTacTemp *tmpValue = NULL;
	CTacLabel *tmpTrue = NULL;
	CTacLabel *tmpFalse = NULL;
	CTacLabel *tmpTest = NULL;
	CTacLabel *tmpEnd = NULL;

	CTacAddr *resultLeft = NULL;
	CTacAddr *resultRight = NULL;

	switch (GetOperation()) {
		case opAdd:
		case opSub:
		case opMul:
		case opDiv:
			resultLeft = _left->ToTac(cb, NULL, NULL);
			resultRight = _right->ToTac(cb, NULL, NULL);
			tmpValue = cb->CreateTemp(CTypeManager::Get()->GetInt());
			cb->AddInstr(new CTacInstr(GetOperation(), tmpValue, resultLeft, resultRight));
			break;

		case opAnd:
			tmpTrue = ltrue;
			tmpFalse = lfalse;

			if (ltrue == NULL && lfalse == NULL) {
				tmpTrue = cb->CreateLabel();
				tmpFalse = cb->CreateLabel();
				tmpEnd = cb->CreateLabel();
			} 
			tmpTest = cb->CreateLabel();
			
			resultLeft = _left->ToTac(cb, tmpTest, tmpFalse);
			if (resultLeft != NULL) {
				cb->AddInstr(new CTacInstr(opEqual, tmpTest, resultLeft, new CTacConst(1)));
				cb->AddInstr(new CTacInstr(opGoto, tmpFalse));
			}
			cb->AddInstr(tmpTest);
			resultRight = _right->ToTac(cb, tmpTrue, tmpFalse);
			if (resultRight != NULL) {
				cb->AddInstr(new CTacInstr(opEqual, tmpTrue, resultRight, new CTacConst(1)));
				cb->AddInstr(new CTacInstr(opGoto, tmpFalse));
			}
			
			if (ltrue == NULL && lfalse == NULL) {
				tmpValue = cb->CreateTemp(CTypeManager::Get()->GetBool());

				cb->AddInstr(tmpTrue);
				cb->AddInstr(new CTacInstr(opAssign, tmpValue, new CTacConst(1)));
				cb->AddInstr(new CTacInstr(opGoto, tmpEnd));

				cb->AddInstr(tmpFalse);
				cb->AddInstr(new CTacInstr(opAssign, tmpValue, new CTacConst(0)));

				cb->AddInstr(tmpEnd);
			}
			break;

		case opOr:
			tmpTrue = ltrue;
			tmpFalse = lfalse;
			if (ltrue == NULL && lfalse == NULL) {
				tmpTrue = cb->CreateLabel();
				tmpFalse = cb->CreateLabel();
				tmpEnd = cb->CreateLabel();
			}
			tmpTest = cb->CreateLabel();

			resultLeft = _left->ToTac(cb, tmpTrue, tmpTest);
			if (resultLeft != NULL) {
				cb->AddInstr(new CTacInstr(opEqual, tmpTrue, resultLeft, new CTacConst(1)));
				cb->AddInstr(new CTacInstr(opGoto, tmpTest));
			}
			cb->AddInstr(tmpTest);
			resultRight = _right->ToTac(cb, tmpTrue, tmpFalse);
			if (resultRight != NULL) {
				cb->AddInstr(new CTacInstr(opEqual, tmpTrue, resultRight, new CTacConst(1)));
				cb->AddInstr(new CTacInstr(opGoto, tmpFalse));
			}

			if (ltrue == NULL && lfalse == NULL) {
				tmpValue = cb->CreateTemp(CTypeManager::Get()->GetBool());

				cb->AddInstr(tmpTrue);
				cb->AddInstr(new CTacInstr(opAssign, tmpValue, new CTacConst(1)));
				cb->AddInstr(new CTacInstr(opGoto, tmpEnd));

				cb->AddInstr(tmpFalse);
				cb->AddInstr(new CTacInstr(opAssign, tmpValue, new CTacConst(0)));

				cb->AddInstr(tmpEnd);
			}
			break;

		case opEqual:
		case opNotEqual:
		case opLessThan:
		case opLessEqual:
		case opBiggerThan:
		case opBiggerEqual:
			tmpTrue = ltrue;
			tmpFalse = lfalse;
			
			if (ltrue == NULL && lfalse == NULL) {
				tmpTrue = cb->CreateLabel();
				tmpFalse = cb->CreateLabel();
				tmpEnd = cb->CreateLabel();
			}

			resultLeft = _left->ToTac(cb, NULL, NULL);
			resultRight = _right->ToTac(cb, NULL, NULL);

			cb->AddInstr(new CTacInstr(GetOperation(), tmpTrue, resultLeft, resultRight));
			cb->AddInstr(new CTacInstr(opGoto, tmpFalse));
			if (ltrue == NULL && lfalse == NULL) {
				tmpValue = cb->CreateTemp(CTypeManager::Get()->GetBool());

				cb->AddInstr(tmpTrue);
				cb->AddInstr(new CTacInstr(opAssign, tmpValue, new CTacConst(1)));
				cb->AddInstr(new CTacInstr(opGoto, tmpEnd));
				
				cb->AddInstr(tmpFalse);
				cb->AddInstr(new CTacInstr(opAssign, tmpValue, new CTacConst(0)));

				cb->AddInstr(tmpEnd);
			}
			break;
	}
  return tmpValue;
}


//------------------------------------------------------------------------------
// CAstUnaryOp
//
CAstUnaryOp::CAstUnaryOp(CToken t, EOperation oper, CAstExpression *e)
  : CAstOperation(t, oper), _operand(e)
{
  assert((oper == opNeg) || (oper == opNot));
  assert(e != NULL);
}

CAstExpression* CAstUnaryOp::GetOperand(void) const
{
  return _operand;
}

bool CAstUnaryOp::TypeCheck(CToken *t, string *msg) const
{
	ostringstream ostr;
	if (!_operand->TypeCheck(t, msg)) return false;
	const CType *st = _operand->GetType();
	
	switch (GetOperation()) {
		case opNeg:
			if (!st->Match(CTypeManager::Get()->GetInt())) {
				if (t != NULL) *t = _operand->GetToken();
				if (msg != NULL){
					ostr <<  " expected " << CTypeManager::Get()->GetInt() << ", but " << st;
				 	*msg = ostr.str();
				}
				return false;
			}
			break;
		case opNot:
			if (!st->IsBoolean()) {
				if (t != NULL) *t = _operand->GetToken();
				if (msg != NULL){
					ostr << " expected " << CTypeManager::Get()->GetBool() << ", but " << st;
				 	*msg = ostr.str();
				}
				return false;
			}
			break;
		default:
			break;
	}
	return true;
}

const CType* CAstUnaryOp::GetType(void) const
{
  const CType *t = NULL;

  switch (GetOperation()) {
    case opNeg:
    case opNot:
      t = _operand->GetType();
      break;

    default:
      break;
  }

  return t;
}

ostream& CAstUnaryOp::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << GetOperation() << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";
  out << endl;

  _operand->print(out, indent+2);

  return out;
}

string CAstUnaryOp::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << GetOperation() << "\",shape=box]";
  return out.str();
}

void CAstUnaryOp::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  _operand->toDot(out, indent);
  out << ind << dotID() << "->" << _operand->dotID() << ";" << endl;
}

CTacAddr* CAstUnaryOp::ToTac(CCodeBlock *cb, CTacLabel *ltrue, CTacLabel *lfalse)
{
	CTacTemp *tmpValue = NULL;
	CTacLabel *tmpTrue = NULL;
	CTacLabel *tmpFalse = NULL;
	CTacLabel *tmpEnd = NULL;

	CTacAddr *resultOperand = NULL;

	switch (GetOperation()) {
		case opNeg:
			resultOperand = _operand->ToTac(cb, NULL, NULL);
			tmpValue = cb->CreateTemp(CTypeManager::Get()->GetInt());
			cb->AddInstr(new CTacInstr(GetOperation(), tmpValue, resultOperand));
			break;
		case opNot:
			tmpTrue = ltrue;
			tmpFalse = lfalse;
			if (ltrue == NULL && lfalse == NULL) {
				tmpTrue = cb->CreateLabel();
				tmpFalse = cb->CreateLabel();
				tmpEnd = cb->CreateLabel();
			}

			resultOperand = _operand->ToTac(cb, tmpFalse, tmpTrue);
			if (resultOperand != NULL) {
				cb->AddInstr(new CTacInstr(opEqual, tmpFalse, resultOperand, new CTacConst(1)));
				cb->AddInstr(new CTacInstr(opGoto, tmpTrue));
			}

			if (ltrue == NULL && lfalse == NULL) {
				tmpValue = cb->CreateTemp(CTypeManager::Get()->GetBool());

				cb->AddInstr(tmpTrue);
				cb->AddInstr(new CTacInstr(opAssign, tmpValue, new CTacConst(1)));
				cb->AddInstr(new CTacInstr(opGoto, tmpEnd));
				
				cb->AddInstr(tmpFalse);
				cb->AddInstr(new CTacInstr(opAssign, tmpValue, new CTacConst(0)));
				
				cb->AddInstr(tmpEnd);
			}

			break;
	}
  return tmpValue;
}


//------------------------------------------------------------------------------
// CAstFunctionCall
//
CAstFunctionCall::CAstFunctionCall(CToken t, const CSymProc *symbol)
  : CAstExpression(t), _symbol(symbol)
{
  assert(symbol != NULL);
}

const CSymProc* CAstFunctionCall::GetSymbol(void) const
{
  return _symbol;
}

void CAstFunctionCall::AddArg(CAstExpression *arg)
{
  _arg.push_back(arg);
}

int CAstFunctionCall::GetNArgs(void) const
{
  return (int)_arg.size();
}

CAstExpression* CAstFunctionCall::GetArg(int index) const
{
  assert((index >= 0) && (index < _arg.size()));
  return _arg[index];
}

bool CAstFunctionCall::TypeCheck(CToken *t, string *msg) const
{
	bool result = true;
	ostringstream ostr;

	if (GetSymbol()->GetNParams() != GetNArgs()) {
		if (t != NULL) *t = GetToken();
		if (msg != NULL){
			ostr << "number of params not matched. " << GetSymbol()->GetNParams() << " expected but " << GetNArgs();
		 	*msg = ostr.str();
		}
		return false;
	}
	try {
		for (int i=0; i<GetNArgs(); i++){
			result = result && GetArg(i)->TypeCheck(t, msg);
			if(!result) return false;

			result = result && GetSymbol()->GetParam(i)->GetDataType()->Match(GetArg(i)->GetType());
			if(!result) {
				if (t != NULL) *t = GetArg(i)->GetToken();
				if (msg != NULL) *msg = "param type mismatch.";
				return false;
			}
		}
	} catch (...) {
		result = false;
	}
  return result;
}

const CType* CAstFunctionCall::GetType(void) const
{
  return GetSymbol()->GetDataType();
}

ostream& CAstFunctionCall::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "call " << _symbol << " ";
  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";
  out << endl;

  for (size_t i=0; i<_arg.size(); i++) {
    _arg[i]->print(out, indent+2);
  }

  return out;
}

string CAstFunctionCall::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"call " << _symbol->GetName() << "\",shape=box]";
  return out.str();
}

void CAstFunctionCall::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  for (size_t i=0; i<_arg.size(); i++) {
    _arg[i]->toDot(out, indent);
    out << ind << dotID() << "->" << _arg[i]->dotID() << ";" << endl;
  }
}

CTacAddr* CAstFunctionCall::ToTac(CCodeBlock *cb, CTacLabel *ltrue, CTacLabel *lfalse)
{
	CTacTemp *tmpValue = NULL;
	
	for (int i=GetNArgs()-1; i>=0; i--) {
		cb->AddInstr(new CTacInstr(opParam, new CTacConst(i), GetArg(i)->ToTac(cb, NULL, NULL)));
	}

	if (GetType()->IsScalar()) {
		tmpValue = cb->CreateTemp(GetType());
	}
	
	cb->AddInstr(new CTacInstr(opCall, tmpValue, new CTacName(GetSymbol())));
  return tmpValue;
}


//------------------------------------------------------------------------------
// CAstOperand
//
CAstOperand::CAstOperand(CToken t)
  : CAstExpression(t)
{
}


//------------------------------------------------------------------------------
// CAstDesignator
//
CAstDesignator::CAstDesignator(CToken t, const CSymbol *symbol,
                               CAstExpression *offset)
  : CAstOperand(t), _symbol(symbol), _offset(offset)
{
  assert(symbol != NULL);
}

const CSymbol* CAstDesignator::GetSymbol(void) const
{
  return _symbol;
}

bool CAstDesignator::TypeCheck(CToken *t, string *msg) const
{
  return true;
}

const CType* CAstDesignator::GetType(void) const
{
  const CType *t = GetSymbol()->GetDataType();

  if (_offset != NULL) {
    if (t->IsArray() && (_offset->GetType()->IsScalar())) {
      const CArrayType *at = dynamic_cast<const CArrayType*>(t);
      assert(at != NULL);
      t = at->GetBaseType();
    } else {
      t = NULL;
    }
  }

  return t;
}

ostream& CAstDesignator::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << _symbol << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  if (_offset != NULL) _offset->print(out, indent+2);

  return out;
}

string CAstDesignator::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << _symbol->GetName();
  if (_offset != NULL) out << "[]";
  out << "\",shape=ellipse]";
  return out.str();
}

void CAstDesignator::toDot(ostream &out, int indent) const
{
  string ind(indent, ' ');

  CAstNode::toDot(out, indent);

  if (_offset != NULL) {
    _offset->toDot(out, indent);
    out << ind << dotID() << "->" << _offset->dotID() << ";" << endl;
  }
}

CTacAddr* CAstDesignator::ToTac(CCodeBlock *cb, CTacLabel *ltrue, CTacLabel *lfalse)
{
	CTacName *result = new CTacName(GetSymbol());
  return result;
}


//------------------------------------------------------------------------------
// CAstConstant
//
CAstConstant::CAstConstant(CToken t, const CType *type, long long value)
  : CAstOperand(t), _type(type), _value(value)
{
}

void CAstConstant::SetValue(long long value)
{
  _value = value;
}

long long CAstConstant::GetValue(void) const
{
  return _value;
}

string CAstConstant::GetValueStr(void) const
{
  ostringstream out;

  if (GetType() == CTypeManager::Get()->GetBool()) {
    out << (_value == 0 ? "false" : "true");
  } else {
    out << dec << _value;
  }

  return out.str();
}

bool CAstConstant::TypeCheck(CToken *t, string *msg) const
{
  return true;
}

const CType* CAstConstant::GetType(void) const
{
  return _type;
}

ostream& CAstConstant::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << GetValueStr() << " ";

  const CType *t = GetType();
  if (t != NULL) out << t; else out << "<INVALID>";

  out << endl;

  return out;
}

string CAstConstant::dotAttr(void) const
{
  ostringstream out;
  out << " [label=\"" << GetValueStr() << "\",shape=ellipse]";
  return out.str();
}

CTacAddr* CAstConstant::ToTac(CCodeBlock *cb, CTacLabel *ltrue, CTacLabel *lfalse)
{
	CTacConst *result = new CTacConst(GetValue());
  return result;
}
