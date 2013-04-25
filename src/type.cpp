//------------------------------------------------------------------------------
/// @brief SNUPL type system
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
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

#include <cassert>
//#include <typeinfo>

#include "type.h"
using namespace std;


//------------------------------------------------------------------------------
// CType
//
CType::CType(void)
{
}

CType::~CType(void)
{
}

ostream& operator<<(ostream &out, const CType &t)
{
  return t.print(out);
}

ostream& operator<<(ostream &out, const CType *t)
{
  return t->print(out);
}


//------------------------------------------------------------------------------
// CNullType
//
CNullType::CNullType(void)
  : CType()
{
}

bool CNullType::Match(const CType *t) const
{
  // types are singletons, i.e., for null types we can simply compare the
  // pointers
  return (t == this);
}

ostream& CNullType::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "<NULL>";
  return out;
}

//------------------------------------------------------------------------------
// CIntType
//
CIntType::CIntType(void)
  : CType()
{
}

bool CIntType::Match(const CType *t) const
{
  // types are singletons, i.e., for scalar types we can simply compare the
  // pointers
  return (t == this);
}

ostream& CIntType::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "<" << "int";
  //out << "," << GetSize() << "," << GetAlign();
  out << ">";
  return out;
}


//------------------------------------------------------------------------------
// CBoolType
//
CBoolType::CBoolType(void)
  : CType()
{
}

bool CBoolType::Match(const CType *t) const
{
  // types are singletons, i.e., for scalar types we can simply compare the
  // pointers
  return (t == this);
}

ostream& CBoolType::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "<" << "bool";
  //out << "," << GetSize() << "," << GetAlign();
  out << ">";
  return out;
}


//------------------------------------------------------------------------------
// CArrayType
//
CArrayType::CArrayType(int nelem, const CType *basetype)
  : CType(), _nelem(nelem), _basetype(basetype)
{
  assert(_nelem > 0);
  assert(_basetype != NULL);
}

CArrayType::~CArrayType(void)
{
  if (_basetype->IsArray()) delete _basetype;
}

int CArrayType::GetSize(void) const
{
  return GetNElem()*GetBaseType()->GetSize();
}

int CArrayType::GetAlign(void) const
{
  return GetBaseType()->GetAlign();
}

bool CArrayType::Match(const CType *t) const
{
  if (t->IsArray()) {
    const CArrayType *at = dynamic_cast<const CArrayType*>(t);
    assert(at != NULL);
    return (GetNElem() == at->GetNElem()) &&
           (GetBaseType()->Match(at->GetBaseType()));
  } else {
    return false;
  }
}

ostream& CArrayType::print(ostream &out, int indent) const
{
  string ind(indent, ' ');

  out << ind << "<array " << GetNElem() << " of ";
  GetBaseType()->print(out);
  //out << "," << GetSize() << "," << GetAlign();
  out << ">";
  return out;
}


//------------------------------------------------------------------------------
// CTypeManager
//
CTypeManager* CTypeManager::_global_tm = NULL;

CTypeManager::CTypeManager(void)
{
  _null = new CNullType();
  _integer = new CIntType();
  _boolean = new CBoolType();
}

CTypeManager::~CTypeManager(void)
{
  delete _null;
  delete _integer;
  delete _boolean;
  for (size_t i=0; i<_array.size(); i++) delete _array[i];
}

CTypeManager* CTypeManager::Get(void)
{
  if (_global_tm == NULL) _global_tm = new CTypeManager();

  return _global_tm;
}

const CNullType* CTypeManager::GetNull(void) const
{
  return _null;
}

const CIntType* CTypeManager::GetInt(void) const
{
  return _integer;
}

const CBoolType* CTypeManager::GetBool(void) const
{
  return _boolean;
}

const CArrayType* CTypeManager::GetArray(int nelem, const CType *basetype)
{
  for (size_t i=0; i<_array.size(); i++) {
    if ((_array[i]->GetNElem() == nelem) &&
        (_array[i]->GetBaseType()->Match(basetype))) {
      return _array[i];
    }
  }

  CArrayType *a = new CArrayType(nelem, basetype);
  _array.push_back(a);

  return a;
}

