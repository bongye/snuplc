//------------------------------------------------------------------------------
/// @brief SnuPL type system
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

#ifndef __SnuPL_TYPE_H__
#define __SnuPL_TYPE_H__

#include <iostream>
#include <vector>
using namespace std;


//------------------------------------------------------------------------------
/// @brief SnuPL base type
///
/// abstract base type
///
class CType {
  friend class CArrayType;

  protected:
    /// @brief constructor
    CType(void);
    virtual ~CType(void);

  public:
    /// @name property querying
    /// @{

    /// @brief return @a true for the NULL type, @a false otherwise
    virtual bool IsNull(void) const { return false; };

    /// @brief return @a true for boolean types, @a false otherwise
    virtual bool IsBoolean(void) const { return false; };

    /// @brief return @a true for scalar types, @a false otherwise
    virtual bool IsScalar(void) const { return false; };

    /// @brief return @a true for array types, @a false otherwise
    virtual bool IsArray(void) const { return false; };

    /// @brief return the storage size for this type
    /// @retval int storage size in bytes
    virtual int GetSize(void) const = 0;

    /// @brief return the alignment requirements for this type
    /// @retval int aligmnent in bytes
    virtual int GetAlign(void) const = 0;

    /// @}

    /// @name type comparisons
    /// @{

    /// @brief compare two types
    /// @param t type to compare this type to
    /// @retval true if the types match (are compatible)
    /// @retval false if the types do not match (are not compatible)
    virtual bool Match(const CType *t) const = 0;

    /// @}

    /// @brief print the type to an output stream
    ///
    /// @param out output stream
    /// @param indent indentation
    virtual ostream&  print(ostream &out, int indent=0) const = 0;
};

/// @name CType output operators
/// @{

/// @brief CType output operator
///
/// @param out output stream
/// @param d reference to CType
/// @retval output stream
ostream& operator<<(ostream &out, const CType &t);

/// @brief CType output operator
///
/// @param out output stream
/// @param d reference to CType
/// @retval output stream
ostream& operator<<(ostream &out, const CType *t);

/// @}


//------------------------------------------------------------------------------
/// @brief NULL type
///
/// NULL base type used whenever a symbol has no type
///
class CNullType : public CType {
  friend class CTypeManager;

  protected:
    /// @brief constructor
    CNullType(void);

  public:
    /// @name property querying
    /// @{

    /// @brief return @a true for the NULL type, @a false otherwise
    virtual bool IsNull(void) const { return true; };

    /// @brief return the storage size for this type
    /// @retval int storage size in bytes
    virtual int GetSize(void) const { return 0; };

    /// @brief return the alignment requirements for this type
    /// @retval int aligmnent in bytes
    virtual int GetAlign(void) const { return 0; };

    /// @}

    /// @name type comparisons
    /// @{

    /// @brief compare two types
    /// @param t type to compare this type to
    /// @retval true if the types match (are compatible)
    /// @retval false if the types do not match (are not compatible)
    virtual bool Match(const CType *t) const;

    /// @}

    /// @brief print the type to an output stream
    ///
    /// @param out output stream
    /// @param indent indentation
    virtual ostream&  print(ostream &out, int indent=0) const;
};


//------------------------------------------------------------------------------
/// @brief integer type
///
/// integer base type
///
class CIntType : public CType {
  friend class CTypeManager;

  protected:
    /// @brief constructor
    CIntType(void);

  public:
    /// @name property querying
    /// @{

    /// @brief return @a true for scalar types, @a false otherwise
    virtual bool IsScalar(void) const { return true; };

    /// @brief return the storage size for this type
    /// @retval int storage size in bytes
    virtual int GetSize(void) const { return 4; };

    /// @brief return the alignment requirements for this type
    /// @retval int aligmnent in bytes
    virtual int GetAlign(void) const { return 4; };

    /// @}

    /// @name type comparisons
    /// @{

    /// @brief compare two types
    /// @param t type to compare this type to
    /// @retval true if the types match (are compatible)
    /// @retval false if the types do not match (are not compatible)
    virtual bool Match(const CType *t) const;

    /// @}

    /// @brief print the type to an output stream
    ///
    /// @param out output stream
    /// @param indent indentation
    virtual ostream&  print(ostream &out, int indent=0) const;
};


//------------------------------------------------------------------------------
/// @brief boolean type
///
/// boolean base type
///
class CBoolType : public CType {
  friend class CTypeManager;

  protected:
    /// @brief constructor
    CBoolType(void);

  public:
    /// @name property querying
    /// @{

    /// @brief return @a true for scalar types, @a false otherwise
    virtual bool IsScalar(void) const { return true; };

    /// @brief return the storage size for this type
    /// @retval int storage size in bytes
    virtual int GetSize(void) const { return 1; };

    /// @brief return the alignment requirements for this type
    /// @retval int aligmnent in bytes
    virtual int GetAlign(void) const { return 1; };

    /// @}

    /// @name type comparisons
    /// @{

    /// @brief compare two types
    /// @param t type to compare this type to
    /// @retval true if the types match (are compatible)
    /// @retval false if the types do not match (are not compatible)
    virtual bool Match(const CType *t) const;

    /// @}

    /// @brief print the type to an output stream
    ///
    /// @param out output stream
    /// @param indent indentation
    virtual ostream&  print(ostream &out, int indent=0) const;
};


//------------------------------------------------------------------------------
/// @brief array type
///
/// type for array types
///
class CArrayType : public CType {
  friend class CTypeManager;

  protected:
    /// @brief constructor
    ///
    /// @param nelem    element count
    /// @param basetype base type
    CArrayType(int nelem, const CType *basetype);
    virtual ~CArrayType(void);

  public:
    /// @name property querying
    /// @{

    /// @brief return @a true for array types, @a false otherwise
    virtual bool IsArray(void) const { return true; };

    /// @brief return the storage size for this type
    /// @retval int storage size in bytes
    virtual int GetSize(void) const;

    /// @brief return the alignment requirements for this type
    /// @retval int aligmnent in bytes
    virtual int GetAlign(void) const;

    /// @brief return the base type
    /// @retval CType* base type
    const CType* GetBaseType(void) const { return _basetype; };

    /// @brief return the element count
    /// @retval int element count
    int GetNElem(void) const { return _nelem; };

    /// @}

    /// @name type comparisons
    /// @{

    /// @brief compare two types
    /// @param t type to compare this type to
    /// @retval true if the types match (are compatible)
    /// @retval false if the types do not match (are not compatible)
    virtual bool Match(const CType *t) const;

    /// @}

    /// @brief print the type to an output stream
    ///
    /// @param out output stream
    /// @param indent indentation
    virtual ostream&  print(ostream &out, int indent=0) const;

  private:
    int            _nelem;        ///< element count
    const CType   *_basetype;     ///< base type
};


//------------------------------------------------------------------------------
/// @brief type manager
///
/// manages all types in a module
///
class CTypeManager {
  public:
    /// @brief return the global type manager
    static CTypeManager* Get(void);

    /// @name base types
    /// @{

    const CNullType* GetNull(void) const;
    const CIntType* GetInt(void) const;
    const CBoolType* GetBool(void) const;

    /// @}

    /// @name composite types
    /// @{

    const CArrayType* GetArray(int nelem, const CType* basetype);

    /// @}

    /// @brief print the type to an output stream
    ///
    /// @param out output stream
    /// @param indent indentation
    //virtual ostream&  print(ostream &out, int indent=0) const;

  private:
    /// @name constructor/destructor
    /// @{

    CTypeManager(void);
    virtual ~CTypeManager(void);

    /// @}

    CNullType     *_null;         ///< null base type
    CIntType      *_integer;      ///< integer base type
    CBoolType     *_boolean;      ///< boolean base type

    vector<CArrayType*> _array;   ///< array types

    static CTypeManager *_global_tm; ///< global type manager instance
};



#endif // __SnuPL_TYPE_H__
