#include "TypeDescription.hpp"

TypeDescription::TypeDescription(SgType * type, intrinsic_e fortranType, SgExpression * kind)
: roseType(type), fortranType(fortranType), kind(kind)
{}

// Compares td to this. If we can tell for sure that they are the
// same it returns true and false in all other cases.  This can
// lead to false negatives when the kind is an arbitrary expression
bool TypeDescription::operator==(const TypeDescription &td) const
{
   if( &td == this ) return true;

   if( td.get_fortran_type() == this->get_fortran_type() ){
     if( td.get_kind() == NULL && this->get_kind() == NULL )
       return true;
     if( td.get_kind() == NULL || this->get_kind() == NULL ){
       // Need the alias information to resolve this
       ROSE_ASSERT( false );
       return false;
     }
     SgValueExp* kindValue   = isSgValueExp(get_kind());
     SgValueExp* tdKindValue = isSgValueExp(td.get_kind());
     if (kindValue != NULL && tdKindValue != NULL)
     {
       return kindValue->get_constant_folded_value_as_string() ==
              tdKindValue->get_constant_folded_value_as_string();
     }
   }
   return false;
}

bool TypeDescription::operator<(const TypeDescription &td) const
{
  // TODO: does this need to incorporate kind information?
  if( td.get_fortran_type() == this->get_fortran_type() ){
    return this->get_kind() < td.get_kind();
  }
  return this->get_fortran_type() < td.get_fortran_type();
}

SgExpression * TypeDescription::get_kind() const
{
   return kind;
}

TypeDescription::intrinsic_e TypeDescription::get_fortran_type() const
{
   return fortranType;
}

SgType * TypeDescription::get_rose_type() const
{
   return roseType;
}

// Interrogates an SgType to build up a TypeDescription
TypeDescription buildTypeDescription(SgType * const type, SgExpression * const kind)
{
   ROSE_ASSERT( type != NULL );

  //SgExpression * kind = type->get_type_kind();
  TypeDescription::intrinsic_e fortranType = TypeDescription::eNOTINTRINSIC;
  // TODO: is this complete?
  if( isSgTypeChar(type)         != NULL ||
      isSgTypeUnsignedChar(type) != NULL ){
    fortranType = TypeDescription::eCHARACTER;
  } else if( isSgTypeBool(type) != NULL ){
    // need to check for bool before int
    // as SgBoolType.isIntegerType() == true
    fortranType = TypeDescription::eLOGICAL;
  } else if( type->isFloatType() ){
    fortranType = TypeDescription::eREAL;
  } else if( type->isIntegerType() ){
    fortranType = TypeDescription::eINTEGER;
  } else if( isSgTypeComplex(type) != NULL ){
    fortranType = TypeDescription::eCOMPLEX;
  }
  return TypeDescription(type, fortranType, kind);
}

std::ostream& operator<<(std::ostream &out, const TypeDescription::intrinsic_e type) {
  switch(type) {
  case TypeDescription::eINTEGER:      out << "integer";      break;
  case TypeDescription::eREAL:         out << "real";         break;
  case TypeDescription::eCOMPLEX:      out << "complex";      break;
  case TypeDescription::eLOGICAL:      out << "logical";      break;
  case TypeDescription::eCHARACTER:    out << "character";    break;
  case TypeDescription::eNOTINTRINSIC: out << "notintrinsic"; break;
  }
  return out;
}

std::ostream& operator<<(std::ostream &out, const TypeDescription &typeDescription) {
  out << typeDescription.get_fortran_type();
  if( typeDescription.get_kind() != NULL ){
    SgValueExp* kindValue = isSgValueExp(typeDescription.get_kind());
    if (kindValue != NULL){
      out << "_" << kindValue->get_constant_folded_value_as_string();
    } else {
      out << "_kind";
    }
  }
  return out;
}

