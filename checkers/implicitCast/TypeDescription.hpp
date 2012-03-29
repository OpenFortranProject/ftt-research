#ifndef TYPEDESCRIPTION_HPP
#define TYPEDESCRIPTION_HPP

#include <rose.h>
#include <iostream>

class TypeDescription {
public:
  enum intrinsic_e { eINTEGER, eREAL, eCOMPLEX, eLOGICAL, eCHARACTER, eNOTINTRINSIC };

  TypeDescription(SgType * type, intrinsic_e fortranType, SgExpression * kind = NULL);

  bool operator==(const TypeDescription &td) const;
  bool operator<(const TypeDescription &td) const;

  SgExpression * get_kind() const;
  intrinsic_e get_fortran_type() const;
  SgType * get_rose_type() const;

private:
  SgType * const roseType;
  const intrinsic_e fortranType;
  SgExpression * const kind;
  

};

TypeDescription buildTypeDescription(SgType * const, SgExpression * const);

std::ostream& operator<<(std::ostream &out, const TypeDescription::intrinsic_e type);
std::ostream& operator<<(std::ostream &out, const TypeDescription &typeDescription);

#endif /* TYPEDESCRIPTION_HPP */
