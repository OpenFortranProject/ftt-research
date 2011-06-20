#include <rose.h>
#include "FortranTraversal.hpp"

bool FortranTraversal::isErrorParam(SgInitializedName * name)
{
   SgType * type = name->get_typeptr();  // TODO - check type
   if (name->get_name() == "ierror") return true;
   return false;
}

bool FortranTraversal::isErrorReturn(SgVariableDeclaration * var_decl)
{
   SgTypeModifier & typemod = var_decl->get_declarationModifier().get_typeModifier();

   SgInitializedNamePtrList & vars = var_decl->get_variables();
   SgInitializedNamePtrList::iterator it = vars.begin();
   SgType * type = (*it)->get_typeptr();

   if (isSgTypeInt(type) && typemod.isOptional() && typemod.isIntent_out()) {
      if ((*it)->get_name() == "ierror") return true;
      return false;
   }

   return false;
}

bool FortranTraversal::isChoiceBuffer(SgVariableDeclaration * var_decl)
{
   SgTypeModifier & typemod = var_decl->get_declarationModifier().get_typeModifier();

   SgInitializedNamePtrList & vars = var_decl->get_variables();
   SgInitializedNamePtrList::iterator it = vars.begin();
   SgType * type = (*it)->get_typeptr();

   if (isSgArrayType(type)) {
      SgArrayType * array_type = isSgArrayType(type);
      type = array_type->get_base_type();
      if (isSgNamedType(type) != NULL) {
         if (isSgNamedType(type)->get_name() == "MPI_Choice") return true;
      }
      return false;
   }
   return false;
}

bool FortranTraversal::isArrayOfRequests(SgVariableDeclaration * var_decl)
{
   SgTypeModifier & typemod = var_decl->get_declarationModifier().get_typeModifier();

   SgInitializedNamePtrList & vars = var_decl->get_variables();
   SgInitializedNamePtrList::iterator it = vars.begin();
   SgType * type = (*it)->get_typeptr();

   if (isSgArrayType(type)) {
      SgArrayType * array_type = isSgArrayType(type);
      type = array_type->get_base_type();
      if (isSgNamedType(type) && (*it)->get_name() == "array_of_requests") {
         if (isSgNamedType(type)->get_name() == "MPI_Request") return true;
      }
      return false;
   }
   return false;
}

bool FortranTraversal::isArrayOfStatuses(SgVariableDeclaration * var_decl)
{
   SgTypeModifier & typemod = var_decl->get_declarationModifier().get_typeModifier();

   SgInitializedNamePtrList & vars = var_decl->get_variables();
   SgInitializedNamePtrList::iterator it = vars.begin();
   SgType * type = (*it)->get_typeptr();

   if (isSgArrayType(type)) {
      SgArrayType * array_type = isSgArrayType(type);
      type = array_type->get_base_type();
      if (isSgNamedType(type) && (*it)->get_name() == "array_of_statuses") {
         if (isSgNamedType(type)->get_name() == "MPI_Status") return true;
      }
      return false;
   }
   return false;
}

FortranTraversal::FortranTraversal(const char * filename)
{
   this->fp = fopen(filename, "w");
   if (fp == NULL) {
      printf("FortranTraversal:: ERROR in opening file %s\n", filename);
   }
   ROSE_ASSERT(fp != NULL);
}

void FortranTraversal::atTraversalEnd()
{
   fclose(fp);
}

void FortranTraversal::unparseOptionalModifier(SgTypeModifier & typemod)
{
   if (typemod.isOptional()) fprintf(fp, ", optional");
}

void FortranTraversal::unparseValueModifier(SgTypeModifier & typemod)
{
   if (typemod.isIntent_in()) fprintf(fp, ", value");
}

void FortranTraversal::unparseIntentModifier(SgTypeModifier & typemod)
{
   if (typemod.isIntent_in()) fprintf(fp, ", intent(in)");
   if (typemod.isIntent_out()) fprintf(fp, ", intent(out)");
   if (typemod.isIntent_inout()) fprintf(fp, ", intent(inout)");
}

