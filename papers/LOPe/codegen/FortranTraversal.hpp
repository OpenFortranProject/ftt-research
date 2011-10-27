#ifndef FORTRAN_TRAVERSAL_HPP_
#define FORTRAN_TRAVERSAL_HPP_

#include <rose.h>
#include <stdio.h>

using namespace SageBuilder;
using namespace SageInterface;

class FortranTraversal : public AstSimpleProcessing
{

public:
   
   FortranTraversal(const char * filename);
   virtual void visit(SgNode * node) = 0;
   virtual void atTraversalEnd();

   static bool isErrorParam(SgInitializedName * name);
   static bool isErrorReturn(SgVariableDeclaration * var_decl);
   static bool isChoiceBuffer(SgVariableDeclaration * var_decl);
   static bool isArrayOfRequests(SgVariableDeclaration * var_decl);
   static bool isArrayOfStatuses(SgVariableDeclaration * var_decl);

   void unparseOptionalModifier(SgTypeModifier & typemod);
   void unparseValueModifier(SgTypeModifier & typemod);
   void unparseIntentModifier(SgTypeModifier & typemod);

protected:

   FILE * fp;  // output file
};

#endif // FORTRAN_TRAVERSAL_HPP_
