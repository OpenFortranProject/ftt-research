#ifndef ELEMENTAL_TRAVERSAL_HPP_
#define ELEMENTAL_TRAVERSAL_HPP_

#include "FortranTraversal.hpp"

class ElementalTraversal : public FortranTraversal
{

public:
   
   ElementalTraversal(const char * filename);
   ElementalTraversal(const char * filename, const char * array_list);
   virtual ~ElementalTraversal();

   virtual void visit(SgNode * node);

   void open(const char * filename);

   void visit(SgInterfaceBody * ibody);
   void visit(SgFunctionDeclaration * func_decl);
   void visit(SgFunctionDefinition  * func_def);

   void unparseVarDecl(SgVariableDeclaration * var_decl, bool isLocal=false);
   void unparseVarDef(SgVariableDeclaration * var_decl, bool isLocal=false);
   void unparseParamDef(SgVariableDeclaration * var_decl);
   void unparseFunctionCall(SgFunctionDefinition * func_def);

   void unparseFuncParam(SgInitializedName * name, SgFunctionDefinition * func_def);
   void unparseFuncParamReturn(SgFunctionDeclaration * func_decl, const char * comma);
   void unparseVarDeclListForScalars(SgVariableDeclaration * var_decl, bool isLocal);
   void unparseVarDeclListForArrays(SgVariableDeclaration * var_decl, bool isLocal);
   void unparseIntVarDeclStmt(SgVariableDeclaration * var_decl, bool isLocal=false);
   void unparseFloatVarDeclStmt(SgVariableDeclaration * var_decl, bool isLocal=false);

   void unparseType(SgType *);

   bool isElementalArrayType(SgInitializedName * iname);
   bool isElementalArrayType(SgFunctionDeclaration * func_decl);

protected:

   int num_arrays;
   char ** array_list;

};

#endif // ELEMENTAL_TRAVERSAL_HPP_
