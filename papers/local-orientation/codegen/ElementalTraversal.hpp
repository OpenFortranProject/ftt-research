#ifndef ELEMENTAL_TRAVERSAL_HPP_
#define ELEMENTAL_TRAVERSAL_HPP_

#include "FortranTraversal.hpp"

class ElementalTraversal : public FortranTraversal
{

public:
   
   ElementalTraversal(int argc, char * argv[], const char * filename);

   virtual void visit(SgNode * node);

   void open(const char * filename);

   //   void visit(SgInterfaceBody * ibody);
   //   void visit(SgFunctionDeclaration * func_decl);
   void visit(SgFunctionDefinition  * func_def);

   void unparseStmt(SgStatement * stmt);

   void unparseExpr(SgExpression * expr);
   void unparseBinaryOp(SgBinaryOp * op);
   void unparseUnaryOp(SgUnaryOp * op);
   void unparseFuncCallExpr(SgFunctionCallExp * expr);
   void unparseVarRefExpr(SgVarRefExp * var);

   void unparseVarDecl(SgVariableDeclaration * var_decl, SgFunctionDefinition * func_def);
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

   void unparseArrayDescs(SgFunctionDeclaration * func_decl);
   void unparseIndexVars(SgFunctionDeclaration * func_decl);


protected:

   int line;
   int num_dims;

   int num_arrays;
   char ** array_list;

};

#endif // ELEMENTAL_TRAVERSAL_HPP_
