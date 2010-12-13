#include <rose.h>
#include <iostream>

using namespace SageBuilder;
using namespace SageInterface;


class FortranTraversal : public AstSimpleProcessing
{

public:
   
   FortranTraversal(SgGlobal * scope);
   virtual void visit(SgNode * node);
   virtual void atTraversalEnd();

   void visit(SgAllocateStatement   * alloc_stmt);
   void visit(SgFunctionDeclaration * func_decl);
   void visit(SgVariableDeclaration * var_decl);
   void visit(SgFunctionCallExp     * func_call_exp);
   void visit(SgExprStatement       * expr_stmt);

   // build statements
   //

   SgExprStatement   * buildCExprStatement(SgExprStatement * expr);

   // build expressions
   //

   SgExpression      * buildCExpr(SgExpression * expr);
   SgBinaryOp        * buildCBinaryOp(SgBinaryOp * expr);
   SgUnaryOp         * buildCUnaryOp(SgUnaryOp * expr);
   SgFunctionCallExp * buildCFunctionCallExp(SgFunctionCallExp * expr);
   SgValueExp        * buildCValueExp(SgValueExp * expr);
   SgExpression      * buildForVarRefExp(SgVarRefExp * expr);
   SgExprListExp     * buildCExprListExp(SgExprListExp * expr);

   SgAggregateInitializer * buildCAggregateInitializer(SgAggregateInitializer * expr);

   // helper functions
   //

   bool isFunctionArg(SgInitializedName * name);
   bool isRegionSelector(SgInitializedName * var);
   const char * isFunctionCall(const char * name, SgExprStatement * expr_stmt);
   const char * insertTransferHaloVarDecl(SgFunctionCallExp * fcall);
   void insertTileOffsetFor(std::string name);

protected:

   SgGlobal * cl_global_scope;   
   SgBasicBlock * cl_block;
   SgFunctionDeclaration * src_func_decl;

   int tile_idx;

   // list of variables that are used as region selectors
   std::vector<SgInitializedName *> selectors;
};
