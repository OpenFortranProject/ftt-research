#include <rose.h>
#include <iostream>

using namespace SageBuilder;
using namespace SageInterface;


class FortranTraversal : public AstSimpleProcessing
{

public:
   
   FortranTraversal(SgGlobal * const scope);
   virtual void visit(SgNode * node);
   virtual void atTraversalEnd() const;

   void visit(const SgAllocateStatement        * const alloc_stmt)         ;
   void visit(const SgProcedureHeaderStatement * const func_decl)          ;
   void visit(const SgVariableDeclaration      * const var_decl)      const;
   void visit(const SgFunctionCallExp          * const func_call_exp) const;
   void visit(const SgExprStatement            * const expr_stmt)     const;
   void visit(const SgVarRefExp                * const var_ref)       const;

   // build statements
   //

   SgExprStatement   * buildCExprStatement(const SgExprStatement * const expr) const;

   // build expressions
   //

   SgExpression      * buildCExpr(SgExpression * const expr) const;
   SgBinaryOp        * buildCBinaryOp(const SgBinaryOp * const expr) const;
   SgUnaryOp         * buildCUnaryOp(const SgUnaryOp * const expr) const;
   SgFunctionCallExp * buildCFunctionCallExp(const SgFunctionCallExp * const expr) const;
   SgValueExp        * buildCValueExp(SgValueExp * const expr) const;
   SgExpression      * buildForVarRefExp(const SgVarRefExp * const expr) const;
   SgExprListExp     * buildCExprListExp(const SgExprListExp * const expr) const;
   SgExpression      * buildForPntrArrRefExp(const SgVarRefExp * const expr) const;

   SgAggregateInitializer * buildCAggregateInitializer(const SgAggregateInitializer * const expr) const;

   // helper functions
   //

   bool isFunctionArg(const SgInitializedName * const name) const;
   bool isRegionSelector(const SgInitializedName * const var) const;
   const char * isFunctionCall(const char * const name, const SgExprStatement * const expr_stmt) const;
   const char * insertTransferHaloVarDecl(const SgFunctionCallExp * const fcall);
   void insertTileOffsetFor(const std::string name);

protected:

   SgGlobal * const cl_global_scope;
   SgBasicBlock * cl_block;
   const SgFunctionDeclaration * src_func_decl;

   int tile_idx;

   // list of variables that are used as region selectors
   const std::vector<SgInitializedName *> selectors;

   const std::string arrayIndexVar;
};
