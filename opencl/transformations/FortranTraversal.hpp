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

private:
   void visitNode(const SgAllocateStatement        * const alloc_stmt)         ;
   void visitNode(const SgProcedureHeaderStatement * const func_decl)          ;
   void visitNode(const SgVariableDeclaration      * const var_decl)      const;
   void visitNode(const SgFunctionCallExp          * const func_call_exp) const;
   void visitNode(const SgExprStatement            * const expr_stmt)     const;
   void visitNode(const SgVarRefExp                * const var_ref)       const;
   void visitNode(const SgInitializedName          * const name)          const;
   void visitNode(      SgIfStmt                   * const ifstmt)        const;

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
   SgInitializedName * buildDopeVecInitializedName(const std::string dopeVecName) const;
   SgExpression      * buildCBoundsCheck(const SgPntrArrRefExp * const arrRefExp) const;
   SgExpression      * buildCPntrArrRefExp(const SgPntrArrRefExp * const expr) const;
   SgStatement       * buildCBasicBlock(const SgBasicBlock * const stmt) const;

   SgAggregateInitializer * buildCAggregateInitializer(const SgAggregateInitializer * const expr) const;

   // helper functions
   //

   bool isParentVisited(SgNode * const node);
   bool isParentVisited(const SgNode * const) const;
   bool needBoundsCheck(const SgPntrArrRefExp * const arrRefExp) const;
   bool isFunctionArg(const SgInitializedName * const name) const;
   bool isRegionSelector(const SgInitializedName * const var) const;
   const char * isFunctionCall(const char * const name, const SgExprStatement * const expr_stmt) const;
   const char * insertTransferHaloVarDecl(const SgFunctionCallExp * const fcall);
   void insertTileOffsetFor(const std::string name);
   std::string buildDopeVecName(const std::string baseName) const;
   std::string buildDopeVecName(const char * const baseName) const;
   std::string getDopeVectorName(const SgVarRefExp * const varRef) const;

   SgGlobal * const cl_global_scope;
   SgBasicBlock * cl_block;
   const SgFunctionDeclaration * src_func_decl;

   int tile_idx;

   // list of variables that are used as region selectors
   const std::vector<SgInitializedName *> selectors;

   // The arrayIndexVar is what we use to look at our working set (the result of get_global_id())
   const std::string arrayIndexVar;
   // name of the array reference attribute
   const std::string arrayRefAttr;
   // name of 'already visited' attribute
   const std::string alreadyVisitedAttr;
   // This is the name of the type of our dope vector struct
   const std::string dopeVecStructName;
   // Suffix for naming dope vectors based on array name
   const std::string dopeVecNameSuffix;
   // name of tiles array in C
   const std::string tilesName;
   // name of tile's size parameter in C
   const std::string tileSizeName;
   typedef const SgVariableSymbol * varref_t;
   typedef std::map<varref_t, varref_t> dopeVectorMap_t;
   // arrays stores the names of the arrays input to the opencl kernel
   std::vector<varref_t> arrays;
   // for each array we will also pass a dope vector
   dopeVectorMap_t dopeVectors;

};
