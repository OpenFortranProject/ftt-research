#include <rose.h>

using namespace SageBuilder;
using namespace SageInterface;

class FortranAnalysis : public AstSimpleProcessing
{

public:
   
   FortranAnalysis(SgGlobal * scope);
   virtual void visit(SgNode * node);
   virtual void atTraversalEnd();

   void visit(SgAllocateStatement   * alloc_stmt);
   void visit(SgFunctionDeclaration * func_decl);
   void visit(SgVariableDeclaration * var_decl);
   void visit(SgFunctionCallExp     * func_call_exp);
   void visit(SgExprStatement       * expr_stmt);

   // helper functions
   //

   bool matchRegionAssignment(SgExprStatement * expr_stmt);

   bool isFunctionArg(SgInitializedName * name);
   bool isRegionSelector(SgInitializedName * var);

   // accessor function
   //

   SgSymbol * lastHaloSymbol()    {return last_halo_symbol;}

protected:

   SgGlobal * src_global_scope;   
   SgBasicBlock * src_block;
   SgFunctionDeclaration * src_func_decl;
   SgSymbol * last_halo_symbol;
   SgSymbol * first_array_dummy_ref;
};
