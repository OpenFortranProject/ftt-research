#include <rose.h>

using namespace SageBuilder;
using namespace SageInterface;

class FortranAnalysis : public AstSimpleProcessing
{

public:
   
   FortranAnalysis(int argc, char * argv[], SgGlobal * scope);
   virtual ~FortranAnalysis();

   virtual void visit(SgNode * node);
   virtual void atTraversalEnd();

   void visit(SgFunctionDeclaration * func_decl);
   void visit(SgFunctionDefinition  * func_def);
   void visit(SgVariableDeclaration * var_decl);
   void visit(SgFunctionCallExp     * func_call_exp);
   void visit(SgExprStatement       * expr_stmt);

   // helper functions
   //

   bool matchRegionAssignment(SgExprStatement * expr_stmt);

   bool isFunctionArg(SgInitializedName * name);
   bool isRegionSelector(SgInitializedName * var);

   bool isElementalArrayType(SgInitializedName * iname);
   bool isElementalArrayType(SgFunctionDeclaration * func_decl);

   bool hasArrayDescriptor(SgInitializedName * iname);

   static bool isDummyVariable(SgSymbol * sym);
   static bool hasLocalVariable(SgVariableDeclaration * var_decl, SgFunctionDefinition * func_def);

   static const char * getAttributeValue(SgSymbol * sym, std::string attr_name);

   // accessor function
   //

   SgSymbol * lastHaloSymbol()    {return last_halo_symbol;}

protected:

   SgGlobal * src_global_scope;   
   SgBasicBlock * src_block;
   SgFunctionDeclaration * src_func_decl;
   SgSymbol * last_halo_symbol;
   SgSymbol * first_array_dummy_ref;

   int num_arrays;
   char ** array_list;

   int num_descs;
   char ** desc_list;
};
