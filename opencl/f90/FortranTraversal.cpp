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
   SgFunctionCallExp * buildCFunctionCallExp(SgFunctionCallExp * expr);
   SgVarRefExp       * buildCVarRefExp(SgVarRefExp * expr);
   SgExprListExp     * buildCExprListExp(SgExprListExp * expr);

   // helper functions
   //

   bool isFunctionArg(SgInitializedName * name);
   const char * isFunctionCall(const char * name, SgExprStatement * expr_stmt);

protected:

   SgGlobal * cl_scope;   
   SgBasicBlock * cl_block;
   SgFunctionDeclaration * src_func_decl;
};

FortranTraversal::FortranTraversal(SgGlobal * scope)
{
   this->cl_scope = scope;
}

void FortranTraversal::visit(SgNode * node)
{
   if (isSgFunctionDeclaration(node) != NULL) {
      visit( (SgFunctionDeclaration *) node);
   }
   if (isSgVariableDeclaration(node) != NULL) {
      visit( (SgVariableDeclaration *) node);
   }
   if (isSgFunctionCallExp(node) != NULL) {
      visit( (SgFunctionCallExp *) node);
   }
   if (isSgExprStatement(node) != NULL) {
      visit( (SgExprStatement *) node);
   }
}

void FortranTraversal::visit(SgFunctionDeclaration * func_decl)
{
   int numArrayParams = 0;
   SgType * array_type = NULL;

   this->src_func_decl = func_decl;

   // get parameters from source function
   //
   SgInitializedNamePtrList vars = func_decl->get_parameterList()->get_args();
   SgInitializedNamePtrList::iterator it_vars;

   // create new parameter list and add parameters
   //
   SgFunctionParameterList * params = buildFunctionParameterList();

   for (it_vars = vars.begin(); it_vars != vars.end(); it_vars++) {
      SgInitializedName * param = isSgInitializedName(*it_vars);
      SgType * param_type = param->get_type();

      // create new parameter
      //
      if (isSgArrayType(param_type) != NULL) {
         array_type = param_type;
         numArrayParams += 1;

         // TODO - add __global
         // I think this should work but may not be unparsed
         param->get_storageModifier().setOpenclGlobal();
      }

      SgInitializedName * param_name = buildInitializedName(param->get_name(), param_type);
      appendArg(params, param_name);
   }

   // add tile for local storage
   //
   if (numArrayParams > 0) {
      //SgType * param_type = buildPointerType(array_type->findBaseType());
      SgInitializedName * param_name = buildInitializedName("tiles", array_type);
      param_name->get_storageModifier().setOpenclLocal();
      appendArg(params, param_name);
   }

   // create function declaration
   //
   SgFunctionDeclaration *
   cl_func = buildDefiningFunctionDeclaration(func_decl->get_name(),
                                              buildVoidType(),
                                              params,
                                              cl_scope);
   cl_func->get_functionModifier().setOpenclKernel();
   appendStatement(cl_func, cl_scope);

   // save basic block for later processing
   //
   cl_block = cl_func->get_definition()->get_body();

   // add common kernel declarations
   //
   std::string code = "   // these variables will be needed by all kernels\n";
   code += "   const int2 t_size = {get_local_size(0) + 2*NPAD, get_local_size(1) + 2*NPAD};\n";
   code += "   const int k = (get_global_id(0) + NPAD)\n";
   code += " + (get_global_id(1) + NPAD) * (get_global_size(0) + 2*NPAD);\n\n";
   code += "   int kl = KXL + KYL*t_size.s0;";
}

void FortranTraversal::visit(SgVariableDeclaration * var_decl)
{
   SgInitializedNamePtrList vars = var_decl->get_variables();
   SgInitializedNamePtrList::iterator it_vars;

   printf("SgVariableDeclaration:\n");
   for (it_vars = vars.begin(); it_vars != vars.end(); it_vars++) {
      SgInitializedName * var = isSgInitializedName(*it_vars);
      SgType * var_type = var->get_type();

      // ignore function arguments contained in params list
      if (isFunctionArg(var)) continue;

      printf("  var name is %s\n", var->get_name().str());
      if (isSgArrayType(var_type) != NULL) {
         SgArrayType * array_type = isSgArrayType(var_type);

         //         SgType * base_type = buildPointerType(array_type->findBaseType());

         SgExprListExp* dim_list = array_type->get_dim_info();
         std::string dim_list_str = dim_list->unparseToString();
         printf("  array rank is %d diminfo is %s\n", array_type->get_rank(), dim_list_str.c_str());

         if (array_type->get_rank() == 1 && isSgTypeInt(array_type->findBaseType()) != NULL) {
            printf("     array is integer type\n");

            //            SgClassDeclaration * var_type = buildStructDeclaration("int4", cl_blockf);

            //SgExpressionPtr SgExpressionPtrList::iterator it = dim_list->get_expressions().begin();
            //            std::string str = (*it)->unparseToString();
            // sgValueExp and SgIntVal
            //            printf("     exp is %s %d\n", str.c_str(), str=="4");
         }
      }


      SgVariableDeclaration * cl_var_decl = buildVariableDeclaration(var->get_name(), var_type);
      appendStatement(cl_var_decl, cl_block);

      // allocatable variables need to be make local on device
      //
      if (var_decl->get_declarationModifier().get_typeModifier().isAllocatable()) {
         cl_var_decl->get_declarationModifier().get_storageModifier().setOpenclLocal();
      }
   }
}

void FortranTraversal::visit(SgFunctionCallExp * func_call_expr) {
   SgExpression * func = func_call_expr->get_function();
   SgFunctionRefExp * fref = isSgFunctionRefExp(func);

   if (fref != NULL) {
      std::string name = fref->get_symbol()->get_name().str();
      //      printf("SgFunctionCallExp: symbol name is %s\n", name.c_str());
   }
}

void FortranTraversal::visit(SgExprStatement * expr_stmt)
{
   const char * var = isFunctionCall("transfer_halo", expr_stmt);
   if (var != NULL) {
      printf("visit:SgExprStatement: found transfer_halo call\n");
   }

   SgExprStatement * c_expr_stmt = buildCExprStatement(expr_stmt);
   printf("visit SgExprStatement: c_expr_stmt==%p\n", c_expr_stmt);
   if (c_expr_stmt != NULL) {
      appendStatement(c_expr_stmt, cl_block);
   }
}

void FortranTraversal::atTraversalEnd()
{
   printf("FortranTraversal::atTraversalEnd\n");
}


// build statements
//

SgExprStatement * FortranTraversal::buildCExprStatement(SgExprStatement * expr_stmt)
{
   // TODO - other cases, likely subroutine call is a function call expr
   SgBinaryOp * bin_op = isSgBinaryOp(expr_stmt->get_expression());
   ROSE_ASSERT(bin_op != NULL);

   SgExpression * c_lhs = buildCExpr(bin_op->get_lhs_operand());
   SgExpression * c_rhs = buildCExpr(bin_op->get_rhs_operand());
   SgBinaryOp   * c_bin_op = buildBinaryExpression<SgAssignOp>(c_lhs, c_rhs);

   return buildExprStatement(c_bin_op);
}

SgExpression * FortranTraversal::buildCExpr(SgExpression * expr)
{
   if (expr == NULL) return NULL;

   switch (expr->variantT())
   {
      case V_SgFunctionCallExp: return buildCFunctionCallExp(isSgFunctionCallExp(expr));

      // symbol references
      case V_SgVarRefExp: return buildCVarRefExp(isSgVarRefExp(expr));
   }

   //printf("buildCExpr: Unimplemented\n");
   return NULL;
}

SgFunctionCallExp * FortranTraversal::buildCFunctionCallExp(SgFunctionCallExp * expr)
{
   SgFunctionCallExp * fcall = isSgFunctionCallExp(expr);
   SgFunctionRefExp  * fref  = isSgFunctionRefExp(fcall->get_function());
   SgExprListExp     * exprs = buildCExprListExp(fcall->get_args());

   printf("buildCFunctionCallExp: for function %s\n", fref->get_symbol()->get_name().str());
   return buildFunctionCallExp(fref->get_symbol(), exprs);
}

SgExprListExp * FortranTraversal::buildCExprListExp(SgExprListExp * expr_list)
{
   SgExprListExp * c_expr_list = buildExprListExp();
   SgExpressionPtrList::iterator it = expr_list->get_expressions().begin();

   while (it != expr_list->get_expressions().end()) {
      SgExpression * c_expr = buildCExpr(*it);
      if (c_expr != NULL) {
         c_expr_list->append_expression(c_expr);
      }
      it++;
   }
   return c_expr_list;
}

SgVarRefExp * FortranTraversal::buildCVarRefExp(SgVarRefExp * expr)
{
   printf("buildCExpr: buildVarRefExp for %s\n", expr->get_symbol()->get_name().str());
   return buildVarRefExp(expr->get_symbol()->get_name(), cl_block);
}

bool FortranTraversal::isFunctionArg(SgInitializedName * arg)
{
   SgInitializedNamePtrList func_args = src_func_decl->get_parameterList()->get_args();
   SgInitializedNamePtrList::iterator it_args;

   for (it_args = func_args.begin(); it_args != func_args.end(); it_args++) {
      SgInitializedName * func_arg = isSgInitializedName(*it_args);
      if (arg->get_name() == func_arg->get_name()) {
         return true;
      }
   }
   return false;
}

const char * FortranTraversal::isFunctionCall(const char * name, SgExprStatement * expr_stmt)
{
   SgBinaryOp * bin_op = isSgBinaryOp(expr_stmt->get_expression());
   ROSE_ASSERT(bin_op != NULL);

   SgVarRefExp * lhs = isSgVarRefExp(bin_op->get_lhs_operand());
   if (lhs != NULL) {
      return lhs->get_symbol()->get_name().str();
   }
   return NULL;
}


int main(int argc, char ** argv)
{
   ROSE_ASSERT(argc == 3);

   SgProject* project = frontend(argc, argv);
   ROSE_ASSERT(project != NULL);

   SgSourceFile * src_file = isSgSourceFile((*project)[0]);
   ROSE_ASSERT(src_file);
	
   SgSourceFile * cl_file = isSgSourceFile((*project)[1]);
   SgGlobal * cl_scope = cl_file->get_globalScope();
   ROSE_ASSERT(cl_scope);

   FortranTraversal traversal(cl_scope);

   SgDeclarationStatementPtrList & decls = src_file->get_globalScope()->get_declarations();
   SgDeclarationStatementPtrList::iterator it_decls;
   for (it_decls = decls.begin(); it_decls != decls.end(); it_decls++) {
      if (isSgFunctionDeclaration(*it_decls) != NULL) {
         traversal.traverse((SgFunctionDeclaration*) *it_decls, preorder);
      }
   }

   project->unparse();

   return 0;

#ifdef NOT_ME

   // old stuff
   //

   SgDeclarationStatementPtrList & decl_list = sfile->get_globalScope()->get_declarations();
   SgDeclarationStatementPtrList::iterator it_decl_list;
   for (it_decl_list = decl_list.begin(); it_decl_list != decl_list.end(); it_decl_list++) {
		
      SgFunctionDeclaration * kernel = isSgFunctionDeclaration(*it_decl_list);
      if (kernel) {
         printf("\nfunc_decl name is %s\n\n", kernel->get_name().str());

         SgScopeStatement * scope = kernel->get_scope();
         SgDeclarationStatementPtrList & decl_list_f = scope->getDeclarationList();
         SgDeclarationStatementPtrList::iterator it_decl_list_f;
         for (it_decl_list_f = decl_list_f.begin(); it_decl_list_f != decl_list_f.end(); it_decl_list_f++) {
            SgVariableDeclaration * var = isSgVariableDeclaration(*it_decl_list);
            if (var) {
               printf("var_decl name is %s\n\n", var->get_variables()[0]->get_name().str());
            } else {
               printf("decl not a var\n");
            }
         }
      //         var->get_declarationModifier().get_storageModifier().setOpenclGlobal();

      }

#ifdef OLD_STUFF
      if (kernel && kernel->get_name().getString() == "kernel") {
		
         kernel->get_functionModifier().setOpenclKernel();
			
         kernel->get_functionModifier().setOpenclVecTypeHint();
         kernel->get_functionModifier().set_opencl_vec_type(SageBuilder::buildFloatType());
			
         kernel->get_functionModifier().setOpenclWorkGroupSizeHint();
         kernel->get_functionModifier().setOpenclWorkGroupSizeReq();
         SgFunctionModifier::opencl_work_group_size_t vect = {1, 1, 1};
         kernel->get_functionModifier().set_opencl_work_group_size(vect);
		
      }
		
      SgVariableDeclaration * var = isSgVariableDeclaration(*it_decl_list);
      if (var && var->get_variables()[0]->get_name().getString() == "var") {
         var->get_declarationModifier().get_storageModifier().setOpenclGlobal();
      }
#endif

   }

   return 0;

#endif
}
