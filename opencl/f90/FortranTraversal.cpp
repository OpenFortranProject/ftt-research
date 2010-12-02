#include <rose.h>
#include <iostream>

#define CL_SPECIALIZE

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
   SgFunctionCallExp * buildCFunctionCallExp(SgFunctionCallExp * expr);
   SgValueExp        * buildCValueExp(SgValueExp * expr);
   SgVarRefExp       * buildCVarRefExp(SgVarRefExp * expr);
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

FortranTraversal::FortranTraversal(SgGlobal * scope)
{
   this->cl_global_scope = scope;
   this->tile_idx = 0;
}

void FortranTraversal::visit(SgNode * node)
{
   // handle array assignment
   //SgPntrArrRefExp* arrayReference = isSgPntrArrRefExp(*i);
   //FortranCodeGeneration_locatedNode::unparseInitializerList(SgExpression* expr, SgUnparse_Info& info)

   // TODO - replace with switch
   if (isSgAllocateStatement(node) != NULL) {
      visit( (SgAllocateStatement *) node);
   }
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

void FortranTraversal::visit(SgAllocateStatement * alloc_stmt)
{
   SgExprListExp* exprs = alloc_stmt->get_expr_list();
   ROSE_ASSERT(exprs != NULL);

   std::string exprs_str = exprs->unparseToString();
   printf("SgAllocateStatement: allocate exprs are %s\n", exprs_str.c_str());

   SgExpressionPtrList::iterator i = exprs->get_expressions().begin();
   SgPntrArrRefExp * arrayRef = isSgPntrArrRefExp(*i);
   ROSE_ASSERT(arrayRef != NULL);
   SgVarRefExp * lhs = isSgVarRefExp(arrayRef->get_lhs_operand());
   ROSE_ASSERT(lhs != NULL);

   // define local tile, e.g., hx = TILE_OFFSET(tiles,3,get_tile_size())
   insertTileOffsetFor(lhs->get_symbol()->get_name().getString());
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
                                              cl_global_scope);
   cl_func->get_functionModifier().setOpenclKernel();
   appendStatement(cl_func, cl_global_scope);

   // save basic block for later processing
   //
   cl_block = cl_func->get_definition()->get_body();
}

void FortranTraversal::visit(SgVariableDeclaration * var_decl)
{
   SgVariableDeclaration * cl_var_decl;
   SgInitializedNamePtrList vars = var_decl->get_variables();
   SgInitializedNamePtrList::iterator it_vars;

   printf("SgVariableDeclaration:\n");
   for (it_vars = vars.begin(); it_vars != vars.end(); it_vars++) {
      SgInitializedName * var = isSgInitializedName(*it_vars);
      SgType * var_type = var->get_type();

      // ignore function arguments contained in params list
      if (isFunctionArg(var)) continue;

      printf("  var name is %s\n", var->get_name().str());
#ifdef CL_SPECIALIZE
      if (isSgArrayType(var_type) != NULL) {
         // if var is a region selector, create index variable for associated local tile
         if (isRegionSelector(var)) {
            printf("    region selector\n");
            selectors.insert(var);
            var_type = buildOpaqueType("int4", cl_block);
         }
      }
      else if (isSgPointerType(var_type) != NULL) {
         printf("    pointer type\n");
         // TODO - look to see if variable is defined by transfer_halo call
         // for now just assume it is
         var_type = var_type->findBaseType();
      }
#endif

      if (isSgArrayType(var_type) != NULL) {
         var_type = buildPointerType(isSgArrayType(var_type)->findBaseType());
      }

      cl_var_decl = buildVariableDeclaration(var->get_name(), var_type);
      appendStatement(cl_var_decl, cl_block);

      // allocatable variables need to be local on device
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

/**
 * Matches assignment statements (including pointer association)
 */
void FortranTraversal::visit(SgExprStatement * expr_stmt)
{
   SgExprStatement * c_expr_stmt = buildCExprStatement(expr_stmt);
   if (c_expr_stmt != NULL) {
      appendStatement(c_expr_stmt, cl_block);
   }

#ifdef CL_SPECIALIZE
   // if lhs is a region selector, define index variable for associated local tile
#endif
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
   ROSE_ASSERT(c_lhs != NULL);
   ROSE_ASSERT(c_rhs != NULL);

#ifdef CL_SPECIALIZE
   // insert cast if required by lhs and rhs is SgAggregateInitializer
   // TODO - check that lhs is int4 variable
   if (isSgAggregateInitializer(c_rhs) != NULL) {
      c_rhs = buildCastExp(c_rhs, buildOpaqueType("int4", cl_block));
   }
#endif

   SgBinaryOp   * c_bin_op = buildBinaryExpression<SgAssignOp>(c_lhs, c_rhs);

   return buildExprStatement(c_bin_op);
}

SgExpression * FortranTraversal::buildCExpr(SgExpression * expr)
{
   if (expr == NULL) {
      printf("buildCExpr: NULL expression\n");
      return NULL;
   }

   switch (expr->variantT())
   {
      case V_SgFunctionCallExp:      return buildCFunctionCallExp(isSgFunctionCallExp(expr));
      case V_SgVarRefExp:            return buildCVarRefExp(isSgVarRefExp(expr));
      case V_SgAggregateInitializer: return buildCAggregateInitializer(isSgAggregateInitializer(expr));
   }

   // lots of unary, binary and value exprs so general catch all here
   //
   if (isSgBinaryOp(expr) != NULL) {
      return buildCBinaryOp(isSgBinaryOp(expr));
   }
   if (isSgUnaryOp(expr) != NULL) {
      return buildCUnaryOp(isSgUnaryOp(expr));
   }
   if (isSgValueExp(expr) != NULL) {
      return buildCValueExp(isSgValueExp(expr));
   }

   printf("buildCExpr: Unimplemented variantT==%d\n", expr->variantT());
   return NULL;
}

SgBinaryOp * FortranTraversal::buildCBinaryOp(SgBinaryOp * expr)
{
   SgExpression * c_lhs = buildCExpr(expr->get_lhs_operand());
   SgExpression * c_rhs = buildCExpr(expr->get_rhs_operand());
   ROSE_ASSERT(c_lhs != NULL);
   ROSE_ASSERT(c_rhs != NULL);

   switch (expr->variantT())
   {
      case V_SgMinusOp:    return buildUnaryExpression<SgMinusOp>(c_lhs, c_rhs);
      case V_SgAddOp:      return buildBinaryExpression<SgAddOp>(c_lhs, c_rhs);
      case V_SgDivideOp:   return buildBinaryExpression<SgDivideOp>(c_lhs, c_rhs);
      case V_SgMultiplyOp: return buildBinaryExpression<SgMultiplyOp>(c_lhs, c_rhs);
      case V_SgSubtractOp: return buildBinaryExpression<SgSubtractOp>(c_lhs, c_rhs);
   }

   if (isSgExponentiationOp(expr) != NULL) {
      if (isSgIntVal(c_rhs) != NULL) {
         SgIntVal * val = isSgIntVal(c_rhs);
         if (val->get_value() == 2) {
            return buildBinaryExpression<SgMultiplyOp>(c_lhs, c_lhs);

         }
      }
      printf("buildCBinaryOp: Unimplemented exponentiation lhs==%d rhs==%d\n",
             expr->get_lhs_operand()->variantT(), expr->get_rhs_operand()->variantT());
   }
      
   printf("buildCBinaryOp: Unimplemented variantT==%d\n", expr->variantT());
   return NULL;
}

SgBinaryOp * FortranTraversal::buildCBinaryOp(SgBinaryOp * expr)
{
   SgExpression * c_lhs = buildCExpr(expr->get_lhs_operand());
   SgExpression * c_rhs = buildCExpr(expr->get_rhs_operand());
   ROSE_ASSERT(c_lhs != NULL);
   ROSE_ASSERT(c_rhs != NULL);

   switch (expr->variantT())
   {
      case V_SgMinusOp:    return buildUnaryExpression<SgMinusOp>(c_lhs, c_rhs);
      case V_SgAddOp:      return buildBinaryExpression<SgAddOp>(c_lhs, c_rhs);
      case V_SgDivideOp:   return buildBinaryExpression<SgDivideOp>(c_lhs, c_rhs);
      case V_SgMultiplyOp: return buildBinaryExpression<SgMultiplyOp>(c_lhs, c_rhs);
      case V_SgSubtractOp: return buildBinaryExpression<SgSubtractOp>(c_lhs, c_rhs);
   }

   if (isSgExponentiationOp(expr) != NULL) {
      if (isSgIntVal(c_rhs) != NULL) {
         SgIntVal * val = isSgIntVal(c_rhs);
         if (val->get_value() == 2) {
            return buildBinaryExpression<SgMultiplyOp>(c_lhs, c_lhs);

         }
      }
      printf("buildCBinaryOp: Unimplemented exponentiation lhs==%d rhs==%d\n",
             expr->get_lhs_operand()->variantT(), expr->get_rhs_operand()->variantT());
   }
      
   printf("buildCBinaryOp: Unimplemented variantT==%d\n", expr->variantT());
   return NULL;
}

SgFunctionCallExp * FortranTraversal::buildCFunctionCallExp(SgFunctionCallExp * expr)
{
   SgFunctionCallExp * fcall = isSgFunctionCallExp(expr);
   SgFunctionRefExp  * fref  = isSgFunctionRefExp(fcall->get_function());
   SgExprListExp     * exprs = buildCExprListExp(fcall->get_args());

   std::string name = fref->get_symbol()->get_name().getString();

   printf("buildCFunctionCallExp: for function %s\n", name.c_str());

#ifdef CL_SPECIALIZE
   // define pointer to tile variable for transfer_halo and add variable to call
   if (name == "transfer_halo") {
      const char * tile = insertTransferHaloVarDecl(fcall);
      exprs->append_expression(buildVarRefExp(tile, cl_block));
   }
   // region call to an array dereference
   if (name == "region") {
      printf("buildCFunctionCallExp: MODIFYING CALL TO REGION\n");
      //      const char * tile = insertTransferHaloVarDecl(fcall);
      //      exprs->append_expression(buildVarRefExp(tile, cl_block));
   }
#endif

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

SgValueExp * FortranTraversal::buildCValueExp(SgValueExp * expr)
{
   SgValueExp * val = NULL;
   switch (expr->variantT())
   {
      case V_SgFloatVal: {
         SgFloatVal * f_val = isSgFloatVal(expr);
         SgFloatVal * c_val = buildFloatVal(f_val->get_value());  val = c_val;
         c_val->set_valueString(f_val->get_valueString());        break;
      }
      case V_SgIntVal: {
         SgIntVal * f_val = isSgIntVal(expr);
         SgIntVal * c_val = buildIntVal(f_val->get_value());  val = c_val;
         c_val->set_valueString(f_val->get_valueString());    break;
      }
   }

   if (val == NULL) {
      printf("buildCValueExp: val is NULL, variantT==%d\n", expr->variantT());
   }

   return val;
}

SgVarRefExp * FortranTraversal::buildCVarRefExp(SgVarRefExp * expr)
{
   printf("buildCExpr: buildVarRefExp for %s\n", expr->get_symbol()->get_name().str());
   return buildVarRefExp(expr->get_symbol()->get_name(), cl_block);
}

SgAggregateInitializer * FortranTraversal::buildCAggregateInitializer(SgAggregateInitializer * expr)
{
   return buildAggregateInitializer(expr->get_initializers(), expr->get_type());
}


// helper functions
//

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

bool FortranTraversal::isRegionSelector(SgInitializedName * var)
{
   bool isSelector = false;
   SgType * var_type = var->get_type();

   // see if var is already in selector list
   //
   for (int i = 0; i < selectors.size(); i++) {
      if (var->get_name().getString() == selectors[i]->get_name().getString()) {
         return true;
      }
   }

   // first do the easy stuff
   //
   if (isSgArrayType(var_type) != NULL) {
      SgArrayType * array_type = isSgArrayType(var_type);
      SgExpressionPtrList & dim_ptrs = array_type->get_dim_info()->get_expressions();

      if (array_type->get_rank() == 1 && isSgTypeInt(array_type->findBaseType()) != NULL) {
         // TODO - could be a variable reference rather than a value expression
         if (isSgIntVal(dim_ptrs.front()) != NULL) {
            if (isSgIntVal(dim_ptrs.front())->get_value() == 4) isSelector = true;
         }
      }
   }

   // look for var in region and transfer_halo calls
   //
   if (isSelector == true) {
      SgStatementPtrList & stmts = src_func_decl->get_definition()->get_body()->getStatementList();
   }

   return isSelector;
}

/**
 * This function is not used and doesn't work as named
 */
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

const char * FortranTraversal::insertTransferHaloVarDecl(SgFunctionCallExp * fcall)
{
   SgExpressionPtrList::iterator it = fcall->get_args()->get_expressions().begin();
   SgVarRefExp * arg = isSgVarRefExp(*it);
   SgInitializedName * argName = arg->get_symbol()->get_declaration();
   std::string str = arg->get_symbol()->get_name().getString() + "_tile";

   // declare local tile, e.g., __local float *h_tile
   SgVariableDeclaration *
      cl_var_decl = buildVariableDeclaration(SgName(str), buildPointerType(arg->get_type()->findBaseType()));
   cl_var_decl->get_declarationModifier().get_storageModifier().setOpenclLocal();
   appendStatement(cl_var_decl, cl_block);

   // define local tile, e.g., h_tile = TILE_OFFSET(tiles,0,get_tile_size())
   insertTileOffsetFor(str);

   return str.c_str();
}

void FortranTraversal::insertTileOffsetFor(std::string name)
{
   SgExpression * expr;

   SgFunctionSymbol * tile_offset = isSgFunctionSymbol(cl_global_scope->lookup_symbol("TILE_OFFSET"));
   SgFunctionSymbol * get_tile_size = isSgFunctionSymbol(cl_global_scope->lookup_symbol("get_tile_size"));
   ROSE_ASSERT(tile_offset != NULL && get_tile_size != NULL);

   SgExprListExp * exprs = buildExprListExp();
   exprs->append_expression(buildVarRefExp("tiles", cl_block));
   exprs->append_expression(buildIntVal(tile_idx++));
   exprs->append_expression(buildFunctionCallExp(get_tile_size, buildExprListExp()));

   SgExpression * lhs = buildVarRefExp(SgName(name), cl_block);
   SgExpression * rhs = buildFunctionCallExp(tile_offset, exprs);

   SgBinaryOp * bin_expr = buildBinaryExpression<SgAssignOp>(lhs, rhs);

   SgExprStatement * assign_stmt = buildExprStatement(bin_expr);
   appendStatement(assign_stmt, cl_block);
}



int main(int argc, char ** argv)
{
   ROSE_ASSERT(argc == 3);

   SgProject* project = frontend(argc, argv);
   ROSE_ASSERT(project != NULL);

   SgSourceFile * src_file = isSgSourceFile((*project)[0]);
   ROSE_ASSERT(src_file);
	
   SgSourceFile * cl_file = isSgSourceFile((*project)[1]);
   SgGlobal * cl_global_scope = cl_file->get_globalScope();
   ROSE_ASSERT(cl_global_scope);

   FortranTraversal traversal(cl_global_scope);

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
