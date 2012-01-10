#include <rose.h>
#include <iostream>
#include "FortranTraversal.hpp"

FortranTraversal::FortranTraversal(SgGlobal * scope)
: cl_global_scope(NULL), cl_block(NULL), src_func_decl(NULL), tile_idx(0), arrayIndexVar("k")
{
   this->cl_global_scope = scope;
   this->tile_idx = 0;
}

void FortranTraversal::visit(SgNode * node)
{
   switch (node->variantT())
   {
     case V_SgAllocateStatement        :  visit( (SgAllocateStatement        *) node);  break;
     case V_SgVariableDeclaration      :  visit( (SgVariableDeclaration      *) node);  break;
     case V_SgFunctionCallExp          :  visit( (SgFunctionCallExp          *) node);  break;
     case V_SgExprStatement            :  visit( (SgExprStatement            *) node);  break;
     case V_SgProcedureHeaderStatement :  visit( (SgProcedureHeaderStatement *) node);  break;
     case V_SgVarRefExp                :  visit( (SgVarRefExp                *) node);  break;
     default: break;
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

void FortranTraversal::visit(SgProcedureHeaderStatement * func_decl)
{
   ROSE_ASSERT( cl_global_scope != NULL );
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
      SgPointerType * param_pointer_type(new SgPointerType());
      param_pointer_type->set_base_type(param_type);

      // create new parameter
      //
      array_type = param_pointer_type;
      numArrayParams += 1;

      // TODO - add __global
      // BUG: This does not unparse correctly.  Probably a ROSE bug
      param->get_storageModifier().setOpenclGlobal();

      SgInitializedName * param_name = buildInitializedName(param->get_name(),
                                                            param_pointer_type);
      appendArg(params, param_name);
   }

   // add tile for local storage
   //
   if (numArrayParams > 0) {
      // Add an input array length parameter
      SgInitializedName * inputSize_param_name =
                         buildInitializedName("inputSize", new SgTypeUnsignedInt());
      inputSize_param_name->get_storageModifier().setOpenclLocal();
      appendArg(params, inputSize_param_name);

      SgInitializedName * param_name = buildInitializedName("tiles", array_type);
      param_name->get_storageModifier().setOpenclLocal();
      appendArg(params, param_name);

      // Add tile size parameter
      SgInitializedName * tileSize_param_name =
                         buildInitializedName("tileSize", new SgTypeUnsignedInt());
      tileSize_param_name->get_storageModifier().setOpenclLocal();
      appendArg(params, tileSize_param_name);
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

   // add variables definitinions for indexing
   //
   SgExpression          * const zero                    = buildIntVal(0);
   SgExprListExp         * const cl_get_global_id_params = buildExprListExp(zero);
   SgFunctionSymbol      * const cl_get_global_id_sym    = lookupFunctionSymbolInParentScopes(SgName("get_global_id"), cl_block);
   SgExpression          * const cl_get_global_id_call   = buildFunctionCallExp(cl_get_global_id_sym, cl_get_global_id_params);
   SgAssignInitializer   * const cl_var_assign           = buildAssignInitializer(cl_get_global_id_call, NULL);
   // TODO: Use a free variable name instead of hardcoding arrayIndexVar
   SgVariableDeclaration * const cl_var_decl             = buildVariableDeclaration(arrayIndexVar, buildIntType(), cl_var_assign);

   appendStatement(cl_var_decl, cl_block);
}

void FortranTraversal::visit(SgVariableDeclaration * var_decl)
{
   ROSE_ASSERT( cl_block != NULL );
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
            //            selectors.insert(var);
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
void FortranTraversal::visit(SgExprStatement * const expr_stmt)
{
   ROSE_ASSERT( cl_block != NULL );
   SgStatement * const c_stmt = buildCExprStatement(expr_stmt);
   if (c_stmt != NULL) {
      appendStatement(c_stmt, cl_block);
   }
   // TODO: finish me
   // Add a bounds check around the index expression
   // First build the conditional expression
   // const SgName boundsName = cl_block->lookup_variable_symbol("inputSize")->get_name();
   // SgExpression * const c_cond = buildLessThanOp(buildVarRefExp(indexName, cl_block), buildVarRefExp(boundsName, cl_block));

   // return buildIfStmt(c_cond, c_indexStmt, NULL);

#ifdef CL_SPECIALIZE
   // if lhs is a region selector, define index variable for associated local tile
#endif
}

void FortranTraversal::visit(SgVarRefExp * const var_ref)
{
   ROSE_ASSERT( var_ref != NULL );
   std::cout << "FortranTraversal::" << __func__
             << "(SgVarRefExp * '" << var_ref->unparseToString()
             << "')" << std::endl;
   if( var_ref->getAttribute("arrayRef")->toString() == "arrayRef" ){
     std::cout << __func__ << ": " << buildForPntrArrRefExp(var_ref)->unparseToString() << std::endl;
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
   std::cout << "expr_stmnt = '" << expr_stmt->unparseToString() << "'" << std::endl;
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
   ROSE_ASSERT( cl_block != NULL );
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
      case V_SgVarRefExp:            return buildForVarRefExp(isSgVarRefExp(expr));
      case V_SgAggregateInitializer: return buildCAggregateInitializer(isSgAggregateInitializer(expr));
      default: break;
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

SgUnaryOp * FortranTraversal::buildCUnaryOp(SgUnaryOp * expr)
{
   SgExpression * op = buildCExpr(expr->get_operand());
   ROSE_ASSERT(op != NULL);

   switch (expr->variantT())
   {
      case V_SgMinusOp:    return buildUnaryExpression<SgMinusOp>(op);
      default: break;
   }
      
   printf("buildCUnaryOp: Unimplemented variantT==%d\n", expr->variantT());
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
      case V_SgAddOp:      return buildBinaryExpression<SgAddOp>(c_lhs, c_rhs);
      case V_SgDivideOp:   return buildBinaryExpression<SgDivideOp>(c_lhs, c_rhs);
      case V_SgMultiplyOp: return buildBinaryExpression<SgMultiplyOp>(c_lhs, c_rhs);
      case V_SgSubtractOp: return buildBinaryExpression<SgSubtractOp>(c_lhs, c_rhs);
      default: break;
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
   // define pointer to tile variable for interior() and add variable to call
   ROSE_ASSERT( cl_block != NULL );
   if (name == "interior") {
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
      default: break;
   }

   if (val == NULL) {
      printf("buildCValueExp: val is NULL, variantT==%d\n", expr->variantT());
   }

   return val;
}

SgExpression * FortranTraversal::buildForPntrArrRefExp(SgVarRefExp * expr)
{
   ROSE_ASSERT( cl_block != NULL );
   SgExpression * c_expr = NULL;
   SgSymbol * sym = expr->get_symbol();

   std::cout << "[" << __func__ << "]" << std::endl;
   c_expr = buildVarRefExp(sym->get_name(), cl_block);

   SgName name = cl_block->lookup_variable_symbol(arrayIndexVar)->get_name();
   c_expr = buildBinaryExpression<SgPntrArrRefExp>(c_expr, buildVarRefExp(name, cl_block));

   return c_expr;
}

SgExpression * FortranTraversal::buildForVarRefExp(SgVarRefExp * const expr)
{
   std::cout << "[" << __func__ << "]" << std::endl;
   ROSE_ASSERT( cl_block != NULL );
   const SgSymbol * const sym = expr->get_symbol();
   SgExpression * const c_refExpr = buildVarRefExp(sym->get_name(), cl_block);
   const SgName indexName = cl_block->lookup_variable_symbol(arrayIndexVar)->get_name();

   // create the array index expr
   SgExpression * const c_indexExpr = buildBinaryExpression<SgPntrArrRefExp>(c_refExpr, buildVarRefExp(indexName, cl_block));

   return c_indexExpr;
}

SgAggregateInitializer * FortranTraversal::buildCAggregateInitializer(SgAggregateInitializer * expr)
{
   return buildAggregateInitializer(expr->get_initializers(), expr->get_type());
}


// helper functions
//

bool FortranTraversal::isFunctionArg(SgInitializedName * arg)
{
   ROSE_ASSERT( src_func_decl != NULL );
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
   ROSE_ASSERT( src_func_decl != NULL );
   bool isSelector = false;
   SgType * var_type = var->get_type();

   // see if var is already in selector list
   //
   for (size_t i = 0; i < selectors.size(); i++) {
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

   // look for var in region and interior calls
   //
   if (isSelector == true) {
      /* SgStatementPtrList & stmts = */ src_func_decl->get_definition()->get_body()->getStatementList();
   }

   return isSelector;
}

/**
 * This function is not used and doesn't work as named
 */
/*
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
*/

const char * FortranTraversal::insertTransferHaloVarDecl(SgFunctionCallExp * fcall)
{
   ROSE_ASSERT( cl_block != NULL );
   SgExpressionPtrList::iterator it = fcall->get_args()->get_expressions().begin();
   SgVarRefExp * arg = isSgVarRefExp(*it);
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
   ROSE_ASSERT( cl_global_scope != NULL );
   ROSE_ASSERT( cl_block != NULL );

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
