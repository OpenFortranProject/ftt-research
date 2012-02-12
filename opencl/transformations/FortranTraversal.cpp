#include <cstdarg>
#include <rose.h>
#include <sageInterface.h>
#include <iostream>
#include "Util.hpp"
#include "FortranTraversal.hpp"

FortranTraversal::FortranTraversal(SgGlobal * const scope)
: cl_global_scope(scope), cl_block(NULL), src_func_decl(NULL),
  tile_idx(0), arrayIndexVar("global_id_0"), arrayRefAttr("arrayRef"),
  alreadyVisitedAttr("visited"), dopeVecStructName("CFI_cdesc_t"),
  dopeVecNameSuffix("_dopeV"), tilesName("tiles"), tileSizeName("tileSize")
{}

void FortranTraversal::visit(SgNode * node)
{
   ROSE_ASSERT( node != NULL );
   
   // exit early when we find a node that we've already visited
   if( isParentVisited(node) ) return;

   switch (node->variantT())
   {
     case V_SgAllocateStatement        :  visitNode( (const SgAllocateStatement        * const) node);  break;
     case V_SgVariableDeclaration      :  visitNode( (const SgVariableDeclaration      * const) node);  break;
     case V_SgFunctionCallExp          :  visitNode( (const SgFunctionCallExp          * const) node);  break;
     case V_SgExprStatement            :  visitNode( (const SgExprStatement            * const) node);  break;
     case V_SgProcedureHeaderStatement :  visitNode( (const SgProcedureHeaderStatement * const) node);  break;
     case V_SgVarRefExp                :  visitNode( (const SgVarRefExp                * const) node);  break;
     case V_SgInitializedName          :  visitNode( (const SgInitializedName          * const) node);  break;
     case V_SgIfStmt                   :  visitNode( (      SgIfStmt                   * const) node);  break;
     default: {
        dout << "Unhandled visit to '" << node->class_name() << "'" << std::endl;
        break;
     }
   }
}

void FortranTraversal::visitNode(const SgInitializedName * const name) const
{
   // Check that libpaul added an annotation
   AstAttribute * attr = name->getAttribute("LOPE");
   if(attr != NULL){
     printf("attr is non-NULL: %s\n", attr->toString().c_str());
   }

}

void FortranTraversal::visitNode(const SgAllocateStatement * const alloc_stmt)
{
   const SgExprListExp* const exprs = alloc_stmt->get_expr_list();
   ROSE_ASSERT(exprs != NULL);

   const std::string exprs_str = exprs->unparseToString();
   debug("SgAllocateStatement: allocate exprs are %s\n", exprs_str.c_str());

   SgExpressionPtrList::const_iterator i = exprs->get_expressions().begin();
   const SgPntrArrRefExp * const arrayRef = isSgPntrArrRefExp(*i);
   ROSE_ASSERT(arrayRef != NULL);
   const SgVarRefExp * const lhs = isSgVarRefExp(arrayRef->get_lhs_operand());
   ROSE_ASSERT(lhs != NULL);

   // define local tile, e.g., hx = TILE_OFFSET(tiles,3,get_tile_size())
   insertTileOffsetFor(lhs->get_symbol()->get_name().getString());
}

void FortranTraversal::visitNode(const SgProcedureHeaderStatement * const func_decl)
{
   ROSE_ASSERT( cl_global_scope != NULL );
   int numArrayParams = 0;
   SgType * array_type = NULL;

   this->src_func_decl = func_decl;

   // create new parameter list and add parameters
   //
   SgFunctionParameterList * const params = buildFunctionParameterList();

   // get parameters from source function
   //
   const SgInitializedNamePtrList vars = func_decl->get_parameterList()->get_args();
   SgInitializedNamePtrList::const_iterator it_vars;
   std::vector<const SgInitializedName *> inputArrays;
   for (it_vars = vars.begin(); it_vars != vars.end(); it_vars++) {
      SgInitializedName * const param = isSgInitializedName(*it_vars);
      SgType * const param_type = param->get_type();
      SgPointerType * const param_pointer_type(buildPointerType(param_type));

      // create new parameter
      //
      array_type = param_pointer_type;
      debug("[%s]: param_type = '%s'\n", __func__, param_type->sage_class_name());
      numArrayParams += 1;
      inputArrays.push_back(param);

      // TODO - add __global
      // BUG: This does not unparse correctly.  Probably a ROSE bug
      param->get_storageModifier().setOpenclGlobal();

      SgInitializedName * const param_name = buildInitializedName(param->get_name(),
                                                                  param_pointer_type);
      appendArg(params, param_name);
   }
   printPointers(inputArrays);

   // add tile for local storage
   //
   std::vector<const SgInitializedName *> dopeVectorNames;
   for(std::vector<const SgInitializedName *>::const_iterator i = inputArrays.begin(); i != inputArrays.end(); i++){
      // Add dope vector parameter
      const std::string dopeVecName(buildDopeVecName((*i)->get_name().str()));
      SgInitializedName * const paramDopeVecName = buildDopeVecInitializedName(dopeVecName);
      debug("adding parameter %s\n", dopeVecName.c_str());
      appendArg(params, paramDopeVecName);
      dopeVectorNames.push_back(paramDopeVecName);
   }
   // If using arrays, add parameters for the tiles and the number of tiles
   if( inputArrays.size() > 0 ) {
      SgInitializedName * const param_name = buildInitializedName(tilesName, array_type);
      param_name->get_storageModifier().setOpenclLocal();
      appendArg(params, param_name);

      // Add tile size parameter
      SgInitializedName * const tileSize_param_name =
                         buildInitializedName(tileSizeName, buildUnsignedIntType());
      tileSize_param_name->get_storageModifier().setOpenclLocal();
      appendArg(params, tileSize_param_name);
   }

   // create function declaration
   //
   SgFunctionDeclaration * const
   cl_func = buildDefiningFunctionDeclaration(func_decl->get_name(),
                                              buildVoidType(),
                                              params,
                                              cl_global_scope);
   cl_func->get_functionModifier().setOpenclKernel();
   appendStatement(cl_func, cl_global_scope);

   // save basic block for later processing
   //
   cl_block = cl_func->get_definition()->get_body();

   // Now build the list of input arrays
   ROSE_ASSERT( dopeVectorNames.size() == inputArrays.size() );
   std::vector<const SgInitializedName *>::const_iterator i = inputArrays.begin();
   std::vector<const SgInitializedName *>::const_iterator j = dopeVectorNames.begin();
   for(; i != inputArrays.end() && j != dopeVectorNames.end(); i++, j++) {
      const SgVariableSymbol * const arrayvarsym = lookupVariableSymbolInParentScopes((*i)->get_name(), cl_block);
      const SgVariableSymbol * const dopevarsym  = lookupVariableSymbolInParentScopes((*j)->get_name(), cl_block);
      arrays.push_back(arrayvarsym);
      dopeVectors[arrayvarsym] = dopevarsym;
   }

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

void FortranTraversal::visitNode(const SgVariableDeclaration * const var_decl) const
{
   ROSE_ASSERT( cl_block != NULL );

   const SgInitializedNamePtrList vars = var_decl->get_variables();
   SgInitializedNamePtrList::const_iterator it_vars;

   debug("SgVariableDeclaration:\n");
   for (it_vars = vars.begin(); it_vars != vars.end(); it_vars++) {
      const SgInitializedName * const var = isSgInitializedName(*it_vars);
      SgType * var_type = var->get_type();

      // ignore function arguments contained in params list
      if (isFunctionArg(var)) continue;

      debug("  var name is %s\n", var->get_name().str());
#ifdef CL_SPECIALIZE
      if (isSgArrayType(var_type) != NULL) {
         // if var is a region selector, create index variable for associated local tile
         if (isRegionSelector(var)) {
            debug("    region selector\n");
            //            selectors.insert(var);
            var_type = buildOpaqueType("int4", cl_block);
         }
      }
      else if (isSgPointerType(var_type) != NULL) {
         debug("    pointer type\n");
         // TODO - look to see if variable is defined by transfer_halo call
         // for now just assume it is
         var_type = var_type->findBaseType();
      }
#endif

      if (isSgArrayType(var_type) != NULL) {
         var_type = buildPointerType(isSgArrayType(var_type)->findBaseType());
      }

      SgVariableDeclaration * const cl_var_decl = buildVariableDeclaration(var->get_name(), var_type);
      appendStatement(cl_var_decl, cl_block);

      // allocatable variables need to be local on device
      //
      if (var_decl->get_declarationModifier().get_typeModifier().isAllocatable()) {
         cl_var_decl->get_declarationModifier().get_storageModifier().setOpenclLocal();
      }
   }
}

void FortranTraversal::visitNode(const SgFunctionCallExp * const func_call_expr) const
{
   const SgExpression     * const func = func_call_expr->get_function();
   const SgFunctionRefExp * const fref = isSgFunctionRefExp(func);

   if (fref != NULL) {
      const std::string name(fref->get_symbol()->get_name().str());
      //      printf("SgFunctionCallExp: symbol name is %s\n", name.c_str());
   }
}

/**
 * Matches assignment statements (including pointer association)
 */
void FortranTraversal::visitNode(const SgExprStatement * const expr_stmt) const
{
   ROSE_ASSERT( cl_block != NULL );
   SgStatement * c_stmt = buildCExprStatement(expr_stmt);
   if (c_stmt == NULL) return;
   debug("arrays = ");
   printPointers(arrays);
   debug("dopeVectors = ");
   printPointers(dopeVectors);
   // 1. check if a the sub expression touches the kernel's array parameter
   std::vector<SgNode*> refs = NodeQuery::querySubTree(c_stmt, V_SgPntrArrRefExp);
   SgExpression * c_cond = NULL;
   debug("varRefs = ");
   for(std::vector<SgNode*>::const_iterator i = refs.begin(); i != refs.end(); i++){
      // 2. wrap the access in a bounds check
      // Add a bounds check around the index expression
      // First build the conditional expression
      if( !needBoundsCheck(isSgPntrArrRefExp(*i)) ) continue;
      SgExpression * const c_comparison = buildCBoundsCheck(isSgPntrArrRefExp(*i));
      // If c_cond is null then this is the first time in the loop,
      // just use the comparison because it's the only conditional
      // otherwise, build a logical and (&&) with existing c_cond expression
      c_cond = c_cond == NULL ? c_comparison : buildAndOp(c_cond, c_comparison);
   }
   debug("\n");
   if( c_cond != NULL ){
      // insert the if statement
      appendStatement(buildIfStmt(c_cond, c_stmt, NULL), cl_block);
   } else {
      // if there are no array refs, just add the stmt to the block
      appendStatement(c_stmt, cl_block);
   }
#ifdef CL_SPECIALIZE
   // if lhs is a region selector, define index variable for associated local tile
#endif
}

void FortranTraversal::visitNode(const SgVarRefExp * const var_ref) const
{
   ROSE_ASSERT( var_ref != NULL );
   dout << "FortranTraversal::" << __func__
        << "(SgVarRefExp * '" << var_ref->unparseToString()
        << "')" << std::endl;
   if( var_ref->getAttribute(arrayRefAttr) != NULL ){
      dout << __func__ << ": " << "taking arrayRef branch" << std::endl;
      dout << __func__ << ": " << buildForPntrArrRefExp(var_ref)->unparseToString() << std::endl;
   }
}

void FortranTraversal::visitNode(SgIfStmt * const ifstmt) const
{
   ROSE_ASSERT( ifstmt != NULL );

   ROSE_ASSERT( isSgExprStatement(ifstmt->get_conditional()) != NULL );
   SgStatement * const c_cond = buildCExprStatement(isSgExprStatement(ifstmt->get_conditional()));
   dout << "true_body is type: " << ifstmt->get_true_body()->class_name() << std::endl;

   ROSE_ASSERT( isSgBasicBlock(ifstmt->get_true_body()) != NULL );
   // TODO: this currently handles only some expression types
   SgStatement * const c_true_body = buildCBasicBlock(isSgBasicBlock(ifstmt->get_true_body()));

   ROSE_ASSERT( ifstmt->get_false_body() == NULL || isSgBasicBlock(ifstmt->get_false_body()) != NULL );
   SgStatement * const c_false_body = ifstmt->get_false_body() == NULL ? NULL : buildCBasicBlock(isSgBasicBlock(ifstmt->get_false_body()));

   appendStatement(buildIfStmt(c_cond, c_true_body, c_false_body), cl_block);

   // One last thing, we do is set this if-statement as visited
   ifstmt->setAttribute(alreadyVisitedAttr, new AstAttribute());
}

void FortranTraversal::atTraversalEnd() const
{
   debug("FortranTraversal::atTraversalEnd\n");
}


// build statements
//
SgStatement * FortranTraversal::buildCBasicBlock(const SgBasicBlock * const stmt) const
{
   ROSE_ASSERT( stmt != NULL );
   const SgStatementPtrList & stmtList = stmt->get_statements();
   
   if ( stmtList.size() < 1 ) return NULL;

   SgBasicBlock * const c_block = buildBasicBlock();
   for(std::vector<SgStatement*>::const_iterator i = stmtList.begin(); i != stmtList.end(); i++)
   {
      // TODO: we need to do something about the other cases
      if( isSgExprStatement((*i)) ) {
        SgExprStatement * const c_expr = buildCExprStatement(isSgExprStatement((*i)));
        c_block->append_statement(c_expr);
      } else {
        ROSE_ASSERT( false );
      }
   }

   return c_block;
}


SgExprStatement * FortranTraversal::buildCExprStatement(const SgExprStatement * const expr_stmt) const
{
   dout << "expr_stmt = '" << expr_stmt->unparseToString() << "'" << std::endl;
   dout << "expr_stmt->get_expression->class_name = '" << expr_stmt->get_expression()->class_name() << "'" << std::endl;
   // TODO - other cases, likely subroutine call is a function call expr
   if( isSgVarRefExp(expr_stmt->get_expression()) != NULL ) {
      SgExpression * const c_expr = buildCExpr(expr_stmt->get_expression());
      return buildExprStatement(c_expr);
   }
   if( isSgBinaryOp(expr_stmt->get_expression()) != NULL ) {
      const SgBinaryOp * const bin_op = isSgBinaryOp(expr_stmt->get_expression());
      ROSE_ASSERT(bin_op != NULL);
   
      dout << "bin_op->get_lhs_operand = '" << bin_op->get_lhs_operand()->unparseToString() << "'" << std::endl;
      dout << "bin_op->get_rhs_operand = '" << bin_op->get_rhs_operand()->unparseToString() << "'" << std::endl;
      SgExpression * const c_lhs = buildCExpr(bin_op->get_lhs_operand());
      SgExpression * const c_rhs = buildCExpr(bin_op->get_rhs_operand());
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
   
      SgBinaryOp * const c_bin_op = buildBinaryExpression<SgAssignOp>(c_lhs, c_rhs);
   
      return buildExprStatement(c_bin_op);
   }
   ROSE_ASSERT( false );
}

SgExpression * FortranTraversal::buildCExpr(SgExpression * const expr) const
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
      //case V_SgExprListExp:          return buildCExprListExp(isSgExprListExp(expr));
      case V_SgPntrArrRefExp:        return buildCPntrArrRefExp(isSgPntrArrRefExp(expr));
      default: break;
   }

   // lots of unary, binary and value exprs so general catch all here
   //
   // TODO: SgPntrArrRefExp should be hitting here, but seems to be failing
   if (isSgBinaryOp(expr) != NULL) {
      return buildCBinaryOp(isSgBinaryOp(expr));
   }
   if (isSgUnaryOp(expr) != NULL) {
      return buildCUnaryOp(isSgUnaryOp(expr));
   }
   if (isSgValueExp(expr) != NULL) {
      return buildCValueExp(isSgValueExp(expr));
   }

   printf("buildCExpr: Unimplemented class_name=%s, expr='%s'\n", expr->class_name().c_str(), expr->unparseToString().c_str());
   ROSE_ASSERT( false );
   return NULL;
}

SgUnaryOp * FortranTraversal::buildCUnaryOp(const SgUnaryOp * const expr) const
{
   SgExpression * const op = buildCExpr(expr->get_operand());
   ROSE_ASSERT(op != NULL);

   switch (expr->variantT())
   {
      case V_SgMinusOp:    return buildUnaryExpression<SgMinusOp>(op);
      case V_SgUnaryAddOp:      return buildUnaryExpression<SgUnaryAddOp>(op);
      default: break;
   }
      
   printf("%s: Unimplemented class_name=%s, expr='%s'\n", __func__,expr->class_name().c_str(), expr->unparseToString().c_str());
   return NULL;
}

SgBinaryOp * FortranTraversal::buildCBinaryOp(const SgBinaryOp * const expr) const
{
   SgExpression * const c_lhs = buildCExpr(expr->get_lhs_operand());
   SgExpression * const c_rhs = buildCExpr(expr->get_rhs_operand());
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
         const SgIntVal * const val = isSgIntVal(c_rhs);
         if (val->get_value() == 2) {
            return buildBinaryExpression<SgMultiplyOp>(c_lhs, c_lhs);

         }
      }
      printf("buildCBinaryOp: Unimplemented exponentiation lhs==%d rhs==%d\n",
             expr->get_lhs_operand()->variantT(), expr->get_rhs_operand()->variantT());
   }
      
   printf("buildCBinaryOp: Unimplemented variantT==%d, class_name=%s\n", expr->variantT(), expr->class_name().c_str());
   return NULL;
}

SgFunctionCallExp * FortranTraversal::buildCFunctionCallExp(const SgFunctionCallExp * const expr) const
{
   const SgFunctionCallExp * const fcall = isSgFunctionCallExp(expr);
   SgFunctionRefExp  * const fref  = isSgFunctionRefExp(fcall->get_function());
   SgExprListExp     * const exprs = buildCExprListExp(fcall->get_args());

   const std::string name(fref->get_symbol()->get_name().getString());

   debug("buildCFunctionCallExp: for function %s\n", name.c_str());

#ifdef CL_SPECIALIZE
   // define pointer to tile variable for interior() and add variable to call
   ROSE_ASSERT( cl_block != NULL );
   if (name == "interior") {
      const char * const tile = insertTransferHaloVarDecl(fcall);
      exprs->append_expression(buildVarRefExp(tile, cl_block));
   }
   // region call to an array dereference
   if (name == "region") {
      debug("buildCFunctionCallExp: MODIFYING CALL TO REGION\n");
      //      const char * tile = insertTransferHaloVarDecl(fcall);
      //      exprs->append_expression(buildVarRefExp(tile, cl_block));
   }
#endif

   return buildFunctionCallExp(fref->get_symbol(), exprs);
}

SgExprListExp * FortranTraversal::buildCExprListExp(const SgExprListExp * const expr_list) const
{
   SgExprListExp * const c_expr_list = buildExprListExp();
   SgExpressionPtrList::const_iterator it = expr_list->get_expressions().begin();

   while (it != expr_list->get_expressions().end()) {
      SgExpression * const c_expr = buildCExpr(*it);
      if (c_expr != NULL) {
         c_expr_list->append_expression(c_expr);
      }
      it++;
   }
   return c_expr_list;
}

SgValueExp * FortranTraversal::buildCValueExp(SgValueExp * const expr) const
{
   SgValueExp * val = NULL;
   switch (expr->variantT())
   {
      case V_SgFloatVal: {
         SgFloatVal * const f_val = isSgFloatVal(expr);
         SgFloatVal * const c_val = buildFloatVal(f_val->get_value());  val = c_val;
         c_val->set_valueString(f_val->get_valueString());        break;
      }
      case V_SgIntVal: {
         SgIntVal * const f_val = isSgIntVal(expr);
         SgIntVal * const c_val = buildIntVal(f_val->get_value());  val = c_val;
         c_val->set_valueString(f_val->get_valueString());    break;
      }
      default: break;
   }

   if (val == NULL) {
      debug("buildCValueExp: val is NULL, variantT==%d, class_name=%s\n", expr->variantT(), expr->class_name().c_str());
   }

   return val;
}

/** Translate A Fortran style array indexing expression 'arr(x,y,z)'
 * into a C style array indexing expression 'arr[x][y][z]'.
 * The hardest part here is that the AST structure is different.
 * In C, we have nested PntrArrRefExp but in Fortran we have one list.
 */
SgExpression * FortranTraversal::buildCPntrArrRefExp(const SgPntrArrRefExp * const expr) const
{
   ROSE_ASSERT( cl_block != NULL );
   ROSE_ASSERT( expr != NULL );
   SgExpression * const lhs = expr->get_lhs_operand();
   ROSE_ASSERT(lhs != NULL);
   SgExpression * const rhs = expr->get_rhs_operand();
   ROSE_ASSERT(rhs != NULL);
   
   /* The LHS expression should contain the variable reference */
   ROSE_ASSERT( isSgVarRefExp(lhs) != NULL);
   SgSymbol * const sym = isSgVarRefExp(lhs)->get_symbol();
   ROSE_ASSERT( sym != NULL );
   SgExpression * const varRef = buildVarRefExp(sym->get_name(), cl_block);
   ROSE_ASSERT( varRef != NULL );
   dout << "varRef = '" << varRef->unparseToString() << "'" << std::endl;

   /* The RHS expression should contain an expression list */
   SgExpression * c_lhs = varRef;
   ROSE_ASSERT(isSgExprListExp(rhs) != NULL);
   const SgExprListExp * const exprListExp = isSgExprListExp(rhs);
   const SgExpressionPtrList& expList = exprListExp->get_expressions();
   for(std::vector<SgExpression*>::const_iterator i = expList.begin(); i != expList.end(); i++)
   {
      ROSE_ASSERT(*i != NULL);
      SgExpression * const c_exp = buildCExpr(*i);
      ROSE_ASSERT(c_exp != NULL);
      c_lhs = buildBinaryExpression<SgPntrArrRefExp>(c_lhs, c_exp);
      ROSE_ASSERT(c_lhs != NULL);
      //dout << "c_lhs = '" << c_lhs->unparseToString() << "'" << std::endl; 
   }

   return c_lhs;
}

SgExpression * FortranTraversal::buildForPntrArrRefExp(const SgVarRefExp * const expr) const
{
   ROSE_ASSERT( expr != NULL );
   ROSE_ASSERT( cl_block != NULL );
   const SgSymbol * const sym = expr->get_symbol();

   dout << "[" << __func__ << "]" << std::endl;
   SgExpression * const c_refExpr = buildVarRefExp(sym->get_name(), cl_block);

   const SgName name = cl_block->lookup_variable_symbol(arrayIndexVar)->get_name();
   SgExpression * const c_indexExpr = buildBinaryExpression<SgPntrArrRefExp>(c_refExpr, buildVarRefExp(name, cl_block));

   return c_indexExpr;
}

SgExpression * FortranTraversal::buildForVarRefExp(const SgVarRefExp * const expr) const
{
   dout << "[" << __func__ << "]" << std::endl;
   ROSE_ASSERT( expr != NULL );
   ROSE_ASSERT( cl_block != NULL );
   const SgSymbol * const sym = expr->get_symbol();
   SgExpression * const c_refExpr = buildVarRefExp(sym->get_name(), cl_block);
   // TODO: is this right?
   if( expr->getAttribute("arrayRef") == NULL ) return c_refExpr;
   const SgName indexName = cl_block->lookup_variable_symbol(arrayIndexVar)->get_name();

   // create the array index expr
   SgExpression * const c_indexExpr = buildBinaryExpression<SgPntrArrRefExp>(c_refExpr, buildVarRefExp(indexName, cl_block));

   return c_indexExpr;
}

SgAggregateInitializer * FortranTraversal::buildCAggregateInitializer(const SgAggregateInitializer * const expr) const
{
   return buildAggregateInitializer(expr->get_initializers(), expr->get_type());
}

SgInitializedName * FortranTraversal::buildDopeVecInitializedName(const std::string dopeVecName) const
{
   SgType * const dopeVecType = buildOpaqueType(dopeVecStructName, cl_global_scope);
   SgInitializedName * const paramDopeVecName =
                      buildInitializedName(dopeVecName, dopeVecType);
   paramDopeVecName->get_storageModifier().setOpenclLocal();
   return paramDopeVecName;
}

/// Traverses up the tree from node until either a) it get to the top and returns false
//  or b) hits a node that is visited and returns true.
bool FortranTraversal::isParentVisited(const SgNode * const node) const
{
   ROSE_ASSERT( node != NULL );
   const SgNode * const parent = node->get_parent();

   // I hope this means we're at the top of the tree
   if( parent == NULL ) return false;

   const AstAttribute * const attr = parent->getAttribute(alreadyVisitedAttr);
   // Just the presence of attr is enough to signal it has been visited
   if( attr != NULL ) return true;

   // We have more work to do...
   return isParentVisited(parent);
}

/// Same as above but with different overloading of const
bool FortranTraversal::isParentVisited(SgNode * const node)
{
   return (static_cast<const FortranTraversal&>(*this).isParentVisited(static_cast<const SgNode * const>(node)));
}

bool FortranTraversal::needBoundsCheck(const SgPntrArrRefExp * const arrRefExp) const
{
   ROSE_ASSERT( arrRefExp != NULL ); 
   SgExpression * ref_lhs = arrRefExp->get_lhs_operand();
   SgVarRefExp * varRef = isSgVarRefExp(ref_lhs);
   if(varRef == NULL ){
      // TODO: handle the error more gracefully
      debug("[%s] unable to get var ref from lhs of SgPntrArrRefExp\n", __func__);
      ROSE_ASSERT(varRef != NULL);
   }
   const SgVariableSymbol * const declVarSym = varRef->get_symbol();
   debug("%p ", (void*)declVarSym);
   if( dopeVectors.find(declVarSym) != dopeVectors.end() ){
      dout << "need bounds check for: '" << declVarSym->get_name().str() << "'" << std::endl;
   }
   return dopeVectors.find(declVarSym) != dopeVectors.end();
}

// Take an array ref "a[i]" and create a comparison, something like:
// ( k < a_dopeV.upper_bound )
SgExpression * FortranTraversal::buildCBoundsCheck(const SgPntrArrRefExp * const arrRefExp) const
{
   ROSE_ASSERT( arrRefExp != NULL ); 
   SgExpression * ref_lhs = arrRefExp->get_lhs_operand();
   SgVarRefExp * varRef = isSgVarRefExp(ref_lhs);
   if(varRef == NULL ){
      // TODO: handle the error more gracefully
      debug("[%s] unable to get var ref from lhs of SgPntrArrRefExp\n", __func__);
      ROSE_ASSERT(varRef != NULL);
   }
   const SgVariableSymbol * const declVarSym = varRef->get_symbol();
   debug("%p ", (void*)declVarSym);
   // If there isn't a dope vector, just return a "true" conditional
   if ( dopeVectors.find(declVarSym) == dopeVectors.end() )
   {
     return buildBoolValExp(1);
   }
   const SgName dopeVecInitName = dopeVectors.find(declVarSym)->second->get_name();
   const std::string boundsVarName(dopeVecInitName.str());
   const SgVariableSymbol * const boundsVarSym = lookupVariableSymbolInParentScopes(boundsVarName, cl_block);
   if( boundsVarSym == NULL ){
     // TODO: handle the error more gracefully
     debug("[%s] unable to lookup bounds variable '%s' in cl_block scope (or parent scope)\n", __func__, boundsVarName.c_str());
     ROSE_ASSERT(boundsVarSym != NULL);
   }
   const SgName boundsName = boundsVarSym->get_name();
   const SgVariableSymbol * const indexVarSym = cl_block->lookup_variable_symbol(arrayIndexVar);
   if( indexVarSym == NULL ){
      // TODO: handle the error more gracefully
      debug("[%s] unable to lookup index variable '%s' in cl_block scope\n", __func__, arrayIndexVar.c_str());
      ROSE_ASSERT(indexVarSym != NULL);
   }
   const SgName indexName = indexVarSym->get_name();
   SgExpression * const sizeAccessExpr = buildDotExp(buildVarRefExp(boundsName, cl_block), buildVarRefExp("upper_bound", cl_block));
   return buildLessThanOp(buildVarRefExp(indexName, cl_block), sizeAccessExpr);
}

// helper functions
//

bool FortranTraversal::isFunctionArg(const SgInitializedName * const arg) const
{
   ROSE_ASSERT( src_func_decl != NULL );
   const SgInitializedNamePtrList func_args = src_func_decl->get_parameterList()->get_args();
   SgInitializedNamePtrList::const_iterator it_args;

   for (it_args = func_args.begin(); it_args != func_args.end(); it_args++) {
      const SgInitializedName * const func_arg = isSgInitializedName(*it_args);
      if (arg->get_name() == func_arg->get_name()) {
         return true;
      }
   }
   return false;
}

bool FortranTraversal::isRegionSelector(const SgInitializedName * const var) const
{
   ROSE_ASSERT( src_func_decl != NULL );
   bool isSelector = false;
   const SgType * const var_type = var->get_type();

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
      const SgArrayType * const array_type = isSgArrayType(var_type);
      const SgExpressionPtrList & dim_ptrs = array_type->get_dim_info()->get_expressions();

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

const char * FortranTraversal::insertTransferHaloVarDecl(const SgFunctionCallExp * const fcall)
{
   ROSE_ASSERT( cl_block != NULL );
   SgExpressionPtrList::const_iterator it = fcall->get_args()->get_expressions().begin();
   const SgVarRefExp * const arg = isSgVarRefExp(*it);
   const std::string str = arg->get_symbol()->get_name().getString() + "_tile";

   // declare local tile, e.g., __local float *h_tile
   SgVariableDeclaration * const
      cl_var_decl = buildVariableDeclaration(SgName(str), buildPointerType(arg->get_type()->findBaseType()));
   cl_var_decl->get_declarationModifier().get_storageModifier().setOpenclLocal();
   appendStatement(cl_var_decl, cl_block);

   // define local tile, e.g., h_tile = TILE_OFFSET(tiles,0,get_tile_size())
   insertTileOffsetFor(str);

   return str.c_str();
}

void FortranTraversal::insertTileOffsetFor(const std::string name)
{
   ROSE_ASSERT( cl_global_scope != NULL );
   ROSE_ASSERT( cl_block != NULL );

   SgFunctionSymbol * const tile_offset = isSgFunctionSymbol(cl_global_scope->lookup_symbol("TILE_OFFSET"));
   SgFunctionSymbol * const get_tile_size = isSgFunctionSymbol(cl_global_scope->lookup_symbol("get_tile_size"));
   ROSE_ASSERT(tile_offset != NULL && get_tile_size != NULL);

   SgExprListExp * const exprs = buildExprListExp();
   exprs->append_expression(buildVarRefExp(tilesName, cl_block));
   exprs->append_expression(buildIntVal(tile_idx++));
   exprs->append_expression(buildFunctionCallExp(get_tile_size, buildExprListExp()));

   SgExpression * const lhs = buildVarRefExp(SgName(name), cl_block);
   SgExpression * const rhs = buildFunctionCallExp(tile_offset, exprs);

   SgBinaryOp * const bin_expr = buildBinaryExpression<SgAssignOp>(lhs, rhs);

   SgExprStatement * const assign_stmt = buildExprStatement(bin_expr);
   appendStatement(assign_stmt, cl_block);
}

std::string FortranTraversal::buildDopeVecName(const std::string baseName) const
{
  return std::string(baseName + dopeVecNameSuffix);
}

std::string FortranTraversal::buildDopeVecName(const char * const baseName) const
{
  return buildDopeVecName(std::string(baseName));
}
