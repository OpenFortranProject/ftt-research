#include "FortranAnalysis.hpp"
#include "Util.hpp"

#undef DEBUG_PRINT

//
// Helper class
//
class HaloRefSearch : public AstSimpleProcessing
{
public:
   HaloRefSearch();
   virtual void visit(SgNode * node);
   static bool findHaloRef(SgExprStatement * expr_stmt);
   bool found;
};

bool HaloRefSearch::findHaloRef(SgExprStatement * expr_stmt)
{
   HaloRefSearch search;   
   SgExpression * expr = expr_stmt->get_expression();

   //   SgBinaryOp * bin_op = isSgBinaryOp(expr_stmt->get_expression());
   //   if (bin_op == NULL) {
   //      expr = expr_stmt->get_expression();
   //   }

   search.traverse(expr, preorder);
   return search.found;
}

HaloRefSearch::HaloRefSearch()
{
   this->found = false;
}

void HaloRefSearch::visit(SgNode * node)
{
   switch (node->variantT()) {
   case V_SgVarRefExp: {
      SgVarRefExp * var = isSgVarRefExp(node);
      if (var->get_symbol()->getAttribute("halo_attr") != NULL) {
         found = true;
      }
      break;
   }
   default: break;
   }
}


FortranAnalysis::FortranAnalysis(SgGlobal * scope)
: src_global_scope(NULL), src_block(NULL), src_func_decl(NULL),
  last_halo_symbol(NULL), first_array_dummy_ref(NULL)
{
   this->src_global_scope = scope;
   this->last_halo_symbol = NULL;
   this->first_array_dummy_ref = NULL;
}

void FortranAnalysis::visit(SgNode * node)
{
   switch (node->variantT())
   {
     case V_SgAllocateStatement        :  visit( (SgAllocateStatement        *) node);  break;
     case V_SgProcedureHeaderStatement :  visit( (SgProcedureHeaderStatement *) node);  break;
     case V_SgVariableDeclaration      :  visit( (SgVariableDeclaration      *) node);  break;
     case V_SgFunctionCallExp          :  visit( (SgFunctionCallExp          *) node);  break;
     case V_SgExprStatement            :  visit( (SgExprStatement            *) node);  break;
     case V_SgVarRefExp                :  visit( (SgVarRefExp                *) node);  break;
     default: break;
   }
}

void FortranAnalysis::visit(SgAllocateStatement *)
{
   debug("FortranAnalysis::visit(SgAllocateStatement *)\n");
}

void FortranAnalysis::visit(SgProcedureHeaderStatement * func_decl)
{
   SgFunctionDefinition * func_def = isSgFunctionDefinition(func_decl->get_definition());
   if (func_def == NULL) return;

   SgInitializedNamePtrList func_args = func_decl->get_parameterList()->get_args();
   SgInitializedNamePtrList::const_iterator it_args;

   for (it_args = func_args.begin(); it_args != func_args.end(); it_args++) {
      SgInitializedName * const func_arg = isSgInitializedName(*it_args);
      SgSymbol * const sym = func_def->lookup_symbol(func_arg->get_name());
      if (sym == NULL) {
         debug("FortranAnalysis::visit: no symbol for name %s\n",
                func_arg->get_name().getString().c_str());
      }
      else if (isSgArrayType(sym->get_type()) != NULL) {
         sym->setAttribute("dummy_attr", new AstTextAttribute("DUMMY_ARRAY_ARG"));
         debug("SgFunctionDeclaration: adding dummy array attribute to %s\n",
                sym->get_name().getString().c_str());
      }
      else {
         sym->setAttribute("dummy_attr", new AstTextAttribute("DUMMY_ARG"));
         debug("SgFunctionDeclaration: adding dummy attribute to %s\n",
                sym->get_name().getString().c_str());
      }
   }
}

void FortranAnalysis::visit(SgVariableDeclaration *)
{
   debug("FortranAnalysis::visit(SgVariableDeclaration *)\n");
}

void FortranAnalysis::visit(SgFunctionCallExp * fcall)
{
   SgFunctionRefExp  * fref  = isSgFunctionRefExp(fcall->get_function());

   if (fref != NULL) {
      SgExpressionPtrList::iterator it = fcall->get_args()->get_expressions().begin();
      std::string name = fref->get_symbol()->get_name().getString();

      if (name == "interior" && it != fcall->get_args()->get_expressions().end()) {
         SgVarRefExp * var = isSgVarRefExp(*it);
         SgSymbol * sym = var->get_symbol();
         sym->setAttribute("halo_attr", new AstTextAttribute("HALO_VAR"));
         debug("SgFunctionCallExp: adding halo attribute to %s\n",
                sym->get_name().getString().c_str());
      }
   }
}

/**
 * Matches assignment statements (including pointer association)
 */
void FortranAnalysis::visit(SgExprStatement *)
{
   /*
   dout << "FortranAnalysis::" << __func__
        << "(SgExprStatement * '" << expr_stmt->unparseToString()
        << "')" << std::endl;
   */
   /* Do we care about matching region assignment? */
   /*
   if (matchRegionAssignment(expr_stmt)) {
      SgBinaryOp * bin_op = isSgBinaryOp(expr_stmt->get_expression());
      SgVarRefExp * var = isSgVarRefExp(bin_op->get_lhs_operand());
      if (var == NULL) return;
      var->get_symbol()->setAttribute("halo_attr", new AstTextAttribute("HALO_VAR"));
      debug("FortranAnalysis:: adding halo attr to %s\n",
             var->get_symbol()->get_name().getString().c_str());
   }
   else if (HaloRefSearch::findHaloRef(expr_stmt)) {
      expr_stmt->setAttribute("halo_ref", new AstTextAttribute("HAS_HALO_REF"));
      debug("FortranAnalysis:: adding halo attr to statement\n");
   }
   */
}

void FortranAnalysis::visit(SgVarRefExp * const var_ref)
{
   dout << "FortranAnalysis::" << __func__
        << "(SgVarRefExp * '" << var_ref->unparseToString()
        << "')" << std::endl;
   // TODO: We really need some form of external knowledge to determine
   // what is an array and what is not an array on the OpenCL side.
   // To get that information we need the call site?
   //if( isSgPntrArrRefExp(var_ref) != NULL ) {
      dout << __func__ << ": " << "marking as arrayRef" << std::endl;
      var_ref->setAttribute("arrayRef", new AstTextAttribute("arrayRef"));
   //}
}

void FortranAnalysis::atTraversalEnd()
{
   debug("FortranAnalysis::atTraversalEnd\n");
}


// helper functions
//

bool FortranAnalysis::matchRegionAssignment(SgExprStatement * expr_stmt)
{
   SgBinaryOp * bin_op = isSgBinaryOp(expr_stmt->get_expression());
   if (bin_op == NULL) return false;

   SgFunctionCallExp * fcall = isSgFunctionCallExp(bin_op->get_rhs_operand());
   if (fcall == NULL) return false;

   SgFunctionRefExp * fref = isSgFunctionRefExp(fcall->get_function());
   if (fref == NULL) return false;

   SgExpressionPtrList::iterator it = fcall->get_args()->get_expressions().begin();
   std::string name = fref->get_symbol()->get_name().getString();
   if (name == "interior" && it != fcall->get_args()->get_expressions().end()) {
      SgVarRefExp * var = isSgVarRefExp(*it);
      if (var == NULL) return false;
      AstTextAttribute * attr = (AstTextAttribute *) var->get_symbol()->getAttribute("dummy_attr");
      if (attr == NULL) return false;
      if (attr->toString() != "DUMMY_ARRAY_ARG") return false;
   }
   return true;
}

bool FortranAnalysis::isFunctionArg(SgInitializedName * arg)
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

bool FortranAnalysis::isRegionSelector(SgInitializedName * var)
{
   bool isSelector = false;
   SgType * var_type = var->get_type();

   // see if var is already in selector list
   //
   //   for (int i = 0; i < selectors.size(); i++) {
   //      if (var->get_name().getString() == selectors[i]->get_name().getString()) {
   //         return true;
   //      }
   //   }

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
      /* SgStatementPtrList & stmts = */ src_func_decl->get_definition()->get_body()->getStatementList();
   }

   return isSelector;
}
