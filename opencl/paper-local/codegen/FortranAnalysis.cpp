#include "FortranAnalysis.hpp"

#define DEBUG_PRINT

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
   case V_SgVarRefExp:
      SgVarRefExp * var = isSgVarRefExp(node);
      if (var->get_symbol()->getAttribute("halo_attr") != NULL) {
         found = true;
      }
      break;
   }
}


FortranAnalysis::FortranAnalysis(SgGlobal * scope)
{
   this->src_global_scope = scope;
   this->last_halo_symbol = NULL;
   this->first_array_dummy_ref = NULL;

   this->num_arrays = 0;
   this->array_list = NULL;
}

FortranAnalysis::FortranAnalysis(SgGlobal * scope, const char * arrays)
{
   int i = 0;
   num_arrays = 0;

   this->src_global_scope = scope;
   this->last_halo_symbol = NULL;
   this->first_array_dummy_ref = NULL;

   if (strncmp(arrays, "-ofp:", 5) == 0) {
      char * str = strdup(&arrays[5]);
      char * chrp = str;

      if (*chrp != '\0') num_arrays = 1;
      while (*chrp != '\0') {
         if (*chrp++ == ' ') num_arrays += 1;
      } 

      array_list = new char * [num_arrays];

      chrp = str;
      array_list[i++] = str;
      while (*chrp != '\0') {
         if (*chrp == ' ') {
            *chrp = '\0';
            array_list[i++] = chrp + 1;
         } 
         chrp += 1;
      } 
   }
   else {
      array_list = NULL;
   }
}

FortranAnalysis::~FortranAnalysis()
{
   if (array_list != NULL) delete [] array_list;
}

void FortranAnalysis::visit(SgNode * node)
{
   switch (node->variantT())
   {
     case V_SgFunctionDeclaration :  visit( (SgFunctionDeclaration *) node);  break;
     case V_SgFunctionDefinition  :  visit( (SgFunctionDefinition *) node);  break;
#ifdef NOTME
     case V_SgVariableDeclaration :  visit( (SgVariableDeclaration *) node);  break;
     case V_SgFunctionCallExp     :  visit( (SgFunctionCallExp     *) node);  break;
     case V_SgExprStatement       :  visit( (SgExprStatement       *) node);  break;
#endif
   }
}

void FortranAnalysis::visit(SgFunctionDeclaration * func_decl)
{
#ifdef DEBUG_PRINT
   printf("FortranAnalysis::visit(SgFunctionDeclaration *)\n");
#endif
}

void FortranAnalysis::visit(SgFunctionDefinition * func_def)
{
   SgFunctionDeclaration * func_decl = isSgFunctionDeclaration(func_def->get_declaration());
   if (func_decl == NULL) return;

   SgInitializedNamePtrList func_args = func_decl->get_parameterList()->get_args();
   SgInitializedNamePtrList::iterator arg = func_args.begin();

   //   for (it_args = func_args.begin(); it_args != func_args.end(); it_args++) {
   while (arg != func_args.end()) {
      SgInitializedName * func_arg = isSgInitializedName(*arg++);
      SgSymbol * sym = func_def->lookup_symbol(func_arg->get_name());
      SgType * type = sym->get_type();
      if (sym == NULL) {
         printf("FortranAnalysis::visit: no symbol for name %s\n",
                func_arg->get_name().getString().c_str());
      }
      else if (isSgArrayType(type) != NULL) {
         sym->setAttribute("dummy_attr", new AstTextAttribute("DUMMY_ARRAY_ARG"));
      }
      else {
         sym->setAttribute("dummy_attr", new AstTextAttribute("DUMMY_ARG"));
      }

      if (sym != NULL && isElementalArrayType(func_arg)) {
         sym->setAttribute("extended_attr", new AstTextAttribute("EXTENDED_ARRAY"));
      }
   }
}

void FortranAnalysis::visit(SgVariableDeclaration * var_decl)
{
#ifdef DEBUG_PRINT
   printf("FortranAnalysis::visit(SgVariableDeclaration *)\n");
#endif
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
         printf("SgFunctionCallExp: adding halo attribute to %s\n",
                sym->get_name().getString().c_str());
      }
   }
}

/**
 * Matches assignment statements (including pointer association)
 */
void FortranAnalysis::visit(SgExprStatement * expr_stmt)
{
   if (matchRegionAssignment(expr_stmt)) {
      SgBinaryOp * bin_op = isSgBinaryOp(expr_stmt->get_expression());
      SgVarRefExp * var = isSgVarRefExp(bin_op->get_lhs_operand());
      if (var == NULL) return;
      var->get_symbol()->setAttribute("halo_attr", new AstTextAttribute("HALO_VAR"));
      printf("FortranAnalysis:: adding halo attr to %s\n",
             var->get_symbol()->get_name().getString().c_str());
   }
   else if (HaloRefSearch::findHaloRef(expr_stmt)) {
      expr_stmt->setAttribute("halo_ref", new AstTextAttribute("HAS_HALO_REF"));
      printf("FortranAnalysis:: adding halo attr to statement\n");
   }
}

void FortranAnalysis::atTraversalEnd()
{
   //printf("FortranAnalysis::atTraversalEnd\n");
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
      SgStatementPtrList & stmts = src_func_decl->get_definition()->get_body()->getStatementList();
   }

   return isSelector;
}

bool FortranAnalysis::isElementalArrayType(SgInitializedName * iname)
{
   for (int i = 0; i < num_arrays; i++) {
      if (strcmp(iname->get_name().str(), array_list[i]) == 0) {
         return true;
      }
   }
   return false;
}

bool FortranAnalysis::isElementalArrayType(SgFunctionDeclaration * func_decl)
{
   return true;
}
