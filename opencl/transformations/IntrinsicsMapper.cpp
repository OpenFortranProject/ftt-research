#include "IntrinsicsMapper.hpp"
#include "Util.hpp"

IntrinsicsMapper::IntrinsicsMapper()
{
   // Actually, for ABS/MIN/MAX we may need to
   // look at type information
   substitutions["ABS"]  = "fabs";
   substitutions["MAX"]  = "max";
   substitutions["MIN"]  = "min";
   substitutions["SIGN"] = "copysign";
}

IntrinsicsMapper::To_t IntrinsicsMapper::map(IntrinsicsMapper::From_t from, SgScopeStatement * const scope) const
{
   /* Get the function call name and look it up in the substitutions */
   //SgFunctionSymbol * const funSym = from->getAssociatedFunctionSymbol();
   const SgExpression     * const func = from->get_function();
   const SgFunctionRefExp * const fref = isSgFunctionRefExp(func);

   if( fref == NULL ) {
      dout << "Can't get function reference expression" << std::endl;
      return NULL; // Not much we can do if we can't tell what function this is
   }
   const std::string name(fref->get_symbol()->get_name().str());

   dout << "Found function '" << name << "'" << std::endl;
   if( substitutions.find(name) != substitutions.end() ) {
      dout << "Found matching substitution: " << substitutions.find(name)->second << std::endl;
      const SgFunctionRefExp * const c_fref = buildFunctionRefExp(SgName(substitutions.find(name)->second), scope);
      ROSE_ASSERT( c_fref != NULL );
      SgExpression * const c_callExp = buildFunctionCallExp(c_fref->get_symbol(), from->get_args());
      ROSE_ASSERT( c_callExp != NULL );
      return c_callExp;
   } else if( name == "MERGE" ){
      return mapMergeIntrinsic(from);
   }
   dout << "No substitution for: " << name << std::endl;
   return NULL;
}

// This is specifically for mapping the fortran intrinsic MERGE to C's ternary op:
// MERGE(A,B,C) == C ? A : B;
IntrinsicsMapper::To_t IntrinsicsMapper::mapMergeIntrinsic(IntrinsicsMapper::From_t from) const
{
   /* Get the function call name and look it up in the substitutions */
   //SgFunctionSymbol * const funSym = from->getAssociatedFunctionSymbol();
   const SgExpression     * const func = from->get_function();
   const SgFunctionRefExp * const fref = isSgFunctionRefExp(func);

   if( fref == NULL ) {
      dout << "Can't get function reference expression" << std::endl;
      return NULL; // Not much we can do if we can't tell what function this is
   }
   const std::string name(fref->get_symbol()->get_name().str());
   if( name != "MERGE" ) return NULL; // bail out early if we are called on the wrong function call expression

   dout << "Found function '" << name << "'" << std::endl;
   const SgExpressionPtrList & expList = from->get_args()->get_expressions();
   ROSE_ASSERT( expList.size() == 3 );
   
   SgExpression * const c_expr = buildConditionalExp(expList[2], expList[0], expList[1]);
   ROSE_ASSERT( c_expr != NULL );
   return c_expr;
}
