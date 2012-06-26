#include <climits>

#include "PathGrader.h"
#include "arrayIndex.h"
#include <boost/foreach.hpp>
#include "sageInterface.h"



#define foreach BOOST_FOREACH

using namespace CompassAnalyses::ArrayIndex;

PathGrader::PathGrader(const ControlFlowGraph& cfg,
                       const SgVariableSymbol& var,
                       SgNode& node,
                       const std::string& index_name,
                       int check_flag, int level, int array_dimension,
                       const std::string& array_name,
                       Compass::OutputObject& output)
: score(UINT_MAX), numPaths(0), cfg(cfg), var(var), node(node), index_name(index_name),
  check_flag(check_flag), level(level), array_dimension(array_dimension),
  array_name(array_name), output(output), intrinsics()
{
  intrinsics.push_back(SgName("size"));
}

void PathGrader::analyzePath(PathT& path) {
  numPaths++;

  // We don't know which path will be taken at runtime so we (conservatively)
  // assume it will be the path with the weakest check. Therefore, this should be
  // the min of the paths
  int checkedOnLine = 0;
  score = min(score, checkIndex(path, checkedOnLine));
  ROSE_ASSERT( score < UINT_MAX );
  const std::string reason = "level-"  + to_string(score) + " found for "+ node.unparseToString()
           + ", checked on line "
           + to_string(checkedOnLine);
  
  output.addOutput(new CheckerOutput(&node,reason));
}

int PathGrader::getNumberOfPaths() const
{
  return numPaths;
}

int PathGrader::getScore() const
{
  return score;
}

// checkNode for each Sg type we care about:
//   * SgFortranDo
//   * SgIfStmt
//   * SgWhileStmt
//   * SgSwitchStatement
//

unsigned int
PathGrader::
checkNode(const SgNode& var,
          SgNode& node,
          const SgFortranDo & dominator,
          int & checkedOnLine)
{
  std::cout << "Calling " << __PRETTY_FUNCTION__ << std::endl;
  std::cout << "Initialization is: " << dominator.get_initialization()->unparseToString() << std::endl;
  std::cout << "Bound is: " << dominator.get_bound()->unparseToString() << std::endl;
  const SgExpression * const expr = dominator.get_bound();

  return checkConditional(expr);
}

unsigned int
PathGrader::
checkNode(const SgNode& var,
          SgNode& node,
          const SgWhileStmt & dominator,
          int & checkedOnLine)
{
  std::cout << "Calling " << __PRETTY_FUNCTION__ << std::endl;
  const SgStatement * const condition = dominator.get_condition();
  if( !isSgExprStatement( condition ) ) return 0;
  const SgExprStatement * const exprStmt = dynamic_cast<const SgExprStatement * const>(condition);
  ROSE_ASSERT( exprStmt != NULL );
  const SgExpression * const expr = exprStmt->get_expression();
 
  return checkConditional(expr);
}

unsigned int
PathGrader::
checkNode(const SgNode& var,
          SgNode& node,
          const SgIfStmt & dominator,
          int & checkedOnLine)
{
  std::cout << "Calling " << __PRETTY_FUNCTION__ << std::endl;
  const SgStatement * const conditional = dominator.get_conditional();
  std::cout << "Conditional is: " << conditional->unparseToString() << std::endl;
  if( !isSgExprStatement( conditional ) ) return 0;
  const SgExprStatement * const exprStmt = dynamic_cast<const SgExprStatement * const>(conditional);
  ROSE_ASSERT( exprStmt != NULL );
  const SgExpression * const expr = exprStmt->get_expression();

  return checkConditional(expr);
}

unsigned int
PathGrader::
checkNode(const SgNode& var,
          SgNode& node,
          const SgSwitchStatement & dominator,
          int & checkedOnLine)
{
  std::cout << "Calling " << __PRETTY_FUNCTION__ << std::endl;
  return 0;
}

unsigned int
PathGrader::
checkNode(const SgNode& var,
          SgNode& node,
          const SgNode & dominator,
          int & checkedOnLine)
{

// Why do we manually dispatch on the type here? Due to C++'s static resolution
// of overloading we would need to use the visitor pattern for the overloading
// to work. While we could use the ROSE_VisitorPattern class instead that
// seemed overly complicated considering we only care about 4 subtypes of
// SgNode. This compromise seems to be in the sweet spot for trading crimes
// against C++ with economy of boiler plate.

#define DISPATCH(t, v)                                          \
  case (V_##t): {                                               \
    const t * const t##v = dynamic_cast<const t * const>(&(v)); \
    ROSE_ASSERT( t##v != NULL );                                \
    const t & t##vref = *(t##v);                                \
    checkedOnLine = dominator.get_file_info()->get_line();      \
    return checkNode(var, node, t##vref, checkedOnLine);        \
  }

  switch( dominator.variantT() ){
    DISPATCH(SgSwitchStatement, dominator);
    DISPATCH(SgIfStmt,          dominator);
    DISPATCH(SgFortranDo,       dominator);
    DISPATCH(SgWhileStmt,       dominator);
    default: {
      //std::cout << "Calling " << __PRETTY_FUNCTION__ << std::endl;
      // default score
      return 0;
    }
  }

#undef DISPATCH

  return 0;
}

int
PathGrader::
checkConditional(const SgExpression * const expr)
{
  if( isSgBinaryOp(expr) ){
    const SgBinaryOp * const binOpExpr = dynamic_cast<const SgBinaryOp * const>(expr);
    ROSE_ASSERT( binOpExpr != NULL );
    // 1. Get LHS
    SgExpression * const lhs = binOpExpr->get_lhs_operand();
    // 2. Get RHS
    SgExpression * const rhs = binOpExpr->get_rhs_operand();
    SgExpression * varSide   = NULL;
    SgExpression * otherSide = NULL;
    // isIdxOnOneSide?
    // if !isIdxOnOneSide -> 0
    if( hasIndexVar(lhs, &var) && !hasIndexVar(rhs, &var) ){
      varSide   = lhs;
      otherSide = rhs;
    } else if( !hasIndexVar(lhs, &var) && hasIndexVar(rhs, &var) ){
      varSide   = rhs;
      otherSide = lhs;
    } else {
      // We must not support this conditional, best to stop now
      return 0;
    }
    // else if isOtherSideLiteralConstant
    //   True  -> 1
    //   False -> if isOtherSideIntrinsic
    //     True  -> 3
    //     False -> 2
    if( isLiteralConstant(otherSide) ){
      return 1;
    } else if( isIntrinsic(otherSide) ){
      return 3;
    } else {
      return 2;
    }
  }
  return 0;
}

unsigned int
PathGrader::
checkIndex(const PathGrader::PathT& path, int & checkedOnLine){
  unsigned int best = 0;
  foreach(const Vertex& vert, path){
    const SgNode * n = cfg[vert]->getNode();
    ROSE_ASSERT( n != NULL );
    int currentCheckedOn = 0;
    unsigned int current = checkNode(this->var, node, *n, currentCheckedOn);
    if( current > best ){
      best = current;
      checkedOnLine = currentCheckedOn;
    }
  }
  return best;
}

bool
PathGrader::
hasIndexVar(SgExpression * const expr, const SgNode * const var){
// This is what it would look like if we looked at the whole subtree for a VarRefExp
//  const Rose_STL_Container<SgNode *> refs = NodeQuery::querySubTree (expr,V_SgVarRefExp);
//  foreach(SgNode * ref, refs){
//    const SgVarRefExp * const refExp = dynamic_cast<const SgVarRefExp * const>(ref);
//    ROSE_ASSERT( refExp != NULL );
//    if( refExp->get_symbol() == var ){
//      return true;
//    }
//  }
//  return false;

  if( !isSgVarRefExp(expr) ) return false;
  
  const SgVarRefExp * const refExp = dynamic_cast<const SgVarRefExp * const>(expr);
  ROSE_ASSERT( refExp );

  if( refExp->get_symbol() == var ){
    return true;
  }

  return false;
}

bool
PathGrader::
isLiteralConstant(const SgExpression * const expr){
  return !!isSgValueExp(expr);
}

bool
PathGrader::
isIntrinsic(const SgExpression * const expr){
  if( !isSgFunctionCallExp(expr) ) return false;

  const SgFunctionCallExp * const funCall = dynamic_cast<const SgFunctionCallExp * const>(expr);
  ROSE_ASSERT( funCall != NULL );
  const SgFunctionSymbol * const funSym = funCall->getAssociatedFunctionSymbol();
  if( funSym == NULL) return false;
  
  SgName funName = funSym->get_name();
  foreach(const SgName intrinsic, intrinsics){
    if( SgName::caseInsensitiveEquality(funName, intrinsic) ) return true;
  }

  return false; // TODO: don't just return false
}
