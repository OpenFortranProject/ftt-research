#include <climits>

#include "PathGrader.h"
#include "arrayIndex.h"
#include <boost/foreach.hpp>



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
  array_name(array_name), output(output)
{}

void PathGrader::analyzePath(PathT& path) {
  numPaths++;

  // We don't know which path will be taken at runtime so we (conservatively)
  // assume it will be the path with the weakest check. Therefore, this should be
  // the min of the paths
  score = min(score, checkIndex(path));
}

int PathGrader::getNumberOfPaths() const
{
  return numPaths;
}

int PathGrader::getScore() const
{
  return score;
}

unsigned int
PathGrader::
checkNode(const SgVariableSymbol* const var,
          SgNode* const node,
          const SgNode * const dominator,
          Compass::OutputObject & output)
{
  // Find previous IF statement or WHILE statement
  if( isSgIfStmt(dominator) ){
    std::cout << "\tDominator " << dominator->sage_class_name() << ": "
              << dominator->unparseToString() << std::endl
              << "\tArrRef " << node->sage_class_name() << ": "
              << node->unparseToString() << std::endl
              << "\tIndex " << var->get_name().getString() << std::endl;
    const SgIfStmt*    const ifstmt    = isSgIfStmt(dominator);
  } else if( isSgWhileStmt(dominator) ){
    std::cout << "\tDominator " << dominator->sage_class_name() << ": "
              << dominator->unparseToString() << std::endl
              << "\tArrRef " << node->sage_class_name() << ": "
              << node->unparseToString() << std::endl
              << "\tIndex " << var->get_name().getString() << std::endl;
    const SgWhileStmt* const whilestmt = isSgWhileStmt(dominator);
  } else if( isSgFortranDo(dominator) ){ 
    std::cout << "\tDominator " << dominator->sage_class_name() << ": "
              << dominator->unparseToString() << std::endl
              << "\tArrRef " << node->sage_class_name() << ": "
              << node->unparseToString() << std::endl
              << "\tIndex " << var->get_name().getString() << std::endl;
    const SgFortranDo* const dostmt    = isSgFortranDo(dominator);
  } else if( isSgAssignOp(dominator) ){
    std::cout << "\tDominator " << dominator->sage_class_name() << ": "
              << dominator->unparseToString() << std::endl
              << "\tArrRef " << node->sage_class_name() << ": "
              << node->unparseToString() << std::endl
              << "\tIndex " << var->get_name().getString() << std::endl;
    const SgAssignOp*  const assignop  = isSgAssignOp(dominator);
    // index  J reassigned a value used in DO bound before referenced
    // DO I= ...I++
    //    J = I - 1
    //    A(J) = ...
    const SgVarRefExp* const assignop_lhs_var = isSgVarRefExp(assignop->get_lhs_operand());
    if( assignop_lhs_var == NULL ) return 0; // TODO: is this the right value to return?
    const SgVariableSymbol* const assignop_lhs_var_symbol = assignop_lhs_var->get_symbol();
    const std::string assignop_lhs_var_symbol_name = assignop_lhs_var_symbol->get_name().getString();
    std::cout << "Assign var symbol name: " << assignop_lhs_var_symbol_name << std::endl;

    // TODO: we're trying to determine if the index we want to use is the same one assigned here, we should
    // actually be using the def-use information.
    // Can we do a symbol table look up here using var and the lhs?
    const std::string node_name = var->get_name().getString();
    if( assignop_lhs_var_symbol_name.compare(node_name) != 0 ) return 0; // TODO: is this the right value to return?

    const int         line_number_check = assignop->get_file_info()->get_line();
    const int         check_flag        = 1;
    const int         level             = 1;
    const std::string reason            = "level "  + to_string(level) + " found for "+ node->unparseToString()
                                        + ", checked on line "
                                        +  to_string(line_number_check);
    output.addOutput(new CheckerOutput(node,reason));
    return 1; // TODO: ??
  } else {
    std::cout << "Dominating node of unexpected type " << dominator->sage_class_name() << ": ";
    if( !isSgFunctionParameterList(dominator) ){
      std::cout << dominator->unparseToString();
    }
    std::cout << std::endl;
    return 0;
  }
  return 0;
}

unsigned int
PathGrader::
checkIndex(const PathGrader::PathT& path){
  unsigned int best = 0;
  foreach(const Vertex& vert, path){
    const SgNode * n = cfg[vert]->getNode();
    best = max(checkNode(&var, &node, n, output), best);
  }
  return best;
}

