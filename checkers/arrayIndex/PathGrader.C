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
  ROSE_ASSERT( score < UINT_MAX );
}

int PathGrader::getNumberOfPaths() const
{
  return numPaths;
}

int PathGrader::getScore() const
{
  return score;
}

//    // checkRHS does not necessary check rhs operand, it can check against any operand
//    CheckLevel* checkRHS(SgExpression* const switch_exp, const int d_check_flag,
//                         const int d_level,  const int array_size, CheckLevel * const cl,
//                         const std::string a_name ) {
//      int c_check_flag;
//      int c_level;
//      c_check_flag  = d_check_flag;
//      c_level = d_level;
//
//      switch(switch_exp->variantT()) {
//      case V_SgIntVal: {  //  IF ( I < 5 ) , rhs_node is SgIntVal
//        const SgIntVal* const int_val = isSgIntVal(switch_exp);
//        c_check_flag = 1;
//        c_level      = 1;
//        if (int_val->get_value() <= array_size) {
//          cl->setFlag(c_check_flag,c_level);
//        } else {
//          cl->setFlag(c_check_flag,c_level);
//          cl->warn = ", Warning: value may  exceed array size " ;
//        }
//      }
//      break;
//
//      case V_SgVarRefExp: { // IF (I < IMAX)
//        c_check_flag = 1;
//        c_level      = 2;
//        cl->setFlag(c_check_flag,c_level);
//        SgVarRefExp* const var_node = isSgVarRefExp(switch_exp);
//        backCFG(var_node, cl);
//      }
//      break;
//      case V_SgFunctionCallExp: { // IF ( I < SIZE(A)) , then rhs_node is SgFunctionCallExp
//        const SgFunctionCallExp* const rhs_func_call_exp = isSgFunctionCallExp(switch_exp);
//        const SgFunctionRefExp* const rhs_func = isSgFunctionRefExp(rhs_func_call_exp->get_function());
//        if (rhs_func) {
//          // check if func is size()
//          if (rhs_func->get_symbol()->get_name().getString().compare("SIZE") == 0 ||
//              rhs_func->get_symbol()->get_name().getString().compare("size") == 0) {
//            // check if the array name is size() is correct
//            const SgExprListExp* const args_list = isSgExprListExp((rhs_func_call_exp)->get_args());
//            const SgExpressionPtrList list = args_list->get_expressions();
//            for (SgExpressionPtrList::const_iterator i = list.begin(); i != list.end(); i++) {
//              if (isSgVarRefExp(*i)) {
//                //std::string fun_array_name;
//                const SgInitializedName* const init_name = isSgVarRefExp(*i)->get_symbol()->get_declaration();
//                const std::string fun_array_name = init_name->get_name().getString();
//                if (fun_array_name == a_name) {
//                  c_check_flag = 1;
//                  c_level      = 3;
//                  cl->setFlag(c_check_flag,c_level);
//                } else {
//                  c_check_flag = 1;
//                  c_level      = 3;
//                  cl->setFlag(c_check_flag,c_level);
//                  cl->warn = ", Warning: array name is not correct. " ;
//                }
//              }
//            }
//          }
//        }
//      }
//      break;
//      default: {
//        c_check_flag = 1;
//        c_level      = 2;
//        cl->setFlag(c_check_flag,c_level);
//        cl->warn = ", Warning: checked against a non-simple DO bound expression. " ;
//        // std::cout << "\tcheckRHS: " << switch_exp->class_name() << " not handled"<< std::endl;
//      }
//      break;
//      } // end switch
//      return cl;
//    }

    // this is the old version and it is going away real soon now...
//    void checkIndex(SgNode* const node, const std::string index_name, int check_flag, int level,
//                    int array_dimension, const std::string array_name, Compass::OutputObject& output ) {
//      //std::cout << "   Start to walk backward CFG..." << std::endl;
//
//      //traverse cfg BK  and find next assign node involving index i
//      vector<FilteredCFGNode<IsDFAFilter> > worklist;
//      vector<FilteredCFGNode<IsDFAFilter> > visited;
//
//      // add this node to worklist and workthrough the outgoing edges
//      FilteredCFGNode < IsDFAFilter > source =
//        FilteredCFGNode < IsDFAFilter > (node->cfgForBeginning());
//      worklist.push_back(source);
//
//      const int line_number_array = node->get_file_info()->get_line();
//      int line_number_cfg   = 0;  //line number of the associated code for this current cfg node
//      int line_number_check = 0;  // line number that index variable may be checked
//      std::string   reason    = "";
//      std::string   reason_error = "";
//
//      while (!worklist.empty() ) {
//        source = worklist.front();
//        worklist.erase(worklist.begin());
//
//        const SgNode* const next = source.getNode();
//
//        // Find previous IF statement or WHILE statement
//        const SgIfStmt*    const ifstmt    = isSgIfStmt(next);
//        const SgWhileStmt* const whilestmt = isSgWhileStmt(next);
//        const SgFortranDo* const dostmt    = isSgFortranDo(next);
//        const SgAssignOp*  const assignop  = isSgAssignOp(next);
//
//        // debug
//#ifdef _DEBUG
//        std::cout << "debug:current line: " << line_number_array << " node:" << node->class_name() << std::endl;
//        std::cout << "debug:next node " << next->class_name() << " line:"
//                  << next->get_file_info()->get_line()
//                  << "." << next->get_file_info()->get_col() << std::endl;
//#endif
//
//        // index  J reassigned a value used in DO bound before referenced
//        // DO I= ...I++
//        //    J = I - 1
//        //    A(J) = ...
//        if (assignop != NULL ) {
//          const SgVarRefExp* const assignop_lhs_var = isSgVarRefExp(assignop->get_lhs_operand());
//          if (assignop_lhs_var) {
//            const SgVariableSymbol* const assignop_lhs_var_symbol = assignop_lhs_var->get_symbol();
//            const std::string assignop_lhs_var_symbol_name = assignop_lhs_var_symbol->get_name().getString();
//            if (assignop_lhs_var_symbol_name.compare(index_name) == 0 ) {
//
//              line_number_check = assignop->get_file_info()->get_line();
//              check_flag = 1;
//              level      = 1;
//              reason = "level-"  + to_string(level) + " found for "+ node->unparseToString()
//                       + ", checked on line "
//                       +  to_string(line_number_check)  ;
//              output.addOutput(new CheckerOutput(node,reason));
//              return;
//
//            }
//          }
//        }
//
//        // Get condition in IF or WHILE
//        SgStatement* condition = NULL;
//        if (ifstmt) {
//          condition = ifstmt->get_conditional();
//        }
//        if (whilestmt) {
//          condition = whilestmt->get_condition();
//        }
//
//        // Check whether index been checked in the condition
//        if (condition ) {
//          // query subtree of node ifstmt_cond for SgVarRefExp
//          const Rose_STL_Container<SgNode*> returns = NodeQuery::querySubTree (condition,V_SgVarRefExp);
//          if (!returns.empty()) {
//            for (Rose_STL_Container<SgNode*>::const_iterator i = returns.begin(); i != returns.end(); i++) {
//              // for "IF (I < XXX )", lhs of condition is SgVarRefExp
//              const SgVarRefExp* const var_ref_exp = isSgVarRefExp(*i);
//
//              line_number_check = var_ref_exp->get_file_info()->get_line();
//
//              const SgVariableSymbol* const var_symbol = var_ref_exp->get_symbol();
//              const std::string var_symbol_name = var_symbol->get_name().getString();
//
//              // variable name in IF is the same as index name
//              if (var_symbol_name.compare(index_name) == 0 ) {
//                // condition shall be a binary operator
//                // and the rhs of condition could be
//                //   1. SgIntVal , like ( I <5 ), or
//                //   2. SgVarRefExp , like (I < IMAX) ,or
//                //   3. SgFunctionCallExp, like (I < SIZE(A))
//                const SgNode* const parent = var_ref_exp->get_parent();
//                const SgBinaryOp* const parent_binary_op = isSgBinaryOp(parent);
//                if (isSgBinaryOp(parent)) {
//                  SgExpression* exp_check = NULL;
//
//                  const SgExpression* const rhs_operand = parent_binary_op->get_rhs_operand();
//                  const SgVarRefExp* const rhs_operand_var = isSgVarRefExp(rhs_operand);
//
//                  if (( rhs_operand_var != NULL ) && (rhs_operand_var == var_ref_exp ))   // i in rhs , then we need check lhs
//                    exp_check = parent_binary_op->get_lhs_operand();
//                  else   // i in lhs like I<5, then we need to check RHS
//                    exp_check = parent_binary_op->get_rhs_operand();
//
//                  CheckLevel* const cl = new CheckLevel(check_flag, level, index_name);
//                  // Check operand expression is a value, var , or size()
//                  const CheckLevel* const return_cl = checkRHS(exp_check,check_flag,level, array_dimension,cl,array_name);
//                  check_flag = return_cl->check_flag;
//                  level      = return_cl->level;
//
//                  reason = "level-"  + to_string(level) + " found for "+ node->unparseToString()
//                           + ", checked on line "
//                           +  to_string(line_number_check) + return_cl->warn ;
//
//                  output.addOutput(new CheckerOutput(node,reason));
//                  return;
//                }
//              }
//            }  // end  for loop
//          }  // end if condition subtree is not empty
//        }
//        // SgFortranDo statement
//        if (dostmt) {
//          // examine DO init expression
//          const SgAssignOp* const do_init_exp = isSgAssignOp(dostmt->get_initialization());
//          if (do_init_exp) {
//            const SgVarRefExp* const do_var = isSgVarRefExp(do_init_exp->get_lhs_operand());
//            line_number_check = do_var->get_file_info()->get_line();
//
//            if (do_var) {
//              const SgVariableSymbol* const do_var_symbol = do_var->get_symbol();
//              const std::string do_var_symbol_name = do_var_symbol->get_name().getString();
//
//              // index appear in DO init expression
//              if (do_var_symbol_name.compare(index_name) == 0 ) {
//                // examine DO bound expression
//                SgExpression* const do_bound_exp = dostmt->get_bound();
//                CheckLevel* const cl = new CheckLevel(check_flag, level, index_name);
//                const CheckLevel* const return_cl = checkRHS(do_bound_exp,check_flag, level, array_dimension,cl,array_name);
//                check_flag = return_cl->check_flag;
//                level      = return_cl->level;
//                reason = "level-"  + to_string(level) + " found for "+ node->unparseToString()
//                         + ", index " + index_name + " checked on line "
//                         +  to_string(line_number_check) + return_cl->warn ;
//
//                output.addOutput(new CheckerOutput(node,reason));
//                return;
//              }
//            }
//          }
//        }
//
//        // Not found any check yet, keep looking next edge and node
//        // Find the next  edge to walk backward
//        if ( check_flag == 0 ) {
//          const vector<FilteredCFGEdge < IsDFAFilter > > in_edges = source.inEdges();
//          for (vector<FilteredCFGEdge <IsDFAFilter> >::const_iterator i = in_edges.begin(); i != in_edges.end(); ++i) {
//            const FilteredCFGEdge<IsDFAFilter> filterEdge = *i;
//            const FilteredCFGNode<IsDFAFilter> filterNode = filterEdge.source();
//
//            const SgNode* const inedge_source_node = filterNode.getNode();
//
//            // check if the node has been visited yet
//            if (find(visited.begin(), visited.end(), filterNode)==visited.end()) {
//              line_number_cfg = inedge_source_node->get_file_info()->get_line();
//
//              // put the node in the list if the line number of underlying node less then the line number of array referred
//              if ( line_number_cfg <= line_number_array ) {
//                worklist.push_back(filterNode);
//                visited.push_back(filterNode);
//              }// end of if line_number_cfg
//              else {
//                //std::cout << "\t\t             *****Discard:line number exceed ******"<< std::endl;
//              }
//            }
//          } // end of for
//        } // end of if check_flag
//      }// end of while
//
//      if ( check_flag == 0 ) {
//        reason = "Warning:level-"  + to_string(level) + " for "+ node->unparseToString()
//                 + " index " + index_name + " : No bound check";
//
//        output.addOutput(new CheckerOutput(node,reason));
//      }
//      return;
//    }

// Make a version of checkNode for each Sg type we care about:
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
          Compass::OutputObject & output)
{
  std::cout << "Calling " << __PRETTY_FUNCTION__ << std::endl;
  std::cout << "Initialization is: " << dominator.get_initialization()->unparseToString() << std::endl;
  std::cout << "Bound is: " << dominator.get_bound()->unparseToString() << std::endl;
  return 0;
}

unsigned int
PathGrader::
checkNode(const SgNode& var,
          SgNode& node,
          const SgWhileStmt & dominator,
          Compass::OutputObject & output)
{
  std::cout << "Calling " << __PRETTY_FUNCTION__ << std::endl;
  return 0;
}

unsigned int
PathGrader::
checkNode(const SgNode& var,
          SgNode& node,
          const SgIfStmt & dominator,
          Compass::OutputObject & output)
{
  std::cout << "Calling " << __PRETTY_FUNCTION__ << std::endl;
  std::cout << "Conditional is: " << dominator.get_conditional()->unparseToString() << std::endl;

  return 0;
}

unsigned int
PathGrader::
checkNode(const SgNode& var,
          SgNode& node,
          const SgSwitchStatement & dominator,
          Compass::OutputObject & output)
{
  std::cout << "Calling " << __PRETTY_FUNCTION__ << std::endl;
  return 0;
}

unsigned int
PathGrader::
checkNode(const SgNode& var,
          SgNode& node,
          const SgNode & dominator,
          Compass::OutputObject & output)
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
    return checkNode(var, node, t##vref, output);               \
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

  std::cout << "Calling " << __PRETTY_FUNCTION__ << std::endl;

  return 0;
}

//unsigned int
//PathGrader::
//checkNode(const SgVariableSymbol* const var,
//          SgNode* const node,
//          const SgNode * const dominator,
//          Compass::OutputObject & output)
//{
//  // TODO: FIXME
//  // This whole function is sketchy-ville. This should be rewritten from
//  // scratch to match the specification.
//  std::cout << "checkNode: " << dominator->sage_class_name() << std::endl;
//  // Find previous IF statement or WHILE statement
//  if( isSgIfStmt(dominator) ){
////    std::cout << "\tDominator " << dominator->sage_class_name() << ": "
////              << dominator->unparseToString() << std::endl
////              << "\tArrRef " << node->sage_class_name() << ": "
////              << node->unparseToString() << std::endl
////              << "\tIndex " << var->get_name().getString() << std::endl;
//    const SgIfStmt*    const ifstmt    = isSgIfStmt(dominator);
//// START COPIED
//    // Get condition in IF
//    SgStatement* condition = ifstmt->get_conditional();
//    ROSE_ASSERT( condition != NULL );
//    // query subtree of node ifstmt_cond for SgVarRefExp
//    const Rose_STL_Container<SgNode*> returns = NodeQuery::querySubTree (condition,V_SgVarRefExp);
//    if (!returns.empty()) {
//      for (Rose_STL_Container<SgNode*>::const_iterator i = returns.begin(); i != returns.end(); i++) {
//        // for "IF (I < XXX )", lhs of condition is SgVarRefExp
//        const SgVarRefExp* const var_ref_exp = isSgVarRefExp(*i);
//
//        int line_number_check = var_ref_exp->get_file_info()->get_line();
//
//        const SgVariableSymbol* const var_symbol = var_ref_exp->get_symbol();
//        const std::string var_symbol_name = var_symbol->get_name().getString();
//
//        // variable name in IF is the same as index name
//        if (var_symbol_name.compare(index_name) == 0 ) {
//          // condition shall be a binary operator
//          // and the rhs of condition could be
//          //   1. SgIntVal , like ( I <5 ), or
//          //   2. SgVarRefExp , like (I < IMAX) ,or
//          //   3. SgFunctionCallExp, like (I < SIZE(A))
//          const SgNode* const parent = var_ref_exp->get_parent();
//          const SgBinaryOp* const parent_binary_op = isSgBinaryOp(parent);
//          if (isSgBinaryOp(parent)) {
//            SgExpression* exp_check = NULL;
//
//            const SgExpression* const rhs_operand = parent_binary_op->get_rhs_operand();
//            const SgVarRefExp* const rhs_operand_var = isSgVarRefExp(rhs_operand);
//
//            if (( rhs_operand_var != NULL ) && (rhs_operand_var == var_ref_exp ))   // i in rhs , then we need check lhs
//              exp_check = parent_binary_op->get_lhs_operand();
//            else   // i in lhs like I<5, then we need to check RHS
//              exp_check = parent_binary_op->get_rhs_operand();
//
//            //CheckLevel* const cl = new CheckLevel(check_flag, level, index_name);
//            // Check operand expression is a value, var , or size()
//            //const CheckLevel* const return_cl = checkRHS(exp_check,check_flag,level, array_dimension,cl,array_name);
//            //check_flag = return_cl->check_flag;
//            //level      = return_cl->level;
//            // TODO: FIXME
//            level = 0;
//
//            const std::string reason = "level-"  + to_string(level) + " found for "+ node->unparseToString()
//                     + ", checked on line "
//                     +  to_string(line_number_check);
//
//            output.addOutput(new CheckerOutput(node,reason));
//            return level;
//          }
//        }
//      }  // end  for loop
//    }  // end if condition subtree is not empty
//// END COPIED
//  } else if( isSgWhileStmt(dominator) ){
////    std::cout << "\tDominator " << dominator->sage_class_name() << ": "
////              << dominator->unparseToString() << std::endl
////              << "\tArrRef " << node->sage_class_name() << ": "
////              << node->unparseToString() << std::endl
////              << "\tIndex " << var->get_name().getString() << std::endl;
//    const SgWhileStmt* const whilestmt = isSgWhileStmt(dominator);
//  } else if( isSgFortranDo(dominator) ){ 
////    std::cout << "\tDominator " << dominator->sage_class_name() << ": "
////              << dominator->unparseToString() << std::endl
////              << "\tArrRef " << node->sage_class_name() << ": "
////              << node->unparseToString() << std::endl
////              << "\tIndex " << var->get_name().getString() << std::endl;
//    const SgFortranDo* const dostmt    = isSgFortranDo(dominator);
//  } else if( isSgAssignOp(dominator) ){
//    // TODO: There is a pretty serious bug here actually.
//    //       The first node in the list seems to always be
//    //       the original assignment and we are mistakenly treating it as
//    //       providing evidence for a check. In reality it's things like,
//    //       "y = 2" which has no check, just assignment.
//    std::cout << "\tDominator " << dominator->sage_class_name() << ": "
//              << dominator->unparseToString() << std::endl
//              << "\tArrRef " << node->sage_class_name() << ": "
//              << node->unparseToString() << std::endl
//              << "\tIndex " << var->get_name().getString() << std::endl;
//    const SgAssignOp*  const assignop  = isSgAssignOp(dominator);
//    // TODO: Does this comment even makes sense? It came in from borrowed code.
//    // index  J reassigned a value used in DO bound before referenced
//    // DO I= ...I++
//    //    J = I - 1
//    //    A(J) = ...
//    const SgVarRefExp* const assignop_lhs_var = isSgVarRefExp(assignop->get_lhs_operand());
//    if( assignop_lhs_var == NULL ) return 0; // TODO: is this the right value to return?
//    const SgVariableSymbol* const assignop_lhs_var_symbol = assignop_lhs_var->get_symbol();
//    const std::string assignop_lhs_var_symbol_name = assignop_lhs_var_symbol->get_name().getString();
//    std::cout << "Assign var symbol name: " << assignop_lhs_var_symbol_name << std::endl;
//
//    // TODO: we're trying to determine if the index we want to use is the same one assigned here, we should
//    // actually be using the def-use information.
//    // Can we do a symbol table look up here using var and the lhs?
//    const std::string node_name = var->get_name().getString();
//    if( assignop_lhs_var_symbol_name.compare(node_name) != 0 ) return 0; // TODO: is this the right value to return?
//
//    const int         line_number_check = assignop->get_file_info()->get_line();
//    const int         check_flag        = 1;
//    const int         level             = 1;
//    const std::string reason            = "level "  + to_string(level) + " found for "+ node->unparseToString()
//                                        + ", checked on line "
//                                        +  to_string(line_number_check);
//    output.addOutput(new CheckerOutput(node,reason));
//    return 1; // TODO: ??
//
//  } else {
////    std::cout << "Dominating node of unexpected type " << dominator->sage_class_name() << ": ";
////    if( !isSgFunctionParameterList(dominator) ){
////      std::cout << dominator->unparseToString();
////    }
////    std::cout << std::endl;
//    return 0;
//  }
//  return 0;
//}

unsigned int
PathGrader::
checkIndex(const PathGrader::PathT& path){
  unsigned int best = 0;
  foreach(const Vertex& vert, path){
    const SgNode * n = cfg[vert]->getNode();
    ROSE_ASSERT( n != NULL );
    best = max(checkNode(this->var, node, *n, output), best);
  }
  return best;
}

