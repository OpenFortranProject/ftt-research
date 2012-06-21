// -*- mode: C++; c-basic-offset: 2; indent-tabs-mode: nil -*-
// vim: expandtab:shiftwidth=2:tabstop=2

#include "arrayIndex.h"

namespace CompassAnalyses {
  namespace ArrayIndex {
    SgExpressionPtrList getIndexExpressions(const SgPntrArrRefExp & pntr) {
      const SgExprListExp* const exp_r = isSgExprListExp(pntr.get_rhs_operand());
      // TODO: check for null
      return exp_r->get_expressions();
    }

    // TODO: keep this function
    int findArraySize(const SgArrayType* const atype, const std::string array_name) {
      int array_dimension = 0;  // IF array size cannot be determinded, return 0 as default
      int array_dimension_lowerbound, array_dimension_upperbound;  // DIMENSION(-2:3)
      if (atype) {
        const SgExprListExp* const dim = atype->get_dim_info();
        const SgExpressionPtrList sig = dim->get_expressions();
        for (SgExpressionPtrList::const_iterator i = sig.begin(); i != sig.end(); i++) {
          switch((*i)->variantT()) {
          case V_SgIntVal:
            // DIMENSION(3) :: X
          {
            const SgIntVal* const dim_value = isSgIntVal(*i);
            array_dimension = dim_value->get_value();
          }
          break;

          case V_SgVarRefExp:
            // DIMENSION(N) :: X
          {
            const SgVarRefExp*         const var_exp  = isSgVarRefExp(*i);
            const SgVariableSymbol*    const var_sym  = var_exp->get_symbol();
            const SgInitializedName*   const var_init = var_sym->get_declaration();
            const SgInitializer*       const init     = var_init->get_initializer();
            const SgAssignInitializer* const ainit    = isSgAssignInitializer(init);
            if (ainit) {
              // find initialized  value (array dimension value)
              const SgIntVal* const int_value = isSgIntVal(ainit->get_operand());
              if (int_value) {
                array_dimension = int_value->get_value();
              }
            }
          }
          break;

          // DIMENSION(-2:2) :: X
          // TBD : modify for SgMinusOp , operand is 2, but need to show value -2
          case V_SgSubscriptExpression: {
            const SgSubscriptExpression* const subscript = isSgSubscriptExpression(*i);
            const SgExpression* const l_exp = subscript->get_lowerBound();
            const SgExpression* const u_exp = subscript->get_upperBound();
            // the lower bound could be negative like A(-2:2)
            if (isSgUnaryOp(l_exp)) {
              const SgUnaryOp* const l_bound = isSgUnaryOp(l_exp);
              const SgIntVal* const l_bound_operand = isSgIntVal(l_bound->get_operand());
              array_dimension_lowerbound = l_bound_operand->get_value();

            } else if (isSgIntVal(l_exp)) {
              const SgIntVal* const l_val = isSgIntVal(l_exp);
              array_dimension_lowerbound = l_val->get_value();

            }
            if (isSgUnaryOp(u_exp)) {
              const SgUnaryOp* const u_bound = isSgUnaryOp(u_exp);
              const SgIntVal* const u_bound_operand = isSgIntVal(u_bound->get_operand());
              array_dimension_upperbound = u_bound_operand->get_value();

            } else if (isSgIntVal(u_exp)) {
              const SgIntVal* const u_val = isSgIntVal(u_exp);
              array_dimension_upperbound = u_val->get_value();

            }
            array_dimension = array_dimension_upperbound - array_dimension_lowerbound + 1;
          }
          break;
          case V_SgAsteriskShapeExp: {
            std::cout << "\t" << array_name << " array dim. expression: 'SgAsteriskShapeExp' not handled." << std::endl;
          }
          break;
          default:
            std::cout <<  "\t" << array_name << " array dim. expression: " << (*i)->class_name() << " not handled." << std::endl;
            break;
            // TBD for DIMENSION(:) :: X
          } // end switch

        } // end for loop

      } // end if atype
      return array_dimension;
    }
  }
}

CompassAnalyses::ArrayIndex::
CheckerOutput::CheckerOutput ( SgNode* node,  const std::string & reason)
  : OutputViolationBase(node,checkerName,shortDescription+reason)
{}

CompassAnalyses::ArrayIndex::Traversal::
Traversal(Compass::Parameters, Compass::OutputObject& output)
  : output(output), ssa(NULL) {
  // Initalize checker specific parameters here, for example:
  // YourParameter = Compass::parseInteger(inputParameters["ArrayIndex.YourParameter"]);


}

void
CompassAnalyses::ArrayIndex::
Traversal::run(SgNode* n) {
  if( isSgProject(n) ){
    SgProject * const project = isSgProject(n);
    ssa = std::auto_ptr<StaticSingleAssignment>(new StaticSingleAssignment(project));
    ssa->run(true, true);
    //ssa->toDOT("out.dot");
    this->visit(n);
    //this->traverse(n, preorder);
  } else {
    // TODO: put something reasonable here
    abort();
  }
}

void
CompassAnalyses::ArrayIndex::Traversal::
scorePath(const SgVariableSymbol* const var,
          SgFunctionDefinition * const fd,
          SgPntrArrRefExp * const arrRef,
          const StaticSingleAssignment::NodeReachingDefTable & defTable,
          const std::string& index_name) const
{
  unsigned int use = 0;
  std::vector<unsigned int> definitions;
  foreach(StaticSingleAssignment::NodeReachingDefTable::const_iterator::
                                               value_type def, defTable) {
    ROSE_ASSERT(def.first.size() == 1); // TODO: why is this always true?
    // TODO: a) Find the correct def
    //       b) find all uses of def that dominate the use in the array
    //       c) check each of those uses to see if they are a conditional
    //       d) grade each conditional based on the array declaration
    foreach(const SgInitializedName * const name, def.first) {
      // Select matching def for var, TODO: is there a more efficient way to do this lookup?
      std::set< SgNode *> defs;
      if( var != name->get_symbol_from_symbol_table() ) continue;
      if( def.second->isPhiFunction() ){
        defs = def.second->getActualDefinitions();
#ifdef _DEBUG
        foreach(const SgNode * const ndef, defs){
          std::cout << ndef->get_file_info()->get_line()
                    << ": "
                    << ndef->unparseToString() << std::endl;
        }
#endif
      } else {
        defs.insert(def.second->getDefinitionNode());
#ifdef _DEBUG
        std::cout << "Definition on line "
                  << def.second->getDefinitionNode()->get_file_info()->get_line()
                  << ": " << def.second->getDefinitionNode()->unparseToString() << std::endl;
#endif
      }
      // Get all conditionals and check if they contain "name" (with
      // the correct renaming number)
      // Still have the issue of checking if the conditional
      // dominates the use we care about
      typedef ssa_private::DataflowCfgFilter CfgNodeT;
      typedef Backstroke::CFG<CfgNodeT> ControlFlowGraph;
      ControlFlowGraph cfg(fd);
      ControlFlowGraph::VertexVertexMap
               dominatorTreeMap = cfg.getDominatorTree();

      foreach(const ControlFlowGraph::VertexVertexMap::value_type& nodeDominatorPair, dominatorTreeMap){
        ControlFlowGraph::CFGNodeType node      = *cfg[nodeDominatorPair.first];
        ControlFlowGraph::CFGNodeType dominator = *cfg[nodeDominatorPair.second];
        // TODO: Without this we get into an infinite loop. Is
        // there a more direct/efficient lookup we could do?
        if( node.getNode() == arrRef ){
          use = nodeDominatorPair.first;
          break;
        }
      }
      ROSE_ASSERT( use != 0 );
      foreach(const ControlFlowGraph::VertexVertexMap::value_type& nodeDominatorPair, dominatorTreeMap){
        ControlFlowGraph::CFGNodeType node      = *cfg[nodeDominatorPair.first];
        ControlFlowGraph::CFGNodeType dominator = *cfg[nodeDominatorPair.second];
        if( find(defs.begin(), defs.end(), node.getNode()) != defs.end() ){
          definitions.push_back(nodeDominatorPair.first);
        }
      }
      foreach(const unsigned int d, definitions){
        const SgVariableSymbol * const var_l = isSgVarRefExp(arrRef->get_lhs_operand())->get_symbol();
        
        // Find array name and size
        if( var_l == NULL ){
          std::cout << "\t!Find array reference 'SgPntrArrRefExp' but lhs(array name) is unknown: "
                    << arrRef->get_lhs_operand()->class_name()
                    << std::endl;
          continue;
        }
        const SgInitializedName * const init_l          = var_l->get_declaration();
        const std::string               array_name      = var_l->get_name().getString();
        const SgArrayType       * const atype           = init_l != NULL ? isSgArrayType(init_l->get_type()) : NULL;
        const int                       array_dimension = findArraySize(atype, array_name);
        ROSE_ASSERT( var    != NULL );
        ROSE_ASSERT( arrRef != NULL );
        PathGrader grader(cfg, *var, *arrRef, index_name, 0, 0, array_dimension, array_name, output); 
        grader.constructPathAnalyzer(&cfg, false, d, use);

#ifdef _DEBUG
        std::cout << "Number of paths between line "
                  << cfg[d]->getNode()->get_file_info()->get_line()
                  << " and line "
                  << cfg[use]->getNode()->get_file_info()->get_line()
                  << ": " << grader.getNumberOfPaths() << std::endl;
        std::cout << "Path score: " << grader.getScore() << std::endl;
#endif
      }
    }
  }
}

void
CompassAnalyses::ArrayIndex::Traversal::
visit(SgNode* node) {
  // From this point on we are dealing with an array reference.
  const Rose_STL_Container<SgNode*> functionDefs =
               NodeQuery::querySubTree(node, V_SgFunctionDefinition);
  foreach(SgNode * const n, functionDefs) {
    SgFunctionDefinition * const fd = isSgFunctionDefinition(n);
    const Rose_STL_Container<SgNode*> arrayRefs =
                 NodeQuery::querySubTree(n, V_SgPntrArrRefExp);
    foreach(SgNode * const a, arrayRefs) {
      SgPntrArrRefExp * const arrRef = isSgPntrArrRefExp(a);
      // 1. For each array reference, get all of the index expressions
      const SgExpressionPtrList exprs = getIndexExpressions(*arrRef);
      foreach(SgExpressionPtrList::const_iterator::value_type exp, exprs) {
        // 2. score each index expression and report the score
        // TODO: This next bit will skip over constants used in the indexes
        const Rose_STL_Container<SgNode*> varrefs =
                     NodeQuery::querySubTree(exp, V_SgVarRefExp);
        foreach(Rose_STL_Container<SgNode*>::const_iterator::value_type v, varrefs){
          const SgVariableSymbol* const var = isSgVarRefExp(v)->get_symbol();
#ifdef _DEBUG
          std::cout << "Line " << v->get_file_info()->get_line()
                    << ": " << v->unparseToString() << std::endl;
#endif

          if (var == NULL) continue;
          const StaticSingleAssignment::NodeReachingDefTable & defTable =
                                                 ssa->getReachingDefsAtNode_(v);
          // 3. Match the variables in the expression up with their definition
          // For example, in the expression A(x,y), we first want to score x, then
          // separately score y, and not score A unless we had something like
          // B(A(x,y)).  This means that scoring should be based on the
          // "var" above.
          const SgVariableSymbol  * const var_r           = isSgVarRefExp(exp)->get_symbol();
          const std::string               index_name      = var_r  != NULL ? var_r->get_name().getString()     : "";
          scorePath(var, fd, arrRef, defTable, index_name);
/* Start old traversal, translated to new code -- but it's probably not doing anything useful */
//          //Find rhs of the referrenced array, i.e.,index node "I"
//          // for "A[I] = " or "A[I-1]", the rhs of SgPntrArrRefExp is SgExprListExp
//          // - for A[I] , index SgVarRefExp is the direct child of SgExprListExp
//          // - while for A[I-1] , need to go through its subtree to find index node SgVarRefExp
//          const SgExprListExp * const exp_r = isSgExprListExp(arrRef->get_rhs_operand());
//          if( exp_r == NULL ) ROSE_ASSERT( false ); // TODO: how can we get here?
//          const SgExpressionPtrList expr_list = exp_r->get_expressions();
//          foreach(const SgExpressionPtrList::const_iterator::value_type exp, expr_list){
//            if (isSgVarRefExp(exp)) { // A[I]
//              const SgVariableSymbol * const var_r = isSgVarRefExp(exp)->get_symbol();
//        
//              // Find index variable name
//              if (var_r) {
//                const SgInitializedName * const init_r     = var_r->get_declaration();
//                const std::string               index_name = var_r->get_name().getString();
//        
//                // not found both array and index variable then return
//                if ( (!init_l) || (!init_r) ) {
//                  continue; // TODO: how did we get here?
//                }
//                //checkIndex(pntr, index_name,check_flag, level,array_dimension,array_name,output);
//              } else {
//                std::cout << "\t!Find array reference 'SgPntrArrRefExp' but this kind of array index expression is not handled: "
//                          << arrRef->get_rhs_operand()->class_name()
//                          << std::endl;
//                continue;
//              }
//            } else { // A[I-1] or A(I,J,K), search subtree for SgVarRefExp
//              const std::vector<SgNode*> subtree = NodeQuery::querySubTree(exp, V_SgVarRefExp);
//              if( !subtree.empty() ){
//                foreach(const std::vector<SgNode *>::const_iterator::value_type node, subtree){
//                  const SgVariableSymbol * const var_r = isSgVarRefExp(node)->get_symbol();
//        
//                  // Find index variable name
//                  if (var_r) {
//                    const SgInitializedName * const init_r     = var_r->get_declaration();
//                    const std::string               index_name = var_r->get_name().getString();
//        
//                    // not found both array and index variable then return
//                    if ( (!init_l) || (!init_r) ) {
//                      continue;
//                    }
//                    //checkIndex(pntr, index_name,check_flag, level,array_dimension,array_name,output);
//                  } else {
//                    std::cout << "\t!Find array reference 'SgPntrArrRefExp' but this kind of array index expreesion is not handled: "
//                              << arrRef->get_rhs_operand()->class_name()
//                              << std::endl;
//                    continue;
//                  }
//                }  // end of for
//              }
//            }
//          } // end of for iterator i
/* End old traversal code */
        }
      }
    }
  }
} //End of the visit function.

// Checker main run function and metadata

static void run(Compass::Parameters params, Compass::OutputObject* output) {
  CompassAnalyses::ArrayIndex::Traversal(params, *output).run(Compass::projectPrerequisite.getProject());
}

// Remove this function if your checker is not an AST traversal
static Compass::AstSimpleProcessingWithRunFunction* createTraversal(Compass::Parameters params, Compass::OutputObject* output) {
  return new CompassAnalyses::ArrayIndex::Traversal(params, *output);
}

extern const Compass::Checker* const arrayIndexChecker =
  new Compass::CheckerUsingAstSimpleProcessing(
  "ArrayIndex",
  // Descriptions should not include the newline character "\n".
  CompassAnalyses::ArrayIndex::shortDescription,
  CompassAnalyses::ArrayIndex::shortDescription,
  Compass::Fortran,
  Compass::PrerequisiteList(1, &Compass::projectPrerequisite),
  run,
  createTraversal);

