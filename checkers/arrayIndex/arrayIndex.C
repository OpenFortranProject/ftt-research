// -*- mode: C++; c-basic-offset: 2; indent-tabs-mode: nil -*-
// vim: expandtab:shiftwidth=2:tabstop=2

// Array Index Analysis
// Author: Fangling Chang,,,
// Date: 30-December-2009

/*
 ! Filename  : arrayIndex.C
 ! Test input: inputCodeArrayIndex02.f03
 ! Purpose   : array index out of upper/lower bound
 !
 ! Note     : FORTRAN code which pass array into function call can run but not analyzable in ROSE.
 !            commented out in inputCodeArrayIndex02.f03,  not test yet.
 ! TBD      : test array in a function call with explicit-shape and assumed-shaped
 !                         const      var     assumed shape(:)
 !     condition             X(5)      X(N)     X(:)               Level Description
 !     none                  0         0        0                  // 0 means bad programming, NO check at all
 !     const i<5             1         1        1                  // 1 means less bad
 !     var   i<n             2         2        2                  // OK
 !     size  i<size(x)       3         3        3                  // very good
*/

#include "rose.h"
#include "compass.h"
//#include <sstream>
using namespace std;
extern const Compass::Checker* const arrayIndexChecker;

// DQ (1/17/2009): Added declaration to match external defined in file:
// rose/projects/compass/extensions/prerequisites/ProjectPrerequisite.h
// I can't tell that it is defined anywhere in compass except the extern
// declaration in ProjectPrerequisite.h
Compass::ProjectPrerequisite Compass::projectPrerequisite;

#ifndef COMPASS_ARRAY_INDEX_H
#define COMPASS_ARRAY_INDEX_H

namespace CompassAnalyses {
  namespace ArrayIndex {
    /*! \brief Array Index: Add your description here
     */
    extern const std::string checkerName= "arrayIndex";
    extern const std::string shortDescription= "index bound check:";
    extern const std::string longDescription= "found statement with array index which may lead to buffer overflow";


    // Specification of Checker Output Implementation
    class CheckerOutput: public Compass::OutputViolationBase {
      public:
        CheckerOutput(SgNode* node, const std::string &);
    };

    // Specification of Checker Traversal Implementation

    class Traversal
        : public Compass::AstSimpleProcessingWithRunFunction {
        Compass::OutputObject* output;
        // Checker specific parameters should be allocated here.

      public:
        Traversal(Compass::Parameters inputParameters, Compass::OutputObject* output);

        // Change the implementation of this function if you are using inherited attributes.
        void *initialInheritedAttribute() const {
          return NULL;
        }

        // The implementation of the run function has to match the traversal being called.
        // If you use inherited attributes, use the following definition:
        // void run(SgNode* n){ this->traverse(n, initialInheritedAttribute()); }
        void run(SgNode* n) {
          this->traverse(n, preorder);
        }

        // Change this function if you are using a different type of traversal, e.g.
        // void *evaluateInheritedAttribute(SgNode *, void *);
        // for AstTopDownProcessing.
        void visit(SgNode* n);
    };
  }
}
// COMPASS_ARRAY_INDEX_H
#endif

namespace CompassAnalyses {
  namespace ArrayIndex {
    template <class T>
    inline std::string to_string (const T& t) {
      std::stringstream ss;
      ss << t;
      return ss.str();
    }

    int findArraySize(SgArrayType* atype,std::string array_name) {
      int array_dimension = 0;  // IF array size cannot be determinded, return 0 as default
      int array_dimension_lowerbound, array_dimension_upperbound;  // DIMENSION(-2:3)
      if (atype) {
        SgExprListExp* dim = atype->get_dim_info();
        SgExpressionPtrList sig = dim->get_expressions();
        for (SgExpressionPtrList::iterator i = sig.begin(); i != sig.end(); i++) {
          switch((*i)->variantT()) {
          case V_SgIntVal:
            // DIMENSION(3) :: X
          {
            SgIntVal* dim_value = isSgIntVal(*i);
            array_dimension = dim_value->get_value();
          }
          break;

          case V_SgVarRefExp:
            // DIMENSION(N) :: X
          {
            SgVarRefExp* var_exp = isSgVarRefExp(*i);
            SgVariableSymbol* var_sym = var_exp->get_symbol();
            SgInitializedName* var_init = var_sym->get_declaration();
            SgInitializer* init = var_init->get_initializer();
            SgAssignInitializer* ainit = isSgAssignInitializer(init);
            if (ainit) {
              // find initialized  value (array dimension value)
              SgIntVal* int_value = isSgIntVal(ainit->get_operand());
              if (int_value) {
                array_dimension = int_value->get_value();
              }
            }
          }
          break;

          // DIMENSION(-2:2) :: X
          // TBD : modify for SgMinusOp , operand is 2, but need to show value -2
          case V_SgSubscriptExpression: {
            SgSubscriptExpression* subscript = isSgSubscriptExpression(*i);
            SgExpression* l_exp = subscript->get_lowerBound();
            SgExpression* u_exp = subscript->get_upperBound();
            // the lower bound could be negative like A(-2:2)
            if (isSgUnaryOp(l_exp)) {
              SgUnaryOp* l_bound = isSgUnaryOp(l_exp);
              SgIntVal* l_bound_operand = isSgIntVal(l_bound->get_operand());
              array_dimension_lowerbound = l_bound_operand->get_value();

            } else if (isSgIntVal(l_exp)) {
              SgIntVal* l_val = isSgIntVal(l_exp);
              array_dimension_lowerbound = l_val->get_value();

            }
            if (isSgUnaryOp(u_exp)) {
              SgUnaryOp* u_bound = isSgUnaryOp(u_exp);
              SgIntVal* u_bound_operand = isSgIntVal(u_bound->get_operand());
              array_dimension_upperbound = u_bound_operand->get_value();

            } else if (isSgIntVal(u_exp)) {
              SgIntVal* u_val = isSgIntVal(u_exp);
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

    // "CheckLevel" object hold information like check_flag , level, etc
    class CheckLevel {
      public:
        int check_flag;
        int level     ;
        std::string  warn;
        std::string  var_name;
        //        int  var_line_number;
        CheckLevel(int init_check, int init_level, std::string init_var_name) {
          check_flag = init_check;
          level      = init_level;
          warn       = "";
          var_name   = init_var_name;
          //             var_line_number = init_var_line_number;
        }
        void setFlag(int s_check_flag, int s_level) {
          check_flag = s_check_flag;
          level      = s_level;
        }
    };
    // backCFG is try to find if var_node is assigned as SIZE()
    CheckLevel* backCFG(SgVarRefExp* var_node,CheckLevel* cl) {
      std::string  var_name        = var_node->get_symbol()->get_name().getString();
      int          var_line_number = var_node->get_file_info()->get_line();
      int          return_flag     = 0;

      // traverse cfg
      vector<FilteredCFGNode<IsDFAFilter> > worklist;
      vector<FilteredCFGNode<IsDFAFilter> > visited;
      int line_number_recursive;

      FilteredCFGNode < IsDFAFilter > source =
        FilteredCFGNode < IsDFAFilter > (var_node->cfgForBeginning());
      worklist.push_back(source);

      //std::cout << "\tRecursive backward CFG ..." <<std::endl;
      while (!worklist.empty() ) {
        source = worklist.front();
        worklist.erase(worklist.begin());

        SgNode* next = source.getNode(); // get underlying AST node while walk back CFG
        // check if current node is a assign

        SgAssignOp* aop       = isSgAssignOp(next);
        if (aop != NULL ) {
          int line_number_assign= aop->get_file_info()->get_line();
          SgVarRefExp* lhs_var = isSgVarRefExp(aop->get_lhs_operand());
          if (lhs_var) {
            SgVariableSymbol* lhs_var_symbol = lhs_var->get_symbol();
            std::string lhs_var_symbol_name = lhs_var_symbol->get_name().getString();
            if (lhs_var_symbol_name.compare(var_name) == 0 ) {
              SgExpression* rhs_operand = isSgExpression(aop->get_rhs_operand());
              switch(rhs_operand->variantT()) {
              case V_SgIntVal: {
                // level 2
                return_flag = 1;
              }
              break;
              case V_SgVarRefExp: {
                // need to go down further, recursive
                SgVarRefExp* next_var_node = isSgVarRefExp(rhs_operand);
                backCFG(next_var_node, cl);
                return_flag = 1;
              }
              break;
              case V_SgFunctionCallExp: {
                // need to check if function call is a SIZE()
                SgFunctionCallExp* func_call_exp = isSgFunctionCallExp(rhs_operand);
                SgFunctionRefExp* func_ref = isSgFunctionRefExp(func_call_exp->get_function());
                if (func_ref != NULL) {
                  // check if func is size()
                  if (func_ref->get_symbol()->get_name().getString().compare("SIZE") == 0 ||
                      func_ref->get_symbol()->get_name().getString().compare("size") == 0) {
                    cl->level = 3;
                  } else {
                    cl->level = 1;
                    cl->warn  = ", Warning: checked but not using function call SIZE()";
                  }
                }

                return_flag = 1;
              }
              break;
              case V_SgSubtractOp: {
                // get SgVarRefExp and go down further, recursive
                SgSubtractOp* subtract_op = isSgSubtractOp(rhs_operand);
                SgVarRefExp* subtract_var_node = isSgVarRefExp(subtract_op->get_lhs_operand());
                if (subtract_var_node != NULL) {
                  backCFG(subtract_var_node,cl);
                }
                return_flag = 1;
              }
              break;
              default :
                std::cout <<  "\tbackCFG(): " << var_name << ",line:" << line_number_assign
                          << ", rhs type not handled:" << rhs_operand->variantT() << std::endl;

                return_flag = 1;
                break;
              }

            }
          } // end if lhs_var

        }// end if aop

        if ( return_flag == 1 ) {
          return cl;
        }

#ifdef _DEBUG
        std::cout << "debug backCFG:var line: " << var_line_number << " node:" << var_node->class_name() << std::endl;
        std::cout << "debug backCFG:next node " << next->class_name() << " line:"
                  << next->get_file_info()->get_line()
                  << "." << next->get_file_info()->get_col() << std::endl;
#endif


        // find next CFG node to add into worklist
        vector<FilteredCFGEdge < IsDFAFilter > > in_edges = source.inEdges();
        for (vector<FilteredCFGEdge <IsDFAFilter> >::const_iterator i = in_edges.begin(); i != in_edges.end(); ++i) {
          FilteredCFGEdge<IsDFAFilter> filterEdge = *i;
          FilteredCFGNode<IsDFAFilter> filterNode = filterEdge.source();

          SgNode* inedge_source_node = filterNode.getNode();
#ifdef _DEBUG
          std::cout << "debug:backCFG:inedge source node is "
                    << inedge_source_node->class_name() << std::endl;
#endif
          if (find(visited.begin(), visited.end(), filterNode)==visited.end()) {
            line_number_recursive = inedge_source_node->get_file_info()->get_line();
            if ( line_number_recursive <= var_line_number ) {
              worklist.push_back(filterNode);
              visited.push_back(filterNode);
            }      // end of if line_number_recursive
          }// end if find
        } // end for loop

      }// end of while

      // var assign not found if you get to this point
      //std::cout << "end of backCFG while loop: " << var_name << ",still not found assignment with SIZE()" << std::endl;

      // cl->level = 0;

      return cl;

    }


    // checkRHS does not necessary check rhs operand, it can check against any operand
    CheckLevel* checkRHS(SgExpression* switch_exp,int d_check_flag, int d_level,  int array_size,CheckLevel* cl,std::string a_name ) {
      int c_check_flag ;
      int c_level;
      c_check_flag  = d_check_flag;
      c_level = d_level;

      switch(switch_exp->variantT()) {
      case V_SgIntVal: {  //  IF ( I < 5 ) , rhs_node is SgIntVal
        SgIntVal* int_val = isSgIntVal(switch_exp);
        c_check_flag = 1;
        c_level      = 1;
        if (int_val->get_value() <= array_size) {
          cl->setFlag(c_check_flag,c_level);
        } else {
          cl->setFlag(c_check_flag,c_level);
          cl->warn = ", Warning: value may  exceed array size " ;
        }
      }
      break;

      case V_SgVarRefExp: { // IF (I < IMAX)
        c_check_flag = 1;
        c_level      = 2;
        cl->setFlag(c_check_flag,c_level);
        SgVarRefExp* var_node        = isSgVarRefExp(switch_exp);
        backCFG(  var_node, cl);
      }
      break;
      case V_SgFunctionCallExp: { // IF ( I < SIZE(A)) , then rhs_node is SgFunctionCallExp
        SgFunctionCallExp* rhs_func_call_exp = isSgFunctionCallExp(switch_exp);
        SgFunctionRefExp* rhs_func = isSgFunctionRefExp(rhs_func_call_exp->get_function());
        if (rhs_func) {
          // check if func is size()
          if (rhs_func->get_symbol()->get_name().getString().compare("SIZE") == 0 ||
              rhs_func->get_symbol()->get_name().getString().compare("size") == 0) {
            // check if the array name is size() is correct
            SgExprListExp* args_list = isSgExprListExp((rhs_func_call_exp)->get_args());
            SgExpressionPtrList list = args_list->get_expressions();
            for (SgExpressionPtrList::iterator i = list.begin(); i != list.end(); i++) {
              if (isSgVarRefExp(*i)) {
                //std::string fun_array_name;
                SgInitializedName* init_name = isSgVarRefExp(*i)->get_symbol()->get_declaration();
                std::string fun_array_name = init_name->get_name().getString();
                if (fun_array_name == a_name) {
                  c_check_flag = 1;
                  c_level      = 3;
                  cl->setFlag(c_check_flag,c_level);
                } else {
                  c_check_flag = 1;
                  c_level      = 3;
                  cl->setFlag(c_check_flag,c_level);
                  cl->warn = ", Warning: array name is not correct. " ;
                }
              }
            }
          }
        }
      }
      break;
      default: {
        c_check_flag = 1;
        c_level      = 2;
        cl->setFlag(c_check_flag,c_level);
        cl->warn = ", Warning: checked against a non-simple DO bound expression. " ;
        // std::cout << "\tcheckRHS: " << switch_exp->class_name() << " not handled"<< std::endl;
      }
      break;
      } // end switch
      return cl;
    }

    void checkIndex(SgNode* node, std::string index_name, int check_flag, int level,
                    int array_dimension,std::string array_name,Compass::OutputObject* output ) {
      //std::cout << "   Start to walk backward CFG..." << std::endl;

      //traverse cfg BK  and find next assign node involving index i
      vector<FilteredCFGNode<IsDFAFilter> > worklist;
      vector<FilteredCFGNode<IsDFAFilter> > visited;

      // add this node to worklist and workthrough the outgoing edges
      FilteredCFGNode < IsDFAFilter > source =
        FilteredCFGNode < IsDFAFilter > (node->cfgForBeginning());
      worklist.push_back(source);

      int line_number_array = node->get_file_info()->get_line();
      int line_number_cfg   = 0;  //line number of the associated code for this current cfg node
      int line_number_check = 0;  // line number that index variable may be checked
      std::string   reason    = "";
      std::string   reason_error = "";

      while (!worklist.empty() ) {
        source = worklist.front();
        worklist.erase(worklist.begin());

        SgNode* next = source.getNode();

        // Find previous IF statement or WHILE statement
        SgIfStmt* ifstmt            = isSgIfStmt(next);
        SgWhileStmt* whilestmt	= isSgWhileStmt(next);
        SgFortranDo* dostmt         = isSgFortranDo(next);
        SgAssignOp* assignop       = isSgAssignOp(next);

        // debug
#ifdef _DEBUG
        std::cout << "debug:current line: " << line_number_array << " node:" << node->class_name() << std::endl;
        std::cout << "debug:next node " << next->class_name() << " line:"
                  << next->get_file_info()->get_line()
                  << "." << next->get_file_info()->get_col() << std::endl;
#endif

        // index  J reassigned a value used in DO bound before referenced
        // DO I= ...I++
        //    J = I - 1
        //    A(J) = ...
        if (assignop != NULL ) {
          SgVarRefExp* assignop_lhs_var = isSgVarRefExp(assignop->get_lhs_operand());
          if (assignop_lhs_var) {
            SgVariableSymbol* assignop_lhs_var_symbol = assignop_lhs_var->get_symbol();
            std::string assignop_lhs_var_symbol_name = assignop_lhs_var_symbol->get_name().getString();
            if (assignop_lhs_var_symbol_name.compare(index_name) == 0 ) {

              line_number_check = assignop->get_file_info()->get_line();
              check_flag = 1;
              level      = 1;
              reason = "level-"  + to_string(level) + " found for "+ node->unparseToString()
                       + ", checked on line "
                       +  to_string(line_number_check)  ;
              output->addOutput(new CheckerOutput(node,reason));
              return;

            }
          }
        }



        // Get condition in IF or WHILE
        SgStatement* condition = NULL;
        if (ifstmt) {
          condition = ifstmt->get_conditional();
        }
        if (whilestmt) {
          condition = whilestmt->get_condition();
        }

        // Check whether index been checked in the condition
        if (condition ) {
          // query subtree of node ifstmt_cond for SgVarRefExp
          Rose_STL_Container<SgNode*> returns = NodeQuery::querySubTree (condition,V_SgVarRefExp);
          if (!returns.empty()) {
            for (Rose_STL_Container<SgNode*>::iterator i = returns.begin(); i != returns.end(); i++) {
              // for "IF (I < XXX )", lhs of condition is SgVarRefExp
              SgVarRefExp* var_ref_exp = isSgVarRefExp(*i);

              line_number_check = var_ref_exp->get_file_info()->get_line();

              SgVariableSymbol* var_symbol = var_ref_exp->get_symbol();
              std::string var_symbol_name = var_symbol->get_name().getString();

              // variable name in IF is the same as index name
              if (var_symbol_name.compare(index_name) == 0 ) {
                // condition shall be a binary operator
                // and the rhs of condition could be
                //   1. SgIntVal , like ( I <5 ), or
                //   2. SgVarRefExp , like (I < IMAX) ,or
                //   3. SgFunctionCallExp, like (I < SIZE(A))
                SgNode* parent = var_ref_exp->get_parent();
                SgBinaryOp* parent_binary_op = isSgBinaryOp(parent);
                if (isSgBinaryOp(parent)) {
                  SgExpression* exp_check = NULL;

                  SgExpression* rhs_operand = parent_binary_op->get_rhs_operand();
                  SgVarRefExp* rhs_operand_var = isSgVarRefExp(rhs_operand);

                  if (( rhs_operand_var != NULL ) && (rhs_operand_var == var_ref_exp ))   // i in rhs , then we need check lhs
                    exp_check = parent_binary_op->get_lhs_operand();
                  else   // i in lhs like I<5, then we need to check RHS
                    exp_check = parent_binary_op->get_rhs_operand();

                  CheckLevel* cl = new CheckLevel(check_flag, level, index_name);
                  // Check operand expression is a value, var , or size()
                  CheckLevel* return_cl = checkRHS(exp_check,check_flag,level, array_dimension,cl,array_name);
                  check_flag = return_cl->check_flag;
                  level      = return_cl->level;

                  reason = "level-"  + to_string(level) + " found for "+ node->unparseToString()
                           + ", checked on line "
                           +  to_string(line_number_check) + return_cl->warn ;

                  output->addOutput(new CheckerOutput(node,reason));
                  return;
                }
              }
            }  // end  for loop
          }  // end if condition subtree is not empty
        }
        // SgFortranDo statement
        if (dostmt) {
          // examine DO init expression
          SgAssignOp* do_init_exp = isSgAssignOp(dostmt->get_initialization());
          if (do_init_exp) {
            SgVarRefExp* do_var = isSgVarRefExp(do_init_exp->get_lhs_operand());
            line_number_check = do_var->get_file_info()->get_line();

            if (do_var) {
              SgVariableSymbol* do_var_symbol = do_var->get_symbol();
              std::string do_var_symbol_name = do_var_symbol->get_name().getString();

              // index appear in DO init expression
              if (do_var_symbol_name.compare(index_name) == 0 ) {
                // examine DO bound expression
                SgExpression* do_bound_exp = dostmt->get_bound();
                CheckLevel* cl = new CheckLevel(check_flag, level, index_name);
                CheckLevel* return_cl = checkRHS(do_bound_exp,check_flag, level, array_dimension,cl,array_name);
                check_flag = return_cl->check_flag;
                level      = return_cl->level;
                reason = "level-"  + to_string(level) + " found for "+ node->unparseToString()
                         + ", index " + index_name + " checked on line "
                         +  to_string(line_number_check) + return_cl->warn ;

                output->addOutput(new CheckerOutput(node,reason));
                return;
              }
            }
          }
        }

        // Not found any check yet, keep looking next edge and node
        // Find the next  edge to walk backward
        if ( check_flag == 0 ) {
          vector<FilteredCFGEdge < IsDFAFilter > > in_edges = source.inEdges();
          for (vector<FilteredCFGEdge <IsDFAFilter> >::const_iterator i = in_edges.begin(); i != in_edges.end(); ++i) {
            FilteredCFGEdge<IsDFAFilter> filterEdge = *i;
            FilteredCFGNode<IsDFAFilter> filterNode = filterEdge.source();

            SgNode* inedge_source_node = filterNode.getNode();

            // check if the node has been visited yet
            if (find(visited.begin(), visited.end(), filterNode)==visited.end()) {
              line_number_cfg = inedge_source_node->get_file_info()->get_line();

              // put the node in the list if the line number of underlying node less then the line number of array referred
              if ( line_number_cfg <= line_number_array ) {
                worklist.push_back(filterNode);
                visited.push_back(filterNode);
              }// end of if line_number_cfg
              else {
                //std::cout << "\t\t             *****Discard:line number exceed ******"<< std::endl;
              }
            }
          } // end of for
        } // end of if check_flag
      }// end of while

      if ( check_flag == 0 ) {
        reason = "Warning:level-"  + to_string(level) + " for "+ node->unparseToString()
                 + " index " + index_name + " : No bound check";

        output->addOutput(new CheckerOutput(node,reason));
      }
      return;
    }

  }
}


CompassAnalyses::ArrayIndex::
CheckerOutput::CheckerOutput ( SgNode* node,  const std::string & reason)
  : OutputViolationBase(node,checkerName,shortDescription+reason)
{}

CompassAnalyses::ArrayIndex::Traversal::
Traversal(Compass::Parameters, Compass::OutputObject* output)
  : output(output) {
  // Initalize checker specific parameters here, for example:
  // YourParameter = Compass::parseInteger(inputParameters["ArrayIndex.YourParameter"]);


}

void
CompassAnalyses::ArrayIndex::Traversal::
visit(SgNode* node) {
  // Implement your traversal here.
  SgPntrArrRefExp* pntr = isSgPntrArrRefExp(node);

  if ( pntr == NULL ) {
    return;
  }

  SgInitializedName* init_l=NULL;
  SgInitializedName* init_r=NULL;

  SgVariableSymbol* var_l = NULL;
  SgVariableSymbol* var_r = NULL;
  var_l = isSgVarRefExp(pntr->get_lhs_operand())->get_symbol();


  int line_number_array = 0;   // line where array is referenced
  line_number_array = pntr->get_file_info()->get_line();
  std::string array_name = "";
  int         array_dimension = 0;            // DIMENSION(5)

  std::string index_name = "";

  int check_flag = 0;
  int level      = 0;

  // Find array name and size
  if (var_l) {
    init_l = var_l->get_declaration();
    array_name = var_l->get_name().getString();

    // Find array size
    SgArrayType* atype = isSgArrayType(init_l->get_type());
    array_dimension = findArraySize(atype, array_name);

  }

  else {
    std::cout << "\t!Find array reference 'SgPntrArrRefExp' but lhs(array name) is unknown :" << pntr->get_lhs_operand()->class_name() << std::endl;
    return;
  }


  // For C
  // var_r = isSgVarRefExp(pntr->get_rhs_operand())->get_symbol();

  // For FORTRAN,
  // ISSUE: this may only work for 1-D  array.  for 2D or 3D, there could be multiple var_r to be considered
  // refer to compass code on how to treat 'argument'
  // TBD : for 2-D, 3-D, try query subtree of pntr->get_rhs_operand() to search for isSgVarRefExp node
  //         because it may not be direct child

  //Find rhs of the referrenced array, i.e.,index node "I"
  // for "A[I] = " or "A[I-1]", the rhs of SgPntrArrRefExp is SgExprListExp
  // - for A[I] , index SgVarRefExp is the direct child of SgExprListExp
  // - while for A[I-1] , need to go through its subtree to find index node SgVarRefExp
  SgExprListExp* exp_r = isSgExprListExp(pntr->get_rhs_operand());
  SgExpressionPtrList list = exp_r->get_expressions();
  for (SgExpressionPtrList::iterator i = list.begin(); i != list.end(); i++) {
    if (isSgVarRefExp(*i)) { // A[I]
      var_r = isSgVarRefExp(*i)->get_symbol();

      // Find index variable name
      if (var_r) {
        init_r = var_r->get_declaration();
        index_name = var_r->get_name().getString();

        // not found both array and index variable then return
        if ( (!init_l) || (!init_r) ) {
          return;
        }
        checkIndex(pntr, index_name,check_flag, level,array_dimension,array_name,output);
      } else {
        //std::cout << "\t!Find array reference 'SgPntrArrRefExp' but this kind of array index expression is not handled:"
        //            << pntr->get_rhs_operand()->class_name() << std::endl;
        return;
      }
    } else { // A[I-1] or A(I,J,K), search subtree for SgVarRefExp
      Rose_STL_Container<SgNode*> subtree = NodeQuery::querySubTree ((*i),V_SgVarRefExp);
      if (!subtree.empty()) {
        for (Rose_STL_Container<SgNode*>::iterator j = subtree.begin(); j != subtree.end(); j++) {
          var_r = isSgVarRefExp(*j)->get_symbol();

          // Find index variable name
          if (var_r) {
            init_r = var_r->get_declaration();
            index_name = var_r->get_name().getString();

            // not found both array and index variable then return
            if ( (!init_l) || (!init_r) ) {
              return;
            }
            checkIndex(pntr, index_name,check_flag, level,array_dimension,array_name,output);
          } else {
            //std::cout << "\t!Find array reference 'SgPntrArrRefExp' but this kind of array index expreesion is not handled:"
            //            << pntr->get_rhs_operand()->class_name() << std::endl;
            continue;
          }
        }  // end of for
      }
    }
  } // end of for iterator i

  // not found both array and index variable then return
  if ( (!init_l) || (!init_r) )
    return;

} //End of the visit function.

// Checker main run function and metadata

static void run(Compass::Parameters params, Compass::OutputObject* output) {
  CompassAnalyses::ArrayIndex::Traversal(params, output).run(Compass::projectPrerequisite.getProject());
}

// Remove this function if your checker is not an AST traversal
static Compass::AstSimpleProcessingWithRunFunction* createTraversal(Compass::Parameters params, Compass::OutputObject* output) {
  return new CompassAnalyses::ArrayIndex::Traversal(params, output);
}

extern const Compass::Checker* const arrayIndexChecker =
  new Compass::CheckerUsingAstSimpleProcessing(
  "ArrayIndex",
  // Descriptions should not include the newline character "\n".
  CompassAnalyses::ArrayIndex::shortDescription,
  CompassAnalyses::ArrayIndex::shortDescription,
  Compass::C | Compass::Cpp,
  Compass::PrerequisiteList(1, &Compass::projectPrerequisite),
  run,
  createTraversal);

