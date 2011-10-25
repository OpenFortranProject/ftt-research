// -*- mode: C++; c-basic-offset: 2; indent-tabs-mode: nil -*-
// vim: expandtab:shiftwidth=2:tabstop=2

// Implicit Save Analysis
// Filename:implicitSave.C
// Author:Fangling Chang,,,
// Date: 16-February-2010

#include "rose.h"
#include "compass.h"

extern const Compass::Checker* const implicitSaveChecker;

// DQ (1/17/2009): Added declaration to match external defined in file:
// rose/projects/compass/extensions/prerequisites/ProjectPrerequisite.h
// I can't tell that it is defined anywhere in compass except the extern
// declaration in ProjectPrerequisite.h
Compass::ProjectPrerequisite Compass::projectPrerequisite;

namespace CompassAnalyses {
  namespace ImplicitSave {
    /*! \brief Implicit Save: Add your description here
     */
    extern const std::string checkerName= "implicitSave";
    extern const std::string shortDescription= "improper operation to an implicit saved variable:";
    extern const std::string longDescription= "find statement assigning value to an implicit saved variable";

    // Specification of Checker Output Implementation
    class CheckerOutput: public Compass::OutputViolationBase {
      public:
        CheckerOutput(SgNode* node, const std::string &);
    };

    // Specification of Checker Traversal Implementation

    class Traversal // visit() in this traversal class is to set attributes
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
    class Traversal2 // visit() in this traversal class is to get attributes
        : public Traversal {
        Compass::OutputObject* output;

      public:
        Traversal2(Compass::Parameters inputParameters, Compass::OutputObject* output);

        void *initialInheritedAttribute() const {
          return NULL;
        }

        void run(SgNode* n) {
#ifdef _DEBUG_
          std::cout << "Multiple traversal using another traversal class..." << std::endl;
#endif
          this->traverse(n, preorder);
        }

        void visit(SgNode* n);
    };
    class myAstAttribute : public AstAttribute {
      public:
        myAstAttribute(int v):implicit_save_flag(v) {}
        int implicit_save_flag;
    };
    int checkModifierIntent(SgInitializedName* var_init) {
      int intent_flag = 0;
      if (var_init != NULL) {

        SgVariableDeclaration* var_decl = isSgVariableDeclaration(var_init->get_parent());

        SgDeclarationModifier var_decl_mod = var_decl->get_declarationModifier();
        SgTypeModifier        var_decl_mod_type = var_decl_mod.get_typeModifier();
#ifdef _DEBUG_
        std::cout << "\tIs argument intent_in:" << var_decl_mod_type.isIntent_in() << std::endl;
        std::cout << "\tIs argument intent_out:" << var_decl_mod_type.isIntent_out() << std::endl;
        std::cout << "\tIs argument intent_inout:" << var_decl_mod_type.isIntent_inout() << std::endl;
#endif
        if ( var_decl_mod_type.isIntent_in() == true) {
          intent_flag = 8; // SgTypeModifier::e_intent_in = 8
        } else if ( var_decl_mod_type.isIntent_out() == true ) {
          intent_flag = 9; // SgTypeModifier::e_intent_out = 9
        } else if ( var_decl_mod_type.isIntent_inout() == true ) {
          intent_flag = 10; // SgTypeModifier::e_intent_out = 10
        }
      } // end if var_init
      return intent_flag;
    }

  }
}

CompassAnalyses::ImplicitSave::
CheckerOutput::CheckerOutput ( SgNode* node,  const std::string & reason )
  : OutputViolationBase(node,::implicitSaveChecker->checkerName,::implicitSaveChecker->shortDescription+reason)
{}

CompassAnalyses::ImplicitSave::Traversal::
Traversal(Compass::Parameters, Compass::OutputObject* output)
  : output(output) {
  // Initalize checker specific parameters here, for example:
  // YourParameter = Compass::parseInteger(inputParameters["ImplicitSave.YourParameter"]);


}

void
CompassAnalyses::ImplicitSave::Traversal::
visit(SgNode* node) { // this visit is to set attribute
  // Implement your traversal here.
#ifdef _DEBUG_
  if ( node->class_name() != "SgProject") {
    std::cout << "1st Traversal: set attributes: " << node->class_name()
              <<  " line:" << node->get_file_info()->get_line() << std::endl;
  } else {
    std::cout << "1st Traversal: set attributes: " << node->class_name()  << std::endl;
  }
#endif

  // This traversal set attribute for the variables which are declared and initialized
  // There will be a SgAssignInitializer node if a variable is declared and initialized
  //       ex, INTEGER :: I = 1

  int return_flag = 0;
  switch(node->variantT()) {
  case V_SgAssignInitializer: {
    SgAssignInitializer* ainit = isSgAssignInitializer(node);
    SgInitializedName* var_init = isSgInitializedName(ainit->get_parent());
    if (var_init != NULL ) {
      //mark the node SgVariableSymbol by setting attribute  value to  2
      SgVariableSymbol* var_sym = isSgVariableSymbol(var_init->get_symbol_from_symbol_table());
      if (var_sym != NULL ) {
        var_sym->setAttribute("att_assign_init",new myAstAttribute(2));
      }
    }
  }
  break;
  case V_SgFunctionCallExp: {
    SgFunctionCallExp*   fc    = isSgFunctionCallExp(node);
    SgExprListExp* arg_list = fc->get_args();
    SgExpressionPtrList arg = arg_list->get_expressions();
    int num_arg = 0;
    for (SgExpressionPtrList::iterator i = arg.begin(); i != arg.end(); i++) {
      num_arg = num_arg + 1;
      SgVarRefExp* arg_var = isSgVarRefExp(*i);
      if (arg_var != NULL ) {
        //check whether argument symbol has att set
        SgVariableSymbol* arg_var_sym_g = arg_var->get_symbol();

        // check if attribute "att_assign_init" is set
        AstAttribute* arg_att = arg_var_sym_g->getAttribute("att_assign_init");
        if ( arg_att != NULL ) {
#ifdef _DEBUG_
          std::cout << "\tFound arg which has been set implicit_save attribute:" <<
                    dynamic_cast<myAstAttribute*>(arg_att)->implicit_save_flag <<  std::endl;
#endif

          // check if argument is inout/out
          SgFunctionRefExp* fc_ref_exp = isSgFunctionRefExp(fc->get_function());
          SgFunctionSymbol* fc_sym     = fc_ref_exp->get_symbol();
          SgFunctionDeclaration* fc_dec = fc_sym->get_declaration();
          SgInitializedNamePtrList fc_dummy = fc_dec->get_args();
          int num_dummy = 0;
          for (SgInitializedNamePtrList::iterator j = fc_dummy.begin();
               j != fc_dummy.end(); j++) {
            int dummy_intent_flag = 0 ;
            num_dummy = num_dummy + 1;
            // check the dummy in the same position with argument
            if (num_arg == num_dummy) {
              SgInitializedName* dummy_init = isSgInitializedName(*j);

              if (dummy_init != NULL) {
                dummy_intent_flag = checkModifierIntent(dummy_init);

                // check whether an argument is intent(out) or (inout)
                // SgTypeModifier::e_intent_out =    9
                // SgTypeModifier::e_intent_inout = 10
                if ( (dummy_intent_flag == 9) || (dummy_intent_flag == 10)) {
                  SgVariableSymbol* dummy_sym =
                    isSgVariableSymbol(dummy_init->get_symbol_from_symbol_table());
                  if (dummy_sym != NULL ) {
                    dummy_sym->setAttribute("att_assign_init",new myAstAttribute(2));
#ifdef _DEBUG_
                    std::cout << "\tSet attribute on node SgVariableSymbol for function argument.\n" << std::endl;
#endif
                  }
                }
              }
            } // end if num_arg
          } // end for loop j
        } // end arg_att
      }  // end arg_var
    } // end for loop i
  }
  break;
  default:
    return_flag = 1;
    break;
  } //  end switch
  if (return_flag == 1) {
    return;
  }

} //End of the visit function.

// Checker main run function and metadata

CompassAnalyses::ImplicitSave::Traversal2::
Traversal2(Compass::Parameters inputParameters, Compass::OutputObject* output)
  : Traversal(inputParameters,output),output(output) {
}

void
CompassAnalyses::ImplicitSave::Traversal2::
visit(SgNode* node) { // this visit is to get attribute
#ifdef _DEBUG_
  std::cout << "2nd Traversal: " << node->class_name() << std::endl;
#endif
  std::string   reason = "";
  //  to find assign op like   I = I + 1
  if (isSgAssignOp(node)) {
    SgAssignOp* aop = isSgAssignOp(node);
    // check if lhs is I
    if (isSgVarRefExp(aop->get_lhs_operand())) {
      SgVariableSymbol* var_sym_g = isSgVarRefExp(aop->get_lhs_operand())->get_symbol();

      // check if attribute "att_assign_init" is set
      AstAttribute* att = var_sym_g->getAttribute("att_assign_init");
      if ( att != NULL ) {
#ifdef _DEBUG_
        std::cout << "\t=> Find a node SgVariableSymbol with the attribute value:"
                  << dynamic_cast<myAstAttribute*>(att)->implicit_save_flag <<  std::endl;
#endif
        reason = "expression '"+node->unparseToString()  + "' value re-assigned" ;
        output->addOutput(new CheckerOutput(node,reason));

      }
    }
  }

  // check whether a "inout/out" argument in a functional call is implicit save
  if (isSgFunctionCallExp(node)) {
    SgFunctionCallExp*   fc    = isSgFunctionCallExp(node);
    SgExprListExp* arg_list = fc->get_args();
    SgExpressionPtrList arg = arg_list->get_expressions();
    int num_arg = 0;
    int isImplicitFound = 0;
    for (SgExpressionPtrList::iterator i = arg.begin(); i != arg.end(); i++) {
      num_arg = num_arg + 1;
      SgVarRefExp* arg_var = isSgVarRefExp(*i);
      if (arg_var != NULL ) {
        //check whether argument symbol has att set
        SgVariableSymbol* arg_var_sym_g = arg_var->get_symbol();

        //check if attribute "att_assign_init" is set
        AstAttribute* arg_att = arg_var_sym_g->getAttribute("att_assign_init");
        if ( arg_att != NULL ) {
#ifdef _DEBUG_
          std::cout << "\tFound arg which has been set implicit_save attribute:" <<
                    dynamic_cast<myAstAttribute*>(arg_att)->implicit_save_flag <<  std::endl;
#endif

          //check if argument is inout/out
          SgFunctionRefExp* fc_ref_exp = isSgFunctionRefExp(fc->get_function());
          SgFunctionSymbol* fc_sym     = fc_ref_exp->get_symbol();
          SgFunctionDeclaration* fc_dec = fc_sym->get_declaration();
          SgInitializedNamePtrList fc_dummy = fc_dec->get_args();
          int num_dummy = 0;
          for (SgInitializedNamePtrList::iterator j = fc_dummy.begin();
               j != fc_dummy.end(); j++) {
            int dummy_intent_flag = 0 ;
            num_dummy = num_dummy + 1;
            //check the dummy in the same position with argument
            if (num_arg == num_dummy) {
              SgInitializedName* dummy_init = isSgInitializedName(*j);

              if (dummy_init != NULL) {
                dummy_intent_flag = checkModifierIntent(dummy_init);

                //check whether an argument is intent(out) or (inout)
                //SgTypeModifier::e_intent_out =    9
                //SgTypeModifier::e_intent_inout = 10
                if ( (dummy_intent_flag == 9) || (dummy_intent_flag == 10)) {
                  isImplicitFound = 1;
                  //reason = "expression '"+node->unparseToString()  +
                  //         "' passed implicit saved variables as inout/out argument " ;
                  //output->addOutput(new CheckerOutput(node,reason));

                }
              }
            } // end if num_arg
          } // end for loop j
        } // end arg_att
      }  // end arg_var
    } // end for loop i
    if (isImplicitFound == 1) {
      reason = "expression '"+node->unparseToString()  +
               "' passed implicit saved variables as inout/out argument " ;
      output->addOutput(new CheckerOutput(node,reason));
    }
  } // end if isSgFunctionCallExp
  return;
} //End of the visit function.


static void run(Compass::Parameters params, Compass::OutputObject* output) {
  CompassAnalyses::ImplicitSave::Traversal(params, output).run(Compass::projectPrerequisite.getProject());
  CompassAnalyses::ImplicitSave::Traversal2(params, output).run(Compass::projectPrerequisite.getProject());
}

// Remove this function if your checker is not an AST traversal
static Compass::AstSimpleProcessingWithRunFunction* createTraversal(Compass::Parameters params, Compass::OutputObject* output) {
  return new CompassAnalyses::ImplicitSave::Traversal(params, output);
}

extern const Compass::Checker* const implicitSaveChecker =
  new Compass::CheckerUsingAstSimpleProcessing(
  "ImplicitSave",
  // Descriptions should not include the newline character "\n".
  CompassAnalyses::ImplicitSave::shortDescription,
  CompassAnalyses::ImplicitSave::longDescription,
  Compass::C | Compass::Cpp,
  Compass::PrerequisiteList(1, &Compass::projectPrerequisite),
  run,
  createTraversal);

