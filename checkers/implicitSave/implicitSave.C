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
        myAstAttribute(bool save = false):save_flag(save) {}
        bool isSave() const { return save_flag; }
      private:
        const bool save_flag;
    };
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

  // This traversal sets attribute for the variables which are declared and initialized
  // There will be a SgAssignInitializer node if a variable is declared and initialized
  //       ex, INTEGER :: I = 1

  switch(node->variantT()) {
    case V_SgAssignInitializer: {
      const SgAssignInitializer* const ainit = isSgAssignInitializer(node);
      const SgInitializedName* const var_init = isSgInitializedName(ainit->get_parent());
      if (var_init != NULL) {
        const SgVariableDeclaration* const var_decl = isSgVariableDeclaration(var_init->get_parent());
        const SgDeclarationModifier var_decl_mod = var_decl->get_declarationModifier();
        const SgTypeModifier        var_decl_mod_type = var_decl_mod.get_typeModifier();
        //mark the node SgVariableSymbol by adding myAstAttribute
        SgVariableSymbol* const var_sym = isSgVariableSymbol(var_init->get_symbol_from_symbol_table());
        if (var_sym != NULL) {
          var_sym->setAttribute("att_assign_init", new myAstAttribute(var_decl_mod_type.isSave()));
        }
      }
    }
    default: break;
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
  //  to find assign op like   I = I + 1
  if (isSgAssignOp(node)) {
    const SgAssignOp* const aop = isSgAssignOp(node);
    // check if lhs is I
    if (isSgVarRefExp(aop->get_lhs_operand())) {
      const SgVariableSymbol* const var_sym_g = isSgVarRefExp(aop->get_lhs_operand())->get_symbol();

      // check if attribute "att_assign_init" is set
      const AstAttribute* const att = var_sym_g->getAttribute("att_assign_init");
      if ( att != NULL ) {
        const myAstAttribute * const myAtt = dynamic_cast<const myAstAttribute*>(att);
        if ( myAtt == NULL ) {
          abort(); // we should never get here
        }
        if ( myAtt && !myAtt->isSave() ) {
          const std::string reason = "expression '" + node->unparseToString() + "' value re-assigned";
          output->addOutput(new CheckerOutput(node,reason));
        }
      }
    }
  }
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

