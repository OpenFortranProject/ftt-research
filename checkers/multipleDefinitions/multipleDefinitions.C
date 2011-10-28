// -*- mode: C++; c-basic-offset: 2; indent-tabs-mode: nil -*-
// vim: expandtab:shiftwidth=2:tabstop=2

// Multiple Definitions Analysis
// Author:
// Date: 27-October-2011

#include "rose.h"
#include "compass.h"

extern const Compass::Checker* const multipleDefinitionsChecker;

// DQ (1/17/2009): Added declaration to match external defined in file:
// rose/projects/compass/extensions/prerequisites/ProjectPrerequisite.h
// I can't tell that it is defined anywhere in compass except the extern
// declaration in ProjectPrerequisite.h
Compass::ProjectPrerequisite Compass::projectPrerequisite;

namespace CompassAnalyses {
  namespace MultipleDefinitions {
    /*! \brief Multiple Definitions: Add your description here
     */
    extern const std::string checkerName      = "Multiple Definitions";
    extern const std::string shortDescription = "Multiple definitions in single declaration found: ";
    extern const std::string longDescription  = "Found declaration statement with multiple definitions:";

    // Specification of Checker Output Implementation
    class CheckerOutput: public Compass::OutputViolationBase {
      public:
        CheckerOutput(SgNode* node, const std::string & reason);
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

CompassAnalyses::MultipleDefinitions::
CheckerOutput::CheckerOutput ( SgNode* node, const std::string & reason )
  : OutputViolationBase(node,::multipleDefinitionsChecker->checkerName,::multipleDefinitionsChecker->shortDescription+reason)
{}

CompassAnalyses::MultipleDefinitions::Traversal::
Traversal(Compass::Parameters, Compass::OutputObject* output)
  : output(output) {
  // Initalize checker specific parameters here, for example:
  // YourParameter = Compass::parseInteger(inputParameters["MultipleDefinitions.YourParameter"]);


}

void
CompassAnalyses::MultipleDefinitions::Traversal::
visit(SgNode* node) {
  // Implement your traversal here.
  const SgVariableDeclaration * const var_decl = isSgVariableDeclaration(node);
  if( var_decl != NULL ){
    const SgInitializedNamePtrList & name_list = var_decl->get_variables();
    if( name_list.size() > 1 ) {
      std::string reason;
      for(SgInitializedNamePtrList::const_iterator i = name_list.begin(); i != name_list.end(); i++){
        reason += std::string((*i)->get_name().str());
        if( i+1 != name_list.end() ) {
          reason += ", ";
        }
      }
      output->addOutput(new CheckerOutput(node,"'"+reason+"'"));
    }
  }

} //End of the visit function.

// Checker main run function and metadata

static void run(Compass::Parameters params, Compass::OutputObject* output) {
  CompassAnalyses::MultipleDefinitions::Traversal(params, output).run(Compass::projectPrerequisite.getProject());
}

// Remove this function if your checker is not an AST traversal
static Compass::AstSimpleProcessingWithRunFunction* createTraversal(Compass::Parameters params, Compass::OutputObject* output) {
  return new CompassAnalyses::MultipleDefinitions::Traversal(params, output);
}

extern const Compass::Checker* const multipleDefinitionsChecker =
  new Compass::CheckerUsingAstSimpleProcessing(
  CompassAnalyses::MultipleDefinitions::checkerName,
  // Descriptions should not include the newline character "\n".
  CompassAnalyses::MultipleDefinitions::shortDescription,
  CompassAnalyses::MultipleDefinitions::longDescription,
  Compass::Fortran,
  Compass::PrerequisiteList(1, &Compass::projectPrerequisite),
  run,
  createTraversal);

