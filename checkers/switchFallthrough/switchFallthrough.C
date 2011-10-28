// -*- mode: C++; c-basic-offset: 2; indent-tabs-mode: nil -*-
// vim: expandtab:shiftwidth=2:tabstop=2

// Switch Fallthrough Analysis
// Author:
// Date: 27-October-2011

#include "rose.h"
#include "compass.h"

extern const Compass::Checker* const switchFallthroughChecker;

// DQ (1/17/2009): Added declaration to match external defined in file:
// rose/projects/compass/extensions/prerequisites/ProjectPrerequisite.h
// I can't tell that it is defined anywhere in compass except the extern
// declaration in ProjectPrerequisite.h
Compass::ProjectPrerequisite Compass::projectPrerequisite;

namespace CompassAnalyses {
  namespace SwitchFallthrough {
    /*! \brief Switch Fallthrough: Add your description here
     */

    // Specification of Checker Output Implementation
    class CheckerOutput: public Compass::OutputViolationBase {
      public:
        CheckerOutput(SgNode* node);
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

CompassAnalyses::SwitchFallthrough::
CheckerOutput::CheckerOutput ( SgNode* node )
  : OutputViolationBase(node,::switchFallthroughChecker->checkerName,::switchFallthroughChecker->shortDescription)
{}

CompassAnalyses::SwitchFallthrough::Traversal::
Traversal(Compass::Parameters, Compass::OutputObject* output)
  : output(output) {
  // Initalize checker specific parameters here, for example:
  // YourParameter = Compass::parseInteger(inputParameters["SwitchFallthrough.YourParameter"]);
}

void
CompassAnalyses::SwitchFallthrough::Traversal::
visit(SgNode* node) {
  // Implement your traversal here.
  const SgSwitchStatement* const theSwitch = isSgSwitchStatement(node);
  if (!theSwitch) return;

  // We're inside a switch statement
  bool has_break = true;
  if (isSgBasicBlock(theSwitch->get_body())) {
    const SgBasicBlock* const BBlock = isSgBasicBlock(theSwitch->get_body());
    const SgStatementPtrList BBlockStmts = BBlock->get_statements();
    for(SgStatementPtrList::const_iterator i = BBlockStmts.begin(); i != BBlockStmts.end(); i++){
      // Now we're iterating over the cases of the switch
      const SgCaseOptionStmt * const caseOption = isSgCaseOptionStmt(*i);
      if( caseOption != NULL ){
        if( isSgBasicBlock(caseOption->get_body()) ) {
          const SgBasicBlock* const caseBlock = isSgBasicBlock(caseOption->get_body());
          const SgStatementPtrList caseStmts = caseBlock->get_statements();
          // Now we have the statements for the case, check for 'break'
          has_break = has_break && !!isSgBreakStmt(&(*caseStmts.back()));
        }
      }
    }
  } else {
    if (isSgDefaultOptionStmt(theSwitch->get_body())) {
      if ( isSgBasicBlock(theSwitch->get_body()) ) {
        const SgBasicBlock* const BBlock = isSgBasicBlock(theSwitch->get_body());
        const SgStatementPtrList BBlockStmts = BBlock->get_statements();
        has_break = !!isSgBreakStmt(&(*BBlockStmts.back()));
      }
    }
  }
  if (!has_break) {
    output->addOutput(new CheckerOutput(node));
  }
} //End of the visit function.

// Checker main run function and metadata

static void run(Compass::Parameters params, Compass::OutputObject* output) {
  CompassAnalyses::SwitchFallthrough::Traversal(params, output).run(Compass::projectPrerequisite.getProject());
}

// Remove this function if your checker is not an AST traversal
static Compass::AstSimpleProcessingWithRunFunction* createTraversal(Compass::Parameters params, Compass::OutputObject* output) {
  return new CompassAnalyses::SwitchFallthrough::Traversal(params, output);
}

extern const Compass::Checker* const switchFallthroughChecker =
  new Compass::CheckerUsingAstSimpleProcessing(
  "SwitchFallthrough",
  // Descriptions should not include the newline character "\n".
  "switch statement has case that does not end with 'break;'",
  "Find switch statements where at least one case does not end with 'break'",
  Compass::C | Compass::Cpp,
  Compass::PrerequisiteList(1, &Compass::projectPrerequisite),
  run,
  createTraversal);

