// -*- mode: C++; c-basic-offset: 2; indent-tabs-mode: nil -*-
// vim: expandtab:shiftwidth=2:tabstop=2
#ifndef ARRAYINDEX_H
#define ARRAYINDEX_H

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
#include "PathGrader.h"
#include <staticSingleAssignment.h>
#include <backstroke/backstrokeCFG.h>
#include <boost/foreach.hpp>

#define foreach BOOST_FOREACH
//#include <sstream>
using namespace std;
extern const Compass::Checker* const arrayIndexChecker;

// DQ (1/17/2009): Added declaration to match external defined in file:
// rose/projects/compass/extensions/prerequisites/ProjectPrerequisite.h
// I can't tell that it is defined anywhere in compass except the extern
// declaration in ProjectPrerequisite.h
extern Compass::ProjectPrerequisite Compass::projectPrerequisite;

namespace CompassAnalyses {
  namespace ArrayIndex {
    /*! \brief Array Index: Add your description here
     */
    const std::string checkerName= "arrayIndex";
    const std::string shortDescription= "index bound check:";
    const std::string longDescription= "found statement with array index which may lead to buffer overflow";


    // Specification of Checker Output Implementation
    class CheckerOutput: public Compass::OutputViolationBase {
      public:
        CheckerOutput(SgNode* node, const std::string &);
    };
    // Specification of Checker Traversal Implementation

    class Traversal
        : public Compass::AstSimpleProcessingWithRunFunction {
        Compass::OutputObject& output;
        // Checker specific parameters should be allocated here.

      public:
        Traversal(Compass::Parameters inputParameters, Compass::OutputObject& output);

        // Change the implementation of this function if you are using inherited attributes.
        void *initialInheritedAttribute() const {
          return NULL;
        }

        // The implementation of the run function has to match the traversal being called.
        // If you use inherited attributes, use the following definition:
        // void run(SgNode* n){ this->traverse(n, initialInheritedAttribute()); }
        void run(SgNode* n);

        // Change this function if you are using a different type of traversal, e.g.
        // void *evaluateInheritedAttribute(SgNode *, void *);
        // for AstTopDownProcessing.
        void visit(SgNode* n);

        void scorePath(const SgVariableSymbol* const var,
                       SgFunctionDefinition * const fd,
                       SgPntrArrRefExp * const arrRef,
                       const StaticSingleAssignment::NodeReachingDefTable & defTable) const;


      private:
        std::auto_ptr<StaticSingleAssignment> ssa;
    };
  }
}
#endif /* ARRAYINDEX_H */
