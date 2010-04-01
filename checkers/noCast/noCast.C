// -*- mode: C++; c-basic-offset: 2; indent-tabs-mode: nil -*-
// vim: expandtab:shiftwidth=2:tabstop=2

// No Cast Analysis for Fortran code
// Author: ,,,
// Date: 18-September-2009

#include "rose.h"
#include "compass.h"

extern const Compass::Checker* const noCastChecker;

// DQ (1/17/2009): Added declaration to match external defined in file:
// rose/projects/compass/extensions/prerequisites/ProjectPrerequisite.h
// I can't tell that it is defined anywhere in compass except the extern 
// declaration in ProjectPrerequisite.h
Compass::ProjectPrerequisite Compass::projectPrerequisite;

namespace CompassAnalyses
   { 
     namespace NoCast
        { 
        /*! \brief No Cast: Add your description here 
         */
           extern const std::string checkerName= "NoCast";
           extern const std::string shortDescription= "Implicit cast in Fortran code found: ";
           extern const std::string longDescription= "Finding statement with implicit cast in a Fortran code :";


       // Specification of Checker Output Implementation
          class CheckerOutput: public Compass::OutputViolationBase
             { 
               public:
                    CheckerOutput(SgNode* node, const std::string &);
             };

       // Specification of Checker Traversal Implementation

          class Traversal
             : public Compass::AstSimpleProcessingWithRunFunction 
             {
                    Compass::OutputObject* output;
            // Checker specific parameters should be allocated here.

               public:
                    Traversal(Compass::Parameters inputParameters, Compass::OutputObject* output);

                 // Change the implementation of this function if you are using inherited attributes.
                    void *initialInheritedAttribute() const { return NULL; }

                 // The implementation of the run function has to match the traversal being called.
                 // If you use inherited attributes, use the following definition:
                 // void run(SgNode* n){ this->traverse(n, initialInheritedAttribute()); }
                    void run(SgNode* n){ this->traverse(n, preorder); }

                 // Change this function if you are using a different type of traversal, e.g.
                 // void *evaluateInheritedAttribute(SgNode *, void *);
                 // for AstTopDownProcessing.
                    void visit(SgNode* n);
             };
        }
   }

CompassAnalyses::NoCast::
CheckerOutput::CheckerOutput ( SgNode* node, const std::string & reason )
   : OutputViolationBase(node,checkerName,shortDescription+reason)
   {}

CompassAnalyses::NoCast::Traversal::
Traversal(Compass::Parameters inputParameters, Compass::OutputObject* output)
   : output(output)
   {
  // Initalize checker specific parameters here, for example: 
  // YourParameter = Compass::parseInteger(inputParameters["NoCast.YourParameter"]);


   }

void
CompassAnalyses::NoCast::Traversal::
visit(SgNode* node)
   { 
  // Implement your traversal here.  
     SgBinaryOp* b_node = isSgBinaryOp(node);

     if ( b_node == NULL) {
       return;
     }
      
      // check operand in a binary operator
      if ( ( isSgAssignOp(b_node) ) || (isSgAddOp(b_node)) || (isSgSubtractOp(b_node)) || (isSgDivideOp(b_node)) || (isSgMultiplyOp(b_node)) )
       { 
             SgType* type = b_node->get_type(); 
             std::string type_operator_string = type->unparseToString();

             SgExpression* l_operand =  b_node->get_lhs_operand();
             SgExpression* r_operand =  b_node->get_rhs_operand();

             SgType* type_l_operand = l_operand->get_type();
             SgType* type_r_operand = r_operand->get_type();
             std::string type_l_operand_string = type_l_operand->unparseToString();
             std::string type_r_operand_string = type_r_operand->unparseToString();

             std::string reason = "" ;
             if ( type_r_operand_string != type_l_operand_string)
               {
                SgFunctionType* ftype_l = isSgFunctionType(type_l_operand);
                SgFunctionType* ftype_r = isSgFunctionType(type_r_operand);
                // if the child node is SgFunctionType 
                if (ftype_l != NULL ) 
                   { if (ftype_l->get_return_type()->unparseToString() == type_r_operand_string)
                      {return;} 
                    }


                if (ftype_r != NULL ) 
                   { if (ftype_r->get_return_type()->unparseToString()  == type_l_operand_string)
                      {return;}
                   }

                // the child node is SgFunctionType and its type different from operator type
                if (type_r_operand_string != type_operator_string)
                 {  
                  reason = " expr '" + r_operand->unparseToString()+"' from "+ type_r_operand_string + " to " + type_operator_string ;
                  output->addOutput(new CheckerOutput(r_operand,reason));               
                  // std::cout << "===>" << checkerName << ":" << b_node->get_file_info()->get_filenameString()<< ": Implicit type conversion from " << type_r_operand_string << " to " << type_operator_string << " on line " <<  b_node->get_file_info()->get_line() << "." << r_operand->get_file_info()->get_col()  << std::endl;
                  
                 }
                if (type_l_operand_string != type_operator_string)
                 {                 
                  reason = " expr '" + l_operand->unparseToString()+"' from "+ type_l_operand_string + " to " + type_operator_string ;
                  output->addOutput(new CheckerOutput(l_operand,reason));
                  // std::cout << "===>" << checkerName << ":" << b_node->get_file_info()->get_filenameString()<< ": Implicit type conversion from " << type_l_operand_string << " to " << type_operator_string << " on line " <<  b_node->get_file_info()->get_line() << "." << l_operand->get_file_info()->get_col()  << std::endl;
                 }

                //std::cout << "===>" << checkerName << ":" << b_node->get_file_info()->get_filenameString()<< ": Implicit type conversion from " << type_r_operand_string << " to " << type_l_operand_string << " on line " <<  b_node->get_file_info()->get_line() << "." << l_operand->get_file_info()->get_col()  << std::endl;
               } 
      

           }


    
 
   } //End of the visit function.

// Checker main run function and metadata

static void run(Compass::Parameters params, Compass::OutputObject* output) {
  CompassAnalyses::NoCast::Traversal(params, output).run(Compass::projectPrerequisite.getProject());
}

// Remove this function if your checker is not an AST traversal
static Compass::AstSimpleProcessingWithRunFunction* createTraversal(Compass::Parameters params, Compass::OutputObject* output) {
  return new CompassAnalyses::NoCast::Traversal(params, output);
}

extern const Compass::Checker* const noCastChecker =
  new Compass::CheckerUsingAstSimpleProcessing(
        CompassAnalyses::NoCast::checkerName,
     // Descriptions should not include the newline character "\n".
        CompassAnalyses::NoCast::shortDescription,
        CompassAnalyses::NoCast::longDescription,
        Compass::C | Compass::Cpp,
        Compass::PrerequisiteList(1, &Compass::projectPrerequisite),
        run,
        createTraversal);
   
