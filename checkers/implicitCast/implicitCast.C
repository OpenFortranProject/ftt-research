// -*- mode: C++; c-basic-offset: 2; indent-tabs-mode: nil -*-
// vim: expandtab:shiftwidth=2:tabstop=2

// No Cast Analysis for Fortran code
// Author: ,,,
// Date: 18-September-2009

#include "rose.h"
#include "sageInterface.h"
#include "compass.h"
#include "ConfigParser.hpp"
#include "TypeDescription.hpp"

extern const Compass::Checker* const implicitCastChecker;

// DQ (1/17/2009): Added declaration to match external defined in file:
// rose/projects/compass/extensions/prerequisites/ProjectPrerequisite.h
// I can't tell that it is defined anywhere in compass except the extern
// declaration in ProjectPrerequisite.h
Compass::ProjectPrerequisite Compass::projectPrerequisite;

namespace CompassAnalyses {
  namespace ImplicitCast {
    /*! \brief No Cast: Add your description here
     */
    extern const std::string checkerName= "ImplicitCast";
    extern const std::string shortDescription= "Implicit cast in Fortran code found: ";
    extern const std::string longDescription= "Finding statement with implicit cast in a Fortran code :";

    const char confFile[] = "implicitCast.conf";


// Specification of Checker Output Implementation
    class CheckerOutput: public Compass::OutputViolationBase {
      public:
        CheckerOutput(SgNode * const node, const std::string &);
    };

// Specification of Checker Traversal Implementation

    class Traversal
        : public Compass::AstSimpleProcessingWithRunFunction {
        Compass::OutputObject* output;
        // Checker specific parameters should be allocated here.

      public:
        typedef std::map<std::pair<TypeDescription, TypeDescription>, ConfigParser::warn_e> rules_t;
        typedef std::map<TypeDescription, TypeDescription> aliases_t;

        Traversal(Compass::Parameters inputParameters, Compass::OutputObject* output,
                  const ConfigParser::rules_t &rules, const ConfigParser::aliases_t &aliases);

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
      private:
        rules_t   rules;
        aliases_t aliases;

        void handleTypes(const TypeDescription &td_l, const TypeDescription &td_r,
                         SgExpression * const l_operand, SgExpression * const r_operand);

        SgType * buildType(const std::string &type) const {
          if( type.size() < 1) return NULL;
          // TODO: there is some risk that these types do not match
          // the default kinds of the underlying fortran compiler
          if( type == "complex" ){
            return SageBuilder::buildComplexType(SageBuilder::buildFloatType());
          } else if( type == "real" ){
            return SageBuilder::buildFloatType();
          } else if( type == "integer" ){
            return SageBuilder::buildIntType();
          } else if( type == "logical" ){
            return SageBuilder::buildBoolType();
          } else if( type == "character" ){
            return SageBuilder::buildCharType();
          }
          // The above works for variables with no explicit kind information
          // Now we handle the other cases.
          const int kind_num = parseKind(type);
          if( kind_num == 0 ) return NULL;

          const size_t pos = type.find_last_of("_");
          if( pos <= 0 || pos >= type.npos ) return NULL;

          SgType * baseType = buildType(type.substr(0, pos));
          if( baseType == NULL ) return NULL;
          SgTreeCopy ch = SgTreeCopy();
          baseType = isSgType(baseType->copy(ch));
          if( baseType == NULL ) return NULL;

          // At this point we have a valid kind and a valid SgType.
          SgIntVal * const kindExpr = SageBuilder::buildIntVal(kind_num);
          baseType->set_type_kind(kindExpr);
          return baseType;
        }

        rules_t buildRules(const ConfigParser::rules_t& inputRules, aliases_t aliases) const
        {
          rules_t ret;
          for(ConfigParser::rules_t::const_iterator i = inputRules.begin(); i != inputRules.end(); i++)
          {
             SgType * const fromType = buildType(i->first.first);
             SgType * const toType   = buildType(i->first.second);

             const int fromTypeKind = parseKind(i->first.first);
             const int toTypeKind   = parseKind(i->first.second);

             SgExpression * fromTypeKindExpr = fromTypeKind == 0 ? NULL : SageBuilder::buildIntVal(fromTypeKind);
             SgExpression * toTypeKindExpr   = toTypeKind   == 0 ? NULL : SageBuilder::buildIntVal(toTypeKind);

             const TypeDescription fromTD = buildTypeDescription(fromType, fromTypeKindExpr);
             const TypeDescription toTD   = buildTypeDescription(toType, toTypeKindExpr);

             // Jumping through this hoop allows us to avoid having a copy constructor, which in turn allows
             // for immutable TypeDescriptions.  Yay?
             const TypeDescription aliasFromTD = aliases.find(fromTD) != aliases.end() ? aliases.find(fromTD)->second : fromTD;
             const TypeDescription aliasToTD   = aliases.find(toTD)   != aliases.end() ? aliases.find(toTD)->second   : toTD;

             const std::pair<TypeDescription, TypeDescription> cast(std::make_pair(aliasFromTD,aliasToTD));

             ret.insert(std::make_pair(cast, i->second));
          }
          return ret;
        }

        aliases_t buildAliases(const ConfigParser::aliases_t& inputAliases) const
        {
          aliases_t ret;
          for(ConfigParser::aliases_t::const_iterator i = inputAliases.begin(); i != inputAliases.end(); i++)
          {
             SgType * const fromType = buildType(i->first);
             SgType * const toType   = buildType(i->second);

             const int fromTypeKind = parseKind(i->first);
             const int toTypeKind   = parseKind(i->second);

             SgExpression * fromTypeKindExpr = fromTypeKind == 0 ? NULL : SageBuilder::buildIntVal(fromTypeKind);
             SgExpression * toTypeKindExpr   = toTypeKind   == 0 ? NULL : SageBuilder::buildIntVal(toTypeKind);

             const TypeDescription fromTD = buildTypeDescription(fromType,fromTypeKindExpr);
             const TypeDescription toTD   = buildTypeDescription(toType, toTypeKindExpr);

             ret.insert(std::make_pair(fromTD, toTD));
          }
          return ret;
        }
    };
  }
}

CompassAnalyses::ImplicitCast::
CheckerOutput::CheckerOutput ( SgNode * const node, const std::string & reason )
  : OutputViolationBase(node,checkerName,shortDescription+reason)
{}

CompassAnalyses::ImplicitCast::Traversal::
Traversal(Compass::Parameters, Compass::OutputObject* output,
          const ConfigParser::rules_t &rules, const ConfigParser::aliases_t &aliases)
  : output(output) {
    this->aliases = buildAliases(aliases);
    this->rules = buildRules(rules, this->aliases);
  // Initalize checker specific parameters here, for example:
  // YourParameter = Compass::parseInteger(inputParameters["ImplicitCast.YourParameter"]);

}

void CompassAnalyses::ImplicitCast::Traversal::
handleTypes(const TypeDescription &td_l, const TypeDescription &td_r,
            SgExpression * const l_operand, SgExpression * const r_operand)
{
  std::pair<TypeDescription, TypeDescription> cast(std::make_pair(td_r, td_l));

  rules_t::const_iterator i = rules.find(cast);
  if( i != rules.end() ) {
    ConfigParser::warn_e action = i->second;
    switch(action) {
      case ConfigParser::eOK: /* Do nothing */ break;
      case ConfigParser::eWARN:
      case ConfigParser::eERROR: {
        std::stringstream reason;
        reason << " expr '" << r_operand->unparseToString() << "' from "
               << td_r << " to " << td_l;
        output->addOutput(new CheckerOutput(l_operand,reason.str()));
      }
      default: break;
    }
  }
}

void
CompassAnalyses::ImplicitCast::Traversal::
visit(SgNode* node) {
  // Implement your traversal here.
  SgBinaryOp const * const b_node = isSgBinaryOp(node);

  if (b_node == NULL) {
    return;
  }

  // check operand in a binary operator
  if ( isSgAssignOp(b_node) || isSgAddOp(b_node) || isSgSubtractOp(b_node)
       || isSgDivideOp(b_node) || isSgMultiplyOp(b_node) ) {
    SgType * const type = b_node->get_type();
    const std::string type_operator_string = type->unparseToString();

    SgExpression * const l_operand = b_node->get_lhs_operand();
    SgExpression * const r_operand = b_node->get_rhs_operand();

    SgType * const type_l_operand = l_operand->get_type();
    SgType * const type_r_operand = r_operand->get_type();

    SgFunctionType const * const ftype_l = isSgFunctionType(type_l_operand);
    SgFunctionType const * const ftype_r = isSgFunctionType(type_r_operand);

    // if the child node is SgFunctionType
    if (ftype_l != NULL ) {
      SgExpression * ftypeKind = ftype_l->get_return_type()->get_type_kind();
      SgExpression * type_r_kind = type_r_operand->get_type_kind();
      handleTypes(buildTypeDescription(ftype_l->get_return_type(), ftypeKind),
                  buildTypeDescription(type_r_operand, type_r_kind), l_operand, r_operand);
    } else if (ftype_r != NULL ) {
      SgExpression * ftypeKind = ftype_r->get_return_type()->get_type_kind();
      SgExpression * type_l_kind = type_l_operand->get_type_kind();
      handleTypes(buildTypeDescription(ftype_r->get_return_type(), ftypeKind),
                  buildTypeDescription(type_l_operand, type_l_kind), r_operand, l_operand);
    } else {
      SgExpression * type_l_kind = type_l_operand->get_type_kind();
      SgExpression * type_r_kind = type_r_operand->get_type_kind();
      TypeDescription td_l(buildTypeDescription(type_l_operand, type_l_kind));
      TypeDescription td_r(buildTypeDescription(type_r_operand, type_r_kind));
      handleTypes(td_l, td_r, l_operand, r_operand);
    }
  }
} //End of the visit function.


// Checker main run function and metadata

static void run(Compass::Parameters params, Compass::OutputObject* output) {
  std::ifstream configFile(CompassAnalyses::ImplicitCast::confFile);
  ConfigParser cp;
  std::pair<ConfigParser::rules_t, ConfigParser::aliases_t> config = cp.parseFile(configFile);
  CompassAnalyses::ImplicitCast::Traversal(params, output, config.first, config.second).run(Compass::projectPrerequisite.getProject());
}

// Remove this function if your checker is not an AST traversal
static Compass::AstSimpleProcessingWithRunFunction* createTraversal(Compass::Parameters params, Compass::OutputObject* output) {
  std::ifstream configFile(CompassAnalyses::ImplicitCast::confFile);
  ConfigParser cp;
  std::pair<ConfigParser::rules_t, ConfigParser::aliases_t> config = cp.parseFile(configFile);
  return new CompassAnalyses::ImplicitCast::Traversal(params, output, config.first, config.second);
}

extern const Compass::Checker* const implicitCastChecker =
  new Compass::CheckerUsingAstSimpleProcessing(
  CompassAnalyses::ImplicitCast::checkerName,
  // Descriptions should not include the newline character "\n".
  CompassAnalyses::ImplicitCast::shortDescription,
  CompassAnalyses::ImplicitCast::longDescription,
  Compass::Fortran,
  Compass::PrerequisiteList(1, &Compass::projectPrerequisite),
  run,
  createTraversal);

