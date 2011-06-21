/**
 * Copyright (c) 2005, 2006 Los Alamos National Security, LLC.  This
 * material was produced under U.S. Government contract DE-
 * AC52-06NA25396 for Los Alamos National Laboratory (LANL), which is
 * operated by the Los Alamos National Security, LLC (LANS) for the
 * U.S. Department of Energy. The U.S. Government has rights to use,
 * reproduce, and distribute this software. NEITHER THE GOVERNMENT NOR
 * LANS MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
 * LIABILITY FOR THE USE OF THIS SOFTWARE. If software is modified to
 * produce derivative works, such modified software should be clearly
 * marked, so as not to confuse it with the version available from
 * LANL.
 */
#include "rose.h"
#include "rewrite.h"
#include "SPUBuilder.h"
#include "SPUProgramAnalyzer.h"

using namespace SageBuilder;
using namespace SageInterface;
using namespace StringUtility;

class PrgmTranslator : public SgSimpleProcessing
{
  public : PrgmTranslator();
           ~PrgmTranslator();

           void visit(SgNode* astNode);

  	   SgProject* input_prj;
           SgTypedefDeclaration* vec_float;
           SgTypedefDeclaration* vec_double;
           SgTypedefDeclaration* vec_int;
           SgTypedefDeclaration* vec_long_long;
           SgTypedefDeclaration* vec_short;
           SgTypedefDeclaration* vec_char;
           SgTypedefDeclaration* vec_uint;
           SgTypedefDeclaration* vec_ulong_long;
           SgTypedefDeclaration* vec_ushort;
           SgTypedefDeclaration* vec_uchar;
           SgTypedefDeclaration* vec_cbool;
           SgTypedefDeclaration* vec_ibool;
           SgTypedefDeclaration* vec_sbool;

           SPUBuilder builder;
           PrgmAnalyzer analyzer;

           void addTypedef(SgGlobal* global_scope);
           void addHeader(SgGlobal* global_scope);
           void handleElementalFunctions(SgFunctionDeclaration* functionDeclaration, SgGlobal* global_scope);
           SgFunctionDeclaration* addFunctionDeclaration(SgFunctionDeclaration* functionDeclaration, SgGlobal* global_scope);
           void replaceVariableName(SgFunctionDeclaration* input_func, const char* oldname, const char* newname);
           void addStatementsToFunction(SgFunctionDeclaration* input_func, SgFunctionDeclaration* output_func, bool elemental);	

           void addVariableDeclarations(SgVariableDeclaration* variable, SgBasicBlock* output_block, bool elemental);
           void addSpecificVariables(SgBasicBlock* output_block);
           void initializeIntentTypeScalarVariables(SgVariableDeclaration* variable, SgBasicBlock* output_block);
           void initializeIntentTypeArrayVariables(SgVariableDeclaration* variable, SgBasicBlock* output_block);
           SgStatement* handleFortranShift(SgStatement* stmt, SgBasicBlock* output_block);
    
           SgType* getBaseType(SgType* vartype);
           void deleteTypedef(SgGlobal* global_scope);
};

