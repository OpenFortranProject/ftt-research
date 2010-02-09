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

#include "SPUProgramTranslator.h"

PrgmTranslator::PrgmTranslator()
{

}

PrgmTranslator::~PrgmTranslator()
{

}

void PrgmTranslator::visit(SgNode* astNode)
{
  SgGlobal* global_scope = isSgGlobal(astNode);
  if(global_scope != NULL)
  {
    addTypedef(global_scope);
    addHeader(global_scope);    

    if(is_C_language())
      printf("File is C file\r\n");
    else if(is_Fortran_language())
      printf("Fortran file\r\n");

    functionWithAttributesPTR functions;
    int noOfFunctions;  


    noOfFunctions = analyzer.analyze(input_prj);

    for(int i = 0; i < noOfFunctions; i++)
    {
      //if((*i)->get_file_info()->isCompilerGenerated() == false)
      {
        SgFunctionDeclaration* functionDeclaration =isSgFunctionDeclaration(analyzer.listOfFunctns[i].functionDeclaration);
        ROSE_ASSERT(functionDeclaration != NULL);

        printf("Dealing with the function %s\r\n", functionDeclaration->get_name().str()); 
     
        SgFunctionModifier func_modifier = functionDeclaration->get_functionModifier();
        bool elemental = analyzer.listOfFunctns[i].elemental | func_modifier.isElemental();
        if(elemental)
        {
          printf("Elemental function\r\n");
          handleElementalFunctions(functionDeclaration, global_scope);
        }
        else
        {
          SgFunctionDeclaration* func = addFunctionDeclaration(functionDeclaration, global_scope);
	
          SgBasicBlock* func_body = func->get_definition()->get_body();

          addStatementsToFunction(functionDeclaration, func, false);
	
          SgReturnStmt* returnstmt = buildReturnStmt(buildIntVal(0));
          appendStatement(returnstmt, func_body);
      
          appendStatement(func, global_scope);	
        }
      }
    }
    deleteTypedef(global_scope);
    printf("Finished Translation\r\n");
  } 
}

void PrgmTranslator::addTypedef(SgGlobal* global_scope)
{
  if(global_scope != NULL)
  {
    pushScopeStack(global_scope);

    vec_float = buildTypedefDeclaration("vec_float4", buildFloatType());
    vec_double = buildTypedefDeclaration("vec_double2", buildDoubleType());
    vec_int = buildTypedefDeclaration("vec_int4", buildIntType());
    vec_long_long = buildTypedefDeclaration("vec_llong2", buildLongLongType());
    vec_short = buildTypedefDeclaration("vec_short8", buildShortType());
    vec_char = buildTypedefDeclaration("vec_char16", buildCharType());
    vec_uint = buildTypedefDeclaration("vec_uint4", buildUnsignedIntType());
    vec_ulong_long = buildTypedefDeclaration("vec_ullong2", buildUnsignedLongLongType());
    vec_ushort = buildTypedefDeclaration("vec_ushort8", buildUnsignedShortType());
    vec_uchar = buildTypedefDeclaration("vec_uchar16", buildUnsignedCharType());
    vec_cbool = buildTypedefDeclaration("vec_bchar16", buildBoolType());
    vec_ibool = buildTypedefDeclaration("vec_bint4", buildBoolType());
    vec_sbool = buildTypedefDeclaration("vec_bshort8", buildBoolType());

    prependStatement(vec_float, global_scope);
    prependStatement(vec_double, global_scope);
    prependStatement(vec_int, global_scope);
    prependStatement(vec_long_long, global_scope);
    prependStatement(vec_short, global_scope);
    prependStatement(vec_char, global_scope);
    prependStatement(vec_uint, global_scope);
    prependStatement(vec_ulong_long, global_scope);
    prependStatement(vec_ushort, global_scope);
    prependStatement(vec_uchar, global_scope);
    prependStatement(vec_cbool, global_scope);
    prependStatement(vec_ibool, global_scope);
    prependStatement(vec_sbool, global_scope);

    SgTypedefDeclaration* dummy = buildTypedefDeclaration("integer", buildIntType());
    prependStatement(dummy, global_scope);

    popScopeStack();
  } 
}

void PrgmTranslator::addHeader(SgGlobal* global_scope)
{
  if(global_scope != NULL)
  {
    pushScopeStack(global_scope);

    insertHeader("spu_intrinsics.h",false);
    insertHeader("simdmath.h",false);
    insertHeader("FTT_SPU.h",false);
    insertHeader("alf_accel.h",false);
 
    popScopeStack();
  }
}

void PrgmTranslator::handleElementalFunctions(SgFunctionDeclaration* functionDeclaration, SgGlobal* global_scope)
{
  SgProcedureHeaderStatement* procedure = isSgProcedureHeaderStatement(functionDeclaration);
  Rose_STL_Container<SgInitializedName*> func_params = functionDeclaration->get_args();
  SgFunctionParameterList* paramlist = buildFunctionParameterList();
  SgTypedefType* typedef_type;
  SgName givename = functionDeclaration->get_name();
  
  for(Rose_STL_Container<SgInitializedName*>::iterator j = func_params.begin(); j != func_params.end(); j++)
  {
    SgInitializedName* varname = isSgInitializedName(*j);
    if(varname != NULL)
    {
      SgVariableDeclaration* varDeclare = NULL;
      SgType* type = NULL;

      type = getBaseType(varname->get_type());
 
      if(type != NULL)
      { 
        SgInitializedName* var = buildInitializedName(varname->get_name(), type);
        appendArg(paramlist, var);
      }
    }
  }

  SgType* return_type = NULL;

  char *varname, *oldname;
  if((procedure != NULL) && (procedure->isFunction()) && (procedure->get_result_name() != NULL) )
  {
    SgName result = procedure->get_result_name()->get_name();
    printf("Result name is %s \r\n", result.str());
    varname = (char *)malloc((result.get_length() + 5) * sizeof(char));
    strcpy(varname, result.str()); 
  }
  else
  {
    varname = (char *)malloc((givename.get_length() + 5) * sizeof(char));
    oldname = (char *)malloc((givename.get_length() + 5) * sizeof(char));

    strcpy(oldname, givename.str());
    strcpy(varname, givename.str()); 

    strcat(varname, "_rtn");       
    replaceVariableName(functionDeclaration, (const char*) oldname, (const char*) varname); 
  }
 
  return_type = getBaseType(functionDeclaration->get_type());    
  
  SgFunctionDeclaration* func =  buildDefiningFunctionDeclaration(givename, return_type, paramlist, global_scope);

  SgBasicBlock* func_body = func->get_definition()->get_body();
  func->get_functionModifier().setInline();

  if(!is_C_language())
  {
    SgVariableDeclaration* varDeclare = buildVariableDeclaration(varname, return_type);
    appendStatement(varDeclare, func_body);
  }

  addStatementsToFunction(functionDeclaration, func, true);

  if(!is_C_language())
  {
    SgReturnStmt* returnstmt = buildReturnStmt(buildVarRefExp(varname, func_body));
    appendStatement(returnstmt, func_body);
  }

  appendStatement(func, global_scope);
}

SgFunctionDeclaration* PrgmTranslator::addFunctionDeclaration(SgFunctionDeclaration* functionDeclaration, SgGlobal* global_scope)
{
  SgName givename = functionDeclaration->get_name();
  char *fname;
  fname = (char *)malloc((givename.get_length() + 5) * sizeof(char));
 
  strcpy(fname, "spu_");       
  strcat(fname, givename.str());
 	 
  SgPointerType* ptr = buildPointerType(buildVoidType());

  SgName var1name = "p_task_context __attribute__ ((unused))";
  SgInitializedName* var1 = buildInitializedName(var1name, ptr);
	
  SgName var2name = "p_parm_ctx_buffer";
  SgInitializedName* var2 = buildInitializedName(var2name, ptr);
	
  SgName var3name = "p_input_buffer";
  SgInitializedName* var3 = buildInitializedName(var3name, ptr);
	
  SgName var4name = "p_output_buffer";
  SgInitializedName* var4 = buildInitializedName(var4name, ptr);
	
  SgName var5name = "p_inout_buffer";
  SgInitializedName* var5 = buildInitializedName(var5name, ptr);
	
  SgName var6name = "current_count __attribute__ ((unused))";
  SgInitializedName* var6 = buildInitializedName(var6name, buildUnsignedIntType());

  SgName var7name = "total_count __attribute__ ((unused))";
  SgInitializedName* var7 = buildInitializedName(var7name, buildUnsignedIntType());

  SgFunctionParameterList* paramlist = buildFunctionParameterList();
  appendArg(paramlist, var1);
  appendArg(paramlist, var2);
  appendArg(paramlist, var3);
  appendArg(paramlist, var4);
  appendArg(paramlist, var5);
  appendArg(paramlist, var6);
  appendArg(paramlist, var7);

  SgName funcname = fname;
  SgFunctionDeclaration* func = buildDefiningFunctionDeclaration(funcname, buildIntType(), paramlist, global_scope);

  return func;
}

void PrgmTranslator::replaceVariableName(SgFunctionDeclaration* input_func, const char* oldname, const char* newname)
{
  Rose_STL_Container<SgNode*> varRefList = NodeQuery::querySubTree(input_func->get_definition()->get_body(),V_SgFunctionRefExp);
  
  for(Rose_STL_Container<SgNode*>::iterator i = varRefList.end() - 1; i != varRefList.begin() - 1; i--)
  { 		
    printf("count \r\n");
    if(isSgFunctionRefExp(isSgNode(*i)) != NULL)
    {
      SgFunctionRefExp* varRef = isSgFunctionRefExp((*i));
     
      if(varRef->get_symbol()->get_name() != NULL)
        printf("%s : Function name is %s\r\n", __FUNCTION__, varRef->get_symbol()->get_name().str());
      if(strcmp(oldname, varRef->get_symbol()->get_name().str()) == 0)
      {
        printf("Replacing %s with variable name is %s\r\n", varRef->get_symbol()->get_name().str(), newname);
        SgVarRefExp* new_varRef = buildVarRefExp(newname, input_func->get_definition()->get_body());
        replaceExpression(varRef, new_varRef);
      }
    }
    else if(isSgFunctionCallExp(*i) != NULL)
          printf("Function call exp\r\n");	
  }
}

void PrgmTranslator::addStatementsToFunction(SgFunctionDeclaration* input_func, SgFunctionDeclaration* output_func, bool elemental)
{
  SgBasicBlock* input_block = input_func->get_definition()->get_body();
  SgBasicBlock* output_block = output_func->get_definition()->get_body();

  Rose_STL_Container<SgStatement*> statementlist = input_block->get_statements();
 
  for(Rose_STL_Container<SgStatement*>::iterator i = statementlist.begin(); i != statementlist.end(); i++)
  { 		
    SgStatement* stmt = isSgStatement(isSgNode(*i));
    if(stmt != NULL)
    {
      if(isSgVariableDeclaration(isSgNode(*i)))
      {
        addVariableDeclarations(isSgVariableDeclaration(isSgNode(*i)), output_block, elemental); 
      }
    }	
  }

  if(!elemental)
  { 
    printf("ADD Specific variables\r\n");
    addSpecificVariables(output_block); 

    printf("Initialize scalar variables\r\n");
    for(Rose_STL_Container<SgStatement*>::iterator i = statementlist.begin(); i != statementlist.end(); i++)
    {
      SgStatement* stmt = isSgStatement(isSgNode(*i));
      if(stmt != NULL)
      {
        if(isSgVariableDeclaration(isSgNode(*i)))
        {
          initializeIntentTypeScalarVariables(isSgVariableDeclaration(isSgNode(*i)), output_block);
        }
      }
    }
     
    printf("Initialize array variables\r\n");
    for(Rose_STL_Container<SgStatement*>::iterator i = statementlist.begin(); i != statementlist.end(); i++)
    {
      SgStatement* stmt = isSgStatement(isSgNode(*i));
      if(stmt != NULL)
      {
        if(isSgVariableDeclaration(isSgNode(*i)))
        {
          initializeIntentTypeArrayVariables(isSgVariableDeclaration(isSgNode(*i)), output_block);
        }
      }
    }
  }
 
  
  int build_for = 0;
  SgBasicBlock* for_loop_blk = buildBasicBlock();
  
  SgName i_var = "i";
  SgVarRefExp* i_ref = buildVarRefExp(i_var, output_block);

  SgName count_var = "count";
  SgVarRefExp* count_ref = buildVarRefExp(count_var, output_block);

  SgExprStatement* condition_stmt = buildExprStatement(buildLessThanOp(i_ref,count_ref));
  SgExprStatement* initialize_stmt = buildAssignStatement(i_ref, buildIntVal(0));
  SgExpression* increment_expr = buildPlusPlusOp(i_ref, SgUnaryOp::postfix);

  for(Rose_STL_Container<SgStatement*>::iterator i = statementlist.begin(); i != statementlist.end(); i++)
  {
    SgStatement* stmt = isSgStatement(isSgNode(*i));

    if(build_for == 2)
    {
      build_for = 0;

      SgBasicBlock* block = buildBasicBlock();
      Rose_STL_Container<SgStatement*> stmtlist = for_loop_blk->get_statements();
 
      for(Rose_STL_Container<SgStatement*>::iterator j = stmtlist.begin(); j != stmtlist.end(); j++)
      {
        appendStatement(isSgStatement(*j), block); 
      }
    
      SgForStatement* for_stmt = buildForStatement(initialize_stmt, condition_stmt, increment_expr, block);
      ROSE_ASSERT(for_stmt != NULL);
      appendStatement(for_stmt, output_block);

      for_loop_blk = buildBasicBlock();
    }

    if(stmt != NULL)
    {
      if(isSgIfStmt(*i)) 
      {
        build_for = 0;
        SgStatementPtrList func_callstmt = builder.build_select_stmt(isSgStatement(*i), output_block);
        if(*func_callstmt.begin() != NULL)
        {
          appendStatementList(func_callstmt, output_block);
        }
      }  
      else if(isSgWhereStatement(*i) || isSgElseWhereStatement(*i))
      {
        if(build_for == 0)
        {
          build_for = 1;
        }         
         
        if((i+1) != statementlist.end())
        { 
          SgStatement* next_stmt = isSgStatement(isSgNode(*(i+1)));
          build_for = (analyzer.checkToEndFor(next_stmt)) ? build_for : 2;
        }          
        else
        {
          build_for = 2; 
        }
  
        SgStatementPtrList func_callstmt = builder.build_select_stmt(isSgStatement(*i), output_block);
       
        if(*func_callstmt.begin() != NULL)
        {
          appendStatementList(func_callstmt, for_loop_blk);
        }
      }     
      else if(isSgExprStatement(stmt))
      {
        build_for = (analyzer.checkToEndFor(stmt)) ? 1 : build_for;  
        if((i+1) != statementlist.end())
        {
          SgStatement* next_stmt = isSgStatement(isSgNode(*(i+1)));
          build_for = build_for ? ((analyzer.checkToEndFor(next_stmt)) ? build_for : 2) : build_for;
        }
        else
          build_for = build_for ? 2 : build_for;        
        
        if(analyzer.checkForFortranShift(stmt))
        {
          printf("Shift calls supported\r\n");
          appendStatement(handleFortranShift(stmt, output_block), output_block);
        }
        else if(isSgExpression(isSgExprStatement(stmt)->get_expression()))
        {
          SgExprStatement* new_stmt = buildExprStatement(builder.build_alternate_expression(isSgExprStatement(stmt)->get_expression(), output_block));     
          appendStatement(new_stmt, ((build_for >= 1) ? for_loop_blk : output_block));
        }
      }
      else if(isSgReturnStmt(stmt))
      {
        printf("Return statement\r\n"); 
        SgReturnStmt* return_stmt = buildReturnStmt(builder.build_alternate_expression(isSgReturnStmt(stmt)->get_expression(), output_block));
        appendStatement(return_stmt, ((build_for >= 1) ? for_loop_blk : output_block)); 
      }
    }
  }
  if(build_for >= 1)
  {
    SgBasicBlock* block = buildBasicBlock();
    Rose_STL_Container<SgStatement*> stmtlist = for_loop_blk->get_statements();
    appendStatementList(stmtlist, block);
 
    SgForStatement* for_stmt = buildForStatement(initialize_stmt, condition_stmt, increment_expr, block);
    ROSE_ASSERT(for_stmt != NULL);
    appendStatement(for_stmt, output_block);
  }
}

void PrgmTranslator::addVariableDeclarations(SgVariableDeclaration* variable, SgBasicBlock* output_block, bool elemental)
{

  if((variable->get_declarationModifier().get_typeModifier().isIntent_in()) ||
     (variable->get_declarationModifier().get_typeModifier().isIntent_out()) ||
     (variable->get_declarationModifier().get_typeModifier().isIntent_inout()))
  {
    if(elemental)
      return;
  }
  
  Rose_STL_Container<SgInitializedName*> variableslist =  variable->get_variables();
  for(Rose_STL_Container<SgInitializedName*>::iterator j = variableslist.begin(); j != variableslist.end(); j++)	
  {
    SgInitializedName* varname = isSgInitializedName(*j);
    if(varname != NULL)
    {
      SgVariableDeclaration* varDeclare = NULL;
      SgType* type = NULL;        
      SgAssignInitializer* initialize = isSgAssignInitializer(varname->get_initializer()); 
      type = getBaseType(varname->get_type());

      if(type != NULL)
      {
       if(initialize != NULL) 
         varDeclare = buildVariableDeclaration(varname->get_name(), type, buildAssignInitializer(builder.build_alternate_expression(initialize->get_operand(), output_block)));
       else
         varDeclare = buildVariableDeclaration(varname->get_name(), type, varname->get_initializer() , output_block);
      }        
      if(varDeclare != NULL) 
        appendStatement(varDeclare, output_block);
    } 
  }
}

void PrgmTranslator::addSpecificVariables(SgBasicBlock* output_block)
{
  SgVariableDeclaration* varDeclare, *varDeclare1;
  varDeclare = buildVariableDeclaration("i, count, iIn, iInout, iOut", buildUnsignedIntType());
  appendStatement(varDeclare, output_block);
  varDeclare1 = buildVariableDeclaration("sIn, sInout, sOut", buildUnsignedIntType());
  appendStatement(varDeclare1, output_block);
  appendStatement(buildAssignStatement (buildVarRefExp("iIn", output_block), buildIntVal(0)), output_block);
  appendStatement(buildAssignStatement (buildVarRefExp("iOut", output_block), buildIntVal(0)), output_block);
  appendStatement(buildAssignStatement (buildVarRefExp("iInout", output_block), buildIntVal(0)), output_block);
  
  SgExpression* cast_expr = buildCastExp(buildVarRefExp("p_parm_ctx_buffer",output_block), buildUnsignedIntType());
  appendStatement(buildAssignStatement (buildVarRefExp("count", output_block), buildDivideOp(cast_expr, buildIntVal(4))), output_block);

  appendStatement(buildAssignStatement (buildVarRefExp("sIn", output_block), buildIntVal(0)), output_block);
  appendStatement(buildAssignStatement (buildVarRefExp("sOut", output_block), buildIntVal(0)), output_block);
  appendStatement(buildAssignStatement (buildVarRefExp("sInout", output_block), buildIntVal(0)), output_block);
}

void PrgmTranslator::initializeIntentTypeScalarVariables(SgVariableDeclaration* variable, SgBasicBlock* output_block)
{
  int intent_flag = 0;
  SgName* buffer;
  SgExpression* expr;
 
  if(variable->get_declarationModifier().get_typeModifier().isIntent_in())
  {
    buffer = new SgName("p_input_buffer");
    expr = buildVarRefExp("sIn", output_block);
    intent_flag = 1;
  }
  else if(variable->get_declarationModifier().get_typeModifier().isIntent_out())
  {
    buffer = new SgName("p_output_buffer");
    expr = buildVarRefExp("sOut", output_block);
    intent_flag = 1;
  }  
  else if(variable->get_declarationModifier().get_typeModifier().isIntent_inout())
  {
    buffer = new SgName("p_inout_buffer"); 
    expr = buildVarRefExp("sInout", output_block);
    intent_flag = 1;
  }
 
  if(intent_flag != 0)
  {
    Rose_STL_Container<SgInitializedName*> variableslist =  variable->get_variables();
    for(Rose_STL_Container<SgInitializedName*>::iterator j = variableslist.begin(); j != variableslist.end(); j++)	
    {
      SgInitializedName* varname = isSgInitializedName(*j);
      if(varname != NULL)
      {
        SgTypedefType* typedef_type; 
        SgExprStatement* cast_stmt = NULL;
        SgType* type = NULL;
        SgExprStatement* incr_stmt = NULL;

        if((!(isSgArrayType(varname->get_type()))) && (!(isSgPointerType(varname->get_type()))))
          type = getBaseType(varname->get_type());
           
        if(type != NULL)
        {
          SgExpression* expression = buildAddOp(buildVarRefExp(*buffer, output_block), expr);
          SgPointerDerefExp* ptrderef = new SgPointerDerefExp(expression, type); 
          SgExpression* cast_expr = buildCastExp(ptrderef, type);
          cast_stmt = buildAssignStatement(buildVarRefExp(varname, output_block), cast_expr);
          incr_stmt = buildAssignStatement(expr, buildAddOp(expr, buildSizeOfOp(type)));
        }

        if(cast_stmt != NULL)
        {
          appendStatement(cast_stmt, output_block);
          appendStatement(incr_stmt, output_block);
        }

      }
    }
  }
}

void PrgmTranslator::initializeIntentTypeArrayVariables(SgVariableDeclaration* variable, SgBasicBlock* output_block)
{
  int intent_flag = 0;
  SgName* buffer;
  SgExpression* expr;
 
  if(variable->get_declarationModifier().get_typeModifier().isIntent_in())
  {
    printf("Variables are intent in\r\n");
    buffer = new SgName("p_input_buffer");
    expr = buildAddOp(buildMultiplyOp(buildVarRefExp("count", output_block), buildPlusPlusOp(buildVarRefExp("iIn", output_block), SgUnaryOp::postfix)), buildVarRefExp("sIn", output_block));
    intent_flag = 1;
  }
  if(variable->get_declarationModifier().get_typeModifier().isIntent_out())
  {
    buffer = new SgName("p_output_buffer");
    expr = buildAddOp(buildMultiplyOp(buildVarRefExp("count", output_block), buildPlusPlusOp(buildVarRefExp("iOut", output_block), SgUnaryOp::postfix)), buildVarRefExp("sOut", output_block));
    intent_flag = 1;
  }  
  if(variable->get_declarationModifier().get_typeModifier().isIntent_inout())
  {
    buffer = new SgName("p_inout_buffer"); 
    expr = buildAddOp(buildMultiplyOp(buildVarRefExp("count", output_block), buildPlusPlusOp(buildVarRefExp("iInout", output_block), SgUnaryOp::postfix)), buildVarRefExp("sInout", output_block));
    intent_flag = 1;
  }
 
  if(intent_flag != 0)
  {
    Rose_STL_Container<SgInitializedName*> variableslist =  variable->get_variables();
    for(Rose_STL_Container<SgInitializedName*>::iterator j = variableslist.begin(); j != variableslist.end(); j++)	
    {
      SgInitializedName* varname = isSgInitializedName(*j);
      if(varname != NULL)
      {
        SgTypedefType* typedef_type; 
        SgExprStatement* cast_stmt = NULL;
        SgType* type = NULL;
        if(isSgArrayType(varname->get_type()))
        {
          type = getBaseType(varname->get_type());
        
          if(type != NULL)
          {
            SgExpression* cast_expr = buildCastExp(buildVarRefExp(*buffer, output_block), type);
            cast_stmt = buildAssignStatement(buildVarRefExp(varname, output_block), buildAddOp(cast_expr, expr));
          }
          else
          {
            printf("ERROR: Type not found\r\n");
          }
        }
        if(cast_stmt != NULL)
          appendStatement(cast_stmt, output_block);
      }
    }
  }
}

SgStatement* PrgmTranslator::handleFortranShift(SgStatement* stmt, SgBasicBlock* output_block)
{
  SgExpression* expr = (isSgExprStatement(stmt))->get_expression();
  SgBasicBlock* block = buildBasicBlock();
  if(isSgAssignOp(expr))
  {
    SgAssignOp* assgn_op = isSgAssignOp(expr);

    SgExpression* LHS = assgn_op->get_lhs_operand();
    SgExpression* RHS = assgn_op->get_rhs_operand();
     
    Rose_STL_Container<SgVariableSymbol*> varList;   
    Rose_STL_Container<bool> varDirList;
    Rose_STL_Container<SgExpression*> varBoundaryList;
 
    Rose_STL_Container<SgNode*> funcCallExprList = NodeQuery::querySubTree(RHS, V_SgFunctionCallExp);
    
    for(Rose_STL_Container<SgNode*>::iterator i = funcCallExprList.begin(); i != funcCallExprList.end(); i++)
    {   
      SgFunctionCallExp* func = isSgFunctionCallExp(*i); 
      if(func != NULL)
      {
        SgName func_name = isSgFunctionRefExp(func->get_function())->get_symbol()->get_name();
        if((strcmp(func_name.str(), "EOSHIFT") == 0) || (strcmp(func_name.str(), "eoshift") == 0))
        { 
          SgExprListExp* arg_exprlist = func->get_args();
          SgExpressionPtrList arg_list = arg_exprlist->get_expressions();
          Rose_STL_Container<SgExpression*> :: iterator j = arg_list.begin();
          varList.insert(varList.end(), isSgVariableSymbol(isSgVarRefExp(*j)->get_symbol()));
          if(isSgMinusOp(*(j+1)))
            varDirList.insert(varDirList.end(), true);
          else
            varDirList.insert(varDirList.end(), false);
          varBoundaryList.insert(varBoundaryList.end(), *(j+2));
        }
      }
    }
 
    int count = 0;
    for(Rose_STL_Container<SgVariableSymbol*>::iterator i = varList.begin(); i != varList.end(); i++)
    {
      string strV = "v";
      string strM1 = "m1";
      string strP1 = "p1";
      string varT = strV;
      SgVariableSymbol* var = isSgVariableSymbol(*i);
      varT.append(var->get_name().str());
      string varT2 = varT;
      bool right = varDirList[count];
      if(right == true)
        varT2.append(strM1);
      else
        varT2.append(strP1);
      string temp = "temp";
  
      SgVariableDeclaration* varDeclare1 = NULL;
      SgVariableDeclaration* varDeclare2 = NULL;
      SgVariableDeclaration* tmpDeclare = NULL;
      SgType* type = NULL;
      type = getBaseType(var->get_type());
      type = isSgPointerType(type)->get_base_type();

      SgExpression* varT_0 = new SgPntrArrRefExp(output_block->get_file_info(), buildVarRefExp(var->get_name(), output_block), buildIntVal(0), type);
      SgExpression* varT_1 = new SgPntrArrRefExp(output_block->get_file_info(), buildVarRefExp(var->get_name(), output_block), buildIntVal(1), type);
      if(type != NULL)
      {
        varDeclare1 = buildVariableDeclaration(varT, type, buildAssignInitializer(varT_0));
        if(right == true)
        {
          tmpDeclare = buildVariableDeclaration(temp, type, buildAssignInitializer(varT_0));
          varDeclare2 = buildVariableDeclaration(varT2, type, buildAssignInitializer(varBoundaryList[count]));
        }
        else
          varDeclare2 = buildVariableDeclaration(varT2, type, buildAssignInitializer(varT_1));
      }        
      if((varDeclare1 != NULL) && (varDeclare2 != NULL)) 
      {
        SgName varTname = varT;
        SgName varT2name = varT2;
        SgName tempName = temp;
        SgVariableSymbol* varTSymbol = lookupVariableSymbolInParentScopes(varTname, block);
        SgVariableSymbol* varT2Symbol = lookupVariableSymbolInParentScopes(varT2name, block);
        SgVariableSymbol* tempSymbol = lookupVariableSymbolInParentScopes(tempName, block);
        
        if(varTSymbol == NULL)        
          appendStatement(varDeclare1, block);
        if(varT2Symbol == NULL)
          appendStatement(varDeclare2, block);
        if((right == true) && (tmpDeclare != NULL) && (tempSymbol == NULL))
          appendStatement(tmpDeclare, block);  
      }
      count++;
    }
    
    SgType* LHS_type = NULL;
    LHS_type = getBaseType(isSgVarRefExp(LHS)->get_symbol()->get_type());     
    LHS_type = isSgPointerType(LHS_type)->get_base_type();
 
    SgExpression* shift_expr = builder.build_alternate_expression(isSgExprStatement(stmt)->get_expression(), output_block);
    SgExpression* RHS_shift_expr = isSgAssignOp(shift_expr)->get_rhs_operand();

    SgExpression* initialLHS = new SgPntrArrRefExp(output_block->get_file_info(), LHS, buildIntVal(0), LHS_type); 
    SgExprStatement* initialStmt = buildAssignStatement(initialLHS, RHS_shift_expr);
    
    SgExpression* finalLHS = new SgPntrArrRefExp(output_block->get_file_info(), LHS, buildSubtractOp(buildVarRefExp("count",output_block), buildIntVal(1)), LHS_type); 
    SgExprStatement* finalStmt = buildAssignStatement(finalLHS, RHS_shift_expr);

    appendStatement(initialStmt, block);
    
    SgBasicBlock* for_block = buildBasicBlock();
  
    count = 0;
    for(Rose_STL_Container<SgVariableSymbol*>::iterator i = varList.begin(); i != varList.end(); i++)
    {
      string strV = "v";
      string strM1 = "m1";
      string strP1 = "p1";
      string varT = strV;
      SgVariableSymbol* var = isSgVariableSymbol(*i);
      varT.append(var->get_name().str());
      string varT2 = varT;
      bool right = varDirList[count];
      if(right == true)
        varT2.append(strM1);
      else
        varT2.append(strP1);
      string temp = "temp";

      SgStatement* varDeclare1 = NULL;
      SgStatement* varDeclare2 = NULL;
      SgStatement* tmpDeclare = NULL;
      SgType* type = NULL;
      type = getBaseType(var->get_type());
      type = isSgPointerType(type)->get_base_type();

      SgExpression* varT_i = new SgPntrArrRefExp(output_block->get_file_info(), buildVarRefExp(var->get_name(), output_block), buildVarRefExp("i", output_block), type);
      SgExpression* varT_ip1 = new SgPntrArrRefExp(output_block->get_file_info(), buildVarRefExp(var->get_name(), output_block), buildAddOp(buildVarRefExp("i", output_block), buildIntVal(1)), type);
      if(type != NULL)
      {
        varDeclare1 = buildAssignStatement(buildVarRefExp(varT, block), varT_i);
        if(right == true)
        {
          varDeclare2 = buildAssignStatement(buildVarRefExp(varT2, block), buildVarRefExp(temp, block));
          tmpDeclare = buildAssignStatement(buildVarRefExp(temp, block), varT_i);
        }
        else
          varDeclare2 = buildAssignStatement(buildVarRefExp(varT2, block), varT_ip1);
      }
      if((varDeclare1 != NULL) && (varDeclare2 != NULL))
      {
        appendStatement(varDeclare1, for_block);
        appendStatement(varDeclare2, for_block);
        if((right == true) && (tmpDeclare != NULL))
          appendStatement(tmpDeclare, for_block);
      }
      count++;
    }

    appendStatement(buildExprStatement(shift_expr), for_block);
    
    SgExprStatement* condition_stmt = buildExprStatement(buildLessThanOp(buildVarRefExp("i", output_block),buildSubtractOp(buildVarRefExp("count", output_block), buildIntVal(1))));
    SgExprStatement* initialize_stmt = buildAssignStatement(buildVarRefExp("i", output_block), buildIntVal(1));
    SgExpression* increment_expr = buildPlusPlusOp(buildVarRefExp("i", output_block), SgUnaryOp::postfix);

    SgForStatement* for_stmt = buildForStatement(initialize_stmt, condition_stmt, increment_expr, for_block);
    ROSE_ASSERT(for_stmt != NULL);
    appendStatement(for_stmt, block);

    count = 0;
    for(Rose_STL_Container<SgVariableSymbol*>::iterator i = varList.begin(); i != varList.end(); i++)
    {
      string strV = "v";
      string strM1 = "m1";
      string strP1 = "p1";
      string varT = strV;
      SgVariableSymbol* var = isSgVariableSymbol(*i);
      varT.append(var->get_name().str());
      string varT2 = varT;
      bool right = varDirList[count];
      if(right == true)
        varT2.append(strM1);
      else
        varT2.append(strP1);
      string temp = "temp";
   
      SgStatement* varDeclare1 = NULL;
      SgStatement* varDeclare2 = NULL;
      SgType* type = NULL;
      type = getBaseType(var->get_type());
      type = isSgPointerType(type)->get_base_type();

      SgExpression* varT_last = new SgPntrArrRefExp(output_block->get_file_info(), buildVarRefExp(var->get_name(), output_block), buildSubtractOp(buildVarRefExp("count", output_block), buildIntVal(1)), type);
      if(type != NULL)
      {
        varDeclare1 = buildAssignStatement(buildVarRefExp(varT, block), varT_last);
        if(right == true)
          varDeclare2 = buildAssignStatement(buildVarRefExp(varT2, block), buildVarRefExp(temp, block));
        else
          varDeclare2 = buildAssignStatement(buildVarRefExp(varT2, block), varBoundaryList[count]);
      }        
      if((varDeclare1 != NULL) && (varDeclare2 != NULL)) 
      {
        appendStatement(varDeclare1, block);
        appendStatement(varDeclare2, block);
      }
      count++;
    }
    appendStatement(finalStmt, block);   
  }
  return block;
}

SgType* PrgmTranslator::getBaseType(SgType* vartype)
{
  SgTypedefType* typedef_type;
  SgType* type = NULL;
  
  if(isSgModifierType(vartype))
  {
   // printf("Modifier type variable, name - %s\r\n", varname->get_name().str());
    SgModifierType* mod_type = isSgModifierType(vartype);

    if(isSgTypeInt(mod_type->get_base_type()))
    {
      type = typedef_type->createType(vec_int);
    }
        
    else if(isSgTypeFloat(mod_type->get_base_type()))
    {
      printf("Float type modifier\r\n");
      type = typedef_type->createType(vec_float);
    }

    else if(isSgTypeBool(mod_type->get_base_type()))
    {
      printf("Bool type modifier\r\n");
      type = typedef_type->createType(vec_sbool);
    }

    else if(isSgTypeDouble(mod_type->get_base_type()))
    {
      printf("Double type modifier\r\n");
      type = typedef_type->createType(vec_double);
    }

    else if(isSgArrayType(mod_type->get_base_type()))
    {
      //printf("Array variable name is %s\r\n", varname->get_name().str());	
      SgArrayType* array = isSgArrayType(vartype);

      if(isSgTypeFloat(array->get_base_type()))
      {
       // printf("Array to float type variable name is %s\r\n", varname->get_name().str());
        type = buildPointerType(typedef_type->createType(vec_float));
      }

      else if(isSgTypeBool(array->get_base_type()))
      {   
        type = buildPointerType(typedef_type->createType(vec_sbool));
      }

      else if(isSgTypeInt(array->get_base_type()))
      {   
        type = buildPointerType(typedef_type->createType(vec_int));
      }

      else if(isSgTypeDouble(array->get_base_type()))
      {
     //   printf("Array to Double type variable name is %s\r\n", varname->get_name().str());
        type = buildPointerType(typedef_type->createType(vec_double));
      }
    }
  }    
    
  else if(isSgArrayType(vartype))
  {
    //printf("Array variable name is %s\r\n", varname->get_name().str());	
    SgArrayType* array = isSgArrayType(vartype);

    if(isSgTypeFloat(array->get_base_type()))
    {
     // printf("Array to float type variable name is %s\r\n", varname->get_name().str());
      type = buildPointerType(typedef_type->createType(vec_float));
    }

    else if(isSgTypeBool(array->get_base_type()))
    {   
      type = buildPointerType(typedef_type->createType(vec_sbool));
    }

    else if(isSgTypeInt(array->get_base_type()))
    {   
      type = buildPointerType(typedef_type->createType(vec_int));
    }

    else if(isSgTypeDouble(array->get_base_type()))
    {
   //   printf("Array to Double type variable name is %s\r\n", varname->get_name().str());
      type = buildPointerType(typedef_type->createType(vec_double));
    }

    else if(isSgModifierType(array->get_base_type()))
    {
   //   printf("Array modifier variable name is %s\r\n", varname->get_name().str());	
      SgModifierType* mod_type = isSgModifierType(array->get_base_type());

      if(isSgTypeFloat(mod_type->get_base_type()))
      {
  //      printf("Array to float type variable name is %s\r\n", varname->get_name().str());
        type = buildPointerType(typedef_type->createType(vec_float));
      }

      else if(isSgTypeBool(mod_type->get_base_type()))
      {   
        type = buildPointerType(typedef_type->createType(vec_sbool));
      }

      else if(isSgTypeInt(mod_type->get_base_type()))
      {   
        type = buildPointerType(typedef_type->createType(vec_int));
      }

      else if(isSgTypeDouble(mod_type->get_base_type()))
      {
  //      printf("Array to Double type variable name is %s\r\n", varname->get_name().str());
        type = buildPointerType(typedef_type->createType(vec_double));
      }
    }

/*        SgExprListExp* exprlist = array->get_dim_info();	
        Rose_STL_Container<SgExpression*> exprs = exprlist->get_expressions();
	
        int count = 0;	   
        for(Rose_STL_Container<SgExpression*>::iterator k = exprs.begin(); k != exprs.end(); k++)
        {
          count++;
        }
        printf("Variable %s has dimension %d\r\n",varname->get_name().str(), count);
*/
  }

  else if(isSgTypeInt(vartype))
  {
 //   printf("Integer variable name is %s\r\n", varname->get_name().str());	 	
    type = typedef_type->createType(vec_int);
  }

  else if(isSgTypeBool(vartype))
  {
 //   printf("Logical bool type variable name is %s\r\n", varname->get_name().str());
    type = typedef_type->createType(vec_sbool);
  }

  else if(isSgTypeDouble(vartype))
  {
  //  printf("Double type variable name is %s\r\n", varname->get_name().str());
    type = typedef_type->createType(vec_double);
  }

  else if(isSgTypeFloat(vartype))
  {
    printf("Float type variable \r\n");
    type = typedef_type->createType(vec_float);
  }
	    
  else if(isSgPointerType(vartype))
  {
 //   printf("Pointer type variable (not doing anything) name is %s\r\n", varname->get_name().str()); 
    SgPointerType* ptr = isSgPointerType(vartype);

   /* if(isSgTypeUnsignedInt(ptr->get_base_type()))
       printf("Pointer to unsigned int type variable name is %s\r\n", varname->get_name().str());	

    if(isSgTypeUnsignedShort(ptr->get_base_type()))
       printf("Pointer to unsigned short type variable name is %s\r\n", varname->get_name().str());	

    if(isSgTypeUnsignedLong(ptr->get_base_type()))
      printf("Pointer to usigned long type variable name is %s\r\n", varname->get_name().str());	
  
    if(isSgTypeSignedInt(ptr->get_base_type()))
      printf("Pointer to signed int type variable name is %s\r\n", varname->get_name().str());	

    if(isSgTypeSignedShort(ptr->get_base_type()))
       printf("Pointer to signed short type variable name is %s\r\n", varname->get_name().str());	

    if(isSgTypeSignedLong(ptr->get_base_type()))
      printf("Pointer to signed long type variable name is %s\r\n", varname->get_name().str());	

    if(isSgTypeDouble(ptr->get_base_type()))
      printf("Pointer to double type variable name is %s\r\n", varname->get_name().str());	

    if(isSgTypeFloat(ptr->get_base_type()))
      printf("Pointer to float type variable name is %s\r\n", varname->get_name().str());	
		 
    if(isSgTypeInt(ptr->get_base_type()))
      printf("Pointer to int type variable name is %s\r\n", varname->get_name().str());	
	  
    if(isSgTypeChar(ptr->get_base_type()))
      printf("Pointer logical char type variable name is %s\r\n", varname->get_name().str());	

    if(isSgTypeShort(ptr->get_base_type()))
      printf("Pointer logical short type variable name is %s\r\n", varname->get_name().str());	
		
    if(isSgTypeBool(ptr->get_base_type()))
      printf("Pointer logical bool type variable name is %s\r\n", varname->get_name().str());	
		
    if(isSgTypeLongDouble(ptr->get_base_type()))
      printf("Pointer to a pointer type variable name is %s\r\n", varname->get_name().str());	*/
  }
  else if(isSgFunctionType(vartype))
  { 
    SgFunctionType* func_type = isSgFunctionType(vartype);
    if(isSgTypeFloat(func_type->get_return_type()))
    {
     // printf("Array to float type variable name is %s\r\n", varname->get_name().str());
      type = (typedef_type->createType(vec_float));
    }

    else if(isSgTypeBool(func_type->get_return_type()))
    {   
      type = (typedef_type->createType(vec_sbool));
    }

    else if(isSgTypeInt(func_type->get_return_type()))
    {   
      type = (typedef_type->createType(vec_int));
    }

    else if(isSgTypeDouble(func_type->get_return_type()))
    {
   //   printf("Array to Double type variable name is %s\r\n", varname->get_name().str());
      type = (typedef_type->createType(vec_double));
    }
    else if(isSgTypeVoid(func_type->get_return_type()))
    {
      printf("Void type \r\n");
      type = buildVoidType();
    }
  }
  else if(isSgTypeVoid(vartype))
  {
    printf("Void type \r\n");
    type = buildVoidType();
  }

 
  if(type == NULL)
   printf("Error: BASE TYPE not found\r\n");

  return type;
}

void PrgmTranslator::deleteTypedef(SgGlobal* global_scope)
{
  if(global_scope != NULL)
  {
    pushScopeStack(global_scope);

    removeStatement(vec_float);
    removeStatement(vec_double);
    removeStatement(vec_int);
    removeStatement(vec_long_long);
    removeStatement(vec_short);
    removeStatement(vec_char);
    removeStatement(vec_uint);
    removeStatement(vec_ulong_long);
    removeStatement(vec_ushort);
    removeStatement(vec_uchar);
    removeStatement(vec_cbool);
    removeStatement(vec_ibool);
    removeStatement(vec_sbool); 
 
    popScopeStack();
  }
}

