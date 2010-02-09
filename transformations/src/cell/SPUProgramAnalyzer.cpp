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

#include "SPUProgramAnalyzer.h"

PrgmAnalyzer::PrgmAnalyzer()
{

}

PrgmAnalyzer::~PrgmAnalyzer()
{

}

int PrgmAnalyzer::analyze(SgProject* input_prj)
{   
  Rose_STL_Container<SgNode*> functionDeclarationList = NodeQuery::querySubTree(input_prj,V_SgFunctionDeclaration);

  Rose_STL_Container<SgNode*> NewFunctionList;

  int count = 0;
  for(Rose_STL_Container<SgNode*>::iterator i = functionDeclarationList.begin(); i != functionDeclarationList.end(); i++)
  {
    
    if((*i)->get_file_info()->isCompilerGenerated() == false)
    {
      AttachedPreprocessingInfoType* comments = isSgLocatedNode((*i))->getAttachedPreprocessingInfo();
      if(comments != NULL)
      {    
        AttachedPreprocessingInfoType::iterator j;
        for(j= comments->begin(); j != comments->end(); j++)
        {
          if((*j)->getRelativePosition() == PreprocessingInfo::before)
          {
            printf("Comment position = %s\r\n Comment is %s \r\n", ((*j)->getRelativePosition() == PreprocessingInfo::before) ? "before" : "after", (*j)->getString().c_str());
            string str = (*j)->getString();  
            size_t found;
            found = str.find("VECTORIZE");
            if((str.find("VECTORIZE") != string::npos) || (str.find("vectorize") != string::npos))
            {
              printf("Found Vectorize\r\n");
              count++;
              NewFunctionList.insert(NewFunctionList.end(), (*i));
            }
          }
        }
      }
    }
  }  

	
  listOfFunctns = (functionWithAttributesPTR)malloc(count * sizeof(functionWithAttributes));  

  count = 0;
  for(Rose_STL_Container<SgNode*>::iterator i = NewFunctionList.begin(); i != NewFunctionList.end(); i++)
  {
    
    if((*i)->get_file_info()->isCompilerGenerated() == false)
    {
      AttachedPreprocessingInfoType* comments = isSgLocatedNode((*i))->getAttachedPreprocessingInfo();
      if(comments != NULL)
      {
        AttachedPreprocessingInfoType::iterator j;
        for(j= comments->begin(); j != comments->end(); j++)
        {
          if((*j)->getRelativePosition() == PreprocessingInfo::before)
          { 
            printf("Comment position = %s\r\n Comment is %s \r\n", ((*j)->getRelativePosition() == PreprocessingInfo::before) ? "before" : "after", (*j)->getString().c_str());
            
            string str = (*j)->getString();
            if((str.find("ELEMENTAL") != string::npos) || (str.find("elemantal") != string::npos))
               listOfFunctns[count].elemental = true;   
            else 
               listOfFunctns[count].elemental = false;
          }
        }
        listOfFunctns[count].functionDeclaration = isSgFunctionDeclaration(*i);
        count++;
      }
    }
  }  
  
  printf("Number of functions to translate are %d\r\n", count); 
  return count;
}

int PrgmAnalyzer::checkToEndFor(SgStatement* stmt)
{
   if(checkForFortranShift(stmt))  
     return 0;
   else
    return (checkForArrayRefVariables(stmt));
}

int PrgmAnalyzer::checkForFortranShift(SgStatement* stmt)
{
  if(isSgExprStatement(stmt))
  {
    SgExpression* expr = (isSgExprStatement(stmt))->get_expression();
    if(isSgAssignOp(expr))
    {
      SgAssignOp* assgn_op = isSgAssignOp(expr);

      SgExpression* LHS = assgn_op->get_lhs_operand();
      SgExpression* RHS = assgn_op->get_rhs_operand();
     
      Rose_STL_Container<SgNode*> funcCallExprList = NodeQuery::querySubTree(RHS,V_SgFunctionCallExp);
      
      for(Rose_STL_Container<SgNode*>::iterator i = funcCallExprList.begin(); i != funcCallExprList.end(); i++)
      {   
        SgFunctionCallExp* func = isSgFunctionCallExp(*i); 
        if(func != NULL)
        {
          SgName func_name = isSgFunctionRefExp(func->get_function())->get_symbol()->get_name();
          if((strcmp(func_name.str(), "EOSHIFT") == 0) || (strcmp(func_name.str(), "eoshift") == 0))
          { 
            printf("Got eoshift\r\n"); 
            return 1; 
          }
        }
      } 
    }
    else
     return 0;
  }
 
  return 0;
}

int PrgmAnalyzer::checkForArrayRefVariables(SgStatement* stmt)
{
  if(stmt == NULL)
  {
    return 0;
  }
  else if(isSgExprStatement(stmt))
  {
    SgExpression* expr = (isSgExprStatement(stmt))->get_expression();
    if(isSgAssignOp(expr))
    {
      SgAssignOp* assgn_op = isSgAssignOp(expr);

      SgExpression* LHS = assgn_op->get_lhs_operand();
      SgExpression* RHS = assgn_op->get_rhs_operand();
     
      if(isSgVarRefExp(LHS))
      {
        if(isSgArrayType(isSgVarRefExp(LHS)->get_type()))
          return 1;
        else 
          return 0;
      }
      else
       return 0; 
    }
    else
     return 0;
  }
  else if(isSgIfStmt(stmt))
  {
    return 0;    
  }
  else if(isSgWhereStatement(stmt) || isSgElseWhereStatement(stmt))
  {
    return 1;
  }
  return 1;
}


