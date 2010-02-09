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

/*
 *  Created by Tanveer on 10/28/08.
 */


#include "GPUProgramTranslator.h"

GPUProgramTranslator::GPUProgramTranslator()
{

}

GPUProgramTranslator::~GPUProgramTranslator()
{

}

void GPUProgramTranslator::visit(SgNode* astNode)
{
	SgGlobal* global_scope = isSgGlobal(astNode);
	if (global_scope != NULL)
	{
		int program_language;
		add_gpu_typedefs(global_scope);
		add_gpu_headers(global_scope);
		program_language = determine_input_program_language();
		
		switch(program_language)
		{
			case FORTRAN:
				printf("\n Input program language = FORTRAN \r\n");
				handle_fortran_program(global_scope);
				break;
			case C_LANGUAGE:
				printf("\n Input program language = C_LANGUAGE \r\n");
				handle_c_program(global_scope);
				break;
			default:
				printf("\n This input program language is currently not supported \r\n");
				ROSE_ASSERT(false);
				break;
		}				
	}
}

// To be implemented
void GPUProgramTranslator::add_gpu_typedefs(SgGlobal* global_scope)
{
	
}

//To be implemented
void GPUProgramTranslator::add_gpu_headers(SgGlobal* global_scope)
{
	
}

int GPUProgramTranslator::determine_input_program_language()
{
	int input_language = 0;
    if(is_C_language())
	{
		input_language = C_LANGUAGE;
	}
    else if(is_Fortran_language())
	{
		input_language = FORTRAN;
	}
	
	return input_language;
}

void GPUProgramTranslator::handle_fortran_program(SgGlobal* global_scope)
{
	Rose_STL_Container<SgNode*> func_decl_list = NodeQuery::querySubTree(input_project, V_SgFunctionDeclaration);
	
	for (Rose_STL_Container<SgNode*>::iterator i = func_decl_list.begin(); i != func_decl_list.end(); i++)
	{
		SgFunctionDeclaration* func_decl = isSgFunctionDeclaration(*i);
		printf("Function name = %s \r\n", func_decl->get_name().str());
		SgName func_name = func_decl->get_name().str();
		
		Rose_STL_Container<SgInitializedName*> func_param_list = func_decl->get_args();
		SgFunctionParameterList* new_func_param_list = buildFunctionParameterList();
		SgType* param_type;
		SgType* new_param_type;
		
		for (Rose_STL_Container<SgInitializedName*>::iterator j = func_param_list.begin(); j != func_param_list.end(); j++)
		{
			SgInitializedName* param_init_name = isSgInitializedName(*j);
			if (param_init_name != NULL)
			{
				param_type = param_init_name -> get_type();
				
				if (param_type != NULL)
				{
					if (isSgArrayType(param_type))
					{
						printf("Found Array Type Parameter \r\n");
						SgArrayType* array_type = isSgArrayType(param_type);
						SgType* array_base_type = array_type->get_base_type();
						
						if (isSgTypeFloat(array_base_type))
						{
							new_param_type = buildPointerType(buildFloatType());
						}
						else
						{
							printf("This type is currently not supported. \r\n");
							ROSE_ASSERT(false);
						}
					}
					else
					{
						printf("This type is currently not supported.\r\n");
						ROSE_ASSERT(false);
					}
					
					SgInitializedName* new_param_init_name  = buildInitializedName(param_init_name->get_name(), new_param_type);
					appendArg(new_func_param_list, new_param_init_name);
				}
			}
		}
		
		SgType* return_type = buildVoidType();
		
		SgFunctionDeclaration* new_func_decl = buildDefiningFunctionDeclaration(func_name, return_type, new_func_param_list, global_scope);
		
		attachArbitraryText(new_func_decl, "__global__", PreprocessingInfo::before);
		//addTextForUnparser(new_func_decl, "__global__", AstUnparseAttribute::e_before);
		appendStatement(new_func_decl, global_scope);
		
	}
	
}

// To be implemented
void GPUProgramTranslator::handle_c_program(SgGlobal* global_scope)
{
	
}


// To be implemented 
/*
SgExprStatement* expr_stmt = isSgExprStatement(*i);
if (expr_stmt != NULL)
{
	printf("\n FOUND EXPR STATEMENT !!!");
	
	SgFunctionCallExp* func_call_expr = isSgFunctionCallExp(expr_stmt->get_expression());
	
	if (func_call_expr != NULL)
	{
		printf("\n FOUND FUNC CALL EXPR !!!");
		
		char func_str[100]="  ";
		
		SgFunctionRefExp* func_ref = isSgFunctionRefExp(func_call_expr->get_function());
		SgExprListExp* arg_exprlist = func_call_expr->get_args();
		SgExpressionPtrList arg_list = arg_exprlist->get_expressions();
		
		SgName func_name = (func_ref->get_symbol())->get_name();
		printf("\n Function name = %s", func_name.str());
		strcat(func_str, func_name.str());
		strcat(func_str,"<<<1,N>>>");
		strcat(func_str,"(");
		for (Rose_STL_Container<SgExpression*> :: iterator i = arg_list.begin(); i != arg_list.end(); i++)
		{
			SgVarRefExp* varref_expr = isSgVarRefExp(*i);
			SgVariableSymbol* var_symbol = varref_expr->get_symbol();
			SgName var_name = var_symbol->get_name();
			
			strcat(func_str, var_name.str());
			if (i+1 != arg_list.end())
			{
				strcat(func_str, ",");
			}
		}
		strcat(func_str,");");
		printf("\n Returning function = %s\n", func_str);
		attachArbitraryText(func_stmt, func_str, PreprocessingInfo::before);
		//addTextForUnparser(func_stmt, func_str, AstUnparseAttribute::e_after);
	}
}
*/
