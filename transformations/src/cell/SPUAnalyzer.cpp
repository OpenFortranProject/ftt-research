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
 *  SPUAnalyzer.cpp
 *
 *  Created by Tanveer on 9/16/08.
 *
 */

#include "SPUAnalyzer.h"
#include "table.c"
using namespace SageBuilder;
using namespace SageInterface;

TBL function_lookup_table;

SPUAnalyzer::SPUAnalyzer()
{


}

SPUAnalyzer::~SPUAnalyzer()
{

}

SgExpression* SPUAnalyzer::analyze_binary_op(SgBinaryOp* originalExpr, SgScopeStatement* expr_scope)
{
	SgExpression* expr = NULL;
	SgNode* parent = originalExpr->get_parent();
	
	SgName func_name;
	SgExprListExp* func_call_param_list;
	SgType* ret_type;
	SgScopeStatement* func_scope;

	func_scope = isSgScopeStatement(expr_scope);
	
	ROSE_ASSERT(func_scope != NULL);
	
	SgExpression* last_binary_op_lhs = originalExpr->get_lhs_operand();
	SgExpression* last_binary_op_rhs = originalExpr->get_rhs_operand();
	SgExpression* last_binary_op = originalExpr;
	SgBinaryOp* last_binary_op_final;
	int binary_op_cnt_lhs=0;
	int binary_op_cnt_rhs=0;
	int binary_op_cnt=0;
	
	while ((isSgBinaryOp(last_binary_op_lhs) != NULL) || (isSgBinaryOp(last_binary_op_rhs) != NULL))
	{
		if ((isSgPntrArrRefExp(last_binary_op_lhs) != NULL) && (isSgPntrArrRefExp(last_binary_op_rhs) == NULL))
		{
			last_binary_op_lhs = NULL;
		}
		
		else if ((isSgPntrArrRefExp(last_binary_op_lhs) == NULL) && (isSgPntrArrRefExp(last_binary_op_rhs) != NULL))
		{
			last_binary_op_rhs = NULL;
		}
		
		else if ((isSgPntrArrRefExp(last_binary_op_lhs) != NULL) && (isSgPntrArrRefExp(last_binary_op_rhs) != NULL))
		{
			last_binary_op_lhs = NULL;
			last_binary_op_rhs = NULL;
		}
		
		else if((isSgBinaryOp(last_binary_op_lhs) != NULL) && (isSgBinaryOp(last_binary_op_rhs) == NULL))
		{
			last_binary_op = last_binary_op_lhs;
			last_binary_op_rhs = isSgBinaryOp(last_binary_op_lhs)->get_rhs_operand();
			last_binary_op_lhs = isSgBinaryOp(last_binary_op_lhs)->get_lhs_operand();
			binary_op_cnt_lhs++;
			binary_op_cnt++;
		}
		
		else if((isSgBinaryOp(last_binary_op_lhs) == NULL) && (isSgBinaryOp(last_binary_op_rhs) != NULL))
		{
			last_binary_op = last_binary_op_rhs;
			last_binary_op_lhs = isSgBinaryOp(last_binary_op_rhs)->get_lhs_operand();
			last_binary_op_rhs = isSgBinaryOp(last_binary_op_rhs)->get_rhs_operand();
			binary_op_cnt_rhs++;
			binary_op_cnt++;
		}
		
		else if((isSgBinaryOp(last_binary_op_lhs) != NULL) && (isSgBinaryOp(last_binary_op_rhs) != NULL))
		{								
			expr = analyze_dual_binary_child((isSgBinaryOp(last_binary_op_lhs)), (isSgBinaryOp(last_binary_op_rhs)), expr_scope);
			
			last_binary_op = isSgExpression(last_binary_op->get_parent());
		
			last_binary_op_lhs = NULL;
			last_binary_op_rhs = NULL;
			binary_op_cnt--;
		}
	}
	
	/*
	printf("\n In function %s: Binary op count LHS= %d", __FUNCTION__, binary_op_cnt_lhs);
	printf("\n In function %s: Binary op count RHS = %d", __FUNCTION__, binary_op_cnt_rhs);
	printf("\n In function %s: TOTAL Binary op count = %d", __FUNCTION__, binary_op_cnt);
	*/

	last_binary_op_final = isSgBinaryOp(last_binary_op);

	if (last_binary_op_final != NULL )
	{
		int flag;
		
		if (expr != NULL)
		{
			flag = 2;
		}
		else
		{
			flag = 1;
		}
		
		while( binary_op_cnt >= 0)
		{
			originalExpr = last_binary_op_final;
						
			SgBinaryOp* op_parent = isSgBinaryOp(originalExpr->get_parent());
			
			SgExpression* originalExpr_lhs_operand = analyze_expression_for_binary_op(originalExpr->get_lhs_operand(), func_scope);
			SgExpression* originalExpr_rhs_operand = analyze_expression_for_binary_op(originalExpr->get_rhs_operand(), func_scope);
			SgExpression* op_parent_lhs_operand = NULL; 
			SgExpression* op_parent_rhs_operand = NULL; 			
			
			SgExpression* originalExpr_lhs_operand_c = originalExpr->get_lhs_operand();
			SgExpression* originalExpr_rhs_operand_c = originalExpr->get_rhs_operand();
			SgExpression* op_parent_lhs_operand_c = NULL; 
			SgExpression* op_parent_rhs_operand_c = NULL; 			
			
			if (op_parent != NULL)
			{
				//printf(" In function %s: op_parent is not null", __FUNCTION__);
				
				op_parent_lhs_operand = analyze_expression_for_binary_op(op_parent->get_lhs_operand(), func_scope);
				op_parent_rhs_operand = analyze_expression_for_binary_op(op_parent->get_rhs_operand(), func_scope);
				op_parent_lhs_operand_c = op_parent->get_lhs_operand();
				op_parent_rhs_operand_c = op_parent->get_rhs_operand();
				
				if (isSgBinaryOp(op_parent_rhs_operand_c) && isSgBinaryOp(op_parent_lhs_operand_c) && (isSgPntrArrRefExp(op_parent_rhs_operand_c) == NULL) && (isSgPntrArrRefExp(op_parent_lhs_operand_c) == NULL))
				{
					//printf(" In function %s: op_parent is set to null", __FUNCTION__);
					op_parent = NULL;
				}
			}
												
			if (isSgAddOp(originalExpr))
			{
				//printf("\n In function %s: Found Add Op", __FUNCTION__);
							
				if (isSgAddOp(op_parent) )
				{
					//printf("\n In function %s: In ADDX", __FUNCTION__);
					func_name = "spu_addx";
					if (flag == 1)
					{
						func_call_param_list = buildExprListExp(originalExpr_lhs_operand, originalExpr_rhs_operand);
						flag++;
					}
					else
					{
						if (isSgVarRefExp(originalExpr_lhs_operand_c))
						{
							func_call_param_list = buildExprListExp(originalExpr_lhs_operand, expr); //changed from originalExpr_rhs_operand
						}
						else
						{
							func_call_param_list = buildExprListExp(expr, originalExpr_rhs_operand);
						} 
					}
					
					
					if (isSgBinaryOp(originalExpr) == isSgBinaryOp(op_parent_lhs_operand_c))
					{
						appendExpression(func_call_param_list, op_parent_rhs_operand);
					}
					else
					{
						appendExpression(func_call_param_list, op_parent_lhs_operand);
					}
					if (binary_op_cnt > 0)
					{
						last_binary_op_final = isSgBinaryOp(last_binary_op_final->get_parent());
						ROSE_ASSERT(last_binary_op_final != NULL);
					}
					binary_op_cnt--;
				}
					
				else
				{
					//printf("\n In function %s: AddOp - No Binary Op found in Parent", __FUNCTION__);
					func_name = "spu_add";
					if (flag == 1)
					{
						func_call_param_list = buildExprListExp(originalExpr_lhs_operand, originalExpr_rhs_operand);
						flag++;
					}
					else
					{
						if ( isSgBinaryOp(originalExpr_rhs_operand_c) && (isSgPntrArrRefExp(originalExpr_rhs_operand_c) == NULL))
						{
							func_call_param_list =buildExprListExp(expr, originalExpr_lhs_operand);
						}
						else
						{
							func_call_param_list =buildExprListExp(expr, originalExpr_rhs_operand);
						}
					}
				}
			}
									
			else if (isSgSubtractOp(originalExpr))
			{								
				//printf("\n In function %s: Found Sub Op", __FUNCTION__);
							
				if (isSgSubtractOp(op_parent) )
				{
					//printf("\n In function %s: In SUBX", __FUNCTION__);
					func_name = "spu_subx";
					
					if (flag == 1)
					{
						func_call_param_list = buildExprListExp(originalExpr_lhs_operand, originalExpr_rhs_operand);
						flag++;
					}
					else
					{
						if (isSgVarRefExp(originalExpr_lhs_operand_c))
						{
							func_call_param_list = buildExprListExp(originalExpr_lhs_operand, expr); //changed from originalExpr_rhs_operand
						}
						else
						{
							func_call_param_list = buildExprListExp(expr, originalExpr_rhs_operand);
						}
					}
					
					if ( isSgBinaryOp(originalExpr) == isSgBinaryOp(op_parent_lhs_operand_c))
					{
						appendExpression(func_call_param_list, op_parent_rhs_operand);
					}
					else
					{
						appendExpression(func_call_param_list, op_parent_lhs_operand);
					}
						
					if (binary_op_cnt > 0)
					{
						last_binary_op_final = isSgBinaryOp(last_binary_op_final->get_parent());
						ROSE_ASSERT(last_binary_op_final != NULL);
					}
					binary_op_cnt--;
				}
					
				else
				{
					//printf("\n In function %s: SubOp - No Binary Op found in Parent", __FUNCTION__);
					func_name = "spu_sub";
					
					if (flag == 1)
					{
						func_call_param_list =buildExprListExp(originalExpr_lhs_operand, originalExpr_rhs_operand);
						flag++;
					}
					else
					{
						if ( isSgBinaryOp(originalExpr_rhs_operand_c) && (isSgPntrArrRefExp(originalExpr_rhs_operand_c) == NULL))
						{
							func_call_param_list =buildExprListExp(expr, originalExpr_lhs_operand);
						}
						else
						{
							func_call_param_list =buildExprListExp(expr, originalExpr_rhs_operand);
						}
					}
				}
			}
										
			else if (isSgMultiplyOp(originalExpr))
			{
				//printf("\n In function %s: Found Mul Op", __FUNCTION__);
				SgMultiplyOp* mul_op = isSgMultiplyOp(originalExpr);
				
				if (isSgAddOp(op_parent) )
				{
					//printf("\n In function %s: In MADD", __FUNCTION__);
					SgAddOp* add_op = isSgAddOp(op_parent);
					func_name = analyze_madd_op(mul_op, add_op);//"spu_madd";
					
					if (flag == 1)
					{
						func_call_param_list = buildExprListExp(originalExpr_lhs_operand, originalExpr_rhs_operand);
						flag++;
					}
					else
					{
						if (isSgVarRefExp(originalExpr_lhs_operand_c))
						{
							func_call_param_list = buildExprListExp(originalExpr_lhs_operand, expr); //changed from originalExpr_rhs_operand
						}
						else
						{
							func_call_param_list = buildExprListExp(expr, originalExpr_rhs_operand);
						}
					}
					
					if ( isSgBinaryOp(originalExpr) == isSgBinaryOp(op_parent_lhs_operand_c))
					{
						appendExpression(func_call_param_list, op_parent_rhs_operand);
					}
					else
					{
						appendExpression(func_call_param_list, op_parent_lhs_operand);
					}
					
					if (binary_op_cnt > 0)
					{
						last_binary_op_final = isSgBinaryOp(last_binary_op_final->get_parent());
						ROSE_ASSERT(last_binary_op_final != NULL);
					}
					binary_op_cnt--;
				}
					
				else if (isSgSubtractOp(op_parent) )
				{
					//printf("\n In function %s: In MSUB", __FUNCTION__);
					SgSubtractOp* sub_op = isSgSubtractOp(op_parent);
					func_name = analyze_msub_op(mul_op, sub_op);//"spu_msub";
					
					if (flag == 1)
					{
						func_call_param_list = buildExprListExp(originalExpr_lhs_operand, originalExpr_rhs_operand);
						flag++;
					}
					else
					{
						if (isSgVarRefExp(originalExpr_lhs_operand_c))
						{
							func_call_param_list = buildExprListExp(originalExpr_lhs_operand, expr); //changed from originalExpr_rhs_operand
						}
						else
						{
							func_call_param_list = buildExprListExp(expr, originalExpr_rhs_operand);
						}
					}
					
					if ( isSgBinaryOp(originalExpr) == isSgBinaryOp(op_parent_lhs_operand_c))
					{
						appendExpression(func_call_param_list, op_parent_rhs_operand);
					}
					else
					{
						appendExpression(func_call_param_list, op_parent_lhs_operand);
					}
					
					if (binary_op_cnt > 0)
					{
						last_binary_op_final = isSgBinaryOp(last_binary_op_final->get_parent());
						ROSE_ASSERT(last_binary_op_final != NULL);
					}
					binary_op_cnt--;
				}
				
				else
				{
					//printf("\n In function %s: MulOp - No Binary Op found in Parent", __FUNCTION__);
					func_name = analyze_multiply_op(mul_op);//"spu_mul";
								
					if (flag == 1)
					{
						func_call_param_list =buildExprListExp(originalExpr_lhs_operand, originalExpr_rhs_operand);
						flag++;
					}
					else
					{
						if ( isSgBinaryOp(originalExpr_rhs_operand_c) && (isSgPntrArrRefExp(originalExpr_rhs_operand_c) == NULL))
						{
							func_call_param_list =buildExprListExp(originalExpr_lhs_operand, expr);
						}
						else
						{
							func_call_param_list =buildExprListExp(expr, originalExpr_rhs_operand);
						}							
					}
				}
			}
										
			else if (isSgDivideOp(originalExpr))
			{
				//printf("\n In function %s: Found Divide Op", __FUNCTION__);
				SgDivideOp* div_op = isSgDivideOp(originalExpr);
				func_name = analyze_divide_op(div_op); //"divf4";

												
				if (flag == 1)
				{
					func_call_param_list = buildExprListExp(originalExpr_lhs_operand, originalExpr_rhs_operand);					
					flag++;
				}
				else
				{
					if ( isSgBinaryOp(originalExpr_rhs_operand_c) && (isSgPntrArrRefExp(originalExpr_rhs_operand_c) == NULL))
					{
						func_call_param_list =buildExprListExp(originalExpr_lhs_operand, expr);
					}
					else
					{
						func_call_param_list =buildExprListExp(expr, originalExpr_rhs_operand);
					}
				}				
			}
			
			else if(isSgExponentiationOp(originalExpr))
			{
				SgExponentiationOp* exp_op = isSgExponentiationOp(originalExpr);
				func_name = analyze_exponentiation_op(exp_op); //"divf4";				
				
				if (flag == 1)
				{
					func_call_param_list = buildExprListExp(originalExpr_lhs_operand, originalExpr_rhs_operand);					
					flag++;
				}
				else
				{
					if ( isSgBinaryOp(originalExpr_rhs_operand_c) && (isSgPntrArrRefExp(originalExpr_rhs_operand_c) == NULL))
					{
						func_call_param_list =buildExprListExp(originalExpr_lhs_operand, expr);
					}
					else
					{
						func_call_param_list =buildExprListExp(expr, originalExpr_rhs_operand);
					}
				}		
			}
				
			// Comparison Operators
						
			else if (isSgEqualityOp(originalExpr))
			{	
				//printf("\n In function %s: Found Equality Op", __FUNCTION__);
				func_name = "spu_cmpeq";
				
				if (flag == 1)
				{
					func_call_param_list = buildExprListExp(originalExpr_lhs_operand, originalExpr_rhs_operand);
					ROSE_ASSERT(func_call_param_list != NULL);
					flag++;
				}
				else
				{
					if ( isSgBinaryOp(originalExpr_rhs_operand_c) && (isSgPntrArrRefExp(originalExpr_rhs_operand_c) == NULL))
					{
						func_call_param_list =buildExprListExp(expr, originalExpr_lhs_operand);
					}
					else
					{
						func_call_param_list =buildExprListExp(expr, originalExpr_rhs_operand);
					}	
				}
			}
									
			else if (isSgGreaterThanOp(originalExpr) || isSgLessThanOp(originalExpr))
			{	
				//printf("\n In function %s: Found Greater than/Less than Op", __FUNCTION__);
				func_name = "spu_cmpgt";
				
				if (isSgGreaterThanOp(originalExpr) != NULL)
				{					
					if (flag == 1)
					{
						func_call_param_list = buildExprListExp(originalExpr_lhs_operand, originalExpr_rhs_operand);
						flag++;
					}
					else
					{
						if ( isSgBinaryOp(originalExpr_rhs_operand_c) && (isSgPntrArrRefExp(originalExpr_rhs_operand_c) == NULL))
						{
							func_call_param_list =buildExprListExp(originalExpr_lhs_operand, expr);
						}
						else
						{
							func_call_param_list =buildExprListExp(expr, originalExpr_rhs_operand);
						}	
					}
				}
				
				else
				{				
					if (flag == 1)
					{
						func_call_param_list = buildExprListExp( originalExpr_rhs_operand, originalExpr_lhs_operand);
						flag++;
					}
					else
					{
						if ( isSgBinaryOp(originalExpr_rhs_operand_c) && (isSgPntrArrRefExp(originalExpr_rhs_operand_c) == NULL))
						{
							func_call_param_list =buildExprListExp(expr, originalExpr_lhs_operand);
						}
						else
						{
							func_call_param_list =buildExprListExp(originalExpr_rhs_operand, expr);
						}	
					}
				}
			}
					
			ret_type = originalExpr_rhs_operand->get_type();
			expr = buildFunctionCallExp(func_name, ret_type, func_call_param_list, func_scope);
						
			if (binary_op_cnt > 0)
			{
				last_binary_op_final = isSgBinaryOp(last_binary_op_final->get_parent());
				ROSE_ASSERT(last_binary_op_final != NULL);
			}
			binary_op_cnt--;
		}
	}
	
	return expr;
}


SgExpression* SPUAnalyzer::analyze_dual_binary_child(SgBinaryOp* expr1, SgBinaryOp* expr2, SgScopeStatement* expr_scope)
{
	SgExpression* expr = NULL;
	
	SgName func_name;
	SgExprListExp* func_call_param_list;
	SgType* ret_type;
	SgScopeStatement* func_scope;
	
	func_scope = isSgScopeStatement(expr_scope);
	
	SgBinaryOp* parent_op = isSgBinaryOp(expr1->get_parent());
	
	if (isSgAddOp(parent_op))
	{
		func_name = "spu_add";
		
		func_call_param_list = buildExprListExp(analyze_binary_op(expr1, func_scope),analyze_binary_op(expr2, func_scope));
	}
	
	else if (isSgSubtractOp(parent_op))
	{
		func_name = "spu_sub";
		
		func_call_param_list = buildExprListExp(analyze_binary_op(expr1, func_scope),analyze_binary_op(expr2, func_scope));
	}
	
	else if (isSgMultiplyOp(parent_op))
	{
		func_name = "spu_mul";
		
		func_call_param_list = buildExprListExp(analyze_binary_op(expr1, func_scope),analyze_binary_op(expr2, func_scope));
	}
	
	else if (isSgDivideOp(parent_op))
	{
		func_name = "divf4";
		
		func_call_param_list = buildExprListExp(analyze_binary_op(expr1, func_scope),analyze_binary_op(expr2, func_scope));
	}
	
	else if (isSgEqualityOp(parent_op))
	{
		func_name = "spu_cmpeq";
		
		func_call_param_list = buildExprListExp(analyze_binary_op(expr1, func_scope),analyze_binary_op(expr2, func_scope));
	}
	
	else if (isSgGreaterThanOp(parent_op) || isSgLessThanOp(parent_op))
	{
		func_name = "spu_cmpgt";
		
		if (isSgGreaterThanOp(parent_op) != NULL)
		{
			func_call_param_list = buildExprListExp(analyze_binary_op(expr1, func_scope),analyze_binary_op(expr2, func_scope));
		}
		else
		{
			func_call_param_list = buildExprListExp(analyze_binary_op(expr2, func_scope),analyze_binary_op(expr1, func_scope));
		}
	}			
	
	ret_type = (expr1->get_type());
	expr = buildFunctionCallExp(func_name, ret_type, func_call_param_list, func_scope);
	
	return expr;	
}

SgExpression* SPUAnalyzer::analyze_unary_op(SgUnaryOp* originalExpr, SgScopeStatement* expr_scope)
{
	SgExpression* expr = NULL;
	SgType* ret_type;
	SgNode* parent = originalExpr->get_parent();
		
	// bitwise NOT operator
	if (isSgBitComplementOp(originalExpr))
	{
		//printf("\n In function %s: Found BitComplement Op", __FUNCTION__);
		SgNode* operand = originalExpr->get_operand_i();
		
		if (isSgBitAndOp(operand) || isSgBitOrOp(operand))
		{
			SgExpression* lhs_opd = isSgExpression(isSgBinaryOp(operand)->get_lhs_operand());
			SgExpression* rhs_opd = isSgExpression(isSgBinaryOp(operand)->get_rhs_operand());
			
			
			if (isSgBitAndOp(operand))
			{
				expr = buildFunctionCallExp("spu_nand", buildDoubleType(),
											buildExprListExp(lhs_opd, rhs_opd),
											expr_scope);
			}
			
			else if (isSgBitOrOp(operand))
			{
				expr = buildFunctionCallExp("spu_nor", buildDoubleType(),
											buildExprListExp(lhs_opd, rhs_opd),
											expr_scope);
			}
		}
	}
	// YET TO IMPLEMENT COMPLETELY
	
	else if (isSgMinusOp(originalExpr))
	{
		//printf("\n In function %s: Found Unary Minus Op", __FUNCTION__);
		SgExpression* operand = originalExpr->get_operand_i();
		ret_type = (originalExpr->get_operand_i())->get_type();
		if (isSgBinaryOp(operand))
		{
			expr = buildFunctionCallExp("spu_sub", ret_type, buildExprListExp(buildIntVal(0), analyze_binary_op(isSgBinaryOp(operand), expr_scope)), expr_scope);
		}
		else
		{
			expr = analyze_expression(operand, expr_scope);	
		}
	}
	
	return expr;
}


SgExpression* SPUAnalyzer::fix_variable(SgExpression* var, SgScopeStatement* var_scope)
{
	SgType* var_type = isSgType(var->get_type());
	SgExpression* new_var = NULL;
	
	if (isSgArrayType(var_type))
	{
		//printf("\n In function %s: Found Array Type", __FUNCTION__);
		SgExpression* index;
		SgName index_name = "i";
		index = buildVarRefExp(index_name, var_scope);
		
		if (isSgVarRefExp(var))
		{
			SgName var_name = ((isSgVarRefExp(var))->get_symbol())->get_name();
			//printf("\n In function %s: In Array type - Replacing var = %s with new_var", __FUNCTION__, var_name.str());
			new_var = buildVarRefExp(var_name, var_scope);
		}
		if(new_var != NULL) 
		{
			//printf("In function %s: In Array type - Created a new variable with array reference\r\n", __FUNCTION__);	
			SgExpression* new_arr_ref = new SgPntrArrRefExp(var_scope->get_file_info(), new_var, index, isSgArrayType(var_type)->get_base_type());
			ROSE_ASSERT(new_arr_ref != NULL); 	
			return new_arr_ref;
		}
		else
		{
			//printf("In function %s: Returning unchanged - Unable to create array reference.\r\n", __FUNCTION__);
			return var;
		}
	}
	else if (isSgPointerType(var_type))
	{
		//printf("In function %s: Found Pointer Type\r\n", __FUNCTION__);
		return var;
	}
	else if (isSgReferenceType(var_type))
	{
		//printf("In function %s: Found Reference Type\r\n", __FUNCTION__);
		return var;
	}
	else if (isSgPntrArrRefExp(var))
	{
		//printf("\n Found PntrArrRefExp");
		return var;
	}
	
	else if (isSgVarRefExp(var))
	{
		SgName var_name = ((isSgVarRefExp(var))->get_symbol())->get_name();
		//printf("\n Replacing var = %s with new_var", var_name.str());
		new_var = buildVarRefExp(var_name, var_scope);
		ROSE_ASSERT(new_var != NULL); 
		return new_var;
	}
	
	else if (isSgValueExp(var))
	{
		new_var = analyze_value_expression(isSgValueExp(var), var_scope);
		return new_var;
	}
	
	else
	{
		//printf("\n Returning unchanged");
		return var;
	}
}

SgStatementPtrList SPUAnalyzer::analyze_if_stmt_type(SgIfStmt* if_stmt, SgScopeStatement* stmt_scope, int if_type)
{
	SgStatementPtrList op_stmt_list;
	int func_call_flag = 1;
	SgAssignOp* true_assgn_op = NULL;
	SgAssignOp* false_assgn_op = NULL;
	
	if (if_stmt != NULL)
	{
		int true_cnt = 0;
		int false_cnt = 0;
		SgStatement* true_stmt = if_stmt->get_true_body();
		SgStatement* false_stmt = if_stmt->get_false_body();
		SgStatementPtrList true_stmt_list;
		SgStatementPtrList false_stmt_list;
		SgStatement* single_true_stmt;
		SgStatement* single_false_stmt;
		if (isSgExprStatement(true_stmt))
		{
			single_true_stmt = isSgStatement(true_stmt);
			true_cnt++;
		}
		else if (isSgScopeStatement(true_stmt))
		{
			true_stmt_list = isSgScopeStatement(true_stmt)->getStatementList();
			for (Rose_STL_Container<SgStatement*>::iterator i = true_stmt_list.begin(); i!= true_stmt_list.end(); i++)
			{
				true_cnt++;
				single_true_stmt = isSgStatement(*i);
			}
		}
		else
		{
			printf("\n ERROR: In function %s - Could not get statements in TRUE body of the IF statement", __FUNCTION__);
			ROSE_ASSERT(false);
		}
					
		if (isSgIfStmt(false_stmt) == NULL)
		{
			if (isSgExprStatement(false_stmt))
			{
				single_false_stmt = isSgStatement(false_stmt);
				false_cnt++;
			}
			else if (isSgScopeStatement(false_stmt))
			{
				false_stmt_list = isSgScopeStatement(false_stmt)->getStatementList();
				for (Rose_STL_Container<SgStatement*>::iterator i = false_stmt_list.begin(); i!= false_stmt_list.end(); i++)
				{
					false_cnt++;
					single_false_stmt = isSgStatement(*i);
				}
			}
			else
			{
				printf("\n ERROR: In function %s - Could not get statements in FALSE body of the IF statement", __FUNCTION__);
				ROSE_ASSERT(false);
			}
		}
		
		if( true_cnt == 1 && false_cnt == 1)
		{
			if (isSgExprStatement(single_true_stmt) && isSgExprStatement(single_false_stmt))
			{
			SgExpression* true_expr = isSgExprStatement(single_true_stmt)->get_expression();
			true_assgn_op = isSgAssignOp(true_expr);
			SgExpression* false_expr = isSgExprStatement(single_false_stmt)->get_expression();
			false_assgn_op = isSgAssignOp(false_expr);
			}
			
			if (true_assgn_op != NULL && false_assgn_op != NULL)
			{
				SgVarRefExp* true_LHS = isSgVarRefExp(true_assgn_op->get_lhs_operand());
				SgVarRefExp* false_LHS = isSgVarRefExp(false_assgn_op->get_lhs_operand());
										
				if( strcmp(true_LHS->get_symbol()->get_name().str(), false_LHS->get_symbol()->get_name().str()) == 0)
				{
					func_call_flag = 0;
				}
				else 
				{
					func_call_flag = 1;
				}
			}
			else 
			{
				func_call_flag = 1;
			}
		}
		else
		{
			func_call_flag = 1;
		}
		
		if (func_call_flag == 0)
		{
			op_stmt_list = analyze_simple_if_else_stmt(if_stmt, stmt_scope, true_assgn_op, false_assgn_op);
		}
		else
		{
			if (if_type == 0)
			{
				op_stmt_list = analyze_if_stmt(if_stmt, stmt_scope);
			}
			else
			{
				op_stmt_list = analyze_else_if_stmt(if_stmt, stmt_scope);
			}
		}
	}
	
	ROSE_ASSERT(!op_stmt_list.empty());
	return op_stmt_list;	
}
												

SgStatementPtrList SPUAnalyzer::analyze_simple_if_else_stmt(SgIfStmt* if_stmt, SgScopeStatement* stmt_scope, SgAssignOp* true_assgn_op, SgAssignOp* false_assgn_op)
{
	SgStatement* func_callstmt = NULL;
	SgStatementPtrList op_stmt_list;
	
	SgForStatement* for_stmt;
	SgBasicBlock* for_body;
	int for_flag = 1;
	int list_insert_flag = 1;
	
	if (if_stmt != NULL)
	{
		SgStatement* true_stmt = if_stmt->get_true_body();
		
		int cnt =0;
		if(true_stmt != NULL)
		{
			SgExpression* if_cond = isSgExpression(isSgExprStatement(if_stmt->get_conditional())->get_expression());
			
			SgExpression* other_if_cond = NULL;
			SgExpression* other_func_expr;
			
			SgExpression* if_cond_list = NULL;
			SgNode* if_parent = if_stmt->get_parent();
			
			int flag =1;
			int basic_block_flag = 0;
			
			if_cond = analyze_expression(if_cond, stmt_scope);
			/*
			if ( isSgBinaryOp(if_cond) != NULL)
			{
				if_cond = analyze_binary_op(isSgBinaryOp(if_cond), stmt_scope);
			}
			else
			{
				if_cond = fix_variable(if_cond, stmt_scope);
			}
			*/
			
			
			while(isSgIfStmt(if_parent) || isSgBasicBlock(if_parent))
			{
				if (isSgIfStmt(if_parent))
				{
					SgIfStmt* temp_if_stmt = isSgIfStmt(if_parent);
					other_if_cond = analyze_expression(isSgExpression(isSgExprStatement(temp_if_stmt->get_conditional())->get_expression()), stmt_scope);
					/*if ( isSgBinaryOp(other_if_cond) != NULL)
					{
						other_if_cond = analyze_binary_op(isSgBinaryOp(other_if_cond), stmt_scope);
					}
					else
					{
						other_if_cond = fix_variable(other_if_cond, stmt_scope);
					}*/
									
					other_if_cond = buildFunctionCallExp("spu_nand", buildIntType(), buildExprListExp(other_if_cond, other_if_cond), stmt_scope);
				}
				
				else if (isSgBasicBlock(if_parent))
				{					
					if_parent = if_parent->get_parent();
					basic_block_flag = 1;
					
					if (isSgIfStmt(if_parent))
					{
						basic_block_flag = 0;
						SgIfStmt* temp_if_stmt = isSgIfStmt(if_parent);
						other_if_cond = analyze_expression(isSgExpression(isSgExprStatement(temp_if_stmt->get_conditional())->get_expression()), stmt_scope);
						/*
						if ( isSgBinaryOp(other_if_cond) != NULL)
						{
							other_if_cond = analyze_binary_op(isSgBinaryOp(other_if_cond), stmt_scope);
						}
						else
						{
							other_if_cond = fix_variable(other_if_cond, stmt_scope);
						}*/						
					}
				}   
				
				if (flag ==1)
				{					
					other_func_expr = other_if_cond;					
				}
				else 
				{
					if ( basic_block_flag == 0)
					{						
						other_func_expr = buildFunctionCallExp("spu_and", buildIntType(), buildExprListExp(other_if_cond, if_cond_list),stmt_scope);
					}					
				}
				++ flag;
				if_cond_list=other_func_expr;
				if_parent = if_parent->get_parent();
			}
			
			SgExpression* cond_expr_func;
			
			if ( if_cond_list != NULL)
			{
				cond_expr_func = buildFunctionCallExp("spu_and", buildIntType(), buildExprListExp(if_cond, if_cond_list),stmt_scope);
			}
			else
			{
				cond_expr_func = if_cond; 
			}					
					
			SgExpression* LHS = fix_variable(true_assgn_op->get_lhs_operand(), stmt_scope);
			SgExpression* true_RHS = fix_variable(true_assgn_op->get_rhs_operand(), stmt_scope);
			SgExpression* false_RHS = fix_variable(false_assgn_op->get_rhs_operand(), stmt_scope);
						
			SgExpression* LHS_c = true_assgn_op->get_lhs_operand();
						
			SgStatement* assign_stmt = buildAssignStatement(LHS,buildFunctionCallExp("spu_sel", buildIntType(), buildExprListExp(false_RHS, true_RHS, cond_expr_func),stmt_scope));
					
			if (isSgArrayType(isSgType(LHS_c->get_type())))
			{
				if (for_flag == 1)
				{
					SgName for_cnt = "i";
					SgVarRefExp* for_cnt_ref = buildVarRefExp(for_cnt, stmt_scope);
					SgName for_test_cond = "count";
					SgVarRefExp* for_test_cond_ref = buildVarRefExp(for_test_cond, stmt_scope);
					SgExprStatement* for_init = buildExprStatement(buildAssignOp(for_cnt_ref, buildIntVal(0)));
					SgExprStatement* for_cond = buildExprStatement(buildLessThanOp(for_cnt_ref, for_test_cond_ref));
					SgExpression* for_cnt_incr = buildPlusPlusOp(for_cnt_ref);
					for_body = buildBasicBlock();
					
					for_stmt = new SgForStatement(stmt_scope->get_file_info(), for_cond, for_cnt_incr, for_body);
					setOneSourcePositionForTransformation(for_stmt);
					for_cond->set_parent(for_stmt);
					for_body->set_parent(for_stmt);
					
					SgForInitStatement* for_init_stmt = new SgForInitStatement(stmt_scope->get_file_info());
					for_init_stmt->append_init_stmt(for_init);			 
					for_stmt->set_for_init_stmt(for_init_stmt);
				}
							
				appendStatement(assign_stmt, for_body);
				func_callstmt = isSgStatement(for_stmt);
				for_flag = 0;
			}
			else
			{				
				for_flag = 1;
				func_callstmt = isSgStatement(assign_stmt);
			}
			
			if (list_insert_flag == 1)
			{
				op_stmt_list.insert(op_stmt_list.end(), func_callstmt);
			}					
		}
	}	
	return op_stmt_list;
}



SgStatementPtrList SPUAnalyzer::analyze_if_stmt(SgIfStmt* if_stmt, SgScopeStatement* stmt_scope)
{
	SgStatement* func_callstmt = NULL;
	
	SgStatementPtrList op_stmt_list;
			
	SgForStatement* for_stmt;
	SgBasicBlock* for_body;
	int for_flag = 1;
	int list_insert_flag = 1;
	int func_call_flag = 0;
	int insert_for_stmt_flag = 0;

	if (if_stmt != NULL)
	{
		SgStatement* true_stmt = if_stmt->get_true_body();
		SgStatement* false_stmt = if_stmt->get_false_body();
		SgStatementPtrList true_stmt_list;
		SgStatementPtrList false_stmt_list;
	
		if (isSgExprStatement(true_stmt))
		{
			true_stmt_list.insert(true_stmt_list.begin(), true_stmt);
		}
		else if (isSgScopeStatement(true_stmt))
		{
			true_stmt_list = isSgScopeStatement(true_stmt)->getStatementList(); 
		}
		else
		{
			printf("\n ERROR: In function %s - Could not get statements in TRUE body of the IF statement", __FUNCTION__);
			ROSE_ASSERT(false);
		}	
		
		if (isSgIfStmt(false_stmt) == NULL)
		{
			if (isSgExprStatement(false_stmt))
			{
				false_stmt_list.insert(false_stmt_list.begin(), false_stmt);
			}
			else if (isSgScopeStatement(false_stmt))
			{
				false_stmt_list = isSgScopeStatement(false_stmt)->getStatementList(); 
			}
			else
			{
				printf("\n ERROR: In function %s - Could not get statements in FALSE body of the IF statement", __FUNCTION__);
				ROSE_ASSERT(false);
			}	
		}
					
		int cnt =0;
		if(true_stmt != NULL)
		{		
			SgExpression* if_cond = isSgExpression(isSgExprStatement(if_stmt->get_conditional())->get_expression());
						
			SgExpression* other_if_cond = NULL;
			SgExpression* other_func_expr;
			
			SgExpression* if_cond_list = NULL;
			SgNode* if_parent = if_stmt->get_parent();
			
			int flag =1;
			int basic_block_flag = 0;
			
			if ( isSgBinaryOp(if_cond) != NULL)
			{
				if_cond = analyze_binary_op(isSgBinaryOp(if_cond), stmt_scope);
			}
			else
			{
				if_cond = fix_variable(if_cond, stmt_scope);
			}
			
			
			
			while(isSgIfStmt(if_parent) || isSgBasicBlock(if_parent))
			{
				if (isSgIfStmt(if_parent))
				{
					SgIfStmt* temp_if_stmt = isSgIfStmt(if_parent);
					other_if_cond = isSgExpression(isSgExprStatement(temp_if_stmt->get_conditional())->get_expression());
					if ( isSgBinaryOp(other_if_cond) != NULL)
					{
						other_if_cond = analyze_binary_op(isSgBinaryOp(other_if_cond), stmt_scope);
					}
					else
					{
						other_if_cond = fix_variable(other_if_cond, stmt_scope);
					}
					
					other_if_cond = buildFunctionCallExp("spu_nand", buildIntType(), buildExprListExp(other_if_cond, other_if_cond), stmt_scope);
				}
				
				else if (isSgBasicBlock(if_parent))
				{					
					if_parent = if_parent->get_parent();
					basic_block_flag = 1;
					
					if (isSgIfStmt(if_parent))
					{
						basic_block_flag = 0;
						SgIfStmt* temp_if_stmt = isSgIfStmt(if_parent);
						other_if_cond = isSgExpression(isSgExprStatement(temp_if_stmt->get_conditional())->get_expression());
						
						if ( isSgBinaryOp(other_if_cond) != NULL)
						{
							other_if_cond = analyze_binary_op(isSgBinaryOp(other_if_cond), stmt_scope);
						}
						else
						{
							other_if_cond = fix_variable(other_if_cond, stmt_scope);
						}
					}
				}   
				
				if (flag ==1)
				{					
					other_func_expr = other_if_cond;					
				}
				else 
				{
					if ( basic_block_flag == 0)
					{						
						other_func_expr = buildFunctionCallExp("spu_and", buildIntType(), buildExprListExp(other_if_cond, if_cond_list),stmt_scope);
					}					
				}
				++ flag;
				if_cond_list=other_func_expr;
				if_parent = if_parent->get_parent();
			}
				
			SgExpression* cond_expr_func;
			
			if ( if_cond_list != NULL)
			{
				cond_expr_func = buildFunctionCallExp("spu_and", buildIntType(), buildExprListExp(if_cond, if_cond_list),stmt_scope);
			}
			else
			{
				cond_expr_func = if_cond; 
			}					
							
			for (Rose_STL_Container<SgStatement*> :: iterator i = true_stmt_list.begin(); i != true_stmt_list.end(); i++)
			{
				if (isSgIfStmt(*i))
				{
					SgStatementPtrList nested_if_stmt_list = analyze_if_stmt_type(isSgIfStmt(*i), stmt_scope, 0);
					for (Rose_STL_Container<SgStatement*>::iterator k = nested_if_stmt_list.begin(); k != nested_if_stmt_list.end(); k++)
					{
						op_stmt_list.insert(op_stmt_list.end(), (*k));
					}																	   
				}
				
				else if (isSgExprStatement(*i)!=NULL)
				{
					SgExpression* expr = (isSgExprStatement(*i))->get_expression();
					
					if ( i+1 != true_stmt_list.end())
					{
						if(isSgExprStatement(*(i+1))!=NULL)
						{						
							SgExpression* next_expr = (isSgExprStatement(*(i+1)))->get_expression();
							if(isSgAssignOp(next_expr))
							{
								SgAssignOp* next_assgn_op = isSgAssignOp(next_expr);
								SgExpression* next_LHS_c = next_assgn_op->get_lhs_operand();
								if ( isSgArrayType(isSgType(next_LHS_c->get_type())) == NULL)
								{
									insert_for_stmt_flag = 1;
								}							
							}
						}
						else if (isSgIfStmt(*(i+1)) != NULL)
						{
							insert_for_stmt_flag = 1;
						}
					}
					else if (!false_stmt_list.empty())
					{
						insert_for_stmt_flag = 0;
						
						if (isSgIfStmt(*false_stmt_list.begin()))
						{
							insert_for_stmt_flag = 1;
						}
					}
					else 
					{
						insert_for_stmt_flag = 1;
					}
		
					if(isSgAssignOp(expr))
					{					
						SgAssignOp* assgn_op = isSgAssignOp(expr);
														
						SgExpression* LHS = fix_variable(assgn_op->get_lhs_operand(), stmt_scope);
						SgExpression* RHS = fix_variable(analyze_expression(assgn_op->get_rhs_operand(), stmt_scope), stmt_scope);
						
						SgExpression* LHS_c = assgn_op->get_lhs_operand();
															
						SgStatement* assign_stmt = buildAssignStatement(LHS,buildFunctionCallExp("spu_sel", buildIntType(), buildExprListExp(LHS, RHS, cond_expr_func),stmt_scope));
						
						if (isSgArrayType(isSgType(LHS_c->get_type())))
						{
							if (for_flag == 1)
							{							
								SgName for_cnt = "i";
								SgVarRefExp* for_cnt_ref = buildVarRefExp(for_cnt, stmt_scope);
								SgName for_test_cond = "count";
								SgVarRefExp* for_test_cond_ref = buildVarRefExp(for_test_cond, stmt_scope);
								SgExprStatement* for_init = buildExprStatement(buildAssignOp(for_cnt_ref, buildIntVal(0)));
								SgExprStatement* for_cond = buildExprStatement(buildLessThanOp(for_cnt_ref, for_test_cond_ref));
								SgExpression* for_cnt_incr = buildPlusPlusOp(for_cnt_ref);
								for_body = buildBasicBlock();
								
								for_stmt = new SgForStatement(stmt_scope->get_file_info(), for_cond, for_cnt_incr, for_body);
								setOneSourcePositionForTransformation(for_stmt);
								for_cond->set_parent(for_stmt);
								for_body->set_parent(for_stmt);
								
								SgForInitStatement* for_init_stmt = new SgForInitStatement(stmt_scope->get_file_info());
								for_init_stmt->append_init_stmt(for_init);			 
								for_stmt->set_for_init_stmt(for_init_stmt);
							}

							appendStatement(assign_stmt, for_body);
							if (insert_for_stmt_flag == 1)
							{
								op_stmt_list.insert(op_stmt_list.end(), for_stmt);
								insert_for_stmt_flag = 0;
							}
							
							for_flag = 0;
						}
						else
						{							
							for_flag = 1;
							func_callstmt = isSgStatement(assign_stmt);
							op_stmt_list.insert(op_stmt_list.end(), func_callstmt);
						}						
					}					
				}
			}
		}
				
		if(false_stmt != NULL)
		{		
			if (isSgIfStmt(false_stmt))
			{
				SgStatementPtrList else_if_stmt_list = analyze_if_stmt_type(isSgIfStmt(false_stmt), stmt_scope, 1);//analyze_if_stmt_type(isSgIfStmt(false_stmt), stmt_scope);
				for (Rose_STL_Container<SgStatement*>::iterator k = else_if_stmt_list.begin(); k != else_if_stmt_list.end(); k++)
				{
					op_stmt_list.insert(op_stmt_list.end(), (*k));
				}																	   
			}
			
			else
			{
				SgExpression* other_if_cond = NULL;
				SgExpression* other_func_expr;
				
				SgExpression* else_cond_list = NULL;
				SgNode* else_parent = if_stmt;
				
				int flag =1;				
				int basic_block_flag = 0;
				
				while(isSgIfStmt(else_parent) || isSgBasicBlock(else_parent))
				{
					if (isSgIfStmt(else_parent))
					{
						SgIfStmt* temp_if_stmt = isSgIfStmt(else_parent);
						other_if_cond = isSgExpression(isSgExprStatement(temp_if_stmt->get_conditional())->get_expression());
						if ( isSgBinaryOp(other_if_cond) != NULL)
						{
							other_if_cond = analyze_binary_op(isSgBinaryOp(other_if_cond), stmt_scope);
						}
						else
						{
							other_if_cond = fix_variable(other_if_cond, stmt_scope);
						}
						
						other_if_cond = buildFunctionCallExp("spu_nand", buildIntType(), buildExprListExp(other_if_cond, other_if_cond), stmt_scope);
					}
					else if (isSgBasicBlock(else_parent))
					{
						else_parent = else_parent->get_parent();
						basic_block_flag = 1;
					
						if (isSgIfStmt(else_parent))
						{
							basic_block_flag = 0;
							SgIfStmt* temp_if_stmt = isSgIfStmt(else_parent);
							other_if_cond = isSgExpression(isSgExprStatement(temp_if_stmt->get_conditional())->get_expression());
							
							if ( isSgBinaryOp(other_if_cond) != NULL)
							{
								other_if_cond = analyze_binary_op(isSgBinaryOp(other_if_cond), stmt_scope);
							}
							else
							{
								other_if_cond = fix_variable(other_if_cond, stmt_scope);
							}
							
						}
					}   
					
					if (flag ==1)
					{					
						other_func_expr = other_if_cond;
					}
					else 
					{
						if ( basic_block_flag == 0)
						{						
							other_func_expr = buildFunctionCallExp("spu_and", buildIntType(), buildExprListExp(other_if_cond, else_cond_list),stmt_scope);
						}
					}
					++ flag;
					else_cond_list=other_func_expr;
					else_parent = else_parent->get_parent();					
				}
								
				SgExpression* cond_expr_func = else_cond_list;
								
			//	false_stmt_list = isSgScopeStatement(false_stmt)->getStatementList(); 
								
				for (Rose_STL_Container<SgStatement*> :: iterator i = false_stmt_list.begin(); i != false_stmt_list.end(); i++)
				{								
					if (isSgIfStmt(*i))
					{
						SgStatementPtrList nested_if_stmt_list = analyze_if_stmt_type(isSgIfStmt(*i), stmt_scope, 1); //analyze_if_stmt_type(isSgIfStmt(*i), stmt_scope);
						for (Rose_STL_Container<SgStatement*>::iterator k = nested_if_stmt_list.begin(); k != nested_if_stmt_list.end(); k++)
						{
							op_stmt_list.insert(op_stmt_list.end(), (*k));
						}																	   
					}
				
					else if (isSgExprStatement(*i)!=NULL)
					{
						SgExpression* expr = (isSgExprStatement(*i))->get_expression();
						
						if ( i+1 != false_stmt_list.end())
						{
							if(isSgExprStatement(*(i+1))!=NULL)
							{						
								SgExpression* next_expr = (isSgExprStatement(*(i+1)))->get_expression();
								if(isSgAssignOp(next_expr))
								{
									SgAssignOp* next_assgn_op = isSgAssignOp(next_expr);
									SgExpression* next_LHS_c = next_assgn_op->get_lhs_operand();
									if ( isSgArrayType(isSgType(next_LHS_c->get_type())) == NULL)
									{
										insert_for_stmt_flag = 1;
									}
								}
							}
							else if (isSgIfStmt(*(i+1)) != NULL)
							{
								insert_for_stmt_flag = 1;
							}							
						}
						else
						{
							insert_for_stmt_flag = 1;
						}
							
						
						if(isSgAssignOp(expr))
						{										
							SgAssignOp* assgn_op = isSgAssignOp(expr);
							SgExpression* LHS = fix_variable(assgn_op->get_lhs_operand(), stmt_scope);
							SgExpression* RHS = fix_variable(analyze_expression(assgn_op->get_rhs_operand(), stmt_scope), stmt_scope);
							
							SgExpression* LHS_c = assgn_op->get_lhs_operand();
							SgStatement* assign_stmt = buildAssignStatement(LHS,buildFunctionCallExp("spu_sel", buildIntType(), buildExprListExp(LHS, RHS, cond_expr_func),stmt_scope));
							if (isSgArrayType(isSgType(LHS_c->get_type())))
							{
								if (for_flag == 1)
								{									
									SgName for_cnt = "i";
									SgVarRefExp* for_cnt_ref = buildVarRefExp(for_cnt, stmt_scope);
									SgName for_test_cond = "count";
									SgVarRefExp* for_test_cond_ref = buildVarRefExp(for_test_cond, stmt_scope);
									SgExprStatement* for_init = buildExprStatement(buildAssignOp(for_cnt_ref, buildIntVal(0)));
									SgExprStatement* for_cond = buildExprStatement(buildLessThanOp(for_cnt_ref, for_test_cond_ref));
									SgExpression* for_cnt_incr = buildPlusPlusOp(for_cnt_ref);
									for_body = buildBasicBlock();
									
									for_stmt = new SgForStatement(stmt_scope->get_file_info(), for_cond, for_cnt_incr, for_body);
									setOneSourcePositionForTransformation(for_stmt);
									for_cond->set_parent(for_stmt);
									for_body->set_parent(for_stmt);
									
									SgForInitStatement* for_init_stmt = new SgForInitStatement(stmt_scope->get_file_info());
									for_init_stmt->append_init_stmt(for_init);			 
									for_stmt->set_for_init_stmt(for_init_stmt);
								}
								appendStatement(assign_stmt, for_body);
								if (insert_for_stmt_flag == 1)
								{
									op_stmt_list.insert(op_stmt_list.end(), for_stmt);
									insert_for_stmt_flag = 0;
								}
								
								for_flag = 0;
							}
							else
							{
								for_flag = 1;
								op_stmt_list.insert(op_stmt_list.end(), assign_stmt);
							}							
						}
					}
				}
			}
		}
	}
	
	return op_stmt_list;
}


SgStatementPtrList SPUAnalyzer::analyze_else_if_stmt(SgIfStmt* else_if_stmt, SgScopeStatement* stmt_scope)
{
	SgStatement* func_callstmt = NULL;
	
	SgStatementPtrList op_stmt_list;
	
	SgForStatement* for_stmt;
	SgBasicBlock* for_body;
	int for_flag = 1;
	int list_insert_flag = 1;
	int func_call_flag = 0;
	int insert_for_stmt_flag = 0;
	
	if (else_if_stmt != NULL)
	{
		SgStatement* true_stmt = else_if_stmt->get_true_body();
		SgStatement* false_stmt = else_if_stmt->get_false_body();
		SgStatementPtrList true_stmt_list;
		SgStatementPtrList false_stmt_list;
		
		if (isSgExprStatement(true_stmt))
		{
			true_stmt_list.insert(true_stmt_list.begin(), true_stmt);
		}
		else if (isSgScopeStatement(true_stmt))
		{
			true_stmt_list = isSgScopeStatement(true_stmt)->getStatementList(); 
		}
		else
		{
			printf("\n ERROR: In function %s - Could not get statements in TRUE body of the IF statement", __FUNCTION__);
			ROSE_ASSERT(false);
		}	
		
		if (isSgIfStmt(false_stmt) == NULL)
		{
			if (isSgExprStatement(false_stmt))
			{
				false_stmt_list.insert(false_stmt_list.begin(), false_stmt);
			}
			else if (isSgScopeStatement(false_stmt))
			{
				false_stmt_list = isSgScopeStatement(false_stmt)->getStatementList(); 
			}
			else
			{
				printf("\n ERROR: In function %s - Could not get statements in FALSE body of the IF statement", __FUNCTION__);
				ROSE_ASSERT(false);
			}	
		}
		
		int cnt =0;
		if(true_stmt != NULL)
		{		
			SgExpression* if_cond = isSgExpression(isSgExprStatement(else_if_stmt->get_conditional())->get_expression());
			
			SgExpression* other_if_cond = NULL;
			SgExpression* other_func_expr;
			
			SgExpression* if_cond_list = NULL;
			SgNode* else_if_parent = else_if_stmt->get_parent();
			
			int flag =1;
			int basic_block_flag = 0;
			
			if ( isSgBinaryOp(if_cond) != NULL)
			{
				if_cond = analyze_binary_op(isSgBinaryOp(if_cond), stmt_scope);
			}
			else
			{
				if_cond = fix_variable(if_cond, stmt_scope);
			}
			
			
			
			while(isSgIfStmt(else_if_parent) || isSgBasicBlock(else_if_parent))
			{
				if (isSgIfStmt(else_if_parent))
				{
					SgIfStmt* temp_else_if_stmt = isSgIfStmt(else_if_parent);
					other_if_cond = analyze_expression(isSgExpression(isSgExprStatement(temp_else_if_stmt->get_conditional())->get_expression()), stmt_scope);
					/*if ( isSgBinaryOp(other_if_cond) != NULL)
					 {
					 other_if_cond = analyze_binary_op(isSgBinaryOp(other_if_cond), stmt_scope);
					 }
					 else
					 {
					 other_if_cond = fix_variable(other_if_cond, stmt_scope);
					 }*/
					
					other_if_cond = buildFunctionCallExp("spu_nand", buildIntType(), buildExprListExp(other_if_cond, other_if_cond), stmt_scope);
				}
				
				else if (isSgBasicBlock(else_if_parent))
				{					
					else_if_parent = else_if_parent->get_parent();
					basic_block_flag = 1;
					
					if (isSgIfStmt(else_if_parent))
					{
						basic_block_flag = 0;
						SgIfStmt* temp_else_if_stmt = isSgIfStmt(else_if_parent);
						other_if_cond = analyze_expression(isSgExpression(isSgExprStatement(temp_else_if_stmt->get_conditional())->get_expression()), stmt_scope);
						/*
						 if ( isSgBinaryOp(other_if_cond) != NULL)
						 {
						 other_if_cond = analyze_binary_op(isSgBinaryOp(other_if_cond), stmt_scope);
						 }
						 else
						 {
						 other_if_cond = fix_variable(other_if_cond, stmt_scope);
						 }*/
						other_if_cond = buildFunctionCallExp("spu_nand", buildIntType(), buildExprListExp(other_if_cond, other_if_cond), stmt_scope);
					}
				}   
				
				if (flag ==1)
				{					
					other_func_expr = other_if_cond;					
				}
				else 
				{
					if ( basic_block_flag == 0)
					{						
						other_func_expr = buildFunctionCallExp("spu_and", buildIntType(), buildExprListExp(other_if_cond, if_cond_list),stmt_scope);
					}					
				}
				++ flag;
				if_cond_list=other_func_expr;
				else_if_parent = else_if_parent->get_parent();
			}
			
			SgExpression* cond_expr_func;
			
			if ( if_cond_list != NULL)
			{
				cond_expr_func = buildFunctionCallExp("spu_and", buildIntType(), buildExprListExp(if_cond, if_cond_list),stmt_scope);
			}
			else
			{
				cond_expr_func = if_cond; 
			}					
			
			//SgStatementPtrList true_stmt_list = isSgScopeStatement(true_stmt)->getStatementList(); 
			
			for (Rose_STL_Container<SgStatement*> :: iterator i = true_stmt_list.begin(); i != true_stmt_list.end(); i++)
			{
				if (isSgIfStmt(*i))
				{
					SgStatementPtrList nested_if_stmt_list = analyze_if_stmt_type(isSgIfStmt(*i), stmt_scope, 0);
					for (Rose_STL_Container<SgStatement*>::iterator k = nested_if_stmt_list.begin(); k != nested_if_stmt_list.end(); k++)
					{
						op_stmt_list.insert(op_stmt_list.end(), (*k));
					}																	   
				}
				
				else if (isSgExprStatement(*i)!=NULL)
				{
					SgExpression* expr = (isSgExprStatement(*i))->get_expression();
					
					if ( i+1 != true_stmt_list.end())
					{
						if(isSgExprStatement(*(i+1))!=NULL)
						{						
							SgExpression* next_expr = (isSgExprStatement(*(i+1)))->get_expression();
							if(isSgAssignOp(next_expr))
							{
								SgAssignOp* next_assgn_op = isSgAssignOp(next_expr);
								SgExpression* next_LHS_c = next_assgn_op->get_lhs_operand();
								if ( isSgArrayType(isSgType(next_LHS_c->get_type())) == NULL)
								{
									insert_for_stmt_flag = 1;
								}							
							}
						}
						else if (isSgIfStmt(*(i+1)) != NULL)
						{
							insert_for_stmt_flag = 1;
						}
					}
					else if (!false_stmt_list.empty())
					{
						insert_for_stmt_flag = 0;
						
						if (isSgIfStmt(*false_stmt_list.begin()))
						{
							insert_for_stmt_flag = 1;
						}
					}
					else 
					{
						insert_for_stmt_flag = 1;
					}
					
					if(isSgAssignOp(expr))
					{					
						SgAssignOp* assgn_op = isSgAssignOp(expr);
						
						SgExpression* LHS = fix_variable(assgn_op->get_lhs_operand(), stmt_scope);
						SgExpression* RHS = fix_variable(analyze_expression(assgn_op->get_rhs_operand(), stmt_scope), stmt_scope);
						
						SgExpression* LHS_c = assgn_op->get_lhs_operand();
						
						SgStatement* assign_stmt = buildAssignStatement(LHS,buildFunctionCallExp("spu_sel", buildIntType(), buildExprListExp(LHS, RHS, cond_expr_func),stmt_scope));
						
						if (isSgArrayType(isSgType(LHS_c->get_type())))
						{
							if (for_flag == 1)
							{							
								SgName for_cnt = "i";
								SgVarRefExp* for_cnt_ref = buildVarRefExp(for_cnt, stmt_scope);
								SgName for_test_cond = "count";
								SgVarRefExp* for_test_cond_ref = buildVarRefExp(for_test_cond, stmt_scope);
								SgExprStatement* for_init = buildExprStatement(buildAssignOp(for_cnt_ref, buildIntVal(0)));
								SgExprStatement* for_cond = buildExprStatement(buildLessThanOp(for_cnt_ref, for_test_cond_ref));
								SgExpression* for_cnt_incr = buildPlusPlusOp(for_cnt_ref);
								for_body = buildBasicBlock();
								
								for_stmt = new SgForStatement(stmt_scope->get_file_info(), for_cond, for_cnt_incr, for_body);
								setOneSourcePositionForTransformation(for_stmt);
								for_cond->set_parent(for_stmt);
								for_body->set_parent(for_stmt);
								
								SgForInitStatement* for_init_stmt = new SgForInitStatement(stmt_scope->get_file_info());
								for_init_stmt->append_init_stmt(for_init);			 
								for_stmt->set_for_init_stmt(for_init_stmt);
							}
							
							appendStatement(assign_stmt, for_body);
							if (insert_for_stmt_flag == 1)
							{
								op_stmt_list.insert(op_stmt_list.end(), for_stmt);
								insert_for_stmt_flag = 0;
							}
							
							for_flag = 0;
						}
						else
						{							
							for_flag = 1;
							func_callstmt = isSgStatement(assign_stmt);
							op_stmt_list.insert(op_stmt_list.end(), func_callstmt);
						}						
					}					
				}
			}
		}
		
		if(false_stmt != NULL)
		{		
			if (isSgIfStmt(false_stmt))
			{
				SgStatementPtrList else_if_stmt_list = analyze_if_stmt_type(isSgIfStmt(false_stmt), stmt_scope, 1);
				for (Rose_STL_Container<SgStatement*>::iterator k = else_if_stmt_list.begin(); k != else_if_stmt_list.end(); k++)
				{
					op_stmt_list.insert(op_stmt_list.end(), (*k));
				}																	   
			}
			
			else
			{
				SgExpression* other_if_cond = NULL;
				SgExpression* other_func_expr;
				
				SgExpression* else_cond_list = NULL;
				SgNode* else_parent = else_if_stmt;
				
				int flag =1;				
				int basic_block_flag = 0;
				
				while(isSgIfStmt(else_parent) || isSgBasicBlock(else_parent))
				{
					if (isSgIfStmt(else_parent))
					{
						SgIfStmt* temp_else_if_stmt = isSgIfStmt(else_parent);
						other_if_cond = analyze_expression(isSgExpression(isSgExprStatement(temp_else_if_stmt->get_conditional())->get_expression()), stmt_scope);
						/*if ( isSgBinaryOp(other_if_cond) != NULL)
						 {
						 other_if_cond = analyze_binary_op(isSgBinaryOp(other_if_cond), stmt_scope);
						 }
						 else
						 {
						 other_if_cond = fix_variable(other_if_cond, stmt_scope);
						 }*/
						
						other_if_cond = buildFunctionCallExp("spu_nand", buildIntType(), buildExprListExp(other_if_cond, other_if_cond), stmt_scope);
					}
					else if (isSgBasicBlock(else_parent))
					{
						else_parent = else_parent->get_parent();
						basic_block_flag = 1;
						
						if (isSgIfStmt(else_parent))
						{
							basic_block_flag = 0;
							SgIfStmt* temp_else_if_stmt = isSgIfStmt(else_parent);
							other_if_cond = analyze_expression(isSgExpression(isSgExprStatement(temp_else_if_stmt->get_conditional())->get_expression()), stmt_scope);
							/*
							 if ( isSgBinaryOp(other_if_cond) != NULL)
							 {
							 other_if_cond = analyze_binary_op(isSgBinaryOp(other_if_cond), stmt_scope);
							 }
							 else
							 {
							 other_if_cond = fix_variable(other_if_cond, stmt_scope);
							 }*/
							other_if_cond = buildFunctionCallExp("spu_nand", buildIntType(), buildExprListExp(other_if_cond, other_if_cond), stmt_scope);
							
						}
					}   
					
					if (flag ==1)
					{					
						other_func_expr = other_if_cond;
					}
					else 
					{
						if ( basic_block_flag == 0)
						{						
							other_func_expr = buildFunctionCallExp("spu_and", buildIntType(), buildExprListExp(other_if_cond, else_cond_list),stmt_scope);
						}
					}
					++ flag;
					else_cond_list=other_func_expr;
					else_parent = else_parent->get_parent();					
				}
				
				SgExpression* cond_expr_func = else_cond_list;
				
			//	false_stmt_list = isSgScopeStatement(false_stmt)->getStatementList(); 
				
				for (Rose_STL_Container<SgStatement*> :: iterator i = false_stmt_list.begin(); i != false_stmt_list.end(); i++)
				{								
					if (isSgIfStmt(*i))
					{
						SgStatementPtrList nested_if_stmt_list = analyze_if_stmt_type(isSgIfStmt(*i), stmt_scope, 1);
						for (Rose_STL_Container<SgStatement*>::iterator k = nested_if_stmt_list.begin(); k != nested_if_stmt_list.end(); k++)
						{
							op_stmt_list.insert(op_stmt_list.end(), (*k));
						}																	   
					}
					
					else if (isSgExprStatement(*i)!=NULL)
					{
						SgExpression* expr = (isSgExprStatement(*i))->get_expression();
						
						if ( i+1 != false_stmt_list.end())
						{
							if(isSgExprStatement(*(i+1))!=NULL)
							{						
								SgExpression* next_expr = (isSgExprStatement(*(i+1)))->get_expression();
								if(isSgAssignOp(next_expr))
								{
									SgAssignOp* next_assgn_op = isSgAssignOp(next_expr);
									SgExpression* next_LHS_c = next_assgn_op->get_lhs_operand();
									if ( isSgArrayType(isSgType(next_LHS_c->get_type())) == NULL)
									{
										insert_for_stmt_flag = 1;
									}
								}
							}
							else if (isSgIfStmt(*(i+1)) != NULL)
							{
								insert_for_stmt_flag = 1;
							}							
						}
						else
						{
							insert_for_stmt_flag = 1;
						}
						
						
						if(isSgAssignOp(expr))
						{										
							SgAssignOp* assgn_op = isSgAssignOp(expr);
							SgExpression* LHS = fix_variable(assgn_op->get_lhs_operand(), stmt_scope);
							SgExpression* RHS = fix_variable(analyze_expression(assgn_op->get_rhs_operand(), stmt_scope), stmt_scope);
							
							SgExpression* LHS_c = assgn_op->get_lhs_operand();
							SgStatement* assign_stmt = buildAssignStatement(LHS,buildFunctionCallExp("spu_sel", buildIntType(), buildExprListExp(LHS, RHS, cond_expr_func),stmt_scope));
							if (isSgArrayType(isSgType(LHS_c->get_type())))
							{
								if (for_flag == 1)
								{									
									SgName for_cnt = "i";
									SgVarRefExp* for_cnt_ref = buildVarRefExp(for_cnt, stmt_scope);
									SgName for_test_cond = "count";
									SgVarRefExp* for_test_cond_ref = buildVarRefExp(for_test_cond, stmt_scope);
									SgExprStatement* for_init = buildExprStatement(buildAssignOp(for_cnt_ref, buildIntVal(0)));
									SgExprStatement* for_cond = buildExprStatement(buildLessThanOp(for_cnt_ref, for_test_cond_ref));
									SgExpression* for_cnt_incr = buildPlusPlusOp(for_cnt_ref);
									for_body = buildBasicBlock();
									
									for_stmt = new SgForStatement(stmt_scope->get_file_info(), for_cond, for_cnt_incr, for_body);
									setOneSourcePositionForTransformation(for_stmt);
									for_cond->set_parent(for_stmt);
									for_body->set_parent(for_stmt);
									
									SgForInitStatement* for_init_stmt = new SgForInitStatement(stmt_scope->get_file_info());
									for_init_stmt->append_init_stmt(for_init);			 
									for_stmt->set_for_init_stmt(for_init_stmt);
								}
								appendStatement(assign_stmt, for_body);
								if (insert_for_stmt_flag == 1)
								{
									op_stmt_list.insert(op_stmt_list.end(), for_stmt);
									insert_for_stmt_flag = 0;
								}
								
								for_flag = 0;
							}
							else
							{
								for_flag = 1;
								op_stmt_list.insert(op_stmt_list.end(), assign_stmt);
							}							
						}
					}
				}
			}
		}
	}
	
	return op_stmt_list;
}


SgStatementPtrList SPUAnalyzer::analyze_where_stmt(SgWhereStatement* where_stmt, SgScopeStatement* stmt_scope)
{
	SgExprStatement* func_callstmt = NULL;
	
	SgStatementPtrList op_stmt_list;
	
	if (where_stmt != NULL)
	{		
		SgExpression* where_cond = where_stmt->get_condition();
		SgBasicBlock* where_body = where_stmt->get_body();
		
		SgExpression* other_where_cond;
		SgExpression* other_func_expr;
		
		SgExpression* ew_cond_list = NULL;
		SgNode* ew_parent = where_stmt->get_parent();
		
		int flag =1;
		int cond_flag = 0;
		int unmasked_elsewhere_cond_flag = 0;
		
		if ( isSgBinaryOp(where_cond) != NULL)
		{
			where_cond = analyze_binary_op(isSgBinaryOp(where_cond), stmt_scope);
		}
		else
		{
			where_cond = fix_variable(where_cond, stmt_scope);
		}
		
		
		while(isSgWhereStatement(ew_parent) || isSgBasicBlock(ew_parent) ||isSgElseWhereStatement(ew_parent))
		{
			if (isSgWhereStatement(ew_parent) || isSgElseWhereStatement(ew_parent))
			{
				if(isSgWhereStatement(ew_parent))
				{
					other_where_cond = (isSgWhereStatement(ew_parent))->get_condition();
					if ( isSgBinaryOp(other_where_cond) != NULL)
					{
						other_where_cond = analyze_binary_op(isSgBinaryOp(other_where_cond), stmt_scope);
					}
					else
					{
						other_where_cond = fix_variable(other_where_cond, stmt_scope);
					}
					
					
					if (cond_flag == 1)
					{
						other_where_cond = buildFunctionCallExp("spu_nand", buildIntType(), buildExprListExp(other_where_cond, other_where_cond), stmt_scope);
					}
					cond_flag = 0;		
					unmasked_elsewhere_cond_flag++;
				}
				else
				{					
					if (isSgNullExpression((isSgElseWhereStatement(ew_parent))->get_condition()) == NULL)
					{
						other_where_cond = (isSgElseWhereStatement(ew_parent))->get_condition();
						if ( isSgBinaryOp(other_where_cond) != NULL)
						{
							other_where_cond = analyze_binary_op(isSgBinaryOp(other_where_cond), stmt_scope);
						}
						else
						{
							other_where_cond = fix_variable(other_where_cond, stmt_scope);
						}
					
						if (cond_flag == 1)
						{
							other_where_cond = buildFunctionCallExp("spu_nand", buildIntType(), buildExprListExp(other_where_cond, other_where_cond), stmt_scope);
						}
						unmasked_elsewhere_cond_flag++;
					}
					else
					{
						other_where_cond = NULL;
						unmasked_elsewhere_cond_flag = 1;
					}
					cond_flag = 1;
				}   
				
				if (flag ==1 )
				{					
					other_func_expr = other_where_cond;
				}
				else 
				{
					if (unmasked_elsewhere_cond_flag != 2)
					{
						other_func_expr = buildFunctionCallExp("spu_and", buildIntType(), buildExprListExp(other_where_cond, ew_cond_list),stmt_scope);
					}
					else
					{
						other_func_expr = other_where_cond;
					}
				}
				
				ew_cond_list=other_func_expr;
				++ flag;
			}
			ew_parent = ew_parent->get_parent();
			
		}	
		
		SgExpression* cond_expr_func;
		
		if ( ew_cond_list != NULL)
		{
			cond_expr_func = buildFunctionCallExp("spu_and", buildIntType(), buildExprListExp(where_cond, ew_cond_list),stmt_scope);
		}
		else
		{
			cond_expr_func = where_cond; 
		}
		
		Rose_STL_Container<SgStatement*> where_body_stmt_list = where_body->get_statements();
		
		int cnt = 0;
		
		for (Rose_STL_Container<SgStatement*>::iterator j = where_body_stmt_list.begin();j!=where_body_stmt_list.end();j++)
		{
			if (isSgWhereStatement(*j))
			{
				SgStatementPtrList nested_where_op_stmt_list = analyze_where_stmt(isSgWhereStatement(*j), stmt_scope);
				
				for (Rose_STL_Container<SgStatement*>::iterator k = nested_where_op_stmt_list.begin(); k != nested_where_op_stmt_list.end(); k++)
				{
					op_stmt_list.insert(op_stmt_list.end(), (*k));
				}
			}
			
			else if (isSgExprStatement(*j)!=NULL)
			{
				SgExpression* expr = (isSgExprStatement(*j))->get_expression();
				
				if(isSgAssignOp(expr))
				{										
					SgAssignOp* assgn_op = isSgAssignOp(expr);
					
					SgExpression* LHS = fix_variable(assgn_op->get_lhs_operand(), stmt_scope);
					SgExpression* RHS = assgn_op->get_rhs_operand();
					
					if (isSgBinaryOp(RHS))
					{
						RHS = analyze_binary_op(isSgBinaryOp(RHS), stmt_scope);
					}
					else
					{
						RHS = fix_variable(RHS, stmt_scope);
					}
					
					SgFunctionCallExp* func_call_expr = buildFunctionCallExp("spu_sel", buildIntType(), buildExprListExp(LHS, RHS, cond_expr_func), stmt_scope);
					
					SgExprStatement* func_callstmt = buildAssignStatement(LHS,func_call_expr);
					
					op_stmt_list.insert(op_stmt_list.end(), func_callstmt);
				}
			}	
		}
		
		SgElseWhereStatement* elsewhere_stmt = where_stmt->get_elsewhere();
		
		if(elsewhere_stmt != NULL)
		{
			SgStatementPtrList elsewhere_op_stmt_list = analyze_elsewhere_stmt(elsewhere_stmt, stmt_scope);
			
			for (Rose_STL_Container<SgStatement*>::iterator k = elsewhere_op_stmt_list.begin(); k != elsewhere_op_stmt_list.end(); k++)
			{
				op_stmt_list.insert(op_stmt_list.end(), (*k));
			}
		}
	}
	
	return op_stmt_list;
}

SgStatementPtrList SPUAnalyzer::analyze_elsewhere_stmt(SgElseWhereStatement* elsewhere_stmt, SgScopeStatement* stmt_scope)
{
	SgExprStatement* func_callstmt = NULL;
	
	SgStatementPtrList op_stmt_list;
	
	if (elsewhere_stmt != NULL)
	{		
		SgExpression* elsewhere_cond = elsewhere_stmt->get_condition();
		SgBasicBlock* elsewhere_body = elsewhere_stmt->get_body();
		
		SgExpression* other_where_cond;
		SgExpression* other_func_expr;
		
		SgExpression* ew_cond_list = NULL;
		SgNode* ew_parent = elsewhere_stmt->get_parent();
		int flag =1;
		int cond_flag = 1;
		int unmasked_elsewhere_flag = 0;
		int unmasked_elsewhere_cond_flag = 0;
		
		if ( isSgBinaryOp(elsewhere_cond) != NULL)
		{
			elsewhere_cond = analyze_binary_op(isSgBinaryOp(elsewhere_cond), stmt_scope);
		}
		else
		{
			elsewhere_cond = fix_variable(elsewhere_cond, stmt_scope);
		}
		
		if (isSgNullExpression(elsewhere_cond))
		{
			unmasked_elsewhere_flag = 1;			
		}
		
		while(isSgWhereStatement(ew_parent) || isSgBasicBlock(ew_parent) ||isSgElseWhereStatement(ew_parent))
		{
			if (isSgWhereStatement(ew_parent) || isSgElseWhereStatement(ew_parent))
			{
				if(isSgWhereStatement(ew_parent))
				{
					other_where_cond = (isSgWhereStatement(ew_parent))->get_condition();
					if ( isSgBinaryOp(other_where_cond) != NULL)
					{
						other_where_cond = analyze_binary_op(isSgBinaryOp(other_where_cond), stmt_scope);
					}
					else
					{
						other_where_cond = fix_variable(other_where_cond, stmt_scope);
					}
					
					if (cond_flag == 1 && unmasked_elsewhere_flag == 1)
					{						
						elsewhere_cond = buildFunctionCallExp("spu_nand", buildIntType(), buildExprListExp(other_where_cond, other_where_cond), stmt_scope);
					}
					
					else if (cond_flag == 1 && unmasked_elsewhere_flag == 0)
					{
						other_where_cond = buildFunctionCallExp("spu_nand", buildIntType(), buildExprListExp(other_where_cond, other_where_cond), stmt_scope);
					}
					cond_flag = 0;		
					unmasked_elsewhere_cond_flag++;
				}
				else
				{
					if (isSgNullExpression((isSgElseWhereStatement(ew_parent))->get_condition()) == NULL)
					{
						other_where_cond = (isSgElseWhereStatement(ew_parent))->get_condition();
						if ( isSgBinaryOp(other_where_cond) != NULL)
						{
							other_where_cond = analyze_binary_op(isSgBinaryOp(other_where_cond), stmt_scope);
						}
						else
						{
							other_where_cond = fix_variable(other_where_cond, stmt_scope);
						}
					
						if (cond_flag == 1 && unmasked_elsewhere_flag == 1)
						{
							elsewhere_cond = buildFunctionCallExp("spu_nand", buildIntType(), buildExprListExp(other_where_cond, other_where_cond), stmt_scope);
						}
					
						else if (cond_flag == 1 && unmasked_elsewhere_flag == 0)
						{
							other_where_cond = buildFunctionCallExp("spu_nand", buildIntType(), buildExprListExp(other_where_cond, other_where_cond), stmt_scope);
						}
						unmasked_elsewhere_cond_flag++;
					}
					else
					{
						other_where_cond = NULL;
						unmasked_elsewhere_cond_flag = 1;
					}
					cond_flag = 1;
				}   
				
				if (flag ==1)
				{				
					other_func_expr = other_where_cond;					
				}				
				else if (flag == 2 && isSgNullExpression(elsewhere_stmt->get_condition()))
				{
					other_func_expr = other_where_cond;
				}
				else 
				{
					if (unmasked_elsewhere_cond_flag != 2)
					{
						other_func_expr = buildFunctionCallExp("spu_and", buildIntType(), buildExprListExp(other_where_cond, ew_cond_list),stmt_scope);
					}
					else
					{
						other_func_expr = other_where_cond;
					}
				}
				
				++flag;
				unmasked_elsewhere_flag = 0;
				
				ew_cond_list=other_func_expr;
			}
			ew_parent = ew_parent->get_parent();
			
		}	
		
		SgExpression* cond_expr_func;
		
		if ( ew_cond_list != NULL && (flag!=2 || (isSgNullExpression(elsewhere_stmt->get_condition()) == NULL)))
		{
			cond_expr_func = buildFunctionCallExp("spu_and", buildIntType(), buildExprListExp(elsewhere_cond, ew_cond_list),stmt_scope);
		}
		else
		{
			cond_expr_func = elsewhere_cond;
		}
		
		Rose_STL_Container<SgStatement*> elsewhere_body_stmt_list = elsewhere_body->get_statements();
		
		int cnt = 0;
		
		for (Rose_STL_Container<SgStatement*>::iterator j = elsewhere_body_stmt_list.begin();j!=elsewhere_body_stmt_list.end();j++)
		{
			if (isSgWhereStatement(*j))
			{
				SgStatementPtrList nested_where_op_stmt_list = analyze_where_stmt(isSgWhereStatement(*j), stmt_scope);
				
				for (Rose_STL_Container<SgStatement*>::iterator k = nested_where_op_stmt_list.begin(); k != nested_where_op_stmt_list.end(); k++)
				{
					op_stmt_list.insert(op_stmt_list.end(), (*k));
				}
			}
			
			else if (isSgExprStatement(*j)!=NULL)
			{
				SgExpression* expr = (isSgExprStatement(*j))->get_expression();
				
				if(isSgAssignOp(expr))
				{														
					SgAssignOp* assgn_op = isSgAssignOp(expr);
										
					SgExpression* LHS = fix_variable(assgn_op->get_lhs_operand(), stmt_scope);
					SgExpression* RHS = assgn_op->get_rhs_operand();
					
					if (isSgBinaryOp(RHS))
					{
						RHS = analyze_binary_op(isSgBinaryOp(RHS), stmt_scope);
					}
					else
					{
						RHS = fix_variable(RHS, stmt_scope);
					}
					
					SgFunctionCallExp* func_call_expr = buildFunctionCallExp("spu_sel", buildIntType(), buildExprListExp(LHS, RHS, cond_expr_func), stmt_scope);
					
					SgExprStatement* func_callstmt = buildAssignStatement(LHS,func_call_expr);
					ROSE_ASSERT(func_callstmt != NULL);
					
					op_stmt_list.insert(op_stmt_list.end(), func_callstmt);					
				}
			}	
		}
		
		SgElseWhereStatement* other_elsewhere_stmt = elsewhere_stmt->get_elsewhere();
		
		if(other_elsewhere_stmt != NULL)
		{
			SgStatementPtrList elsewhere_op_stmt_list = analyze_elsewhere_stmt(other_elsewhere_stmt, stmt_scope);
			
			for (Rose_STL_Container<SgStatement*>::iterator k = elsewhere_op_stmt_list.begin(); k != elsewhere_op_stmt_list.end(); k++)
			{
				op_stmt_list.insert(op_stmt_list.end(), (*k));
			}
		}
	}
	
	return op_stmt_list;
}

SgName SPUAnalyzer::analyze_madd_op(SgMultiplyOp* mul_op, SgAddOp* add_op)
{
	SgName madd_func_name = NULL;
	
	SgExpression* LHS = mul_op->get_lhs_operand();
	SgExpression* RHS = mul_op->get_rhs_operand();
	SgExpression* add_operand;
	SgType* mul_LHS_type = analyze_expression_type(LHS);
	SgType* mul_RHS_type = analyze_expression_type(RHS);
	SgType* add_operand_type;
	
	if (mul_op == isSgMultiplyOp(add_op->get_lhs_operand()))
	{
		add_operand = add_op->get_rhs_operand();
	}
	else if (mul_op == isSgMultiplyOp(add_op->get_rhs_operand()))
	{
		add_operand = add_op->get_lhs_operand();
	}
	else
	{		
		printf("\n ERROR: In function %s -> Unable to find operand for Add Op in MADD. \r\n", __FUNCTION__);
		ROSE_ASSERT(false);
	}
	
	add_operand_type = analyze_expression_type(add_operand);
	
	if (isSgTypeSignedShort(mul_LHS_type) && isSgTypeSignedShort(mul_RHS_type) && isSgTypeInt(add_operand_type))
	{
		madd_func_name = "spu_madd";
	}
	else if (isSgTypeFloat(mul_LHS_type) && isSgTypeFloat(mul_RHS_type) && isSgTypeFloat(add_operand_type))
	{
		madd_func_name = "spu_madd";
	}
	else if (isSgTypeDouble(mul_LHS_type) && isSgTypeDouble(mul_RHS_type) && isSgTypeDouble(add_operand_type))
	{
		madd_func_name = "spu_madd";
	}
	else if (isSgTypeInt(mul_LHS_type) && isSgTypeInt(mul_RHS_type) && isSgTypeInt(add_operand_type))
	{
		madd_func_name = "madd_i4";
	}
	else if (isSgTypeUnsignedInt(mul_LHS_type) && isSgTypeUnsignedInt(mul_RHS_type) && isSgTypeUnsignedInt(add_operand_type))
	{
		madd_func_name = "madd_u4";
	}
	else
	{
		printf("\n ERROR: In function %s -> These argument types are currently not supported for 'spu_madd'. \r\n", __FUNCTION__);
		ROSE_ASSERT(false);		
	}
	
	return madd_func_name;
}

SgName SPUAnalyzer::analyze_msub_op(SgMultiplyOp* mul_op, SgSubtractOp* sub_op)
{
	SgName msub_func_name = NULL;
	
	SgExpression* LHS = mul_op->get_lhs_operand();
	SgExpression* RHS = mul_op->get_rhs_operand();
	SgExpression* sub_operand;
	SgType* mul_LHS_type = analyze_expression_type(LHS);
	SgType* mul_RHS_type = analyze_expression_type(RHS);
	SgType* sub_operand_type;
	
	if (mul_op == isSgMultiplyOp(sub_op->get_lhs_operand()))
	{
		sub_operand = sub_op->get_rhs_operand();
	}
	else if (mul_op == isSgMultiplyOp(sub_op->get_rhs_operand()))
	{
		sub_operand = sub_op->get_lhs_operand();
	}
	else
	{		
		printf("\n ERROR: In function %s -> Unable to find operand for Sub Op in MADD. \r\n", __FUNCTION__);
		ROSE_ASSERT(false);
	}
	
	sub_operand_type = analyze_expression_type(sub_operand);
	
	if (isSgTypeSignedShort(mul_LHS_type) && isSgTypeSignedShort(mul_RHS_type) && isSgTypeInt(sub_operand_type))
	{
		msub_func_name = "spu_msub";
	}
	else if (isSgTypeFloat(mul_LHS_type) && isSgTypeFloat(mul_RHS_type) && isSgTypeFloat(sub_operand_type))
	{
		msub_func_name = "spu_msub";
	}
	else if (isSgTypeDouble(mul_LHS_type) && isSgTypeDouble(mul_RHS_type) && isSgTypeDouble(sub_operand_type))
	{
		msub_func_name = "spu_msub";
	}
	else if (isSgTypeInt(mul_LHS_type) && isSgTypeInt(mul_RHS_type) && isSgTypeInt(sub_operand_type))
	{
		msub_func_name = "msub_i4";
	}
	else if (isSgTypeUnsignedInt(mul_LHS_type) && isSgTypeUnsignedInt(mul_RHS_type) && isSgTypeUnsignedInt(sub_operand_type))
	{
		msub_func_name = "msub_u4";
	}
	else
	{
		printf("\n ERROR: In function %s -> These argument types are currently not supported for 'spu_msub'. \r\n", __FUNCTION__);
		ROSE_ASSERT(false);		
	}
	
	return msub_func_name;
}

SgName SPUAnalyzer::analyze_multiply_op(SgMultiplyOp* mul_op)
{
	SgName mul_func_name = NULL;
	
	SgExpression* LHS = mul_op->get_lhs_operand();
	SgExpression* RHS = mul_op->get_rhs_operand();
	SgType* mul_LHS_type = analyze_expression_type(LHS);
	SgType* mul_RHS_type = analyze_expression_type(RHS);
	
	if (isSgTypeFloat(mul_LHS_type) && isSgTypeFloat(mul_RHS_type))
	{
		mul_func_name = "spu_mul";
	}
	else if (isSgTypeDouble(mul_LHS_type) && isSgTypeDouble(mul_RHS_type))
	{
		mul_func_name = "spu_mul";
	}
	else if (isSgTypeInt(mul_LHS_type) && isSgTypeInt(mul_RHS_type))
	{
		printf("\n WARNING: In function %s -> The integer multiply 'mul_i4' converts the INTEGER arguments to DOUBLE and uses 'spu_mul'.", __FUNCTION__);
		mul_func_name = "mul_i4";
	}
	else if (isSgTypeUnsignedInt(mul_LHS_type) && isSgTypeUnsignedInt(mul_RHS_type))
	{
		printf("\n WARNING: In function %s -> The unsigned integer multiply 'mul_u4' converts the UNSIGNED INTEGER arguments to DOUBLE and uses 'spu_mul'.", __FUNCTION__);
		mul_func_name = "mul_u4";
	}
	else
	{
		printf("\n ERROR: In function %s -> These argument types are currently not supported for 'spu_mul'. \r\n", __FUNCTION__);
		ROSE_ASSERT(false);	
	}

	return mul_func_name;
}


SgName SPUAnalyzer::analyze_divide_op(SgDivideOp* div_op)
{
	SgName div_func_name = NULL;
	
	SgExpression* LHS = div_op->get_lhs_operand();
	SgType* div_operand_type = analyze_expression_type(LHS);
	
	if (div_operand_type != NULL)
	{
		if (isSgTypeFloat(div_operand_type))
		{
			div_func_name = "divf4";
		}
		else if (isSgTypeDouble(div_operand_type))
		{
			div_func_name = "divd2";
		}
		else if (isSgTypeInt(div_operand_type))
		{
			div_func_name = "div_i4";
		}
		else if (isSgTypeLongLong(div_operand_type))
		{
			div_func_name = "lldiv_i2";
		}
		else if (isSgTypeUnsignedInt(div_operand_type))
		{
			div_func_name = "div_u4";
		}
		else if (isSgTypeUnsignedLongLong(div_operand_type))
		{
			div_func_name = "lldiv_u2";
		}
		else
		{
			printf("\n ERROR: Could not find a SIMD divide function for the operands type used \r\n");
			ROSE_ASSERT(false);
		}
	}	
		
	return div_func_name;
}

SgName SPUAnalyzer::analyze_exponentiation_op(SgExponentiationOp* exp_op)
{
	SgName exp_func_name = NULL;
	
	SgExpression* LHS = exp_op->get_lhs_operand();
	SgExpression* RHS = exp_op->get_rhs_operand();
	SgType* exp_LHS_type = analyze_expression_type(LHS);
	SgType* exp_RHS_type = analyze_expression_type(RHS);
	
	int LHS_value = 0; 
	
	if (isSgValueExp(LHS))
	{
		SgValueExp* LHS_value_exp = isSgValueExp(LHS);
		if (isSgIntVal(LHS_value_exp))
		{
			LHS_value = isSgIntVal(LHS_value_exp)->get_value();
		}
		else if (isSgUnsignedIntVal(LHS_value_exp))
		{
			LHS_value = isSgUnsignedIntVal(LHS_value_exp)->get_value();
		}
		else if (isSgUnsignedShortVal(LHS_value_exp))
		{
			LHS_value = isSgUnsignedShortVal(LHS_value_exp)->get_value();
		}
		else if (isSgShortVal(LHS_value_exp))
		{
			LHS_value = isSgShortVal(LHS_value_exp)->get_value();
		}		
	}	
		
	if ( (LHS_value == 2) && ((isSgTypeUnsignedShort(exp_RHS_type)) || (isSgTypeUnsignedInt(exp_RHS_type))))
	{
		exp_func_name = "spu_sl";
	}
	else if (isSgTypeInt(exp_LHS_type) && isSgTypeInt(exp_RHS_type))
	{
		exp_func_name = "pow_i4";
	}
	else if (isSgTypeFloat(exp_LHS_type) && isSgTypeFloat(exp_RHS_type))
	{
		exp_func_name = "powf4";
	}
	else if (isSgTypeDouble(exp_LHS_type) && isSgTypeDouble(exp_RHS_type))
	{
		exp_func_name = "powd2";
	}
	else
	{
		printf("\n ERROR: Could not find a SIMD exponentiation function for the operands type used \r\n");
		ROSE_ASSERT(false);
	}
	
	return exp_func_name;
}

SgType* SPUAnalyzer::analyze_expression_type (SgExpression* originalExpr)
{
	SgType* expr_type = isSgType(originalExpr->get_type());
	SgType* expr_base_type = NULL;
	
	if (expr_type != NULL)
	{
		if (isSgQualifiedNameType(expr_type))
		{
			expr_base_type = isSgQualifiedNameType(expr_type)->get_base_type();
		}		
		else if (isSgArrayType(expr_type))
		{
			expr_base_type = isSgArrayType(expr_type)->get_base_type();
		}		
		else if (isSgModifierType(expr_type))
		{
			expr_base_type = isSgModifierType(expr_type)->get_base_type();
		}		
		else if (isSgTypedefType(expr_type))
		{
			expr_base_type = isSgTypedefType(expr_type)->get_base_type();
		}		
		else if (isSgReferenceType(expr_type))
		{
			expr_base_type = isSgReferenceType(expr_type)->get_base_type();
		}		
		else if (isSgPointerType(expr_type))
		{
			expr_base_type = isSgPointerType(expr_type)->get_base_type();
		}
		
		if (expr_base_type != NULL)
		{
			expr_type = expr_base_type;
		}
	}
	else
	{
		printf("\n ERROR: Could not find a valid TYPE for the expression \r\n");
		ROSE_ASSERT(false);
	}
	
	return expr_type;	
}

SgExpression* SPUAnalyzer::analyze_function_call_expr(SgFunctionCallExp* func_call_expr, SgScopeStatement* expr_scope)
{
	SgExpression* new_func_call_expr = NULL;
	
	SgExprListExp* arg_exprlist = func_call_expr->get_args();
	SgExpressionPtrList arg_list = arg_exprlist->get_expressions();
	SgFunctionRefExp* func_ref = isSgFunctionRefExp(func_call_expr->get_function());
	SgFunctionDeclaration* func_decl = (func_ref->get_symbol())->get_declaration();
	SgType* func_arg_type = analyze_expression_type(arg_list.front());
	ROSE_ASSERT(func_arg_type != NULL);
	SgName new_func_name;
	SgType* ret_type = func_decl->get_orig_return_type();
	SgExprListExp* new_arg_exprlist = buildExprListExp();
	char* func_name = (char *)malloc(25*sizeof(char));
	strcpy(func_name, (func_ref->get_symbol())->get_name().str());
	
	//printf("In function %s: the function to be replaced is %s", __FUNCTION__, func_name);
	
	for (Rose_STL_Container<SgExpression*> :: iterator i = arg_list.begin(); i != arg_list.end(); i++)
	{
		appendExpression(new_arg_exprlist, fix_variable(analyze_expression(isSgExpression(*i), expr_scope), expr_scope));
	}

	if ((strcmp(func_name, "int") == 0) || (strcmp(func_name, "INT") == 0))
	{
		if (isSgTypeFloat(func_arg_type))
		{
			new_func_name = "spu_convts";
			appendExpression(new_arg_exprlist, buildUnsignedIntVal(0));
		}
		else
		{
			printf("\n ERROR: Invalid conversion to VEC_INT4 using 'spu_convts' \r\n");
			ROSE_ASSERT(false);
		}
	}
	else if ((strcmp(func_name, "real") == 0) || (strcmp(func_name, "REAL") == 0))
	{
		if (isSgTypeInt(func_arg_type) || (isSgTypeUnsignedInt(func_arg_type)))
		{
			new_func_name = "spu_convtf";	
			appendExpression(new_arg_exprlist, buildUnsignedIntVal(0));
		}
		else
		{
			printf("\n ERROR: Invalid conversion to VEC_FLOAT4 using 'spu_convtf' \r\n");
			ROSE_ASSERT(false);
		}
	}
	else if (strcmp(func_name, "eoshift") == 0 || strcmp(func_name, "EOSHIFT") == 0)
	{
		new_func_name = "spu_shuffle";
		new_arg_exprlist = buildExprListExp();
		char V[25] = "v";
		char temp_arg[25];
		int cnt = 0;
		for (Rose_STL_Container<SgExpression*> :: iterator j = arg_list.begin(); j != arg_list.end(); j++)
		{
			cnt ++;
			if (cnt <= 2)
			{
				if (isSgVarRefExp(*j) && j == arg_list.begin())
				{
					strcpy(temp_arg, strcat(V, ((isSgVarRefExp(*j)->get_symbol())->get_name().str())));
					appendExpression(new_arg_exprlist, buildVarRefExp(strcat(V, "m1"), expr_scope));
					appendExpression(new_arg_exprlist, buildVarRefExp(temp_arg, expr_scope));
				}
				else
				{
					if (isSgMinusOp(*j))
					{
						SgExpression* val_exp = isSgMinusOp(*j)->get_operand_i();
						if(isSgIntVal(val_exp))
						{
							if (isSgIntVal(val_exp)->get_value() == 1)
							{
								appendExpression(new_arg_exprlist, buildVarRefExp( "VSHIFT_RIGHT", expr_scope));
							}
							else
							{
								printf("\n ERROR: In function %s -> This type of SHIFT is currently not supported \r\n", __FUNCTION__);
								ROSE_ASSERT(false);
							}					
						}
						else if (isSgFloatVal(val_exp))
						{
							if (isSgFloatVal(val_exp)->get_value() == 1)
							{
								appendExpression(new_arg_exprlist, buildVarRefExp( "VSHIFT_RIGHT", expr_scope));
							}
							else
							{
								printf("\n ERROR: In function %s -> This type of SHIFT is currently not supported \r\n", __FUNCTION__);
								ROSE_ASSERT(false);
							}					
						}
					}
					else
					{
						SgExpression* val_exp = (*j);
						if(isSgIntVal(val_exp))
						{
							if (isSgIntVal(val_exp)->get_value() == 1)
							{
								appendExpression(new_arg_exprlist, buildVarRefExp( "VSHIFT_LEFT", expr_scope));
							}
							else
							{
								printf("\n ERROR: In function %s -> This type of SHIFT is currently not supported \r\n", __FUNCTION__);
								ROSE_ASSERT(false);
							}					
						}
						else if (isSgFloatVal(val_exp))
						{
							if (isSgFloatVal(val_exp)->get_value() == 1)
							{
								appendExpression(new_arg_exprlist, buildVarRefExp( "VSHIFT_LEFT", expr_scope));
							}
							else
							{
								printf("\n ERROR: In function %s -> This type of SHIFT is currently not supported \r\n", __FUNCTION__);
								ROSE_ASSERT(false);
							}					
						}
					}
				}	
			}
		}
	}	
	else if(strcmp(func_name, "kind") == 0 || strcmp(func_name, "KIND") == 0)
	{
		SgIntVal* int_val = NULL;
		if(isSgTypeInt(func_arg_type))
		{
			int_val = buildIntVal(4);
		}
		else if(isSgTypeFloat(func_arg_type))
		{
			int_val = buildIntVal(4);
		}
		else if(isSgTypeDouble(func_arg_type))
		{
			int_val = buildIntVal(8);
		}
		else if(isSgTypeShort(func_arg_type))
		{
			int_val = buildIntVal(2);
		}
		else if(isSgTypeBool(func_arg_type))
		{
			int_val = buildIntVal(1);
		}
		else if(isSgTypeChar(func_arg_type))
		{
			int_val = buildIntVal(1);
		}
		else
		{
			printf("\n ERROR: In function %s - Invalid Parameter for KIND function", __FUNCTION__);
			ROSE_ASSERT(false);
		}
		
		ROSE_ASSERT(int_val != NULL);
		return int_val;
	}	
	else if (isSgTypeInt(func_arg_type))
	{
		if ((strcmp(func_name, "mod") == 0) || (strcmp(func_name, "MOD") == 0) || (strcmp(func_name, "modulo") == 0) || (strcmp(func_name, "MODULO") == 0))
		{
			new_func_name = "mod_i4";
		}
		else
		{
			new_func_name = function_lookup_table.Search_table(func_name, 0);
		}		
	}
	else if (isSgTypeFloat(func_arg_type))
	{
		new_func_name =  function_lookup_table.Search_table(func_name, 1);
	}	
	else if (isSgTypeDouble(func_arg_type))
	{
		new_func_name = function_lookup_table.Search_table(func_name, 2);
	}
	else if (isSgTypeUnsignedInt(func_arg_type))
	{		
		if ((strcmp(func_name, "mod") == 0) || (strcmp(func_name, "MOD") == 0) || (strcmp(func_name, "modulo") == 0) || (strcmp(func_name, "MODULO") == 0))
		{
			new_func_name = "mod_u4";
		}
	}
	else if (isSgTypeLongLong(func_arg_type))
	{		
		if ((strcmp(func_name, "mod") == 0) || (strcmp(func_name, "MOD") == 0) || (strcmp(func_name, "modulo") == 0) || (strcmp(func_name, "MODULO") == 0))
		{
			new_func_name = "llmod_i2";
		}
	}
	else if (isSgTypeUnsignedLongLong(func_arg_type))
	{		
		if ((strcmp(func_name, "mod") == 0) || (strcmp(func_name, "MOD") == 0) || (strcmp(func_name, "modulo") == 0) || (strcmp(func_name, "MODULO") == 0))
		{
			new_func_name = "llmod_u2";
		}
	}
	else
	{
		//printf("\n In function %s: Could not find an alternate function call, returning original function call", __FUNCTION__);
		new_func_name = func_name;
	}
			
	if (strcmp(new_func_name.str(), "") == 0)
	{	
		//printf("\n In function %s: Could not find an alternate function call, returning original function call", __FUNCTION__);
		new_func_name = func_name;
	}		
	
	new_func_call_expr = buildFunctionCallExp(new_func_name, ret_type, new_arg_exprlist, expr_scope);
	
	ROSE_ASSERT(new_func_call_expr != NULL);
	
	return new_func_call_expr;	
}

SgExpression* SPUAnalyzer::analyze_expression(SgExpression* originalExpr, SgScopeStatement* expr_scope)
{
	SgExpression* expr = NULL;
		
	SgValueExp* value_expr = isSgValueExp(originalExpr);
	SgAssignOp* assgn_expr = isSgAssignOp(originalExpr);
	SgUnaryOp* unary_expr = isSgUnaryOp(originalExpr);
	SgBinaryOp* binary_expr = isSgBinaryOp(originalExpr);
	SgFunctionCallExp* func_call_expr = isSgFunctionCallExp(originalExpr);
	SgVarRefExp* varref_expr = isSgVarRefExp(originalExpr);
	
	if (value_expr != NULL)
	{
		//printf("\n In function %s: Found Value Exp", __FUNCTION__);
		expr = analyze_value_expression(value_expr, expr_scope);
	}	
	else if (assgn_expr !=NULL)
	{
		//printf("\n In function %s: Found Assignment Exp", __FUNCTION__);
		expr =  analyze_assign_expression(assgn_expr, expr_scope);

	}
	else if(unary_expr != NULL)
	{
		//printf("\n In function %s: Found Unary Exp", __FUNCTION__);
		expr =  analyze_unary_op(unary_expr, expr_scope);
	}	
	else if(binary_expr !=NULL)
	{
		//printf("\n In function %s: Found Binary Exp", __FUNCTION__);
		expr = analyze_binary_op(binary_expr, expr_scope);
	}
	else if ( func_call_expr != NULL)
	{
		//printf("\n In function %s: Found Function Call Exp", __FUNCTION__);
		expr = analyze_function_call_expr(func_call_expr, expr_scope);
	}	
	else if (varref_expr != NULL)
	{
		//printf("\n In function %s: Found VarRef Exp", __FUNCTION__);
		expr = fix_variable(varref_expr, expr_scope);
	}	
	else
	{
		//printf("\n In function %s: Could not recognize the type of expression -- Returning unchanged", __FUNCTION__);
		expr = originalExpr;
	}

	ROSE_ASSERT(expr!=NULL);
	
	return expr;	
}

SgExpression* SPUAnalyzer::analyze_assign_expression(SgExpression* assign_expr, SgScopeStatement* stmt_scope)
{
	SgAssignOp* assgn_op = isSgAssignOp(assign_expr);
	SgAssignOp* expr = NULL;
		
	if (assgn_op != NULL)
	{
		SgExpression* RHS = assgn_op->get_rhs_operand();
		SgExpression* LHS = fix_variable(assgn_op->get_lhs_operand(), stmt_scope);
		
		SgExpression* func_callexp = analyze_expression(RHS, stmt_scope);
		ROSE_ASSERT(func_callexp != NULL);
		
		expr = buildAssignOp(LHS, func_callexp);		
	}
	else
	{
		printf("\n ERROR: In function %s -> Not an Assignment Expression \r\n", __FUNCTION__);
		ROSE_ASSERT(false);
	}
	ROSE_ASSERT(expr != NULL);
	return expr;
}

SgExpression* SPUAnalyzer::analyze_value_expression(SgValueExp* value_expr, SgScopeStatement* expr_scope)
{
	SgExpression* expr = NULL;
	SgType* ret_type = analyze_expression_type(value_expr);
	SgExprListExp* arg_list = buildExprListExp();
	if (isSgTypeDouble(ret_type))
	{
		ret_type = buildFloatType();
		appendExpression(arg_list, buildCastExp(value_expr, ret_type));
	}
	else if (isSgTypeFloat(ret_type))
	{		
		appendExpression(arg_list, buildCastExp(value_expr, ret_type));
	}
	else
	{
		appendExpression(arg_list, value_expr);
	}
		
	expr = buildFunctionCallExp("spu_splats", ret_type, arg_list, expr_scope);
	
	ROSE_ASSERT(expr != NULL);
	return expr;	
}

SgExpression* SPUAnalyzer::analyze_expression_for_binary_op(SgExpression* originalExpr, SgScopeStatement* expr_scope)
{
	SgExpression* expr = NULL;
		
	SgValueExp* value_expr = isSgValueExp(originalExpr);
	SgAssignOp* assgn_expr = isSgAssignOp(originalExpr);
	SgUnaryOp* unary_expr = isSgUnaryOp(originalExpr);
	SgFunctionCallExp* func_call_expr = isSgFunctionCallExp(originalExpr);
	SgVarRefExp* varref_expr = isSgVarRefExp(originalExpr);
	
	if (value_expr != NULL)
	{
		expr = analyze_value_expression(value_expr, expr_scope);
	}
	else if(unary_expr != NULL)
	{
		expr =  analyze_unary_op(unary_expr, expr_scope);
	}	
	else if ( func_call_expr != NULL)
	{
		expr = analyze_function_call_expr(func_call_expr, expr_scope);
	}
	else if (varref_expr != NULL)
	{
		expr = fix_variable(varref_expr, expr_scope);
	}	
	else
	{
		expr = originalExpr;
	}
	
	ROSE_ASSERT(expr!=NULL);
	
	return expr;	
}


