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
 *  SPUAnalyzer.h
 *  
 *  Created by Tanveer on 9/16/08.
 *
 */

#ifndef SPUANALYZER_H_
#define SPUANALYZER_H_

#include "FTTBuilder.h"

class SPUAnalyzer //: public SPUBuilder
{
	public:
		SPUAnalyzer();
		
		virtual ~SPUAnalyzer();
		
		virtual SgExpression* analyze_binary_op(SgBinaryOp* originalExpr, SgScopeStatement* expr_scope);
	
		virtual SgExpression* analyze_dual_binary_child(SgBinaryOp* expr1, SgBinaryOp* expr2, SgScopeStatement* expr_scope);
		
		virtual SgExpression* analyze_unary_op(SgUnaryOp* originalExpr, SgScopeStatement* expr_scope);
	
		virtual SgExpression* fix_variable(SgExpression* var, SgScopeStatement* var_scope);

    	virtual SgStatementPtrList analyze_simple_if_else_stmt(SgIfStmt* if_stmt, SgScopeStatement* stmt_scope, SgAssignOp* true_assgn_op, SgAssignOp* false_assgn_op);

		virtual SgStatementPtrList analyze_if_stmt_type(SgIfStmt* if_stmt, SgScopeStatement* stmt_scope, int if_type);
		
		virtual SgStatementPtrList analyze_if_stmt(SgIfStmt* originalStmt, SgScopeStatement* stmt_scope);
	
	    virtual SgStatementPtrList analyze_else_if_stmt(SgIfStmt* else_if_stmt, SgScopeStatement* stmt_scope);
		
		virtual SgStatementPtrList analyze_where_stmt(SgWhereStatement* where_stmt, SgScopeStatement* stmt_scope);
	
		virtual SgStatementPtrList analyze_elsewhere_stmt(SgElseWhereStatement* elsewhere_stmt, SgScopeStatement* stmt_scope);
		
    	virtual SgName analyze_madd_op(SgMultiplyOp* mul_op, SgAddOp* add_op);
	
        virtual SgName analyze_msub_op(SgMultiplyOp* mul_op, SgSubtractOp* sub_op);
	
    	virtual SgName analyze_multiply_op(SgMultiplyOp* mul_op);
	
		virtual SgName analyze_divide_op(SgDivideOp* div_op);
	
    	virtual SgName analyze_exponentiation_op(SgExponentiationOp* exp_op);
	
		virtual SgType* analyze_expression_type(SgExpression* originalExpr);	
	
     	virtual SgExpression* analyze_function_call_expr(SgFunctionCallExp* func_call_expr, SgScopeStatement* expr_scope);
						
		virtual SgExpression* analyze_expression(SgExpression* originalExpr, SgScopeStatement* expr_scope);
	
		virtual SgExpression* analyze_assign_expression(SgExpression* assign_expr, SgScopeStatement* stmt_scope);
	
	    virtual SgExpression* analyze_value_expression(SgValueExp* value_expr, SgScopeStatement* expr_scope);
	
    	virtual SgExpression* analyze_expression_for_binary_op(SgExpression* originalExpr, SgScopeStatement* expr_scope);
 
};

#endif /* SPUANALYZER_H_ */
