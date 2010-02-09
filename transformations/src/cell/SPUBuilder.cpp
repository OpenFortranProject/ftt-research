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
 * SPUBuilder.cpp
 *
 *  Created on: Aug 7, 2008
 *      Author: rasmussn, Tanveer Pathan
 */

#include "SPUBuilder.h"
#include "SPUAnalyzer.h"
using namespace SageBuilder;
using namespace SageInterface;

SPUBuilder::SPUBuilder()
{
	// TODO Auto-generated constructor stub

}

SPUBuilder::~SPUBuilder()
{
	// TODO Auto-generated destructor stub
}

SgExpression* SPUBuilder::build_binary_expr(SgBinaryOp *originalExpr, SgScopeStatement* expr_scope)
{
	SgExpression* expr = NULL;
	
	if (isSgBinaryOp(originalExpr))
	{
		SPUAnalyzer analysis;
		expr = analysis.analyze_binary_op(originalExpr, expr_scope);
	}
	return expr;
}


SgExpression* SPUBuilder::build_unary_expr(SgUnaryOp* originalExpr, SgScopeStatement* expr_scope)
{
	SgExpression* expr = NULL;
	
	if (isSgUnaryOp(originalExpr))
	{
		SPUAnalyzer analysis;
		expr = analysis.analyze_unary_op(originalExpr, expr_scope);
	}
	return expr;
}
		

SgStatementPtrList SPUBuilder::build_select_stmt(SgStatement* originalStmt, SgScopeStatement* stmt_scope)
{
	SgIfStmt* if_stmt = isSgIfStmt(originalStmt);
	SgWhereStatement* where_stmt = isSgWhereStatement(originalStmt);
		
	SPUAnalyzer analysis;
	SgStatementPtrList op_stmt_list;
	
	if (if_stmt != NULL)
	{		
		op_stmt_list = analysis.analyze_if_stmt_type(if_stmt, stmt_scope, 0);		
	}
	
	else if (where_stmt != NULL)
	{
		op_stmt_list = analysis.analyze_where_stmt(where_stmt, stmt_scope);
	}
			
	return op_stmt_list;
}

SgExpression* SPUBuilder::build_alternate_expression(SgExpression* originalExpr, SgScopeStatement* expr_scope)
{
	SgExpression* expr = NULL;
	SPUAnalyzer analysis;
	
	expr = analysis.analyze_expression(originalExpr, expr_scope);
	return expr;
}

SgStatement* SPUBuilder::build_assignment_statement(SgExpression* assign_expr, SgScopeStatement* stmt_scope)
{
	SgAssignOp* assgn_op = isSgAssignOp(assign_expr);
	SgStatement* expr = NULL;
	SPUAnalyzer analysis;
	if (assgn_op != NULL)
	{
		SgExpression* RHS = assgn_op->get_rhs_operand();
		SgExpression* LHS = analysis.fix_variable(assgn_op->get_lhs_operand(), stmt_scope);
		
		SgExpression* func_callexp = analysis.analyze_expression(RHS, stmt_scope);
		ROSE_ASSERT(func_callexp != NULL);
		
		expr = buildAssignStatement(LHS, func_callexp);		
	}
	else
	{
		printf("\n ERROR: In function %s -> Not an Assignment Expression \r\n", __FUNCTION__);
		ROSE_ASSERT(false);
	}
	ROSE_ASSERT(expr != NULL);
	return expr;
}

