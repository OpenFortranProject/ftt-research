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
 * SPUBuilder.h
 *
 *  Created on: Aug 7, 2008
 *      
 *  Author: rasmussn, Tanveer
 */

#ifndef SPUBUILDER_H_
#define SPUBUILDER_H_

#include "FTTBuilder.h"

class SPUBuilder : public FTTBuilder
{
public:
	SPUBuilder();
	virtual ~SPUBuilder();

	virtual SgExpression* build_binary_expr(SgBinaryOp* originalExpr, SgScopeStatement* expr_scope);
	
	virtual SgExpression* build_unary_expr(SgUnaryOp* originalExpr, SgScopeStatement* expr_scope);
	
	virtual SgStatementPtrList build_select_stmt(SgStatement* originalStmt, SgScopeStatement* stmt_scope);
	
	virtual SgExpression* build_alternate_expression(SgExpression* originalExpr, SgScopeStatement* expr_scope);

	virtual SgStatement* build_assignment_statement(SgExpression* assign_expr, SgScopeStatement* stmt_scope);

};

#endif /* SPUBUILDER_H_ */
