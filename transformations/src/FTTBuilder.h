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
 * FTTBuilder.h
 *
 *  Created on: Aug 7, 2008
 *      Author: rasmussn
 */

#ifndef FTTBUILDER_H_
#define FTTBUILDER_H_

#include "rose.h"

class FTTBuilder
{
  public: 
	FTTBuilder();
	
	virtual ~FTTBuilder();

    virtual SgExpression* build_binary_expr(SgBinaryOp* originalExpr, SgScopeStatement* expr_scope) = 0;
	
    virtual SgExpression* build_unary_expr(SgUnaryOp* originalExpr, SgScopeStatement* expr_scope) = 0;
	
    virtual SgStatementPtrList build_select_stmt(SgStatement* originalStmt, SgScopeStatement* stmt_scope) = 0;
	
	virtual SgExpression* build_alternate_expression(SgExpression* originalExpr, SgScopeStatement* expr_scope) = 0;

	virtual SgStatement* build_assignment_statement(SgExpression* assign_expr, SgScopeStatement* stmt_scope) = 0;

};

#endif /* FTTBUILDER_H_ */
