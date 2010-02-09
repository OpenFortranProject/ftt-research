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

#ifndef GPUPROGRAMANALYZER_H_
#define GPUPROGRAMANALYZER_H_

#include "rose.h"
#include <string>
using namespace std;


struct functionWithAttributes
{
	SgFunctionDeclaration* functionDeclaration;
	bool elementalFunction;
	bool normalFuncntion;
	bool GPU_kernel;
};

typedef struct functionWithAttributes* functionWithAttributesPTR;

class GPUProgramAnalyzer
{
	public : GPUProgramAnalyzer();
		~GPUProgramAnalyzer();
		
	functionWithAttributesPTR listOfHostFunctions;
	functionWithAttributesPTR listOfGlobalFunctions;
	functionWithAttributesPTR listOfDeviceFunctions;
	
	int analyze_annotations (SgProject* input_project);
	
//	int analyze_function_type(SgProject* input_prj);
	
/*	int checkToEndFor(SgStatement* stmt);
	int checkForFortranShift(SgStatement* stmt);
	int checkForArrayRefVariables(SgStatement* stmt);
*/
};

#endif /* GPUPROGRAMANALYZER_H_ */