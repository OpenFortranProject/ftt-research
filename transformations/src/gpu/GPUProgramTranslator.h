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

#ifndef GPUPROGRAMTRANSLATOR_H_
#define GPUPROGRAMTRANSLATOR_H_

#include <rose.h>
#include <stdio.h>
#include <stdlib.h>
#include "GPUProgramAnalyzer.h"

#define FORTRAN 1
#define C_LANGUAGE 2

using namespace SageBuilder;
using namespace SageInterface;
using namespace StringUtility;

class GPUProgramTranslator : public SgSimpleProcessing
{
	public : GPUProgramTranslator();
		~GPUProgramTranslator();
			
	SgProject* input_project;
	
	GPUProgramAnalyzer GPU_program_analyzer;
	
	void visit(SgNode* astNode);
	void add_gpu_typedefs(SgGlobal* global_scope);
	void add_gpu_headers(SgGlobal* global_scope);
	int determine_input_program_language();
	void handle_fortran_program(SgGlobal* global_scope);
	void handle_c_program(SgGlobal* global_scope);

	    
};

#endif /* GPUPROGRAMTRANSLATOR_H_ */
