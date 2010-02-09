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

int main(int argc, char* argv[])
{
  char* opf_name;
  opf_name=(char *)malloc(25* sizeof(char));
  strcpy(opf_name,argv[1]);
  opf_name[strlen(opf_name)-4] ='\0';
  strcat(opf_name, "_gpu.c");

  writeFile("",opf_name,"./");  	

  char **fname;
  fname = (char **)malloc(2 * sizeof(char *));
  fname[0] = (char *)malloc(25 * sizeof(char));
  fname[1] = (char *)malloc(25 * sizeof(char));
  strcpy(fname[1], opf_name);
  strcpy(fname[0], argv[0]);
	
  SgProject* project_ip = frontend(argc, argv);
  ROSE_ASSERT(project_ip != NULL);
  
  SgProject* project_op = frontend(2, fname);
  ROSE_ASSERT(project_op != NULL);
 
  GPUProgramTranslator project ;
  project.input_project = project_ip;
	
  printf("\nStarting Translation for GPU ... \r\n\n");
  project.traverseInputFiles(project_op, postorder);
  printf("\n... End of Translation for GPU. \r\n");

  unparseProject(project_op);

  return 0;
}
