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
#include "SPUProgramTranslator.hpp"
#include <string.h>

#define MAX_PATH_LEN 132

int main(int argc, char * argv[])
{
   int len, ext_len;
   char new_name[MAX_PATH_LEN];
   char * old_name = argv[1];

   if (argc < 2) {
      printf("usage gpu_trans file_name\n");
      exit(1);
   }

   ext_len = (int) (strrchr(argv[1], '.') - argv[1]);
   len = strlen(argv[1]);
   ROSE_ASSERT(MAX_PATH_LEN > len);

   // get new file name
   //
   strncpy(new_name, argv[1], len);
   new_name[len-ext_len] ='\0';
   strcat(new_name, "_spu.c");

   printf("new file is %s, '.' position is %d, len = %d\n", new_name, ext_len, len);

   writeFile("", new_name, "./");  	

   // create ROSE project for initial file
   //
   SgProject * project_ip = frontend(argc, argv);
   ROSE_ASSERT(project_ip != NULL);

   // let's look at the original
   unparseProject(project_ip);


   //  char **fname;
   //  fname = (char **)malloc(2 * sizeof(char *));
   //  fname[0] = (char *)malloc(25 * sizeof(char));
   //  fname[1] = (char *)malloc(25 * sizeof(char));
   //  strcpy(fname[1], new_name);
   //  strcpy(fname[0], argv[0]);
	
   argv[1] = new_name;
   SgProject * project_op = frontend(2, argv);
   ROSE_ASSERT(project_op != NULL);
 
   SPUProgramTranslator project ;
   project.input_prj = project_ip;
   project.traverseInputFiles(project_op, postorder);

   unparseProject(project_op);

   return 0;
}
