#include "FortranAnalysis.hpp"
#include "ElementalTraversal.hpp"
#include <stdio.h>

int main(int argc, char ** argv)
{
   FortranAnalysis * analysis;
   ElementalTraversal * traversal;

   if (argc < 2) {
      printf("usage: build_elemental_cl -c -rose:skip_syntax_check elemental_add.f90 ");
      printf("-ofp:<array_list>\n");
   }      

   SgProject* project = frontend(argc, argv);
   ROSE_ASSERT(project != NULL);

   SgSourceFile * src_file = isSgSourceFile((*project)[0]);
   ROSE_ASSERT(src_file);
	
   analysis = new FortranAnalysis(argc, argv, src_file->get_globalScope());
   traversal = new ElementalTraversal(argc, argv, "template.c");

   //src_file->set_outputFormat(SgFile::e_free_form_output_format);
   //src_file->set_outputLanguage(SgFile::e_C_output_language);

   SgDeclarationStatementPtrList & decls = src_file->get_globalScope()->get_declarations();
   SgDeclarationStatementPtrList::iterator it_decls;
   for (it_decls = decls.begin(); it_decls != decls.end(); it_decls++) {

      if (isSgFunctionDeclaration(*it_decls) != NULL) {
         analysis->traverse(*it_decls, preorder);
         traversal->traverse(*it_decls, preorder);
      }

   }

   delete analysis;
   delete traversal;
   return 0;
}
