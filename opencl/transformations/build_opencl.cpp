#include <PaulDecorate.h>
#include "FortranAnalysis.hpp"
#include "FortranTraversal.hpp"
#include "Util.hpp"

int main(int argc, char ** argv)
{
   ROSE_ASSERT(argc == 3);

   SgProject* project = frontend(argc, argv);
   ROSE_ASSERT(project != NULL);

   // Decorate the AST, with HALO annotations from comments
   paulDecorate(project, "lope.pconf");

   SgSourceFile * src_file = isSgSourceFile((*project)[0]);
   ROSE_ASSERT(src_file);
	
   SgSourceFile * cl_file = isSgSourceFile((*project)[1]);
   SgGlobal * cl_global_scope = cl_file->get_globalScope();
   ROSE_ASSERT(cl_global_scope);

   FortranAnalysis analysis(src_file->get_globalScope());
   FortranTraversal traversal(cl_global_scope);

   SgDeclarationStatementPtrList & decls = src_file->get_globalScope()->get_declarations();
   SgDeclarationStatementPtrList::iterator it_decls;
   for (it_decls = decls.begin(); it_decls != decls.end(); it_decls++) {
      if (isSgFunctionDeclaration(*it_decls) != NULL) {
         debug("build_opencl: found function decl\n");
         analysis.visit((SgFunctionDeclaration*) *it_decls);
         // TODO: Do we need to do an analysis traversal?
         analysis.traverse((SgFunctionDeclaration*) *it_decls, preorder);
         traversal.traverse((SgFunctionDeclaration*) *it_decls, preorder);
      }
   }

   //(*project)[0]->set_outputLanguage(SgFile::e_C_output_language);
   project->unparse();

   return 0;
}
