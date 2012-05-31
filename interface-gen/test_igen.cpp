#include <assert.h>
#include <stdio.h>
#include "unparse_to_interface.h"

int main(int argc, char * argv[])
{
   int            err;
   FILE *         fd;
   const char *   name     = "stingray";
   const char *   filename = "stingray_interface" ".f90";

   fd = unparse_to_filename_open(filename);
   assert(fd != NULL);

   err = unparseProcHdrStmt(fd, name);
   assert(err == 0);

   err = unparse_to_filename_close(fd);
   assert(err == 0);

   return 0;
}


int  getArgCount()
{
   /* This number depends on the number of arguments declared below */
   return 20;
}

const char * unparse_arg_list(FILE * fd)
{
   return "u,t,iprec,knx,kny,knodes,kmx,kmy,kmz,zhang,tf_anisotropy,a_r,a_t,a_p";
}

int  unparse_var_decl_statements(FILE * fd)
{
   unparseVarDeclStmt(fd, "u"            , "Real(C_FLOAT)"   , "in",   "(knodes)");
   unparseVarDeclStmt(fd, "t"            , "Real(C_FLOAT)"   , "in",   "(knodes)");
   unparseVarDeclStmt(fd, "iprec"        , "Integer(C_INT)"  , "in",   "(knodes)");

   unparseVarDeclStmt(fd, "knx"          , "Integer(C_INT)"  , "in",    NULL);
   unparseVarDeclStmt(fd, "kny"          , "Integer(C_INT)"  , "in",    NULL);
   unparseVarDeclStmt(fd, "knodes"       , "Integer(C_INT)"  , "in",    NULL);

   unparseVarDeclStmt(fd, "kmx"          , "Integer(C_INT)"  , "in",    NULL);
   unparseVarDeclStmt(fd, "kmy"          , "Integer(C_INT)"  , "in",    NULL);
   unparseVarDeclStmt(fd, "kmz"          , "Integer(C_INT)"  , "in",    NULL);

   unparseVarDeclStmt(fd, "zhang"        , "Real(C_FLOAT)"   , "in",   "(knx,kny)");

   unparseVarDeclStmt(fd, "tf_anisotropy", "Logical(C_BOOL)" , "in",    NULL);

   unparseVarDeclStmt(fd, "a_r"          , "Real(C_FLOAT)"   , "out",  "(knodes)");
   unparseVarDeclStmt(fd, "a_t"          , "Real(C_FLOAT)"   , "out",  "(knodes)");
   unparseVarDeclStmt(fd, "a_p"          , "Real(C_FLOAT)"   , "out",  "(knodes)");
}

int  unparse_ptr_assoc_statements (FILE * fd)
{
   return 0;
}
