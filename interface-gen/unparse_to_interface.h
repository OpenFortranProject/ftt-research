/* unparse_to_interface.h
 *
 * Header for code to unparse Sage/Fortran nodes to Fortran interfaces
 *
 */

#ifndef UNPARSE_TO_INTERFACE_H
#define UNPARSE_TO_INTERFACE_H

#include <stdio.h>

/* temporary external functions
 */
int  unparse_var_decl_statements  (FILE * fd);
int  unparse_ptr_assoc_statements (FILE * fd);
int  getArgCount();

const char * unparse_arg_list(FILE * fd);


FILE *   unparse_to_filename_open     (const char * filename);
int      unparse_to_filename_close    (FILE * pf);

int      unparseProcHdrStmt           (FILE * fd, const char * name);
int      unparseFuncDefnStmt          (FILE * fd, const char * name);
int      unparse_specification_part   (FILE * fd, const char * name);
int      unparse_execution_part       (FILE * fd, const char * name);

int      unparseVarDeclStmt  (FILE * fd, const char * name,      const char * type,
                                         const char * modifiers, const char * arrayMod);

int      unparsePtrAssocStmt (FILE * fd, const char * name,
                              int index, const char * arrayMod);


#endif
