#include <OpenCL/opencl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

void print_addr(void * addr)
{
   printf("print_addr: %p\n", addr);
}

size_t c_sizeof_cl_mem()
{
   return sizeof(cl_mem);
}

void c_free(void * buf)
{
   free(buf);
}

char * load_program_source(const char * filename, size_t * count)
{
    struct stat statbuf;
    FILE * fh;
    char * source;

    printf("load_program_source:%s: len=%ld\n", filename, strlen(filename));

    fh = fopen(filename, "r");
    if (fh == 0) {
       return 0;
    }

    stat(filename, &statbuf);
    source = (char *) malloc(statbuf.st_size + 1);
    fread(source, statbuf.st_size, 1, fh);
    source[statbuf.st_size] = '\0';

    *count = statbuf.st_size;
    printf("load_program_source: len=%ld\n", *count);

    return source;
}

void stop_on_error(int code)
{
   char msg[256];

   switch (code) {
      case CL_SUCCESS:
         return;
      case CL_INVALID_COMMAND_QUEUE:
         sprintf(msg, "%s (%d)", "CL_INVALID_COMMAND_QUEUE", code);
         break;
      case CL_INVALID_CONTEXT:
         sprintf(msg, "%s (%d)", "CL_INVALID_CONTEXT", code);
         break;
      case CL_INVALID_EVENT_WAIT_LIST:
         sprintf(msg, "%s (%d)", "CL_INVALID_EVENT_WAIT_LIST", code);
         break;
      case CL_INVALID_KERNEL_NAME:
         sprintf(msg, "%s (%d)", "CL_INVALID_KERNEL_NAME", code);
         break;
      case CL_INVALID_MEM_OBJECT:
         sprintf(msg, "%s (%d)", "CL_INVALID_MEM_OBJECT", code);
         break;
      case CL_INVALID_WORK_GROUP_SIZE:
         sprintf(msg, "%s (%d)", "CL_INVALID_WORK_GROUP_SIZE", code);
         break;
      case CL_BUILD_PROGRAM_FAILURE:
         sprintf(msg, "%s (%d)", "CL_BUILD_PROGRAM_FAILURE", code);
         break;
      case CL_INVALID_HOST_PTR:
         sprintf(msg, "%s (%d)", "CL_INVALID_HOST_PTR", code);
         break;
      case CL_INVALID_KERNEL_ARGS:
         sprintf(msg, "%s (%d)", "CL_INVALID_KERNEL_ARGS", code);
         break;
      case CL_INVALID_PLATFORM:
         sprintf(msg, "%s (%d)", "CL_INVALID_PLATFORM", code);
         break;
      case CL_INVALID_QUEUE_PROPERTIES:
         sprintf(msg, "%s (%d)", "CL_INVALID_QUEUE_PROPERTIES", code);
         break;
      case CL_INVALID_VALUE:
         sprintf(msg, "%s (%d)", "CL_INVALID_VALUE", code);
         break;
      default:
         sprintf(msg, "%s (%d)\n", "UNKNOWN_CODE", code);
         break;
   }
   printf("ERROR_CODE==%s\n", msg);
   exit(code);
}
