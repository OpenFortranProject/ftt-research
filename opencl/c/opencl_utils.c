#ifdef __APPLE__
#  include <OpenCL/opencl.h>
#  include <CoreServices/CoreServices.h>
#  include <mach/mach.h>
#  include <mach/mach_time.h>
#else
#  include <CL/opencl.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>


int64_t mach_absolute_time_test() {
#ifdef __APPLE__
   uint64_t time = mach_absolute_time();
   printf("mach_time==%ld\n", (long) time);
   return (int64_t) time;
#else
   return 0;
#endif
}

// Convert to milliseconds
double mach_time_to_sec(uint64_t mach_elapsed)
{
   double ms = 0.0;
#ifdef __APPLE__
   static mach_timebase_info_data_t  sTimebaseInfo;
   
   if ( sTimebaseInfo.denom == 0 ) {
      // initialize (yuk, hope it isn't some stray value)
      (void) mach_timebase_info(&sTimebaseInfo);
   }
   
   ms = (double) (mach_elapsed) / 1.0e9;
   ms *= sTimebaseInfo.numer / sTimebaseInfo.denom;
#endif   
   return ms;
}

double print_elapsed_time(uint64_t mach_elapsed)
{
   double elapsed = 0.0;
#ifdef __APPLE__
   elapsed = mach_time_to_sec(mach_elapsed);
   fprintf(stdout, "Mach processor cycle time == %f ms\n", (float) elapsed);
   fflush(stdout);
#endif
   return mach_elapsed;
}

void print_addr(void * addr)
{
   printf("print_addr: %p int(3)==%d\n", addr, ((int*)addr)[2]);
}

size_t c_sizeof_cl_mem()
{
   return sizeof(cl_mem);
}

size_t c_sizeof_cl_ulong()
{
   return sizeof(cl_ulong);
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

    //printf("load_program_source:%s: len=%ld\n", filename, strlen(filename));

    fh = fopen(filename, "r");
    if (fh == 0) {
       return 0;
    }

    stat(filename, &statbuf);
    source = (char *) malloc(statbuf.st_size + 1);
    fread(source, statbuf.st_size, 1, fh);
    source[statbuf.st_size] = '\0';

    *count = statbuf.st_size;
    //printf("load_program_source: len=%ld\n", *count);

    return source;
}

void stop_on_error(int code)
{
   char msg[256];

   switch (code) {
      case CL_SUCCESS:
         return;
      case CL_INVALID_ARG_SIZE:
         sprintf(msg, "%s (%d)", "CL_INVALID_ARG_SIZE", code);
         break;
      case CL_INVALID_ARG_VALUE:
         sprintf(msg, "%s (%d)", "CL_INVALID_ARG_VALUE", code);
         break;
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
      case CL_INVALID_PROGRAM_EXECUTABLE:
         sprintf(msg, "%s (%d)", "CL_INVALID_PROGRAM_EXECUTABLE", code);
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
