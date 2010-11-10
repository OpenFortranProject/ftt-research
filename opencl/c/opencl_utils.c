#ifdef __APPLE__
#  define USE_MACH_TIME
#  include <OpenCL/opencl.h>
#  include <CoreServices/CoreServices.h>
#  include <mach/mach.h>
#  include <mach/mach_time.h>
#else
#  include <CL/opencl.h>
#  include <sys/time.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

uint64_t get_cpu_time() {
#ifdef USE_MACH_TIME
   return mach_absolute_time();
#else
   struct timeval tim;
   //   struct rusage ru;
   //   getrusage(RUSAGE_SELF, &ru);
   //   tim = ru.ru_utime;
   gettimeofday(&tim, NULL);
   //printf("get_cpu_time: sec==%d usec==%d\n", tim.tv_sec, tim.tv_usec);
   return ((uint64_t) tim.tv_sec)*1000000 + (uint64_t) tim.tv_usec;   
#endif
}

// Convert to milliseconds
double cpu_time_to_sec(uint64_t cpu_elapsed)
{
   double us = 0.0;
#ifdef USE_MACH_TIME
   static mach_timebase_info_data_t  info;
   mach_timebase_info(&info);
   cpu_elapsed *= info.numer;
   cpu_elapsed /= info.denom;
   us = (double) (cpu_elapsed/1000);  // microseconds
#else
   us = (double) cpu_elapsed;
#endif   
   return us/1000.0;
}

double print_elapsed_time_c(uint64_t cpu_elapsed)
{
   double elapsed = cpu_time_to_sec(cpu_elapsed);
   fprintf(stdout, "processor cycle time == %f ms\n", (float) elapsed);
   fflush(stdout);
   return elapsed;
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
      case CL_INVALID_WORK_GROUP_SIZE:
         sprintf(msg, "%s (%d)", "CL_INVALID_WORK_GROUP_SIZE", code);
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
