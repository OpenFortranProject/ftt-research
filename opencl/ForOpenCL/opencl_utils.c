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


// guess at maximum work item dimensions
//
#define MAX_WORK_ITEM_DIMENSIONS (3)


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
      case CL_INVALID_ARG_INDEX:
         sprintf(msg, "%s (%d)", "CL_INVALID_ARG_INDEX", code);
         break;
      case CL_INVALID_ARG_SIZE:
         sprintf(msg, "%s (%d)", "CL_INVALID_ARG_SIZE", code);
         break;
      case CL_INVALID_ARG_VALUE:
         sprintf(msg, "%s (%d)", "CL_INVALID_ARG_VALUE", code);
         break;
      case CL_INVALID_BUFFER_SIZE:
         sprintf(msg, "%s (%d)", "CL_INVALID_BUFFER_SIZE", code);
         break;
      case CL_INVALID_COMMAND_QUEUE:
         sprintf(msg, "%s (%d)", "CL_INVALID_COMMAND_QUEUE", code);
         break;
      case CL_INVALID_CONTEXT:
         sprintf(msg, "%s (%d)", "CL_INVALID_CONTEXT", code);
         break;
      case CL_INVALID_DEVICE:
         sprintf(msg, "%s (%d)", "CL_INVALID_DEVICE", code);
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
      case CL_INVALID_WORK_GROUP_SIZE:
         sprintf(msg, "%s (%d)", "CL_INVALID_WORK_GROUP_SIZE", code);
         break;
      case CL_OUT_OF_HOST_MEMORY:
         sprintf(msg, "%s (%d)", "CL_OUT_HOST_MEMORY", code);
         break;
      case CL_OUT_OF_RESOURCES:
         sprintf(msg, "%s (%d)", "CL_OUT_OF_RESOURCES", code);
         break;
      default:
         sprintf(msg, "%s (%d)\n", "UNKNOWN_CODE", code);
         break;
   }
   printf("ERROR_CODE==%s\n", msg);
   exit(code);
}


int query_device_info(int id, cl_device_id device)
{
   const int str_size = 2048;
   const int vals_len = MAX_WORK_ITEM_DIMENSIONS;

   long long val;
   size_t vals[vals_len];
   unsigned int max_dims, i;

   int    status;
   char   param_value[str_size];
   size_t param_value_size;

   status = clGetDeviceInfo(device, CL_DEVICE_NAME, str_size, param_value, &param_value_size);
   param_value[str_size-1] = '\0';

   printf("OpenCL Device # %d == %s  status==%d\n", id, param_value, status);

   status = clGetDeviceInfo(device, CL_DEVICE_TYPE, sizeof(val), &val, NULL);

   if (status == CL_SUCCESS) {
      printf("\tdevice[%p]: Type: ", device);

      if (val & CL_DEVICE_TYPE_DEFAULT) {
         val &= ~CL_DEVICE_TYPE_DEFAULT;
         printf("Default ");
      }

      if (val & CL_DEVICE_TYPE_CPU) {
         val &= ~CL_DEVICE_TYPE_CPU;
         printf("CPU ");
      }

      if (val & CL_DEVICE_TYPE_GPU) {
         val &= ~CL_DEVICE_TYPE_GPU;
         printf("GPU ");
      }

      if (val & CL_DEVICE_TYPE_ACCELERATOR) {
         val &= ~CL_DEVICE_TYPE_ACCELERATOR;
         printf("Accelerator ");
      }

      if (val != 0) {
         printf("Unknown (0x%llx) ", val);
      }
   }
   else {
      printf("\tdevice[%p]: Unable to get TYPE: %s!\n", device, "CLErrString(status)");
      stop_on_error(status);
   }

   status = clGetDeviceInfo(device, CL_DEVICE_MAX_COMPUTE_UNITS, sizeof(val), &val, &param_value_size);
   printf("with %u units/cores", (unsigned int) val);

   status = clGetDeviceInfo(device, CL_DEVICE_MAX_CLOCK_FREQUENCY, sizeof(val), &val, &param_value_size);
   printf(" at %u MHz\n", (unsigned int) val);

   status = clGetDeviceInfo(device, CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT, sizeof(val), &val, &param_value_size);
   printf("\tfloat vector width == %u\n", (unsigned int) val);
   
   status = clGetDeviceInfo(device, CL_DEVICE_MAX_WORK_GROUP_SIZE, sizeof(val), &val, &param_value_size);
   printf("\tMaximum work group size == %lu\n", (size_t) val);
   
   status = clGetDeviceInfo(device, CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS, sizeof(max_dims), &max_dims, &param_value_size);
   printf("\tMaximum work item dimensions == %u\n", max_dims);
   
   status = clGetDeviceInfo(device, CL_DEVICE_MAX_WORK_ITEM_SIZES, vals_len*sizeof(size_t), vals, &param_value_size);
   printf("\tMaximum work item sizes == (");
   for (i = 0; i < max_dims; i++) printf(" %ld", vals[i]);
   printf(" )\n");
   
   status = clGetDeviceInfo(device, CL_DEVICE_LOCAL_MEM_SIZE, sizeof(val), &val, &param_value_size);
   printf("\tLocal mem size == %u\n", (unsigned int) val);

   status = clGetDeviceInfo(device, CL_DEVICE_GLOBAL_MEM_SIZE, sizeof(val), &val, &param_value_size);
   printf("\tGlobal mem size == %u\n", (unsigned int) val);

   status = clGetDeviceInfo(device, CL_DEVICE_GLOBAL_MEM_CACHE_SIZE, sizeof(val), &val, &param_value_size);
   printf("\tGlobal mem cache size == %u\n", (unsigned int) val);

   status = clGetDeviceInfo(device, CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE, sizeof(val), &val, &param_value_size);
   printf("\tGlobal mem cache line size == %u\n", (unsigned int) val);

   status = clGetDeviceInfo(device, CL_DEVICE_EXTENSIONS, str_size, param_value, &param_value_size);
   param_value[str_size-1] = '\0';
   printf("\tExtensions == %s size==%ld status=%d\n", param_value, param_value_size, status);

   printf("\n");

   return status;
}

