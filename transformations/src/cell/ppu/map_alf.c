#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>

#define MAX_ARRAYS 16

/* these must be consistent with Fortran parameters */
#define DATA_IN     1
#define DATA_OUT    2
#define DATA_INOUT  3

#ifdef CELL_PPU
#include <alf.h>
#else
typedef int alf_handle_t;
typedef int alf_task_handle_t;
typedef int alf_task_desc_handle_t;
typedef int alf_wb_handle_t;
typedef int ALF_EXIT_POLICY_T;

#define ALF_WB_SINGLE 1
#define ALF_DATA_BYTE 1
#define ALF_DATA_FLOAT 2
#define ALF_BUF_IN    1
#define ALF_BUF_OUT   2
#define ALF_BUF_INOUT 3
#define ALF_BUF_OVL_INOUT 4

#define ALF_TASK_DESC_MAX_STACK_SIZE 1
#define ALF_TASK_DESC_ACCEL_IMAGE_REF_L 1
#define ALF_TASK_DESC_ACCEL_LIBRARY_REF_L 1
#define ALF_TASK_DESC_ACCEL_KERNEL_REF_L 1
#define ALF_TASK_DESC_WB_PARM_CTX_BUF_SIZE 1
#define ALF_TASK_DESC_PARTITION_ON_ACCEL 1
#define ALF_TASK_DESC_TSK_CTX_SIZE 1
#define ALF_TASK_DESC_WB_IN_BUF_SIZE 1
#define ALF_TASK_DESC_WB_INOUT_BUF_SIZE 1
#define ALF_TASK_DESC_WB_OUT_BUF_SIZE 1

#define ALF_EXIT_POLICY_WAIT 1
#define ALF_QUERY_NUM_ACCEL 1

#endif

/* GUARD(s) runs the ALF command s and exits with a descriptive error                   
 * message if the command fails
 */
#define GUARD(s) \
  { \
    errno = s; \
    if (errno < 0) \
      { \
        printf(#s " failed - error code %d\n", errno); \
        exit(1); \
      } \
  }


const char *spu_image_name = "ftt_cell_spu";
const char *library_name = "ftt_cell_spu64.so";


typedef struct _ftt_params_t
{
  unsigned int num_wb_elements;
} ftt_params_t;


typedef struct MapData_ {
  int ilen;
  int olen;
  int iolen;
  int num_arrays;
  int array_size;		// size in elements
  int element_size;
  void* iptrs[MAX_ARRAYS];
  void* optrs[MAX_ARRAYS];
  void* ioptrs[MAX_ARRAYS];
} MapData;


typedef struct MapTask_ {
  ftt_params_t		  params;	// TODO - make sure this is aligned
  const char*		  kernel;
  alf_handle_t		  half;
  alf_task_handle_t	  htask;
  alf_task_desc_handle_t  htd;
} MapTask;


static void ftt_init_work_blocks(MapTask* task, MapData* data);
static void ftt_init_task_desc(MapTask* task, MapData* data, int buf_size);


void map_create_data_context(MapData** data) {
   int i;

   MapData* dcontext = (MapData*) malloc( sizeof(MapData) );

   dcontext->array_size = 0;		// number of elements per array
   dcontext->element_size = 4;		// 4 bytes for now
   dcontext->num_arrays  = 0;

   dcontext->ilen  = 0;
   dcontext->olen  = 0;
   dcontext->iolen = 0;
   
   for (i = 0; i < MAX_ARRAYS; i++) {
      dcontext->iptrs[i]  = NULL;
      dcontext->optrs[i]  = NULL;
      dcontext->ioptrs[i] = NULL;
   }

   *data = dcontext;

   printf("map_data_create:\n");
}

void map_add_data(MapData* data, void* ptr, int size, int intent) {
  if (data->num_arrays == 0) data->array_size = size;

  /* for now at least, all of the array sizes must be equal */
  assert(size == data->array_size);

   switch (intent) {
   case DATA_IN:
     data->iptrs[data->ilen++] = ptr;
     break;
   case DATA_OUT:
     data->optrs[data->olen++] = ptr;
     break;
   case DATA_INOUT:
     data->ioptrs[data->iolen++] = ptr;
     break;
   default:
     assert(0 == 1);
   }

  data->num_arrays += 1;

  printf("map_add_data: size = %d %p\n", size, ptr);
}


void map_create_task_context(MapTask** task, void* kernel) {
  MapTask tmp;
  *task = &tmp;
  printf("map_task_create:\n");
}


inline static int wb_buf_size(MapData* data)
{
  int bufSize = (220 / data->num_arrays) * 1024;	// size per array
}


static void calc_wb_sizes(MapData* data,
			  int* buf_size,
			  int* num_blocks,
			  int* num_wb_elements)
{
  int nBlocks, nElements;
  int array_size = data->array_size;
  int bufSize = wb_buf_size(data);

  nElements = bufSize / data->element_size;
  nBlocks = array_size / nElements;

  // TODO - make sure array size nicely fits

  if (nBlocks*nElements < array_size) {
     nBlocks += 1;  // finish last partial block
  }

  *buf_size = bufSize;
  *num_blocks = nBlocks;
  *num_wb_elements = nElements;
}


void map(MapTask* task, MapData* data)
{
  int buf_size = wb_buf_size(data);
  alf_handle_t half = task->half;
  unsigned int nodes;
	
  printf("map_apply_task:\n");

  // START: -- Added on 10/01/2008 -- to be added into a function
  GUARD(alf_init(NULL, &half));
 
  // START: From Charles' code
  GUARD(alf_query_system_info(half, ALF_QUERY_NUM_ACCEL, 0, &nodes));
  if (nodes <= 0) {
    fprintf(stderr, "Cannot allocate spe to use.\n");
	exit(1);
  }
	
  if (nodes > 8) nodes = 8;
  GUARD(alf_num_instances_set(half, nodes));
  //END: From Charles' code
  //END: --Added on 10/01/2008	
  ftt_init_task_desc(task, data, buf_size);

  // fdt_init_task(htd[ntask], &htask[ntask]);

  ftt_init_work_blocks(task, data);

  // START: -- Added on 10/01/2008	-- to be added into a function
  GUARD(alf_task_finalize(task->htask)); //(htask[ntask]));
  GUARD(alf_task_wait(task->htask, -1)); //(htask[ntask], -1));
  GUARD(alf_exit(half, ALF_EXIT_POLICY_WAIT, 10000));
  //END: --Added on 10/01/2008

}


static void ftt_init_work_blocks(MapTask* task, MapData* data)
{
   alf_wb_handle_t *hwblist;
   int buf_size, num_blocks, num_wb_elements;
   int i, ii, k;

   int array_size = data->array_size;

   calc_wb_sizes(data, &buf_size, &num_blocks, &num_wb_elements);


   hwblist = malloc(num_blocks * sizeof(alf_wb_handle_t));

   for (i = 0; i < num_blocks; i++)
   {
      ii = i*num_wb_elements;

      if (ii + num_wb_elements > array_size) {
         num_wb_elements = array_size - ii; // finish last partial block 
      }

      task->params.num_wb_elements = num_wb_elements;
      GUARD(alf_wb_create(task->htask, ALF_WB_SINGLE, 1, &hwblist[i]));
      GUARD(alf_wb_parm_add(hwblist[i], &task->params,
			    sizeof(ftt_params_t), ALF_DATA_BYTE, 0));

      if (data->ilen > 0) {
         GUARD(alf_wb_dtl_begin(hwblist[i], ALF_BUF_IN, 0));
         for (k = 0; k < data->ilen; k++) {
            GUARD(alf_wb_dtl_entry_add(hwblist[i], data->iptrs[k] + ii * sizeof(float), num_wb_elements, ALF_DATA_FLOAT));
         }
         GUARD(alf_wb_dtl_end(hwblist[i]));
      }

      if (data->iolen > 0) {
         GUARD(alf_wb_dtl_begin(hwblist[i], ALF_BUF_OVL_INOUT, 0));
         for (k = 0; k < data->iolen; k++) {
            GUARD(alf_wb_dtl_entry_add(hwblist[i], data->ioptrs[k] + ii * sizeof(float), num_wb_elements, ALF_DATA_FLOAT));
         }
         GUARD(alf_wb_dtl_end(hwblist[i]));
      }

      if (data->olen > 0) {
         GUARD(alf_wb_dtl_begin(hwblist[i], ALF_BUF_OUT, 0));
         for (k = 0; k < data->olen; k++) {
            GUARD(alf_wb_dtl_entry_add(hwblist[i], data->optrs[k] + ii * sizeof(float), num_wb_elements, ALF_DATA_FLOAT));
         }
         GUARD(alf_wb_dtl_end(hwblist[i]));
      }

      GUARD(alf_wb_enqueue(hwblist[i]));
   }

   free(hwblist);
}


void ftt_init_task_desc(MapTask* task, MapData* data, int buf_size)
{
   alf_handle_t half = task->half;
   alf_task_desc_handle_t *htd_p;
	alf_task_handle_t htask = task->htask;

   const char* kernel_name = task->kernel;

   int input_buffer_size      = data->ilen*buf_size;
   int output_buffer_size     = data->olen*buf_size;
   int overlapped_buffer_size = data->iolen*buf_size;

   GUARD(alf_task_desc_create(half, 0, htd_p));

   GUARD(alf_task_desc_set_int32(*htd_p, ALF_TASK_DESC_MAX_STACK_SIZE, 4096));
   GUARD(alf_task_desc_set_int64(*htd_p, ALF_TASK_DESC_ACCEL_IMAGE_REF_L, (unsigned long long)spu_image_name));
   GUARD(alf_task_desc_set_int64(*htd_p, ALF_TASK_DESC_ACCEL_LIBRARY_REF_L, (unsigned long long)library_name));
   GUARD(alf_task_desc_set_int64(*htd_p, ALF_TASK_DESC_ACCEL_KERNEL_REF_L, (unsigned long long)kernel_name));
   GUARD(alf_task_desc_set_int32(*htd_p, ALF_TASK_DESC_WB_PARM_CTX_BUF_SIZE, sizeof(ftt_params_t)));

   GUARD(alf_task_desc_set_int32(*htd_p, ALF_TASK_DESC_PARTITION_ON_ACCEL, 0));

   GUARD(alf_task_desc_set_int32(*htd_p, ALF_TASK_DESC_TSK_CTX_SIZE, 0));

   GUARD(alf_task_desc_set_int32(*htd_p, ALF_TASK_DESC_WB_IN_BUF_SIZE, input_buffer_size));
   GUARD(alf_task_desc_set_int32(*htd_p, ALF_TASK_DESC_WB_INOUT_BUF_SIZE, overlapped_buffer_size));
   GUARD(alf_task_desc_set_int32(*htd_p, ALF_TASK_DESC_WB_OUT_BUF_SIZE, output_buffer_size));
	
   // -- Added on 10/01/2008	
   // Not sure about the value of the 5th parameter -> unsigned int wb_dist_size
   GUARD(alf_task_create(*htd_p, NULL, 1, 0, 0, &htask));  
   //END: --Added on 10/01/2008 
}


#ifndef CELL_PPU

int alf_wb_create(alf_task_handle_t htask, int wb_type, int count, alf_wb_handle_t* list)
{
  return 0;
}

int alf_wb_parm_add(alf_wb_handle_t hwb, void* parm, int size, int type, int count)
{
  return 0;
}

int alf_wb_dtl_begin(alf_wb_handle_t hwb, int type, int count)
{
  return 0;
}

int alf_wb_dtl_end(alf_wb_handle_t hwb)
{
  return 0;
}

int alf_wb_dtl_entry_add(alf_wb_handle_t hwb, void* buf, int count, int type)
{
  return 0;
}

int alf_wb_enqueue(alf_wb_handle_t hwb)
{
  return 0;
}

int alf_task_desc_create(alf_handle_t half, int i, alf_task_desc_handle_t* htd_p)
{
  return 0;
}

int alf_task_desc_set_int32(alf_task_desc_handle_t* htd_p, int size, unsigned long count)
{
  return 0;
}

int alf_task_desc_set_int64(alf_task_desc_handle_t* htd_p, int size, unsigned long long count)
{
  return 0;
}

//START: --Added on 10/01/2008
int alf_init(void* p_sys_config_info, alf_handle_t half)
{
  return 0;
}
		
int alf_num_instances_set( alf_handle_t half, unsigned int number_of_instances)
{
  return 0;
}
		
int alf_task_finalize(alf_task_handle_t htask)
{
  return 0;
}
		
int alf_task_wait(alf_task_handle_t htask, int time_out)
{
  return 0;
}
	
int alf_exit(alf_handle_t half, ALF_EXIT_POLICY_T policy, int time_out)
{
  return 0;
}

int alf_task_create(alf_task_desc_handle_t htd, void* p_task_context_data, unsigned int num_instances, unsigned int tsk_attr, unsigned int wb_dist_size, alf_task_handle_t htask_p)
{
  return 0;
}
//END: --Added on 10/01/2008

#endif
