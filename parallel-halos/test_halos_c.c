#include <stdio.h>
#include "parallel_halo.h"

void test_halos_c()
{
   Parallel_Context context;

   Parallel_Start(&context);

   printf("[%d]: size==%d\n", context.rank, context.size);

   //Call Topology

   Parallel_End(&context);

}
