#include <mpi.h>


/* Structure representing a parallel context
 */
typedef struct Parallel_Context_struct
{
   int       rank;              /* id of this process */
   int       size;              /* number of participating processes */
   MPI_Comm  comm;              /* communicator for this context */
}
   Parallel_Context, *pParallel_Context;


/* Structure representing a halo
 */
typedef struct Halo_struct
{
   int       hlx, hrx;          /* halo size (x-direction) */
   int       hly, hry;          /* halo size (y-direction) */
   int       hlz, hrz;          /* halo size (z-direction) */
}
   Halo, *pHalo;


/* Structure representing a parallel topology
 */
typedef struct Parallel_Topology
{
   /* Parallel topology
    */
   int  left, right;            /* Neighbor ranks (x-direction) */
   int  bottom, top;            /* Neighbor ranks (y-direction) */
   int  front, back;            /* Neighbor ranks (z-direction) */

   /* Cartesian decomposition
    */
   int  npex;                   /* Number of processors X direction */
   int  npey;                   /* Number of processors Y direction */
   int  npez;                   /* Number of processors Z direction */
}
   Parallel_Topology, *pParallel_Topology;


void Parallel_Start  (pParallel_Context context);
void Parallel_End    (pParallel_Context context);
