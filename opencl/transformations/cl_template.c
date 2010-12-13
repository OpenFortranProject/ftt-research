const int NPAD = 1;

#define CL_LOCAL
#define CL_GLOBAL

extern int get_local_id(unsigned int dim);
extern int get_local_size(unsigned int dim);
extern int get_global_id(unsigned int dim);
extern int get_global_size(unsigned int dim);

typedef struct _int4 {
   int s0, s1, s2, s3;
} int4;


typedef struct _int2 {
   int s0, s1;
} int2;

CL_LOCAL float * TILE_OFFSET(CL_LOCAL float * base, int idx, int2 size)
{
   return (base + idx*size.s0*size.s1);
}

int2 get_tile_size()
{
   return (int2) {get_local_size(0) + 2*NPAD, get_local_size(1) + 2*NPAD};
}

int get_k_index()
{
   return (get_global_id(0) + NPAD) + (get_global_id(1) + NPAD) * (get_global_size(0) + 2*NPAD);
}

static inline int region_idx(int4 halo, int2 size)
{
   return (get_local_id(0) + halo.s0) + (get_local_id(1) + halo.s2)*size.s0;
}

static inline float sq(float x)
{
   return x*x;
}

static inline float transfer_halo(CL_GLOBAL float * A, int4 halo, CL_LOCAL float * tile);
