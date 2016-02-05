/**
 * C kernel for sweep of forward star implementation
 */

// TODO: look up how to place in constant memory
__kernel void sweep ( const int nx, const int ny
		      , const int nz, const int nfs
		      , __global const float * U
		      , __global float * writeBuf
		      , __constant int * Offset
		      , __global int * Changed
		      , const int rightHalo
		      , const int step)
		      , __local float * readBuf)
{
  // Get x, y, z coordinates and check in correct boundary

  const int halo = 0; // No halo at this point

  return;
  int i = get_global_id(0) + halo;
  int j = get_global_id(1) + halo;
  int k = get_global_id(2) + halo;
  const int sx = 1;
  const int sy = sx * (nx);// + halo + rightHalo);
  const int sz = sy * (ny);// + halo + rightHalo);

  if (i < halo || j < halo || k < halo)
    return;
  if (i >= nx-rightHalo || j >= ny-rightHalo || k >= nz)
    return;

  const int k0 = i + j * sy + k * sz;
   
  /* readBuf[localk0] = writeBuf[k0]; */
    
  /* barrier(CLK_LOCAL_MEM_FENCE); */
  /* writeBuf[k0] = readBuf[localk0]; */
  /* writeBuf[k0] = k0; */

  return;
  int const DIST_FACTOR = 1;
  float dist, delay, t;
  float t0, tt_min;
  int k0s;

  // begin algorithm
  int chg = 0;
  const float u0 = U[k0];

  t0 = readBuf[k0];
  tt_min = t0;
    
  // TODO:
  //   1. change Offset to one dimension and stride by 3
  //   2. put Offsets in local variables so they can go into registers

  // check each node in forward star
  /* int check = 0; */
  /* for (l = 0; l < nfs; ++l) { */
  /*   is = i + Offset[0+l*3];  */
  /*   if (is < 0) continue; if (is >= nx-rightHalo) continue; */
  /*   js = j + Offset[1+l*3];  */
  /*   if (js < 0) continue; if (js >= ny-rightHalo) continue; */
  /*   ks = k + Offset[2+l*3];  */
  /*   if (ks < 0) continue; if (ks >= nz) continue; */
  /*   dist = DIST_FACTOR*sqrt( (float) ((is-i)*(is-i) + (js-j)*(js-j) + (ks-k)*(ks-k)) ); */
  /*   k0s = is + js * sy + ks * sz; */
  /*   delay = 0.5*(u0 + U[k0s]) * dist; */
  /*   t = readBuf[k0s] + delay; */
  /*   // if distance is smaller update */
  /*   if (t < t0) { */
  /*     chg = 1; */
  /*     t0 = t; */
  /*     tt_min = t; */
  /*   } */
  /* } */
  Changed[k0] = chg;
  writeBuf[k0] = tt_min;
}
