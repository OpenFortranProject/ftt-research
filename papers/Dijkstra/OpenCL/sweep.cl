/**
 * C kernel for sweep of forward star implementation
 */

// TODO: look up how to place in constant memory

__kernel void sweep (int nx, int ny, int nz, int nfs
                           , __global const float * U
                           , __global float * TT
                           , __global const int * Offset
                           , __global int * Changed)
{
  // Get x, y, z coordinates and check in correct boundary
  const int halo = 0, rightHalo = 0; // No halo at this point
  const int i = get_global_id(0) + halo;
  const int j = get_global_id(1) + halo;
  const int k = get_global_id(2) + halo;
  if (i < halo || j < halo || k < halo)
    return;
  if (i >= nx || j >= ny || k >= nz)
    return;
  const int sx = 1;
  const int sy = sx * (nx + halo + rightHalo);
  const int sz = sy * (ny + halo + rightHalo);

  // initialize variables
  // TODO: after debugging change to 10.0
  int const DIST_FACTOR = 1;
  int l, is, js, ks;
  float dist, delay, t;
  float t0, tt_min;
  int k0s;

  // begin algorithm

  int chg = 0;
  const int k0 = i + j * sy + k * sz;
  const float u0 = U[k0];

  t0 = TT[k0];
  tt_min = t0;
    
  // TODO:
  //   1. change Offset to one dimension and stride by 3
  //   2. put Offsets in local variables so they can go into registers

  // check each node in forward star
  for (l = 0; l < nfs; ++l) {
    is = i + Offset[0+l*3]; if (is < 0) continue; if (is >= nx) continue;
    js = j + Offset[1+l*3]; if (js < 0) continue; if (js >= ny) continue;
    ks = k + Offset[2+l*3]; if (ks < 0) continue; if (ks >= nz) continue;
    dist = DIST_FACTOR*sqrt( (float) ((is-i)*(is-i) + (js-j)*(js-j) + (ks-k)*(ks-k)) );
    k0s = is + js * sy + ks * sz;
    delay = 0.5*(u0 + U[k0s]) * dist;

    t = TT[k0s] + delay;
    // if distance is smaller update
    if (t < t0) {
      chg = 1;
      t0 = t;
      tt_min = t;
    }
  }
  Changed[k0] = chg;
  TT[k0] = tt_min;
}
