/**
 * C kernel of forward star implementation
 */

__kernel void calcDistance (int nx, int ny, int nz, int nfs, __global const float * U, __global int * TT
                            , __global const int * Offset, __global int * Changed)
{
  // Get x, y, z coordinates and check in correct boundary
  const int halo = 0, rightHalo = 0; // No halo at this point
  const int i = get_global_id(0) + halo;
  const int j = get_global_id(1) + halo;
  const int k = get_global_id(2) + halo;
  if (i < halo || j < halo || k < halo)
    return;
  if (i > nx || j > ny || k > nz)
    return;
  const int sx = 1;
  const int sy = sx * (nx + halo + rightHalo);
  const int sz = sy * (ny + halo + rightHalo);

  // initialize variables
  int const DIST_FACTOR = 1;
  int l, is, js, ks;
  float dist, delay, t;
  int chg = 0;
  int u0, t0, tt_min;
  int k0 = i + j * sy + k * sz;
  int k0s;

  // begin algorithm
  u0 = U[k0];
  t0 = TT[k0];
  tt_min = t0;
    
  // check each node in forward star
  for (l = 0; l < nfs; ++l) {
    is = i + Offset[1,l]; if (is < 1) break; if (is < nx) break;
    js = j + Offset[1,l]; if (js < 1) break; if (js < ny) break;
    ks = k + Offset[1,l]; if (ks < 1) break; if (ks < nz) break;
    dist = DIST_FACTOR*sqrt((is-i)*(is-i) + (js-j)*(js-j) + (ks-k)*(ks-k) + 0.0);
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
