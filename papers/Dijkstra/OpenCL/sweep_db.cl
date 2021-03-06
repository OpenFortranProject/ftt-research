/**
 * C kernel for sweep of forward star implementation
 */

#define UPDATE_FORWARD_STAR_TT
#undef  DOUBLE_BUFFER
#define SWEEP
#define DO_TWICE
#undef  DO_THRICE

// TODO: look up how to place in constant memory
__kernel void sweep_db ( const int nx, const int ny
		      , const int nz, const int nfs
		      , __global const float * U
		      , __global float * TT
		      , __constant int * Offset
		      , __global int * Changed
		      , const int step)
{
  // double buffering offsets
  int ttOff, out_ttOff;

  const int xSize = get_global_size(0);
  const int ySize = get_global_size(1);
  const int zSize = get_global_size(2);

  // Get x, y, z coordinates and check in correct boundary
  const int halo = 0; // No halo at this point

  int i = get_global_id(0) + halo;
  int j = get_global_id(1) + halo;
  int k = get_global_id(2) + halo;

  // decide Sweep
#ifdef SWEEP
  if (step % 6 == 1) {
   i = nx - i - 1;
  }
  else if (step % 6 == 2) {
    i = nx - i - 1;
    k = nz - k - 1;
  }
  else if (step % 6 == 3) {
    j = ny - j - 1;
  }
  else if (step % 6 == 4) {
    i = nx - i - 1;
    j = ny - j - 1;
  }
  else if (step % 6 == 5) {
    k = nz - k - 1;
  }
#endif

  if (i < halo || j < halo || k < halo)
    return;
  if (i >= nx || j >= ny || k >= nz)
    return;

  const int sx = 1;
  const int sy = sx * xSize;
  const int sz = sy * ySize;

  ttOff = 0;
  out_ttOff = 0;

#ifdef DOUBLE_BUFFER
  if (step % 2 == 0) {
    ttOff = 0;
    out_ttOff = xSize * ySize * zSize;
  } else {
    ttOff = xSize * ySize * zSize;
    out_ttOff = 0;
  }
#endif

  // initialize variables
  int l, is, js, ks;
  float dist, delay, t;
  float t0, tt_min;
  int k0s;

  // begin algorithm
  int chg = 0;
  int chg_star = 0;
  const int k0 = i + j * sy + k * sz;
  const float u0 = U[k0];

  t0 = TT[k0 + ttOff];
  tt_min = t0;
    
  // check each node in forward star
  for (l = 0; l < nfs; ++l) {
    int oi, oj, ok;
    is = i + Offset[0+l*3]; if (is < 0) continue; if (is >= nx) continue;
    js = j + Offset[1+l*3]; if (js < 0) continue; if (js >= ny) continue;
    ks = k + Offset[2+l*3]; if (ks < 0) continue; if (ks >= nz) continue;
    oi = is - i;
    oj = js - j;
    ok = ks - k;
    dist = 10.0*sqrt( (float) (oi*oi + oj*oj + ok*ok) );
    k0s = is + js * sy + ks * sz;
    delay = 0.5*(u0 + U[k0s]) * dist;

#ifdef  UPDATE_FORWARD_STAR_TT
    t = t0 + delay;
    if (t < TT[k0s]) {
       chg = 1;
       chg_star = 1;
       TT[k0s] = t;
    }
#endif

    t = TT[k0s + ttOff] + delay;
    // if distance is smaller update
    if (t < t0) {
      chg = 1;
      t0 = t;
      tt_min = t;
    }
  }
  Changed[k0] = chg;
  TT[k0 + out_ttOff] = tt_min;

#ifdef  DO_TWICE

  if (chg == 0) return;

  //mem_fence();
  //barrier(CLK_LOCAL_MEM_FENCE);
  barrier(CLK_GLOBAL_MEM_FENCE);

  chg_star = 0;
  t0 = TT[k0 + ttOff];
  tt_min = t0;
    
  // check each node in forward star
  for (l = 0; l < nfs; ++l) {
    int oi, oj, ok;
    is = i + Offset[0+l*3]; if (is < 0) continue; if (is >= nx) continue;
    js = j + Offset[1+l*3]; if (js < 0) continue; if (js >= ny) continue;
    ks = k + Offset[2+l*3]; if (ks < 0) continue; if (ks >= nz) continue;
    oi = is - i;
    oj = js - j;
    ok = ks - k;
    dist = 10.0*sqrt( (float) (oi*oi + oj*oj + ok*ok) );
    k0s = is + js * sy + ks * sz;
    delay = 0.5*(u0 + U[k0s]) * dist;

#ifdef  UPDATE_FORWARD_STAR_TT
    t = t0 + delay;
    if (t < TT[k0s]) {
       chg_star = 1;
       TT[k0s] = t;
    }
#endif

    t = TT[k0s + ttOff] + delay;
    // if distance is smaller update
    if (t < t0) {
      chg = 1;
      t0 = t;
      tt_min = t;
    }
  }
  TT[k0 + out_ttOff] = tt_min;
#endif

#ifdef  DO_THRICE

  if (chg == 0) return;

  barrier(CLK_GLOBAL_MEM_FENCE);

  chg_star = 0;
  t0 = TT[k0 + ttOff];
  tt_min = t0;
    
  // check each node in forward star
  for (l = 0; l < nfs; ++l) {
    int oi, oj, ok;
    is = i + Offset[0+l*3]; if (is < 0) continue; if (is >= nx) continue;
    js = j + Offset[1+l*3]; if (js < 0) continue; if (js >= ny) continue;
    ks = k + Offset[2+l*3]; if (ks < 0) continue; if (ks >= nz) continue;
    oi = is - i;
    oj = js - j;
    ok = ks - k;
    dist = 10.0*sqrt( (float) (oi*oi + oj*oj + ok*ok) );
    k0s = is + js * sy + ks * sz;
    delay = 0.5*(u0 + U[k0s]) * dist;

#ifdef  UPDATE_FORWARD_STAR_TT
    t = t0 + delay;
    if (t < TT[k0s]) {
       chg_star = 1;
       TT[k0s] = t;
    }
#endif

    t = TT[k0s + ttOff] + delay;
    // if distance is smaller update
    if (t < t0) {
      chg = 1;
      t0 = t;
      tt_min = t;
    }
  }
  TT[k0 + out_ttOff] = tt_min;
#endif

}
