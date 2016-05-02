/**
 * C kernel for sweep of forward star implementation
 */

#define UPDATE_FORWARD_STAR_TT
#undef SWEEP
#undef DO_TWICE

// TODO: look up how to place in constant memory
__kernel void sweep_axes ( const int nx, const int ny
		         , const int nz, const int nfs
		         , __global const float * U
		         , __global float * TT
		         , __constant int * Offset
		         , __global int * Changed
                         , const int xSize
                         , const int ySize
                         , const int iStart
                         , const int jStart
		         , const int kStart
                         , const int axis
                         , const int step)
{
  int i, j, k, idx;
  int l, is, js, ks, k0s;
  int axisSize;
  int chg, chg_star;
  float dist, delay, t;
  float t0, tt_min;

  // make sure that the thread id is within the bounds of the array
  //
  if (axis == 0) {                       // sweep in x
     j = get_global_id(0);
     k = get_global_id(1);
     if (j >= ny || k >= nz)  return;
     axisSize = nx;
  }
  else if (axis == 1) {                  // sweep in y
     i = get_global_id(0);
     k = get_global_id(1);
     if (i >= nx || k >= nz)  return;
     axisSize = ny;
  }
  else if (axis == 2) {                  // sweep in z
     i = get_global_id(0);
     j = get_global_id(1);
     if (i >= nx || j >= ny)  return;
     axisSize = nz;

#ifdef SWEEP
     if (step % 4 == 1) {
        i = nx - i - 1;
     }
     else if (step % 4 == 2) {
        j = ny - j - 1;
     }
     else if (step % 4 == 3) {
        i = nx - i - 1;
        j = ny - j - 1;
     }
#endif
  }

  const int sx = 1;
  const int sy = sx * xSize;
  const int sz = sy * ySize;

  // begin algorithm
  chg = 0;
  chg_star = 0;

  for (idx = 0; idx < axisSize; idx++) {
    if (axis == 0) {
      i = idx;
      if (idx <= iStart) i = iStart - idx;   // sweep backwards from starting point
    }
    else if (axis == 1) {
      j = idx;
      if (idx <= jStart) j = jStart - idx;   // sweep backwards from starting point
    }
    else if (axis == 2) {
      k = idx;
      if (idx <= kStart) k = kStart - idx;   // sweep backwards from starting point
    }

    const k0 = i + j*sy + k*sz;
    float u0 = U[k0];

    t0 = TT[k0];
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

/** barrier doesn't seem to be needed
 *
 *    barrier(CLK_GLOBAL_MEM_FENCE);
 */
  }

#ifdef  DO_TWICE

  if (chg == 0) return;

  //mem_fence();
  //barrier(CLK_LOCAL_MEM_FENCE);

  chg_star = 0;
  t0 = TT[k0];
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

    t = TT[k0s] + delay;
    // if distance is smaller update
    if (t < t0) {
      chg = 1;
      t0 = t;
      tt_min = t;
    }
  }
  TT[k0] = tt_min;

#endif /* DO_TWICE */

}
