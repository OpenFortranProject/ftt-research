/**
 * C kernel for sweep of forward star implementation
 */

#define UPDATE_FORWARD_STAR_TT
#undef  DOUBLE_BUFFER
#undef DO_TWICE

// TODO: look up how to place in constant memory
__kernel void sweep_db ( const int nx, const int ny
		      , const int nz, const int nfs
		      , __global const float * U
		      , __global float * TT
		      , __constant int * Offset
		      , __global int * Changed
		      , const int step)
{
  int l, is, js, ks, k0s;
  int chg, chg_star;
  float dist, delay, t;
  float t0, tt_min;

  // double buffering offsets
  int ttOff, out_ttOff;

  const int xSize = get_global_size(0);
  const int ySize = get_global_size(1);
  const int zSize = get_global_size(2);

  int i = get_global_id(0);
  int j = get_global_id(1);
  int k; // two-dimensional parallel decomposition */

  // make sure that the thread id is within the bounds of the array
  if (i >= nx || j >= ny)  return;

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

  // begin algorithm
  chg = 0;
  chg_star = 0;

  for (k = 0; k < nz; k++) {
    const k0 = i + j*sy + k*sz;
    float u0 = U[k0];

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

    // why does this make it faster?
    barrier(CLK_GLOBAL_MEM_FENCE);
  }

#ifdef  DO_TWICE

  if (chg == 0) return;

  //mem_fence();
  //barrier(CLK_LOCAL_MEM_FENCE);

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

#endif /* DO_TWICE */

}
