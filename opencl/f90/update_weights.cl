#define AMP_LTD   (1.1f)
#define AMP_LTP   (1.0f)
#define TAU_LTD   (20.0f)
#define TAU_LTP   (20.0f)

#undef USE_LOCAL_MEMORY


/**
 * copy shifted region of A to the patch
 */
static inline void shift_to_patch(__local  float * patch,
                                  __global float * A,
                                  int xShift, yShift, int nPad)
{
   uint kxl, kyl;

   const uint pStride = (uint)get_local_size(0); // patch is not padded for shift
   const uint aStride = (uint)get_global_size(0) + 2*nPad;

   // offset into extended array based on thread group location
   //
   const int offset =   ( (uint)get_group_id(0) * (uint)get_local_size(0) + nPad )
                      + ( (uint)get_group_id(1) * (uint)get_local_size(1) + nPad ) * aStride;

   // copy shifted array to patch (all threads participate)
   //
   kxl = get_local_id(0);
   kyl = get_local_id(1);

   patch[kxl + kyl*pStride] = A[(kxl-xShift) + (kyl-yShift)*aStride + offset];
}


/**
 * OpenCL kernel to update weight STDP decrement variable
 */
__kernel void update_weight_decr (int nPad, float dt,
    __global float * APost,
    __global float * M,
    __local  float * patch )
{
   const unsigned int k   = get_global_id(0) + get_global_id(1)*get_global_size(0);
   const unsigned int kl  = get_local_id (0) + get_local_id (1)*get_local_size (0);

   __local float * pAPost = &patch[0];
   __local float * pM     = &patch[get_local_size()*get_local_size(1)*sizeof(float)];

   const float decayLTD = exp(-dt / TAU_LTD);

   // copy to local scratch memory
   //
#ifdef USE_LOCAL_MEMORY
   shift_to_patch(pAPost, APost, 0, 0, 0);
   shift_to_patch(pM, M, 0, 0, 0);
   barrier(CLK_LOCAL_MEM_FENCE);
#endif

   // TODO:
   // both pDecr and activity are extended regions (plus margins)
   // to make processing them together simpler

   // update M
   //

#ifdef USE_LOCAL_MEMORY
   float m = pM[kl];
   float a = pAPost[kl];
#else
   float m = M[k];
   float a = APost[k];
#endif

   M[k] = decayLTD*m - AMP_LTD*a;
}


/**
 * OpenCL kernel to update weights using STDP
 */
__kernel void update_weights (int nPad, int nxp, int nyp, int nfp,
    __global float * A,
    __global float * M,
    __global float * P,
    __global float * W )
{
   const unsigned int kx = get_global_id(0);
   const unsigned int ky = get_global_id(1);
   const unsigned int k  = kx + ky*get_global_size(0);

   const float dt = 0.5f;

   // TODO:
   // both pDecr and activity are extended regions (plus margins)
   // to make processing them together simpler


   // update M
   //

   const float decayLTD = exp(-dt / TAU_LTD);

   M[k] = decayLTD*M[k] - AMP_LTD*A[k];

   // update P
   //

   const float decayLTP = exp(-dt / TAU_LTP);

   int nk  = nfp*nxp;  // one line in x at a time
   int sy  = nfp*nxp;  // TODO - reduced patches

   for (int y = 0; y < nyp; y++) {
      int k;
      for (k = 0; k < nk; k++) {
//         P[k] = decayLTP * P[k] + AMP_LTD * aPre;
      }
   }


//int pvpatch_update_plasticity_incr(int nk, float * RESTRICT p,
//                                   float aPre, float decay, float ltpAmp)

//         pvpatch_update_plasticity_incr(nk, P + y * sy, preActivity, decayLTP, ampLTP);

#ifdef NOTME

   const int numExtended = pre->clayer->numExtended;
   assert(numExtended == numWeightPatches(axonId));

   const pvdata_t * preLayerData = pre->getLayerData();

   // this stride is in extended space for post-synaptic activity and
   // STDP decrement variable
   const int postStrideY = post->clayer->numFeatures
                         * (post->clayer->loc.nx + 2 * post->clayer->loc.nPad);

   for (int kPre = 0; kPre < numExtended; kPre++) {
      PVAxonalArbor * arbor = axonalArbor(kPre, axonId);

      const float preActivity = preLayerData[kPre];

      PVPatch * pIncr   = arbor->plasticIncr;
      PVPatch * w       = arbor->weights;
      size_t postOffset = arbor->offset;

      const float * postActivity = &post->getLayerData()[postOffset];
      const float * M = &pDecr->data[postOffset];  // STDP decrement variable
      float * P = pIncr->data;                     // STDP increment variable
      float * W = w->data;

      int nk  = pIncr->nf * pIncr->nx; // one line in x at a time
      int ny  = pIncr->ny;
      int sy  = pIncr->sy;

      // TODO - unroll

      // update Psij (pIncr variable)
      // we are processing patches, one line in y at a time
      for (int y = 0; y < ny; y++) {
         pvpatch_update_plasticity_incr(nk, P + y * sy, preActivity, decayLTP, ampLTP);
      }

      // update weights
      for (int y = 0; y < ny; y++) {
         pvpatch_update_weights(nk, W, M, P, preActivity, postActivity, dWMax, wMin, wMax);
         //
         // advance pointers in y
         W += sy;
         P += sy;
         //
         // postActivity and M are extended layer
         postActivity += postStrideY;
         M += postStrideY;
      }
   }
#endif

}
