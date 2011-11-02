/**
 *  Copyright (c) 2011, Los Alamos National Security, LLC.
 *  All rights Reserved.
 *
 *  Copyright 2011. Los Alamos National Security, LLC. This software was produced 
 *  under U.S. Government contract DE-AC52-06NA25396 for Los Alamos National 
 *  Laboratory (LANL), which is operated by Los Alamos National Security, LLC 
 *  for the U.S. Department of Energy. The U.S. Government has rights to use, 
 *  reproduce, and distribute this software.  NEITHER THE GOVERNMENT NOR LOS 
 *  ALAMOS NATIONAL SECURITY, LLC MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR 
 *  ASSUMES ANY LIABILITY FOR THE USE OF THIS SOFTWARE.  If software is modified
 *  to produce derivative works, such modified software should be clearly marked,
 *  so as not to confuse it with the version available from LANL.
 *
 *  Additionally, redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the Los Alamos National Security, LLC, Los Alamos 
 *       National Laboratory, LANL, the U.S. Government, nor the names of its 
 *       contributors may be used to endorse or promote products derived from 
 *       this software without specific prior written permission.
 *  
 *  THIS SOFTWARE IS PROVIDED BY THE LOS ALAMOS NATIONAL SECURITY, LLC AND 
 *  CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT 
 *  NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL LOS ALAMOS NATIONAL
 *  SECURITY, LLC OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 *  OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 *  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 *  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 *  
 *  CLAMR -- LA-CC-11-094
 *  This research code is being developed as part of the 
 *  2011 X Division Summer Workshop for the express purpose
 *  of a collaborative code for development of ideas in
 *  the implementation of AMR codes for Exascale platforms
 *  
 *  AMR implementation of the Wave code previously developed
 *  as a demonstration code for regular grids on Exascale platforms
 *  as part of the Supercomputing Challenge and Los Alamos 
 *  National Laboratory
 *  
 *  Authors: Bob Robey       XCP-2   brobey@lanl.gov
 *           Matt Bement     XCP-1   bement@lanl.gov
 *           Neal Davis              davis68@lanl.gov, davis68@illinois.edu
 *           David Nicholaeff        dnic@lanl.gov, mtrxknight@aol.com
 *           Dennis Trujillo         dptrujillo@lanl.gov, dptru10@gmail.com
 * 
 */
#ifndef GPU_DOUBLE_SUPPORT
#define GPU_DOUBLE_SUPPORT
#ifdef HAVE_CL_DOUBLE
#pragma OPENCL EXTENSION cl_khr_fp64 : enable
typedef double  real;
typedef double4 real4;
typedef double8 real8;
#define ZERO 0.0
#define HALF 0.5
#define ONE  1.0
#define GRAVITATIONAL_CONSTANT 9.80
#define THOUSAND 1000.0
#define EPSILON 1.0e-30
#else
typedef float   real;
typedef float4  real4;
typedef float8  real8;
#define ZERO 0.0f
#define HALF 0.5f
#define ONE  1.0f
#define GRAVITATIONAL_CONSTANT 9.80f
#define THOUSAND 1000.0f
#define EPSILON 1.0f-30
#endif
#endif

//#ifdef __APPLE_CC__
//#define max(a,b) ((a) > (b) ? (a) : (b))
//#define fabs(a) ( (a) < 0 ? -(a) : a)
//#endif
void setup_tile(__local        real4  *tile,
                __local        int8   *itile,
                __global const real   *H,
                __global const real   *U,
                __global const real   *V,
                __global const int    *nlft,
                __global const int    *nrht,
                __global const int    *ntop,
                __global const int    *nbot,
                __global const int    *level
                );

void apply_BCs(__local  real4        *tile,
               __local  int8         *itile,
               __global const real   *H,
               __global const real   *U,
               __global const real   *V,
               __global const int    *nlft,
               __global const int    *nrht,
               __global const int    *ntop,
               __global const int    *nbot);

#ifndef SET_TILE_VARIABLES
#define SET_TILE_VARIABLES
//  Define macros for local tile access.

#define Hval(i)     ( tile[i].s0 )
#define Uval(i)     ( tile[i].s1 )
#define Vval(i)     ( tile[i].s2 )



#define Hptrval(i)     ( (*tile)[i].s0 )

#define nlftval(i)  ( itile[i].s0 )
#define nrhtval(i)  ( itile[i].s1 )
#define ntopval(i)  ( itile[i].s2 )
#define nbotval(i)  ( itile[i].s3 )
#define levelval(i) ( itile[i].s4 )
#define mpotval(i)  ( itile[i].s5 )
#endif

#define SQR(x)      ( (x)*(x) )
#define MIN3(a,b,c) ( min(min((a),(b)),(c)) )

#define HXFLUX(ic)  ( U_old[ic] )
#define UXFLUX(ic)  ( SQR(U_old[ic])/H_old[ic] + ghalf*SQR(H_old[ic]) )
#define UVFLUX(ic)  ( U_old[ic]*V_old[ic]/H_old[ic] )

#define HXFLUXIC ( Uic )
#define HXFLUXNL ( Ul )
#define HXFLUXNR ( Ur )
#define HXFLUXNB ( Ub )
#define HXFLUXNT ( Ut )

#define UXFLUXIC ( SQR(Uic)/Hic + ghalf*SQR(Hic) )
#define UXFLUXNL ( SQR(Ul)/Hl + ghalf*SQR(Hl) )
#define UXFLUXNR ( SQR(Ur)/Hr + ghalf*SQR(Hr) )
#define UXFLUXNB ( SQR(Ub)/Hb + ghalf*SQR(Hb) )
#define UXFLUXNT ( SQR(Ut)/Ht + ghalf*SQR(Ht) )

#define UVFLUXIC ( Uic*Vic/Hic )
#define UVFLUXNL ( Ul*Vl/Hl )
#define UVFLUXNR ( Ur*Vr/Hr )
#define UVFLUXNB ( Ub*Vb/Hb )
#define UVFLUXNT ( Ut*Vt/Ht )

#define HYFLUX(ic)  ( V_old[ic] )
#define VUFLUX(ic)  ( V_old[ic]*U_old[ic]/H_old[ic] )
#define VYFLUX(ic)  ( SQR(V_old[ic])/H_old[ic] + ghalf*SQR(H_old[ic]) )

#define HYFLUXIC ( Vic )
#define HYFLUXNL ( Vl )
#define HYFLUXNR ( Vr )
#define HYFLUXNB ( Vb )
#define HYFLUXNT ( Vt )

#define VUFLUXIC  ( Vic*Uic/Hic )
#define VUFLUXNL  ( Vl*Ul/Hl )
#define VUFLUXNR  ( Vr*Ur/Hr )
#define VUFLUXNB  ( Vb*Ub/Hb )
#define VUFLUXNT  ( Vt*Ut/Ht )

#define VYFLUXIC  ( SQR(Vic)/Hic + ghalf*SQR(Hic) )
#define VYFLUXNL  ( SQR(Vl)/Hl + ghalf*SQR(Hl) )
#define VYFLUXNR  ( SQR(Vr)/Hr + ghalf*SQR(Hr) )
#define VYFLUXNB  ( SQR(Vb)/Hb + ghalf*SQR(Hb) )
#define VYFLUXNT  ( SQR(Vt)/Ht + ghalf*SQR(Ht) )

#define HNEWXFLUXMINUS  ( Uxminus )
#define HNEWXFLUXPLUS   ( Uxplus )
#define UNEWXFLUXMINUS  ( SQR(Uxminus)/Hxminus + ghalf*SQR(Hxminus) )
#define UNEWXFLUXPLUS   ( SQR(Uxplus) /Hxplus +  ghalf*SQR(Hxplus)  )
#define UVNEWFLUXMINUS  ( Uxminus*Vxminus/Hxminus )
#define UVNEWFLUXPLUS   ( Uxplus *Vxplus /Hxplus  )

#define HNEWYFLUXMINUS  ( Vyminus )
#define HNEWYFLUXPLUS   ( Vyplus  )
#define VNEWYFLUXMINUS  ( SQR(Vyminus)/Hyminus + ghalf*SQR(Hyminus) )
#define VNEWYFLUXPLUS   ( SQR(Vyplus) /Hyplus  + ghalf*SQR(Hyplus)  )
#define VUNEWFLUXMINUS  ( Vyminus*Uyminus/Hyminus )
#define VUNEWFLUXPLUS   ( Vyplus *Uyplus /Hyplus )



__kernel void calc_one_cycle_cl(
                 const int    ncells,   // 0  Total number of cells.
                 const int    levmx,    // 1  Maximum level
        __global const real  *H_old,    // 2  
        __global const real  *U_old,    // 3  
        __global const real  *V_old,    // 4  
        __global       real  *H_new,    // 5  
        __global       real  *U_new,    // 6  
        __global       real  *V_new,    // 7  
        __global const int   *nlft,     // 8  Array of left neighbors.
        __global const int   *nrht,     // 9  Array of right neighbors.
        __global const int   *ntop,     // 10  Array of bottom neighbors.
        __global const int   *nbot,     // 11  Array of top neighbors.
        __global const int   *level,    // 12  Array of level information.
                 const real   deltaT,   // 14  Size of time step.
        __global const real  *lev_dx,   // 15
        __global const real  *lev_dy,   // 16
        __local        real4 *tile,     // 17  Tile size in real4.
        __local        int8  *itile)    // 18  Tile size in int8.
{

//////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////
////////////////////                       ///////////////////////
////////////////////   calc_one_cycle_cl   ///////////////////////
////////////////////                       ///////////////////////
//////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////



   /////////////////////////////////////////////
   /// Get thread identification information ///
   /////////////////////////////////////////////

   const unsigned int giX  = get_global_id(0);
   const unsigned int tiX  = get_local_id(0);
   
   const unsigned int ngX  = get_global_size(0);
   const unsigned int ntX  = get_local_size(0);
   
   const unsigned int group_id = get_group_id(0);
    
   // Ensure the executing thread is not extraneous
   if(giX >= ncells)
      return;

   /////////////////////////////////////////////
   /////////////////////////////////////////////
   /////////////////////////////////////////////

   setup_tile(tile, itile, H_old, U_old, V_old, nlft, nrht, ntop, nbot, level);

// barrier (CLK_LOCAL_MEM_FENCE);

   apply_BCs(tile, itile, H_old, U_old, V_old, nlft, nrht, ntop, nbot);

   barrier (CLK_LOCAL_MEM_FENCE);

   /////////////////////////////////////////////////
   /// Declare all constants and local variables ///
   /////////////////////////////////////////////////

   const real   g     = GRAVITATIONAL_CONSTANT;   // gravitational constant
   const real   ghalf = HALF*g;

   // Local values for left neighbor, right neighbor, ..., left-left neighbor, right-
   // right neighbor, ... left-top neighbor, right-top neighbor, ..., and level
   int nl, nr, nb, nt;
   int nll, nrr, nbb, ntt;
   int nlt, nrt, nbr, ntr;
   int lvl;

   // Widths and x-axis fluxes across control volume
   real dxminus, dxplus;
   real Hxminus, Hxplus;
   real Uxminus, Uxplus;
   real Vxminus, Vxplus;
   
   // Heights and y-axis fluxes across control volume
   real dyminus, dyplus;
   real Hyminus, Hyplus;
   real Uyminus, Uyplus;
   real Vyminus, Vyplus;

   // Variables for TVD method 
   real duminus1, duminus2;
   real duplus1, duplus2;
   real duhalf1, duhalf2;
   real rdenom, rnumplus, rnumminus, rminus, rplus;
   real nu, q, cv, wminusx, wplusx, wminusy, wplusy;

   real qmax, qpot;

   int inum;

   // Local values for the state variables and cell widths and heights for the local cell as well
   // as its neighboring cells
   real dxic, dxl, dxr, dyic, dyb, dyt;
   real Hic, Hl, Hr, Hb, Ht;
   real Hll, Hrr, Hbb, Htt;
   real Uic, Ul, Ur, Ub, Ut;
   real Ull, Urr, Ubb, Utt;
   real Vic, Vl, Vr, Vb, Vt;
   real Vll, Vrr, Vbb, Vtt;

   /////////////////////////////////////////////////
   /////////////////////////////////////////////////
   /////////////////////////////////////////////////

// barrier(CLK_LOCAL_MEM_FENCE);

   /////////////////////////////////////////////////////////////
   /////////////////////////////////////////////////////////////
   ///                                                       ///
   /// Parallelizing the for() loop of State::calc_one_cycle ///
   ///                                                       ///
   /////////////////////////////////////////////////////////////
   /////////////////////////////////////////////////////////////


   //////////////////////////
   /// Set the local tile ///
   //////////////////////////

   int start_idx = group_id * ntX;
   int end_idx = (group_id + 1) * ntX;

   int ic = giX;

   lvl  = levelval(tiX);

   dxic = lev_dx[lvl];
   dyic = lev_dy[lvl];

   nl = nlftval(tiX);
   nr = nrhtval(tiX);
   nb = nbotval(tiX);
   nt = ntopval(tiX);

   Hic  = Hval(tiX);
   Uic  = Uval(tiX);
   Vic  = Vval(tiX);

   //////////////////////////
   //////////////////////////
   //////////////////////////

// barrier(CLK_LOCAL_MEM_FENCE);

   //////////////////////////////
   //////////////////////////////
   //////////////////////////////


   // Setting the left and left-left neighbor state values for the control volume based
   // on the state variables of the actual left, left-left, and left-top neighbor cells

   // Using global access for the left neighbor values
   if(nl < 0) {
      nl = abs(nl);
      nll = nlft[nl];
      dxl = lev_dx[level[nl]];
      Hl = H_old[nl];
      Ul = U_old[nl];
      Vl = V_old[nl];

      inum = 1;
   
      if (level[nl] < lvl) {
         Hll = H_old[nl];
         Ull = U_old[nl];
         Vll = V_old[nl];
      }
      else if (level[nl] == lvl) {
         Hll = H_old[nll];
         Ull = U_old[nll];
         Vll = V_old[nll];
         if (level[nll] > level[nl]) {
            Hll += H_old[ntop[nll]];
            Ull += U_old[ntop[nll]];
            Vll += V_old[ntop[nll]];
            inum++;
         }
      }
      else if (level[nl] > lvl) {
         nlt = ntop[nl];
         Hl = HALF * (Hl + H_old[nlt]);
         Ul = HALF * (Ul + U_old[nlt]);
         Vl = HALF * (Vl + V_old[nlt]);
         Hll = H_old[nll];
         Ull = U_old[nll];
         Vll = V_old[nll];
         Hll += H_old[nlft[nlt]];
         Ull += U_old[nlft[nlt]];
         Vll += V_old[nlft[nlt]];
         inum++;
      }
   }
   // Using local access for the left neighbor
   else {
      nll = nlftval(nl);
      dxl = lev_dx[levelval(nl)];
      Hl = Hval(nl);
      Ul = Uval(nl);
      Vl = Vval(nl);

      inum = 1;

      if(levelval(nl) < lvl) {
         Hll = Hl;
         Ull = Ul;
         Vll = Vl;
      }
      else if (levelval(nl) == lvl) {
         if(nll >= 0) {
            Hll = Hval(nll);
            Ull = Uval(nll);
            Vll = Vval(nll);
            if (levelval(nll) > levelval(nl)) {
               int nllt = ntopval(nll);
               if (nllt >= 0) {
                  Hll += Hval(nllt);
                  Ull += Uval(nllt);
                  Vll += Vval(nllt);
               }
               else {
                  nllt = abs(nllt);
                  Hll += H_old[nllt];
                  Ull += U_old[nllt];
                  Vll += V_old[nllt];
               }
               inum++;
            }
         }
         else { // Going global for left-left neighbor
            nll = abs(nll);
            Hll = H_old[nll];
            Ull = U_old[nll];
            Vll = V_old[nll];
            if (level[nll] > levelval(nl)) {
               Hll += H_old[ntop[nll]];
               Ull += U_old[ntop[nll]];
               Vll += V_old[ntop[nll]];
               inum++;
            }
         }
      }
      // The left neighbor is more refined than the current cell
      else if (levelval(nl) > lvl) {
         nlt = ntopval(nl);
         if(nll >= 0) {
            Hll = Hval(nll);
            Ull = Uval(nll);
            Vll = Vval(nll);
         }
         else {
            nll = abs(nll);
            Hll = H_old[nll];
            Ull = U_old[nll];
            Vll = V_old[nll];
         }
         if(nlt >= 0) {
            Hl = HALF * (Hl + Hval(nlt));
            Ul = HALF * (Ul + Uval(nlt));
            Vl = HALF * (Vl + Vval(nlt));
            int nltl = nlftval(nlt);
            if (nltl >=0) {
               Hll += Hval(nltl);
               Ull += Uval(nltl);
               Vll += Vval(nltl);
            }
            else {
               nltl = abs(nltl);
               Hll += H_old[nltl];
               Ull += U_old[nltl];
               Vll += V_old[nltl];
            }
            inum++;
         }
         else {
            nlt = abs(nlt);
            Hl = HALF * (Hl + H_old[nlt]);
            Ul = HALF * (Ul + U_old[nlt]);
            Vl = HALF * (Vl + V_old[nlt]);
            Hll += H_old[nlft[nlt]];
            Ull += U_old[nlft[nlt]];
            Vll += V_old[nlft[nlt]];
            inum++;
         }
      }
   }
   Hll /= (real)inum;
   Ull /= (real)inum;
   Vll /= (real)inum; 
   /////////////////////////////////////////////////////////////////////////////////////////////////


   // Setting the right and right-right neighbor state values for the control volume based
   // on the state variables of the actual right, right-right, and right-top neighbor cells

   // Using global access for the right neighbor values
   if(nr < 0) {
      nr = abs(nr);
      nrr = nrht[nr];  
      dxr = lev_dx[level[nr]] ;
      Hr = H_old[nr];
      Ur = U_old[nr];
      Vr = V_old[nr];
   
      inum = 1;
   
      if (level[nr] < lvl) {
         Hrr = H_old[nr];
         Urr = U_old[nr];
         Vrr = V_old[nr];
      }  
      else if (level[nr] == lvl) {
         Hrr = H_old[nrr];
         Urr = U_old[nrr];
         Vrr = V_old[nrr];
         if (level[nrr] > level[nr]) {
            Hrr += H_old[ntop[nrr]];
            Urr += U_old[ntop[nrr]];
            Vrr += V_old[ntop[nrr]];
            inum++;
         }
      }
      else if (level[nr] > lvl) {
         nrt = ntop[nr];
         Hr = HALF * (Hr + H_old[nrt]);
         Ur = HALF * (Ur + U_old[nrt]);
         Vr = HALF * (Vr + V_old[nrt]);
         Hrr = H_old[nrr];
         Urr = U_old[nrr];
         Vrr = V_old[nrr];
         Hrr += H_old[nrht[nrt]];
         Urr += U_old[nrht[nrt]];
         Vrr += V_old[nrht[nrt]];
         inum++;
      }
   }
   // Using local access for the right neighbor
   else {
      nrr = nrhtval(nr);
      dxr = lev_dx[levelval(nr)];
      Hr = Hval(nr);
      Ur = Uval(nr);
      Vr = Vval(nr);

      inum = 1;

      if (levelval(nr) < lvl) {
         Hrr = Hr;
         Urr = Ur;
         Vrr = Vr;
      }
      else if (levelval(nr) == lvl) {
         if (nrr >= 0) {
            Hrr = Hval(nrr);
            Urr = Uval(nrr);
            Vrr = Vval(nrr);
            if (levelval(nrr) > levelval(nr)) {
               int nrrt = ntopval(nrr);
               if (nrrt >= 0) {
                  Hrr += Hval(nrrt);
                  Urr += Uval(nrrt);
                  Vrr += Vval(nrrt);
               }
               else {
                  nrrt = abs(nrrt);
                  Hrr += H_old[nrrt];
                  Urr += U_old[nrrt];
                  Vrr += V_old[nrrt];
               }
               inum++;
            }
         }
         else { // Going global for right-right neighbor
            nrr = abs(nrr);
            Hrr = H_old[nrr];
            Urr = U_old[nrr];
            Vrr = V_old[nrr];
            if (level[nrr] > levelval(nr)) {
               Hrr += H_old[ntop[nrr]];
               Urr += U_old[ntop[nrr]];
               Vrr += V_old[ntop[nrr]];
               inum++;
            }
         }
      }
      else if (levelval(nr) > lvl) {
         nrt = ntopval(nr);
         if(nrr >= 0) {
            Hrr = Hval(nrr);
            Urr = Uval(nrr);
            Vrr = Vval(nrr);
         }
         else {
            nrr = abs(nrr);
            Hrr = H_old[nrr];
            Urr = U_old[nrr];
            Vrr = V_old[nrr];
         }
         if(nrt >= 0) {
            Hr = HALF * (Hr + Hval(nrt));
            Ur = HALF * (Ur + Uval(nrt));
            Vr = HALF * (Vr + Vval(nrt));
            int nrtr = nrhtval(nrt);
            if (nrtr >=0) {
               Hrr += Hval(nrtr);
               Urr += Uval(nrtr);
               Vrr += Vval(nrtr);
            }
            else {
               nrtr = abs(nrtr);
               Hrr += H_old[nrtr];
               Urr += U_old[nrtr];
               Vrr += V_old[nrtr];
            }
            inum++;
         }
         else {
            nrt = abs(nrt);
            Hr = HALF * (Hr + H_old[nrt]);
            Ur = HALF * (Ur + U_old[nrt]);
            Vr = HALF * (Vr + V_old[nrt]);
            Hrr += H_old[nrht[nrt]];
            Urr += U_old[nrht[nrt]];
            Vrr += V_old[nrht[nrt]];
            inum++;
         }
      }
   }
   Hrr /= (real)inum;
   Urr /= (real)inum;
   Vrr /= (real)inum;
   /////////////////////////////////////////////////////////////////////////////////////////////////


   // Setting the bottom and bottom-bottom neighbor state values for the control volume based
   // on the state variables of the actual bottom, bottom-bottom, and bottom-right neighbor cells

   // Using global access for the bottom neighbor values
   if (nb < 0) {
      nb = abs(nb);
      nbb = nbot[nb]; 
      dyb = lev_dy[level[nb]];
      Hb = H_old[nb];
      Ub = U_old[nb];
      Vb = V_old[nb];

      inum = 1;

      if (level[nb] < lvl) {
         Hbb = H_old[nb];
         Ubb = U_old[nb];
         Vbb = V_old[nb];
      }
      else if (level[nb] == lvl) {
         Hbb = H_old[nbb];
         Ubb = U_old[nbb];
         Vbb = V_old[nbb];
         if (level[nbb] > level[nb]) {
            Hbb += H_old[nrht[nbb]];
            Ubb += U_old[nrht[nbb]];
            Vbb += V_old[nrht[nbb]];
            inum++;
         }
      }
      else if (level[nb] > lvl) {
         nbr = nrht[nb];
         Hb = HALF * (Hb + H_old[nbr]);
         Ub = HALF * (Ub + U_old[nbr]);
         Vb = HALF * (Vb + V_old[nbr]);
         Hbb = H_old[nbb];
         Ubb = U_old[nbb];
         Vbb = V_old[nbb];
         Hbb += H_old[nbot[nbr]];
         Ubb += U_old[nbot[nbr]];
         Vbb += V_old[nbot[nbr]];
         inum++;
      }
   }
   // Using local access for the bottom neighbor
   else {
      nbb = nbotval(nb);
      dyb = lev_dy[levelval(nb)];
      Hb = Hval(nb);
      Ub = Uval(nb);
      Vb = Vval(nb);

      inum = 1;

      if (levelval(nb) < lvl) {
         Hbb = Hb;
         Ubb = Ub;
         Vbb = Vb;
      }
      else if (levelval(nb) == lvl) {
         if (nbb >= 0) {
            Hbb = Hval(nbb);
            Ubb = Uval(nbb);
            Vbb = Vval(nbb);
            if (levelval(nbb) > levelval(nb)) {
               int nbbr = nrhtval(nbb);
               if (nbbr >= 0) {
                  Hbb += Hval(nbbr);
                  Ubb += Uval(nbbr);
                  Vbb += Vval(nbbr);
               }
               else {
                  nbbr = abs(nbbr);
                  Hbb += H_old[nbbr];
                  Ubb += U_old[nbbr];
                  Vbb += V_old[nbbr];
               }
               inum++;
            }
         }
         else { // Going global for bottom-bottom neighbor
            nbb = abs(nbb);
            Hbb = H_old[nbb];
            Ubb = U_old[nbb];
            Vbb = V_old[nbb];
            if (level[nbb] > levelval(nb)) {
               Hbb += H_old[nrht[nbb]];
               Ubb += U_old[nrht[nbb]];
               Vbb += V_old[nrht[nbb]];
               inum++;
            }
         }
      }
      else if (levelval(nb) > lvl) {
         nbr = nrhtval(nb);
         if(nbb >= 0) {
            Hbb = Hval(nbb);
            Ubb = Uval(nbb);
            Vbb = Vval(nbb);
         }
         else {
            nbb = abs(nbb);
            Hbb = H_old[nbb];
            Ubb = U_old[nbb];
            Vbb = V_old[nbb];
         }
         if(nbr >= 0) {
            Hb = HALF * (Hb + Hval(nbr));
            Ub = HALF * (Ub + Uval(nbr));
            Vb = HALF * (Vb + Vval(nbr));
            int nbrb = nbotval(nbr);
            if (nbrb >=0) {
               Hbb += Hval(nbrb);
               Ubb += Uval(nbrb);
               Vbb += Vval(nbrb);
            }
            else {
               nbrb = abs(nbrb);
               Hbb += H_old[nbrb];
               Ubb += U_old[nbrb];
               Vbb += V_old[nbrb];
            }
            inum++;
         }
         else {
            nbr = abs(nbr);
            Hb = HALF * (Hb + H_old[nbr]);
            Ub = HALF * (Ub + U_old[nbr]);
            Vb = HALF * (Vb + V_old[nbr]);
            Hbb += H_old[nbot[nbr]];
            Ubb += U_old[nbot[nbr]];
            Vbb += V_old[nbot[nbr]];
            inum++;
         }
      }
   }
   Hbb /= (real)inum;
   Ubb /= (real)inum;
   Vbb /= (real)inum;
   /////////////////////////////////////////////////////////////////////////////////////////////////


   // Setting the top and top-top neighbor state values for the control volume based
   // on the state variables of the actual top, top-top, and top-right neighbor cells
  
   // Using global access for the top neighbor values
   if (nt < 0) {
      nt = abs(nt);
      ntt = ntop[nt]; 
      dyt = lev_dy[level[nt]];
      Ht = H_old[nt];
      Ut = U_old[nt];
      Vt = V_old[nt];

      inum = 1;

      if (level[nt] < lvl) {
         Htt = H_old[nt];
         Utt = U_old[nt];
         Vtt = V_old[nt];
      }
      else if (level[nt] == lvl) {
         Htt = H_old[ntt];
         Utt = U_old[ntt];
         Vtt = V_old[ntt];
         if (level[ntt] > level[nt]) {
            Htt += H_old[nrht[ntt]];
            Utt += U_old[nrht[ntt]];
            Vtt += V_old[nrht[ntt]];
            inum++;
         }
      }
      else if (level[nt] > lvl) {
         ntr = nrht[nt];
         Ht = HALF * (Ht + H_old[ntr]);
         Ut = HALF * (Ut + U_old[ntr]);
         Vt = HALF * (Vt + V_old[ntr]);
         Htt = H_old[ntt];
         Utt = U_old[ntt];
         Vtt = V_old[ntt];
         Htt += H_old[ntop[ntr]];
         Utt += U_old[ntop[ntr]];
         Vtt += V_old[ntop[ntr]];
         inum++;
      }
   }
   // Using local access for the top neighbor
   else {
      ntt = ntopval(nt);
      dyt = lev_dy[levelval(nt)];
      Ht = Hval(nt);
      Ut = Uval(nt);
      Vt = Vval(nt);

      inum = 1;

      if (levelval(nt) < lvl) {
         Htt = Ht;
         Utt = Ut;
         Vtt = Vt;
      }
      else if (levelval(nt) == lvl) {
         if (ntt >= 0) {
            Htt = Hval(ntt);
            Utt = Uval(ntt);
            Vtt = Vval(ntt);
            if (levelval(ntt) > levelval(nt)) {
               int nttr = nrhtval(ntt);
               if (nttr >= 0) {
                  Htt += Hval(nttr);
                  Utt += Uval(nttr);
                  Vtt += Vval(nttr);
               }
               else {
                  nttr = abs(nttr);
                  Htt += H_old[nttr];
                  Utt += U_old[nttr];
                  Vtt += V_old[nttr];
               }
               inum++;
            }
         }
         else { // Going global for top-top neighbor
            ntt = abs(ntt);
            Htt = H_old[ntt];
            Utt = U_old[ntt];
            Vtt = V_old[ntt];
            if (level[ntt] > levelval(nt)) {
               Htt += H_old[nrht[ntt]];
               Utt += U_old[nrht[ntt]];
               Vtt += V_old[nrht[ntt]];
               inum++;
            }
         }
      }
      else if (levelval(nt) > lvl) {
         ntr = nrhtval(nt);
         if(ntt >= 0) {
            Htt = Hval(ntt);
            Utt = Uval(ntt);
            Vtt = Vval(ntt);
         }
         else {
            ntt = abs(ntt);
            Htt = H_old[ntt];
            Utt = U_old[ntt];
            Vtt = V_old[ntt];
         }
         if(ntr >= 0) {
            Ht = HALF * (Ht + Hval(ntr));
            Ut = HALF * (Ut + Uval(ntr));
            Vt = HALF * (Vt + Vval(ntr));
            int ntrt = ntopval(ntr);
            if (ntrt >=0) {
               Htt += Hval(ntrt);
               Utt += Uval(ntrt);
               Vtt += Vval(ntrt);
            }
            else {
               ntrt = abs(ntrt);
               Htt += H_old[ntrt];
               Utt += U_old[ntrt];
               Vtt += V_old[ntrt];
            }
            inum++;
         }
         else {
            ntr = abs(ntr);
            Ht = HALF * (Ht + H_old[ntr]);
            Ut = HALF * (Ut + U_old[ntr]);
            Vt = HALF * (Vt + V_old[ntr]);
            Htt += H_old[ntop[ntr]];
            Utt += U_old[ntop[ntr]];
            Vtt += V_old[ntop[ntr]];
            inum++;
         }
      }
   }
   Htt /= (real)inum;
   Utt /= (real)inum;
   Vtt /= (real)inum;
   /////////////////////////////////////////////////////////////////////////////////////////////////

/**/
/*  
// OLD GLOBAL ACCESS CODE 
   nl = nlft[ic];
   nll = nlft[nl];
   
   dxl = dx[nl];
   Hl = H_old[nl];
   Ul = U_old[nl];
   Vl = V_old[nl];
   
   inum = 1;
   
   if (level[nl] < level[ic]) {
      Hll = H_old[nl];
      Ull = U_old[nl];
      Vll = V_old[nl];
   }
   else if (level[nl] == level[ic]) {
      Hll = H_old[nll];
      Ull = U_old[nll];
      Vll = V_old[nll];
      if (level[nll] > level[nl]) {
         Hll += H_old[ntop[nll]];
         Ull += U_old[ntop[nll]];
         Vll += V_old[ntop[nll]];
         inum++;
      }
   }
   else if (level[nl] > level[ic]) {
      nlt = ntop[nl];
      Hl = HALF * (Hl + H_old[nlt]);
      Ul = HALF * (Ul + U_old[nlt]);
      Vl = HALF * (Vl + V_old[nlt]);
      Hll = H_old[nll];
      Ull = U_old[nll];
      Vll = V_old[nll];
      Hll += H_old[nlft[nlt]];
      Ull += U_old[nlft[nlt]];
      Vll += V_old[nlft[nlt]];
      inum++;
   }
   Hll /= (real)inum;
   Ull /= (real)inum;
   Vll /= (real)inum;

*/
/* 
   nr = nrht[ic];
   nrr = nrht[nr];
   
   dxr = dx[nr];
   Hr = H_old[nr];
   Ur = U_old[nr];
   Vr = V_old[nr];
   
   inum = 1;
   
   if (level[nr] < level[ic]) {
        Hrr = H_old[nr];
      Urr = U_old[nr];
      Vrr = V_old[nr];
   }
   else if (level[nr] == level[ic]) {
      Hrr = H_old[nrr];
      Urr = U_old[nrr];
      Vrr = V_old[nrr];
      if (level[nrr] > level[nr]) {
         Hrr += H_old[ntop[nrr]];
         Urr += U_old[ntop[nrr]];
         Vrr += V_old[ntop[nrr]];
         inum++;
      }
   }
   else if (level[nr] > level[ic]) {
      nrt = ntop[nr];
      Hr = HALF * (Hr + H_old[nrt]);
      Ur = HALF * (Ur + U_old[nrt]);
      Vr = HALF * (Vr + V_old[nrt]);
      Hrr = H_old[nrr];
      Urr = U_old[nrr];
      Vrr = V_old[nrr];
      Hrr += H_old[nrht[nrt]];
      Urr += U_old[nrht[nrt]];
      Vrr += V_old[nrht[nrt]];
      inum++;
   }
   Hrr /= (real)inum;
   Urr /= (real)inum;
   Vrr /= (real)inum;
/**/ 
/* 
   nb = nbot[ic];
   nbb = nbot[nb];
      
   dyb = dy[nb];
   Hb = H_old[nb];
   Ub = U_old[nb];
   Vb = V_old[nb];
   inum = 1;
   if (level[nb] < level[ic]) {
      Hbb = H_old[nb];
      Ubb = U_old[nb];
      Vbb = V_old[nb];
   }
   else if (level[nb] == level[ic]) {
      Hbb = H_old[nbb];
      Ubb = U_old[nbb];
      Vbb = V_old[nbb];
      if (level[nbb] > level[nb]) {
         Hbb += H_old[nrht[nbb]];
         Ubb += U_old[nrht[nbb]];
         Vbb += V_old[nrht[nbb]];
         inum++;
      }
   }
   else if (level[nb] > level[ic]) {
      nbr = nrht[nb];
      Hb = HALF * (Hb + H_old[nbr]);
      Ub = HALF * (Ub + U_old[nbr]);
      Vb = HALF * (Vb + V_old[nbr]);
      Hbb = H_old[nbb];
      Ubb = U_old[nbb];
      Vbb = V_old[nbb];
      Hbb += H_old[nbot[nbr]];
      Ubb += U_old[nbot[nbr]];
      Vbb += V_old[nbot[nbr]];
      inum++;
   }
   Hbb /= (real)inum;
   Ubb /= (real)inum;
   Vbb /= (real)inum;
 /**/
/*     
   nt = ntop[ic];
   ntt = ntop[nt];
     
   dyt = dy[nt];
   Ht = H_old[nt];
   Ut = U_old[nt];
   Vt = V_old[nt];
   
   inum = 1;

   if (level[nt] < level[ic]) {
      Htt = H_old[nt];
      Utt = U_old[nt];
      Vtt = V_old[nt];
   }
   else if (level[nt] == level[ic]) {
      Htt = H_old[ntt];
      Utt = U_old[ntt];
      Vtt = V_old[ntt];
      if (level[ntt] > level[nt]) {
         Htt += H_old[nrht[ntt]];
         Utt += U_old[nrht[ntt]];
         Vtt += V_old[nrht[ntt]];
         inum++;
      }
   }
   else if (level[nt] > level[ic]) {
      ntr = nrht[nt];
      Ht = HALF * (Ht + H_old[ntr]);
      Ut = HALF * (Ut + U_old[ntr]);
      Vt = HALF * (Vt + V_old[ntr]);
      Htt = H_old[ntt];
      Utt = U_old[ntt];
      Vtt = V_old[ntt];
      Htt += H_old[ntop[ntr]];
      Utt += U_old[ntop[ntr]];
      Vtt += V_old[ntop[ntr]];
      inum++;
   }
   Htt /= (real)inum;
   Utt /= (real)inum;
   Vtt /= (real)inum;
/**/
   
   ///////////////////////////////////////////////////////////////////////
   ///                       Lax-Wendroff Method                       ///
   ///////////////////////////////////////////////////////////////////////

   dxminus = (dxic + dxl);
   Hxminus = ( (Hic*dxl+Hl*dxic) + deltaT*(HXFLUXIC-HXFLUXNL) )/dxminus;
   Uxminus = ( (Uic*dxl+Ul*dxic) + deltaT*(UXFLUXIC-UXFLUXNL) )/dxminus;
   Vxminus = ( (Vic*dxl+Vl*dxic) + deltaT*(UVFLUXIC-UVFLUXNL) )/dxminus;
   
   dxplus  = (dxic + dxr);
   Hxplus  = ( (Hic*dxr+Hr*dxic) + deltaT*(HXFLUXNR-HXFLUXIC) )/dxplus;
   Uxplus  = ( (Uic*dxr+Ur*dxic) + deltaT*(UXFLUXNR-UXFLUXIC) )/dxplus;
   Vxplus  = ( (Vic*dxr+Vr*dxic) + deltaT*(UVFLUXNR-UVFLUXIC) )/dxplus;
   
   dyminus = (dyic + dyb);
   Hyminus = ( (Hic*dyb+Hb*dyic) + deltaT*(HYFLUXIC-HYFLUXNB) )/dyminus;
   Uyminus = ( (Uic*dyb+Ub*dyic) + deltaT*(VUFLUXIC-VUFLUXNB) )/dyminus;
   Vyminus = ( (Vic*dyb+Vb*dyic) + deltaT*(VYFLUXIC-VYFLUXNB) )/dyminus;
   
   dyplus  = (dyic + dyt);
   Hyplus  = ( (Hic*dyt+Ht*dyic) + deltaT*(HYFLUXNT-HYFLUXIC) )/dyplus;
   Uyplus  = ( (Uic*dyt+Ut*dyic) + deltaT*(VUFLUXNT-VUFLUXIC) )/dyplus;
   Vyplus  = ( (Vic*dyt+Vt*dyic) + deltaT*(VYFLUXNT-VYFLUXIC) )/dyplus;

   ///////////////////////////////////////////////////////////////////////
   ///////////////////////////////////////////////////////////////////////
   ///////////////////////////////////////////////////////////////////////


   ///////////////////////////////////////////////////////////////////////
   ///                               TVD                               ///
   ///////////////////////////////////////////////////////////////////////
   
   duminus1 = Hl-Hll;
   duminus2 = Ul-Ull;
   duplus1 = Hr-Hic;
   duplus2 = Ur-Uic;
   duhalf1 = Hic-Hl;
   duhalf2 = Uic-Ul;
   rdenom = max(SQR(duhalf1) + SQR(duhalf2),EPSILON);
   rnumplus  = duplus1 *duhalf1 + duplus2 *duhalf2;
   rnumminus = duminus1*duhalf1 + duminus2*duhalf2;
   rplus =rnumplus /rdenom;
   rminus=rnumminus/rdenom;
   q = max(MIN3(ONE, rminus, rplus), ZERO);
   nu=(fabs(Uxminus)+sqrt(g*Hxminus))*deltaT/dxic;
   cv=nu*(ONE-nu);
   wminusx = HALF*cv*(ONE-q);
      
   duminus1 = Hic-Hl;
   duminus2 = Uic-Ul;
   duplus1 = Hrr-Hr;
   duplus2 = Urr-Ur;
   duhalf1 = Hr-Hic;
   duhalf2 = Ur-Uic;
   rdenom = max(SQR(duhalf1) + SQR(duhalf2),EPSILON);
   rnumplus  = duplus1 *duhalf1 + duplus2 *duhalf2;
   rnumminus = duminus1*duhalf1 + duminus2*duhalf2;
   rplus =rnumplus /rdenom;
   rminus=rnumminus/rdenom;
   q = max(MIN3(ONE, rminus, rplus), ZERO);
   nu=(fabs(Uxplus)+sqrt(g*Hxplus))*deltaT/dxic;
   cv=nu*(ONE-nu);
   wplusx = HALF*cv*(ONE-q);
      
   duminus1 = Hb-Hbb;
   duminus2 = Vb-Vbb;
   duplus1 = Ht-Hic;
   duplus2 = Vt-Vic;
   duhalf1 = Hic-Hb;
   duhalf2 = Vic-Vb;
   rdenom = max(SQR(duhalf1) + SQR(duhalf2),EPSILON);
   rnumplus  = duplus1 *duhalf1 + duplus2 *duhalf2;
   rnumminus = duminus1*duhalf1 + duminus2*duhalf2;
   rplus =rnumplus /rdenom;
   rminus=rnumminus/rdenom;
   q = max(MIN3(ONE, rminus, rplus), ZERO);
   nu=(fabs(Vyminus)+sqrt(g*Hyminus))*deltaT/dyic;
   cv=nu*(ONE-nu);
   wminusy = HALF*cv*(ONE-q);
   
   duminus1 = Hic-Hb;
   duminus2 = Vic-Vb;
   duplus1 = Htt-Ht;
   duplus2 = Vtt-Vt;
   duhalf1 = Ht-Hic;
   duhalf2 = Vt-Vic;
   rdenom = max(SQR(duhalf1) + SQR(duhalf2),EPSILON);
   rnumplus  = duplus1 *duhalf1 + duplus2 *duhalf2;
   rnumminus = duminus1*duhalf1 + duminus2*duhalf2;
   rplus =rnumplus /rdenom;
   rminus=rnumminus/rdenom;
   q = max(MIN3(ONE, rminus, rplus), ZERO);
   nu=(fabs(Vyplus)+sqrt(g*Hyplus))*deltaT/dyic;
   cv=nu*(ONE-nu);
   wplusy = HALF*cv*(ONE-q);

   ///////////////////////////////////////////////////////////////////////
   ///////////////////////////////////////////////////////////////////////
   ///////////////////////////////////////////////////////////////////////


   ////////////////////////////////////////////////////////////////////////
   ///        ACTUAL CALCULATIONS COMBINING LAX_WENDROFF AND TVD        ///
   ////////////////////////////////////////////////////////////////////////

   Hic = Hic - deltaT/dxic*( HNEWXFLUXPLUS - HNEWXFLUXMINUS )
      -wminusx*(Hic-Hl)+wplusx*(Hr-Hic)
      - deltaT/dyic*( HNEWYFLUXPLUS - HNEWYFLUXMINUS )
      -wminusy*(Hic-Hb)+wplusy*(Ht-Hic);
   Uic = Uic - deltaT/dxic*( UNEWXFLUXPLUS - UNEWXFLUXMINUS )
      -wminusx*(Uic-Ul)+wplusx*(Ur-Uic)
      - deltaT/dyic*( VUNEWFLUXPLUS - VUNEWFLUXMINUS )
      -wminusy*(Uic-Ub)+wplusy*(Ut-Uic);
   Vic = Vic - deltaT/dxic*( UVNEWFLUXPLUS - UVNEWFLUXMINUS )
      -wminusx*(Vic-Vl)+wplusx*(Vr-Vic)
      - deltaT/dyic*( VNEWYFLUXPLUS - VNEWYFLUXMINUS )
      -wminusy*(Vic-Vb)+wplusy*(Vt-Vic);/**/


   barrier(CLK_LOCAL_MEM_FENCE);
   

    //  Write values for the main cell back to global memory.
    H_new[giX] = Hic;
    U_new[giX] = Uic;
    V_new[giX] = Vic;/**/



//////////////////////////////////////////////////////////////////
////////////////////          END           //////////////////////
////////////////////   calc_one_cycle_cl    //////////////////////
//////////////////////////////////////////////////////////////////

}


__kernel void refine_potential_cl(
                 const int    ncells,   // 0  Total number of cells.
                 const int    levmx,    // 1  Maximum level
        __global const real  *H,        // 2
        __global const real  *U,        // 3
        __global const real  *V,        // 4
        __global const int   *nlft,     // 5  Array of left neighbors.
        __global const int   *nrht,     // 6  Array of right neighbors.
        __global const int   *ntop,     // 7  Array of bottom neighbors.
        __global const int   *nbot,     // 8  Array of top neighbors.
        __global const int   *level,    // 9  Array of level information.
        __global       int   *mpot,     // 10  Array of mesh potential information.
        __global       int   *ioffset,  // 11  Array of new giX offsets.
        __global const real  *lev_dx,   // 12
        __global const real  *lev_dy,   // 13
        __local        real4 *tile,     // 14  Tile size in real4.
        __local        int8  *itile)    // 15  Tile size in int8.
{

//////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////
////////////////////                       ///////////////////////
////////////////////   calc_gradients_cl   ///////////////////////
////////////////////                       ///////////////////////
//////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////

   real qpot = ZERO;
    
   /////////////////////////////////////////////
   /// Get thread identification information ///
   /////////////////////////////////////////////

   const unsigned int giX  = get_global_id(0);
   const unsigned int tiX  = get_local_id(0);

   const unsigned int group_id = get_group_id(0);

   const unsigned int ntX  = get_local_size(0);

   itile[tiX].s0 = 0;

   if(giX >= ncells)
      return;

   setup_tile(tile, itile, H, U, V, nlft, nrht, ntop, nbot, level);

   barrier (CLK_LOCAL_MEM_FENCE);

   int nlt, nrt, nbr, ntr;
   int nl, nr, nb, nt;
   real Hic, Uic, Vic;
   real Hl, Ul, Vl;
   real Hr, Ur, Vr;
   real Hb, Ub, Vb;
   real Ht, Ut, Vt;
   real dxl, dxr, dyb, dyt;

   real duminus1, duminus2;
   real duplus1, duplus2;
   real duhalf1, duhalf2;

   nl = nlftval(tiX);
   nr = nrhtval(tiX);
   nb = nbotval(tiX);
   nt = ntopval(tiX);

   Hic  = Hval(tiX);
   Uic  = Uval(tiX);
   Vic  = Vval(tiX);

   int lvl = levelval(tiX);

   //////////////////////////
   //////////////////////////
   //////////////////////////

   barrier(CLK_LOCAL_MEM_FENCE);

   //////////////////////////////
   //////////////////////////////
   //////////////////////////////


   // Setting the left and left-left neighbor state values for the control volume based
   // on the state variables of the actual left, left-left, and left-top neighbor cells

   // Using global access for the left neighbor values
   if(nl < 0) {
      nl = abs(nl);
      dxl = lev_dx[level[nl]];
      Hl = H[nl];
      Ul = U[nl];
      Vl = V[nl];

      if (level[nl] > lvl) {
         nlt = ntop[nl];
         Hl = HALF * (Hl + H[nlt]);
         Ul = HALF * (Ul + U[nlt]);
         Vl = HALF * (Vl + V[nlt]);
      }
   }
   // Using local access for the left neighbor
   else {
      dxl = lev_dx[level[nl]];
      Hl = Hval(nl);
      Ul = Uval(nl);
      Vl = Vval(nl);

      // The left neighbor is more refined than the current cell
      if (levelval(nl) > lvl) {
         nlt = ntopval(nl);
         if(nlt >= 0) {
            Hl = HALF * (Hl + Hval(nlt));
            Ul = HALF * (Ul + Uval(nlt));
            Vl = HALF * (Vl + Vval(nlt));
         }
         else {
            nlt = abs(nlt);
            Hl = HALF * (Hl + H[nlt]);
            Ul = HALF * (Ul + U[nlt]);
            Vl = HALF * (Vl + V[nlt]);
         }
      }
   }
   /////////////////////////////////////////////////////////////////////////////////////////////////

   // Setting the right and right-right neighbor state values for the control volume based
   // on the state variables of the actual right, right-right, and right-top neighbor cells

   // Using global access for the right neighbor values
   if(nr < 0) {
      nr = abs(nr);
      dxr = lev_dx[level[nr]] ;
      Hr = H[nr];
      Ur = U[nr];
      Vr = V[nr];
   
      if (level[nr] > lvl) {
         nrt = ntop[nr];
         Hr = HALF * (Hr + H[nrt]);
         Ur = HALF * (Ur + U[nrt]);
         Vr = HALF * (Vr + V[nrt]);
      }
   }
   // Using local access for the right neighbor
   else {
      dxr = lev_dx[level[nr]] ;
      Hr = Hval(nr);
      Ur = Uval(nr);
      Vr = Vval(nr);

      if (levelval(nr) > lvl) {
         nrt = ntopval(nr);
         if(nrt >= 0) {
            Hr = HALF * (Hr + Hval(nrt));
            Ur = HALF * (Ur + Uval(nrt));
            Vr = HALF * (Vr + Vval(nrt));
         }
         else {
            nrt = abs(nrt);
            Hr = HALF * (Hr + H[nrt]);
            Ur = HALF * (Ur + U[nrt]);
            Vr = HALF * (Vr + V[nrt]);
         }
      }
   }
   /////////////////////////////////////////////////////////////////////////////////////////////////



   // Setting the bottom and bottom-bottom neighbor state values for the control volume based
   // on the state variables of the actual bottom, bottom-bottom, and bottom-right neighbor cells

   // Using global access for the bottom neighbor values
   if (nb < 0) {
      nb = abs(nb);
      dyb = ONE / lev_dy[level[nb]];
      Hb = H[nb];
      Ub = U[nb];
      Vb = V[nb];

      if (level[nb] > lvl) {
         nbr = nrht[nb];
         Hb = HALF * (Hb + H[nbr]);
         Ub = HALF * (Ub + U[nbr]);
         Vb = HALF * (Vb + V[nbr]);
      }
   }
   // Using local access for the bottom neighbor
   else {
      dyb = ONE / lev_dy[levelval(nb)];
      Hb = Hval(nb);
      Ub = Uval(nb);
      Vb = Vval(nb);

      if (levelval(nb) > lvl) {
         nbr = nrhtval(nb);
         if(nbr >= 0) {
            Hb = HALF * (Hb + Hval(nbr));
            Ub = HALF * (Ub + Uval(nbr));
            Vb = HALF * (Vb + Vval(nbr));
         }
         else {
            nbr = abs(nbr);
            Hb = HALF * (Hb + H[nbr]);
            Ub = HALF * (Ub + U[nbr]);
            Vb = HALF * (Vb + V[nbr]);
         }
      }
   }
   /////////////////////////////////////////////////////////////////////////////////////////////////


   // Setting the top and top-top neighbor state values for the control volume based
   // on the state variables of the actual top, top-top, and top-right neighbor cells
  
   // Using global access for the top neighbor values
   if (nt < 0) {
      nt = abs(nt);
      dyt = ONE / lev_dy[level[nt]];
      Ht = H[nt];
      Ut = U[nt];
      Vt = V[nt];

      if (level[nt] > lvl) {
         ntr = nrht[nt];
         Ht = HALF * (Ht + H[ntr]);
         Ut = HALF * (Ut + U[ntr]);
         Vt = HALF * (Vt + V[ntr]);
      }
   }
   // Using local access for the top neighbor
   else {
      dyt = ONE / lev_dy[levelval(nt)];
      Ht = Hval(nt);
      Ut = Uval(nt);
      Vt = Vval(nt);

      if (levelval(nt) > lvl) {
         ntr = nrhtval(nt);
         if(ntr >= 0) {
            Ht = HALF * (Ht + Hval(ntr));
            Ut = HALF * (Ut + Uval(ntr));
            Vt = HALF * (Vt + Vval(ntr));
         }
         else {
            ntr = abs(ntr);
            Ht = HALF * (Ht + H[ntr]);
            Ut = HALF * (Ut + U[ntr]);
            Vt = HALF * (Vt + V[ntr]);
         }
      }
   }
   /////////////////////////////////////////////////////////////////////////////////////////////////

    //--CALCULATIONS------------------------------------------------------------
    //  Calculate the gradient between the right and left neighbors and the
    //  main cell.
    real invHic = ONE / Hic; //  For faster math.
    real qmax = -THOUSAND;          //  Set the default maximum low to catch the real one.
    
    duplus1 = Hr - Hic;     duplus2 = Ur - Uic;
    duhalf1 = Hic - Hl;     duhalf2 = Uic - Ul;
    qpot = max(fabs(duplus1 * invHic), fabs(duhalf1 * invHic));
    if (qpot > qmax) qmax = qpot;
    
    duminus1= Hic - Hl;     duminus2= Uic - Ul;
    duhalf1 = Hr - Hic;     duhalf2 = Ur - Uic;
    qpot = max(fabs(duminus1 * invHic), fabs(duhalf1 * invHic));
    if (qpot > qmax) qmax = qpot;
    
    //  Calculate the gradient between the top and bottom neighbors and the
    //  main cell.
    duplus1 = Ht - Hic;     duplus2 = Vt - Vic;
    duhalf1 = Hic - Hb;     duhalf2 = Vic - Vb;
    qpot = max(fabs(duplus1 * invHic), fabs(duhalf1 * invHic));
    if (qpot > qmax) qmax = qpot;
    
    duminus1= Hic - Hb;     duminus2= Vic - Vb;
    duhalf1 = Ht - Hic;     duhalf2 = Vt - Vic;
    qpot = max(fabs(duminus1 * invHic), fabs(duhalf1 * invHic));
    if (qpot > qmax) qmax = qpot;


    //--CALCULATIONS------------------------------------------------------------
    //  Refine the mesh if the gradient is large
    int mpotval = 0;
    if (qmax > 0.10 && lvl < levmx)  //  XXX:  eliminate hard-coded vars
    {   mpotval = 1; }
    
    itile[tiX].s0 = mpotval ? 4 : 1;

    barrier(CLK_LOCAL_MEM_FENCE);

    for (int offset = ntX >> 1; offset > 32; offset >>= 1)
    {  if (tiX < offset)
       {  itile[tiX].s0 += itile[tiX+offset].s0; }
       barrier(CLK_LOCAL_MEM_FENCE); }

    //  Unroll the remainder of the loop as 32 threads must proceed in lockstep.
    if (tiX < 32)
    {  itile[tiX].s0 += itile[tiX+32].s0;
       itile[tiX].s0 += itile[tiX+16].s0;
       itile[tiX].s0 += itile[tiX+8].s0;
       itile[tiX].s0 += itile[tiX+4].s0;
       itile[tiX].s0 += itile[tiX+2].s0;
       itile[tiX].s0 += itile[tiX+1].s0; }

    if (tiX == 0) {
      ioffset[group_id] = itile[0].s0;
    }

    /////////////////////
    /// GLOBAL WRITES ///
    /////////////////////

    //  Put the mesh potential on the global array.
    mpot[giX] = mpotval; /**/

}

/* finish_reduction_scan */

__kernel void finish_reduction_scan_cl(
        const    int   isize,
        __global int  *ioffset,
        __global int  *result,
        __local  int  *itile_scratch,
        __local  int  *itile)
{
   const unsigned int tiX  = get_local_id(0);
   const unsigned int ntX  = get_local_size(0);

   int carry = 0;
   int giX = tiX;
   itile[tiX] = ZERO;
   itile_scratch[tiX] = ZERO;

   int nsize = ntX;
   if (isize < ntX) nsize = isize;

   for (int ig = 0; ig < (2*isize-1)/nsize; ig++){
      if (giX <isize) {
         itile[tiX] = ioffset[giX];
         itile_scratch[tiX] = ioffset[giX];
      }
      barrier(CLK_LOCAL_MEM_FENCE);

      for (int offset = 0; offset < ntX; offset++)
      {  if (tiX > offset)
         {  itile[tiX] += itile_scratch[offset]; }
         barrier(CLK_LOCAL_MEM_FENCE);
         }

      if (giX == isize-1){
         result[0] = itile[tiX]+carry;
      }

      if (giX < isize) {
         ioffset[giX] = itile[tiX] + carry - itile_scratch[tiX];
         carry = itile[ntX-1]+carry;
      }

      giX += ntX;
   }
}



void setup_tile(__local        real4  *tile, 
                __local        int8   *itile, 
                __global const real   *H,
                __global const real   *U,
                __global const real   *V,
                __global const int    *nlft,
                __global const int    *nrht,
                __global const int    *ntop,
                __global const int    *nbot,
                __global const int    *level
                )
{
   const unsigned int giX = get_global_id (0);
   const unsigned int tiX = get_local_id (0);

   const unsigned int ntX = get_local_size (0);

   const unsigned int group_id = get_group_id (0);

   int start_idx = group_id * ntX;
   int end_idx = (group_id + 1) * ntX;

   Hval(tiX) = H[giX];
   Uval(tiX) = U[giX];
   Vval(tiX) = V[giX];

   if (nlft[giX] >= start_idx && nlft[giX] < end_idx) {
      // If on block, offset to local index by subtracting start index
      nlftval(tiX) =  nlft[giX] - start_idx;
   } else {
      // If off block, set to negative to indicate global data
      nlftval(tiX) = -nlft[giX];
   }

   if (nrht[giX] >= start_idx && nrht[giX] < end_idx) {
      // If on block, offset to local index by subtracting start index
      nrhtval(tiX) =  nrht[giX] - start_idx;
   } else {
      // If off block, set to negative to indicate global data
      nrhtval(tiX) = -nrht[giX];
   }

   if (ntop[giX] >= start_idx && ntop[giX] < end_idx) {
      // If on block, offset to local index by subtracting start index
      ntopval(tiX) =  ntop[giX] - start_idx;
   } else {
      // If off block, set to negative to indicate global data
      ntopval(tiX) = -ntop[giX];
   }

   if (nbot[giX] >= start_idx && nbot[giX] < end_idx) {
      // If on block, offset to local index by subtracting start index
      nbotval(tiX) =  nbot[giX] - start_idx;
   } else {
      // If off block, set to negative to indicate global data
      nbotval(tiX) = -nbot[giX];
   }

   levelval(tiX) = level[giX];
}

void apply_BCs(__local  real4        *tile, 
               __local  int8         *itile, 
               __global const real   *H,
               __global const real   *U,
               __global const real   *V,
               __global const int    *nlft,
               __global const int    *nrht,
               __global const int    *ntop,
               __global const int    *nbot)
{

   int nr, nl, nt, nb;

   const unsigned int giX = get_global_id (0);
   const unsigned int tiX = get_local_id (0);

   const unsigned int ntX = get_local_size (0);

   const unsigned int group_id = get_group_id (0);

   int start_idx = group_id * ntX;

   ////////////////////////////////////////////////////////////////////////////////////////////
   ///                  Setting the local values for the neighbors                          ///
   ///        If a global read is necessary, the value of neighbor is negated.              ///
   /// If it's a local read, the value is mapped by the relative offset to the start index. ///
   ////////////////////////////////////////////////////////////////////////////////////////////

   //sets left boundary conditions

   int lft_bound = 0;

   // Test for global index -- it will be negative if so
   if (nlftval(tiX) < 0) {
      // set left boundary value if equal to self
      if (abs(nlftval(tiX)) == giX) lft_bound = 1;
   } else {
      // Check local index, but add offset of start_idx
      if (nlftval(tiX) + start_idx == giX) lft_bound = 1;
   }

   if (lft_bound) {
      // if left boundary get right neigbor to update boundary value from
      nr = nrhtval(tiX);

      // Checking for global index or local -- negative is global
      if (nr < 0) {
        // Copy from global data
        Hval(tiX) =  H[abs(nr)];
        Uval(tiX) = -U[abs(nr)];
        Vval(tiX) =  V[abs(nr)];
      } else {
        // Copy from local data
        Hval(tiX) =  Hval(nr);
        Uval(tiX) = -Uval(nr);
        Vval(tiX) =  Vval(nr);
      }
   }

   //sets right boundary conditions

   int rht_bound = 0;

   // Test for global index -- it will be negative if so
   if (nrhtval(tiX) < 0) {
      // set left boundary value if equal to self
      if (abs(nrhtval(tiX)) == giX) rht_bound = 1;
   } else {
      // Check local index, but add offset of start_idx
      if (nrhtval(tiX) + start_idx == giX) rht_bound = 1;
   }

   if (rht_bound) {
      nl = nlftval(tiX);

      if (nl < 0) {
        // Copy from global data
         Hval(tiX) =  H[abs(nl)];
         Uval(tiX) = -U[abs(nl)];
         Vval(tiX) =  V[abs(nl)];
      } else {
        // Copy from local data
         Hval(tiX) =  Hval(nl);
         Uval(tiX) = -Uval(nl);
         Vval(tiX) =  Vval(nl);
      }
   }

   //sets bottom boundary conditions

   int bot_bound = 0;

   // Test for global index -- it will be negative if so
   if (nbotval(tiX) < 0) {
      // set left boundary value if equal to self
      if (abs(nbotval(tiX)) == giX) bot_bound = 1;
   } else {
      // Check local index, but add offset of start_idx
      if (nbotval(tiX) + start_idx == giX) bot_bound = 1;
   }

   if (bot_bound) {
      nt = ntopval(tiX);

      if (nt < 0) {
        // Copy from global data
         Hval(tiX) =  H[abs(nt)];
         Uval(tiX) =  U[abs(nt)];
         Vval(tiX) = -V[abs(nt)];
      } else {
        // Copy from local data
         Hval(tiX) =  Hval(nt);
         Uval(tiX) =  Uval(nt);
         Vval(tiX) = -Vval(nt);
      }
   }

   //sets top boundary conditions

   int top_bound = 0;

   if (ntopval(tiX) < 0) {
      if (abs(ntopval(tiX)) == giX) top_bound = 1;
   } else {
      if (ntopval(tiX) + start_idx == giX) top_bound = 1;
   }

   if (top_bound) {
      nb = nbotval(tiX);

      if (nb < 0) {
         // Copy from global data
         Hval(tiX) =  H[abs(nb)];
         Uval(tiX) =  U[abs(nb)];
         Vval(tiX) = -V[abs(nb)];
      } else {
         // Copy from local data
         Hval(tiX) =  Hval(nb);
         Uval(tiX) =  Uval(nb);
         Vval(tiX) = -Vval(nb);
      }
   }
}

