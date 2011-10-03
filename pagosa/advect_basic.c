static inline void copy_Array(int id, float Array[], float Array_l_[])
{
}

void advect_basic(int id,
                  float Array[],
                  float Eps[],
                  float dFlux[],
                  int lVol[],
                  int lUpw[],
                  int lDnw[]
                  )
{
   const int lft = 0;
   const int rgt = 1;

   const float zero[2] = {0.0f, 0.0f};

   // local variables
   float Array_l_[5];
   float Dnw[rgt-lft+1];
   float Don[rgt-lft+1];
   float Upw[rgt-lft+1];
   float correct[rgt-lft+1];
   float flux[rgt-lft+1];

   // where do these variables come from?
   float Eps3 = 1.0f;
   float Eps4 = 1.0f;
   float TmpA = 1.0f;
   float TmpB = 1.0f;
   int Maskc = 1;

   copy_Array(id, Array, Array_l_);

   Dnw[lft] = lVol[lft] ? Array_l_[ 0] : Array_l_[-1];
   Dnw[rgt] = lVol[rgt] ? Array_l_[ 1] : Array_l_[ 0];
   Don[lft] = lVol[lft] ? Array_l_[-1] : Array_l_[ 0];
   Don[rgt] = lVol[rgt] ? Array_l_[ 0] : Array_l_[ 1];
   Upw[lft] = lVol[lft] ? Array_l_[-2] : Array_l_[ 1];
   Upw[rgt] = lVol[rgt] ? Array_l_[-1] : Array_l_[ 2];

   //... Compute the adjacent cell value differences on either of side flux face
   //
   Upw[lft] = lUpw[lft] ? Don[lft] - Upw[lft] : zero[lft];
   Upw[rgt] = lUpw[rgt] ? Don[rgt] - Upw[rgt] : zero[rgt];
   Dnw[lft] = lDnw[lft] ? Dnw[lft] - Don[lft] : zero[lft];
   Dnw[rgt] = lDnw[rgt] ? Dnw[rgt] - Don[rgt] : zero[rgt];

   //... Compute derivative correction term
   //
   correct[lft] = (Upw[lft]*Dnw[lft] > zero[lft]) ? Eps[lft] : zero[lft];
   correct[rgt] = (Upw[rgt]*Dnw[rgt] > zero[rgt]) ? Eps[rgt] : zero[rgt];

   //... Upwind with Young/van Leer 3rd order gradient limiter
   //
   Upw[lft] = SIGN(correct[lft], Dnw[lft]) *
              MIN(ABS(Upw[lft]), ABS(Dnw[lft]), Eps3*ABS(Upw[lft]) + Eps4*ABS(Dnw[lft]));
   Upw[rgt] = SIGN(correct[rgt], Dnw[rgt]) *
              MIN(ABS(Upw[rgt]), ABS(Dnw[rgt]), Eps3*ABS(Upw[rgt]) + Eps4*ABS(Dnw[rgt]));

   //... Material flux at interfaces
   //
   flux[lft] = dFlux[lft] * (Don[lft] + Upw[lft]);
   flux[rgt] = dFlux[rgt] * (Don[rgt] + Upw[rgt]);

   //... Update Array cell values (boundaries fixed later in set_ghosts)
   //
   if (Maskc) {
      Array[0] = TmpB * (Array_l_[0] * TmpA + (flux[lft] - flux[rgt]));
   }
   
}
