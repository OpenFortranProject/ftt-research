module HyPerLayer
implicit none


contains

!$FTT vectorize
elemental real function xPos(idx, nx, x0, dx, numFeatures) 
    integer, intent(in) :: idx, nx, numFeatures
    real,    intent(in) :: dx, x0
        
    xPos = x0 + dx*(0.5 + MOD(idx/numFeatures, nx))
    
end function xPos

!$FTT vectorize
elemental real function yPos(idx, nx, y0, dy, numFeatures) 
    integer, intent(in) :: idx, nx, numFeatures
    real,    intent(in) :: dy, y0
        
    yPos = y0 + dy*(0.5 + (idx/(nx*numFeatures)))

end function yPos

!$FTT vectorize
elemental real function sign1(x) 
    real, intent(in) :: x
    
    if (x < 0.0) then
        sign1 = -1.0
    else
        sign1 = 1.0
    end if

end function sign1

!--
! Returns difference between two numbers assuming periodic boundary conditions.
! IMPORTANT NOTE - assumes abs(x2-x1) < 2*max and max > 0
! @x2 first number
! @x2 second number
! @max maximum difference
!--
!$FTT vectorize
elemental real function deltaWithPBC(x1, x2, max) result(dx)
    real, intent(in) :: x1, x2, max
    real :: abs_dx

    dx = x2 - x1
    abs_dx = ABS(dx)

    ! Apply periodic boundary conditions
    if (abs_dx > max) then
        dx = sign1(dx) * (abs_dx - 2.0*max)
    end if

end function deltaWithPBC

!$FTT vectorize
elemental integer function featureIndex(idx, numFeatures) 
    integer, intent(in) :: idx, numFeatures
        
    featureIndex = MOD(idx, numFeatures)

end function featureIndex

!$FTT vectorize
elemental integer function globalIndex(kf, x, y, x0, y0, dx, dy, nx, numFeatures) 
    real, intent(in)    :: x, y, x0, y0, dx, dy
    integer, intent(in) :: kf, nx, numFeatures
    integer :: kx, ky    
    real, parameter :: FP_INT_CORR = 0.5

    ky = INT( (y - y0)/dy - 0.5 + FP_INT_CORR )
    globalIndex = (kx + nx*ky)*numFeatures + kf
    
end function globalIndex

!$FTT vectorize
elemental real function gaussianWeight(x0, x, sigma, max) 
    real, intent(in) :: x0, x, sigma, max
    real :: dx

    dx = deltaWithPBC(x0, x, max)
    gaussianWeight = EXP(-0.5 * dx * dx / (sigma * sigma))

end function gaussianWeight

end module HyperLayer

! OUTPUT is given below for SPU_app_gen
!#include "spu_intrinsics.h" 
!#include "simdmath.h" 
!#include "FTT_SPU.h" 
!#include "alf_accel.h" 
!typedef int integer;

!inline vec_float4 xPos(vec_int4 idx,vec_int4 nx,vec_float4 x0,vec_float4 dx,vec_int4 numFeatures)
!{
!  vec_float4 xPos_rtn;
!  xPos_rtn = spu_madd(dx,spu_add(spu_splats(((float )0.5)),mod_i4(div_i4(idx,numFeatures),nx)),x0);
!  return xPos_rtn;
!}
!
!
!inline vec_float4 yPos(vec_int4 idx,vec_int4 nx,vec_float4 y0,vec_float4 dy,vec_int4 numFeatures)
!{
!  vec_float4 yPos_rtn;
!  yPos_rtn = spu_madd(dy,spu_add(div_i4(idx,mul_i4(nx,numFeatures)),spu_splats(((float )0.5))),y0);
!  return yPos_rtn;
!}
!
!
!inline vec_float4 sign1(vec_float4 x)
!{
!  vec_float4 sign1_rtn;
!  sign1_rtn = spu_sel(spu_splats(((float )1.0)),(-1.0),spu_cmpgt(spu_splats(((float )0.0)),x));
!  return sign1_rtn;
!}
!
!
!inline vec_float4 deltaWithPBC(vec_float4 x1,vec_float4 x2,vec_float4 max)
!{
!  vec_float4 dx;
!  vec_float4 abs_dx;
!  dx = spu_sub(x2,x1);
!  abs_dx = fabsf4(dx);
!  dx = spu_sel(dx,spu_mul(sign1(dx),spu_msub(spu_splats(((float )2.0)),max,abs_dx)),spu_cmpgt(abs_dx,max));
!  return dx;
!}
!
!
!inline vec_int4 featureIndex(vec_int4 idx,vec_int4 numFeatures)
!{
!  vec_int4 featureIndex_rtn;
!  featureIndex_rtn = mod_i4(idx,numFeatures);
!  return featureIndex_rtn;
!}
!
!
!inline vec_int4 globalIndex(vec_int4 kf,vec_float4 x,vec_float4 y,vec_float4 x0,vec_float4 y0,vec_float4 dx,vec_float4 dy,vec_int4 nx,vec_int4 numFeatures)
!{
!  vec_int4 globalIndex_rtn;
!  vec_int4 kx;
!  vec_int4 ky;
!  vec_float4 FP_INT_CORR = spu_splats(((float )0.5));
!  ky = spu_convts(spu_add(spu_sub(divf4(spu_sub(y,y0),dy),spu_splats(((float )0.5))),FP_INT_CORR),0U);
!  globalIndex_rtn = madd_i4(madd_i4(nx,ky,kx),numFeatures,kf);
!  return globalIndex_rtn;
!}
!
!
!inline vec_float4 gaussianWeight(vec_float4 x0,vec_float4 x,vec_float4 sigma,vec_float4 max)
!{
!  vec_float4 gaussianWeight_rtn;
!  vec_float4 dx;
!  dx = deltaWithPBC(x0,x,max);
!  gaussianWeight_rtn = expf4(spu_sub(0,divf4(spu_mul(spu_mul(spu_splats(((float )0.5)),dx),dx),spu_mul(sigma,sigma))));
!  return gaussianWeight_rtn;
!}

