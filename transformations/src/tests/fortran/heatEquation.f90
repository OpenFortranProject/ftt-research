
! 1D heat flow

!$ VECTORIZE ELEMENTAL
elemental real function halve(x) result(y)
   real, intent(in) :: x
   y = x/2.0
end function halve


!$ VECTORIZE
pure subroutine advance(T, lb, rb)

  real, dimension(:), intent(inout) :: T
  real, intent(in) :: lb, rb
  real :: l, r
  interface 
    elemental real function halve(x) result(y)
       real, intent(in) :: x
    end function halve
  end interface
      
  T = halve(EOSHIFT(T, -1, lb) + eoshift(T,  1, rb))

!  T = 0.5 *(  eoshift(T, shift=-1, boundary=lb) &
!            + eoshift(T, shift= 1, boundary=rb) &
!           )

end subroutine advance


!OUTPUT is given below for SPU_app_gen

!#include "spu_intrinsics.h" 
!#include "simdmath.h" 
!#include "FTT_SPU.h" 
!#include "alf_accel.h" 
!typedef int integer;

!inline vec_float4 halve(vec_float4 x)
!{
!  vec_float4 y;
!  y = divf4(x,spu_splats(((float )2.0)));
!  return y;
!}

!int spu_advance(void *p_task_context __attribute__ ((unused)),void *p_parm_ctx_buffer,void *p_input_buffer,void *p_output_buffer,void *p_inout_buffer,unsigned int current_count __attribute__ ((unused)),unsigned int total_count __attribute__ ((unused)))
!{
!  vec_float4 *T;
!  vec_float4 lb;
!  vec_float4 rb;
!  vec_float4 l;
!  vec_float4 r;
!  unsigned int i, count, iIn, iInout, iOut;
!  unsigned int sIn, sInout, sOut;
!  iIn = 0;
!  iOut = 0;
!  iInout = 0;
!  count = ((unsigned int )p_parm_ctx_buffer) / 4;
!  sIn = 0;
!  sOut = 0;
!  sInout = 0;
!  lb = ((vec_float4 )( *(p_input_buffer + sIn)));
!  sIn = sIn + sizeof(vec_float4 );
! rb = ((vec_float4 )( *(p_input_buffer + sIn)));
!  sIn = sIn + sizeof(vec_float4 );
!  T = ((vec_float4 *)p_inout_buffer) + (count * iInout++ + sInout);
!{
!    vec_float4 vT = T[0];
!    vec_float4 vTm1 = lb;
!    vec_float4 temp = T[0];
!    vec_float4 vTp1 = T[1];
!    T[0] = halve(spu_add(spu_shuffle(vTm1,vT,VSHIFT_RIGHT),spu_shuffle(vT,vTp1,VSHIFT_LEFT)));
!    for (i = 1; i < count - 1; i++) {
!      vT = T[i];
!      vTm1 = temp;
!      temp = T[i];
!     vT = T[i];
!      vTp1 = T[i + 1];
!      T[i] = halve(spu_add(spu_shuffle(vTm1,vT,VSHIFT_RIGHT),spu_shuffle(vT,vTp1,VSHIFT_LEFT)));
!    }
!    vT = T[count - 1];
!    vTm1 = temp;
!    vT = T[count - 1];
!    vTp1 = rb;
!    T[count - 1] = halve(spu_add(spu_shuffle(vTm1,vT,VSHIFT_RIGHT),spu_shuffle(vT,vTp1,VSHIFT_LEFT)));
!  }
!  return 0;
!}

