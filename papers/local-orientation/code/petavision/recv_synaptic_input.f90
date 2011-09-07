module variable_filter_mod
   use WeightParams
   use WeightType

contains

   subroutine recv_synaptic_input(G, A, W)
      implicit none
      integer :: G(NXEX,NYEX)
      real :: A(NX,NY)
      type(WeightPatch) :: W(NX,NY)
      integer :: i, j, ip, jp

      ! assumes activity==1
      do j = 1, NY
         do i = 1, NX
            if (A(i,j) > 0.0) then
               do jp = -NPAD, NPAD
                  do ip = -NPAD, NPAD
                     G(i+NPAD+ip,j+NPAD+jp) = G(i+NPAD+ip,j+NPAD+jp) + W(i,j)%p(ip,jp)   ! *A(i,j)
                  end do
               end do
            end if
         end do
      end do
   end subroutine recv_synaptic_input

   subroutine init_weights(W)
      implicit none
      type(WeightPatch) :: W(NX,NY)
      integer :: i, j

      W(1,1)%p = 0.0
      W(1,1)%p(:,0) = 1
      W(1,1)%p(0,:) = 1
      W(1,1)%p = W(1,1)%p/sum(W(1,1)%p)         

      do j = 1, NY
         do i = 1, NX
            W(i,j)%p = 1
         end do
      end do

      print *, nxp, nyp, NPAD
      do i = -1, 1
         print *, "W==", W(2,2)%p(i,-1:1)
      end do
      print *

   end subroutine init_weights

   subroutine init_activity(A, limit)
      implicit none
      real :: A(NX,NY), limit

      call random_number(A)
      where (A > limit)
         A = 1.0
      else where
         A = 0.0
      end where

      print *, A(1,:8)
      print *, A(2,:8)
      print *, A(3,:8)

   end subroutine init_activity

end module variable_filter_mod


program test_recv_synaptic_input
   use variable_filter_mod
   use OpenCL
   use Timer_mod
   implicit none

   integer :: status

   real(c_float),     target  :: A(NX,NY)      ! pre-synaptic activity
!   real(c_float),     target  :: G(NXEX,NYEX)  ! post-synaptic conductance
   integer(c_int),     target  :: G(NXEX,NYEX)  ! post-synaptic conductance
   type(WeightPatch), target  :: W(NX,NY)      ! weights

   integer(c_int),     target  :: G_CPU(NXEX,NYEX)

   type(CLDevice) :: device
   type(CLKernel) :: k_recv, k_update
   type(CLBuffer) :: d_A, d_G, d_W

   integer(c_size_t) :: nxLocal=NXL, nyLocal=NXL
   integer(c_size_t) :: filter_mem_size    = NXP*NYP * NX*NY*SIZE_ELEMENT
   integer(c_size_t) :: global_mem_size    = NX*NY*SIZE_ELEMENT
   integer(c_size_t) :: global_ex_mem_size = NXEX*NYEX*SIZE_ELEMENT
   integer(c_size_t) :: tile_mem_size      = (NXL+2*NPAD)*(NYL+2*NPAD) * SIZE_ELEMENT

   type(CPUTimer) :: timer
   real(c_double) :: cpu_time, gpu_time

   integer :: nxGlobal=NX, nyGlobal=NY
   integer :: ii, jj
   integer :: device_id, d_time, nLoops=100, nWarm=20
   logical :: check_results
   real :: bandwidth, throughput, flops, avg_host, avg_device

   check_results = .false.

   if (NXL < 2*NPAD .or. NYL < 2*NPAD) then
      print *, "thread work group size is too small, die!!!"
      stop 1
   end if

   device_id = 0  ! 0=GPU, 1=CPU
   status = init(device, device_id)
!   status = query(device)

   call limitLocalSize(device, nxLocal, nyLocal)
   print *, "device_id   ==", device_id
   print *, "local size  ==", nxLocal, nyLocal
   print *, "global size ==", NX, NY
   print *, "tile_mem_size   ==", tile_mem_size
   print *, "global_mem_size ==", global_mem_size
   print *, "filter_mem_size ==", filter_mem_size

   ! initialize memory
   !

   G = 0;
   call init_weights(W)
   call init_activity(A, .99)

   ! create memory buffers
   !
   d_A = createBuffer(device, CL_MEM_READ_ONLY  + CL_MEM_COPY_HOST_PTR, global_mem_size,    c_loc(A))
   d_G = createBuffer(device,                   + CL_MEM_COPY_HOST_PTR, global_ex_mem_size, c_loc(G))
   d_W = createBuffer(device, CL_MEM_READ_ONLY  + CL_MEM_COPY_HOST_PTR, filter_mem_size,    c_loc(W(1,1)))

   ! create the kernel
   !
   k_recv = createKernel(device, &
                         "recv_synaptic_input.cl" // C_NULL_CHAR, &
                         "recv_synaptic_input"    // C_NULL_CHAR)

   k_update = createKernel(device, &
                           "recv_synaptic_input.cl" // C_NULL_CHAR, &
                           "layer_update"           // C_NULL_CHAR)

   ! add arguments
   !
   status = setKernelArgInt(k_recv, 0, NXP)
   status = setKernelArgInt(k_recv, 1, NYP)
   status = setKernelArgMem(k_recv, 2, clMemObject(d_A))
   status = setKernelArgMem(k_recv, 3, clMemObject(d_G))
   status = setKernelArgMem(k_recv, 4, clMemObject(d_W))

   status = setKernelArgMem(k_update, 0, clMemObject(d_G))

   ! warmup the kernel
   !
   print *, "warmup"
   do ii = 1, nWarm
      status = run(k_update, NX, NY, nxLocal, nyLocal)
      status = run(k_recv,   NX, NY, nxLocal, nyLocal)
   end do
   status = clFinish(k_update%commands)
   status = clFinish(k_recv%commands)

   ! run the kernel on the device
   !

   print *
   print *, "Measuring flops and effective bandwidth for GPU computation:"
   call init(timer)
   call start(timer)
   do ii = 1, nLoops
      status = run(k_update, NX, NY, nxLocal, nyLocal)
      status = run(k_recv,   NX, NY, nxLocal, nyLocal)
   end do
   status = clFinish(k_update%commands)
   status = clFinish(k_recv%commands)
   call stop(timer)

   gpu_time = elapsed_time(k_recv%timer) + elapsed_time(k_update%timer)
   print *, " submit time    ==   ", real(gpu_time)/nLoops
   gpu_time = elapsed_time(timer)
   print *, "   host time    ==   ", real(gpu_time)/nLoops, " msec (avg)"

   ! 1.0e-9 -> GB, 1000 -> ms, 2 -> to/fro
   throughput = (1.0e-9 * 1000) * nLoops * ((2*global_mem_size+filter_mem_size) / gpu_time)
   print *, "   throughput   ==    ", throughput, "GB/s"

   ! 1.0e-9 -> GFlop, 1000 -> ms, 5 -> 4 sums / 1 div
   flops = (1.0e-9 * 1000) * nLoops * (2*NXP*NYP*NX*NY/gpu_time)
   print *, "   flops        ==    ", flops, "GFlops"

   ! get the results
   !
   status = readBuffer(d_G, c_loc(G), global_ex_mem_size)
   
   avg_device = 0.0
   do ii = 1, NX
      do jj = 1, NY
         avg_device = avg_device + G(ii+NPAD,jj+NPAD)
      end do
   end do

!  warmup/initialize

   call init(timer)
   call start(timer)
   G_CPU = 0
   call recv_synaptic_input(G_CPU, A, W)
   call stop(timer)

   cpu_time = elapsed_time(timer)

   print *, "   host time    ==   ", real(cpu_time), " msec"
   print *, "   SPEEDUP      ==", cpu_time/(gpu_time/nLoops)

   avg_host = 0;
   do ii = 1, NX
      do jj = 1, NY
         avg_host = avg_host + G_CPU(ii+NPAD,jj+NPAD)
         if (G(ii+NPAD,jj+NPAD) /= G_CPU(ii+NPAD,jj+NPAD)) then
            print *, "ERROR FOUND at", ii, jj, "=", G(ii+NPAD,jj+NPAD), G_CPU(ii+NPAD,jj+NPAD)
            STOP
         endif
      end do
   end do

   print *, "averages(host,device)==", avg_host/(NX*NY), avg_device/(NX*NY)

   print *, G(NX/2,:8), "   ..."
   print *, G_CPU(NX/2,:8), "   ..."

 end program test_recv_synaptic_input
