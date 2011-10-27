module convolve_mod
   use FilterParams
   use FilterType

contains

!... convolve kernel function using Locally Oriented Programming extensions
!
!   CONCURRENT subroutine convolve_lope(S, Image, F)
!      implicit none
!      real, intent(out) :: S(:,:)
!      real, intent(in ) :: Image(:,:)
!      HALO(NPAD:*:NPAD,NPAD:*:NPAD) :: Image
!      real, intent(in ) :: F(-NPAD:NPAD,-NPAD:NPAD)
!
!      S(0,0) = sum( F*Image )
!
!   end subroutine convolve_lope
!

   subroutine convolve_cpu_loops(S, Image, F)
      implicit none
      real, intent(out) :: S(:,:)
      real, intent(in ) :: Image(:,:)
      type(FilterPatch) :: F
      real :: val
      integer :: i, j, ip, jp

print *, NX, NY

      do j = 1, NY
         do i = 1, NX
            val = 0.0
            do jp = -NPAD, NPAD
               do ip = -NPAD, NPAD
                  val = val + F%p(ip,jp)*Image(i+NPAD,j+NPAD)
               end do
            end do
            S(i,j) = val
         end do
      end do
   end subroutine convolve_cpu_loops

   subroutine convolve_cpu_omp(S, Image, F)
      implicit none
      real, intent(out) :: S(:,:)
      real, intent(in ) :: Image(:,:)
      type(FilterPatch) :: F
      real :: val
      integer :: i, j, ip, jp

!$OMP PARALLEL PRIVATE(val)
!$OMP DO
      do j = 1, NY
         do i = 1, NX
            val = 0.0
            do jp = -NPAD, NPAD
               do ip = -NPAD, NPAD
                  val = val + F%p(ip,jp)*Image(i+NPAD,j+NPAD)
               end do
            end do
            S(i,j) = val
         end do
      end do
!$OMP END DO
!$OMP END PARALLEL

   end subroutine convolve_cpu_omp

   subroutine convolve_cpu_forall(S, Image, F)
      implicit none
      real :: S(:,:), Image(:,:)
      type(FilterPatch) :: F
      real :: val
      integer :: i, j, ip, jp

      forall (i=1:NX, j=1:NY)
!            val = 0.0
!            forall (ip=-NPAD:NPAD, jp=-NPAD:NPAD)
!            do jp = -NPAD, NPAD
!               do ip = -NPAD, NPAD
!                   val = val + F%p(ip,jp)*Image(i+NPAD,j+NPAD)
!               end do
!            end do
!            end forall
            S(i,j) = 0.0
      end forall
   end subroutine convolve_cpu_forall

   subroutine init_filter(F)
      implicit none
      type(FilterPatch) :: F
      integer :: i

      F%p = 0.0
!      F%p(0,0) = 1
      F%p(:,0) = 1
      F%p(0,:) = 1

      F%p = 1.0

      F%p = F%p/sum(F%p)         

      print *, nxp, nyp, NPAD
      do i = -1, 1
         print *, "F==", F%p(i,-1:1)
      end do

    end subroutine init_filter

   subroutine init_image(nxex, nyex, I)
      use file_io
      implicit none
      integer, intent(in) :: nxex, nyex
      real(C_FLOAT), intent(out) :: I(nxex,nyex)

      print *, "will call read_image_file", nxex, nyex
      call read_image_file("lena-sjooblom.jpg", nxex, nyex, I)

      print *, "second row input image"
      print *, I(2,1:5)
      print *, I(1,1:5)

    end subroutine init_image

   subroutine write_image(filename, nx, ny, I)
      use file_io
      implicit none
      character(C_CHAR) :: filename(*)
      integer, intent(in) :: nx, ny
      real(C_FLOAT), intent(in) :: I(nx,ny)

      call write_image_file(filename, nx, ny, I)

    end subroutine write_image

end module convolve_mod


program test_convolve
   use convolve_mod
   use OpenCL
   use Timer_mod
   implicit none

   integer :: status

   real(c_float),     target :: I(NXEX,NYEX)  ! image
   real(c_float),     target :: S(NX,NY)      ! smoothed image
   type(FilterPatch), target :: F             ! filter

   type(CLDevice) :: device
   type(CLKernel) :: kernel
   type(CLBuffer) :: d_I, d_S, d_F

   integer(c_size_t) :: nxLocal=NXL, nyLocal=NXL
   integer(c_size_t) :: filter_mem_size    = NXP*NYP * SIZE_ELEMENT
   integer(c_size_t) :: global_mem_size    = NX*NY*SIZE_ELEMENT
   integer(c_size_t) :: global_ex_mem_size = NXEX*NYEX*SIZE_ELEMENT
   integer(c_size_t) :: tile_mem_size      = (NXL+2*NPAD)*(NYL+2*NPAD) * SIZE_ELEMENT

   type(CPUTimer) :: timer
   real(c_double) :: cpu_time, gpu_time

   integer :: nxGlobal=NX, nyGlobal=NY
   integer :: ii, jj
   integer :: device_id, d_time, nLoops=1, nWarm=20
   logical :: check_results
   real :: bandwidth, throughput, flops

   check_results = .false.

   ! initialize memory
   !

   I = 1.0;  S = 0.0;
   call init_filter(F)
   call init_image(NXEX, NYEX, I)

   call write_image("lena-orig.tiff" // C_NULL_CHAR, NXEX, NYEX, I)

   call convolve_cpu_loops(S, I, F)
   call write_image("lena-new.tiff" // C_NULL_CHAR, NX, NY, S)

   stop

   if (NXL < 2*NPAD .or. NYL < 2*NPAD) then
      print *, "thread work group size is too small, die!!!"
      stop 1
   end if

   device_id = 0  ! 0=GPU, 1=CPU
   status = init(device, device_id)
   call limitLocalSize(device, nxLocal, nyLocal)
   print *, "device_id   ==", device_id
   print *, "local size  ==", nxLocal, nyLocal
   print *, "global size ==", NX, NY
   print *, "tile_mem_size   ==", tile_mem_size
   print *, "global_mem_size ==", global_mem_size

   ! create memory buffers
   !
   d_I = createBuffer(device, CL_MEM_READ_ONLY  + CL_MEM_COPY_HOST_PTR, global_ex_mem_size, c_loc(I))
   d_S = createBuffer(device, CL_MEM_WRITE_ONLY + CL_MEM_COPY_HOST_PTR, global_mem_size,    c_loc(S))
   d_F = createBuffer(device, CL_MEM_READ_ONLY  + CL_MEM_COPY_HOST_PTR, filter_mem_size,    c_loc(F))

   ! create the kernel
   !
   kernel = createKernel(device, &
                         "convolve.cl" // C_NULL_CHAR, &
                         "convolve"    // C_NULL_CHAR)

   ! add arguments
   !
   status = setKernelArgInt(kernel, 0, NXP)
   status = setKernelArgInt(kernel, 1, NYP)
   status = setKernelArgMem(kernel, 2, clMemObject(d_I))
   status = setKernelArgMem(kernel, 3, clMemObject(d_S))
   status = setKernelArgMem(kernel, 4, clMemObject(d_F))
   status = setKernelArgLoc(kernel, 5, 1*tile_mem_size)

   ! warmup the kernel
   !
   print *, "warmup"
   do ii = 1, nWarm
      status = run(kernel, NX, NY, nxLocal, nyLocal)
   end do
   status = clFinish(kernel%commands)

   ! run the kernel on the device
   !

   print *
   print *, "Measuring flops and effective bandwidth for GPU computation:"
   call init(timer)
   call start(timer)
   do ii = 1, nLoops
      status = run(kernel, NX, NY, nxLocal, nyLocal)
   end do
   status = clFinish(kernel%commands)
   call stop(timer)

   gpu_time = elapsed_time(kernel%timer)
   print *, " submit time    ==   ", real(gpu_time)/nLoops
   gpu_time = elapsed_time(timer)
   print *, "   host time    ==   ", real(gpu_time)/nLoops, " msec (avg)"

   ! 1.0e-9 -> GB, 1000 -> ms, 2 -> to/fro
   throughput = (1.0e-9 * 1000) * nLoops * (2*global_mem_size / gpu_time)
   print *, "   throughput   ==    ", throughput, "GB/s"

   ! 1.0e-9 -> GFlop, 1000 -> ms, 5 -> 4 sums / 1 div
   flops = (1.0e-9 * 1000) * nLoops * (2*NXP*NYP*NX*NY/gpu_time)
   print *, "   flops        ==    ", flops, "GFlops"

   ! get the results
   !
   status = readBuffer(d_S, c_loc(S), global_mem_size)
   
   print *, S(1, 1:3)
!   do ii = 1, NX
!      do jj = 1, NY
!         if (abs(S(ii,jj) -1.0) > .000001) then
!            print *, "ERROR FOUND at", ii, jj, "=", S(ii,jj)
!            goto 10
!         endif
!      end do
!   end do

   ! warmup
10 call convolve_cpu_loops(S, I, F)

   print *
   print *, "running convolve_cpu_loops on CPU"
   call init(timer)
   call start(timer)
   call convolve_cpu_loops(S, I, F)
   call stop(timer)

   cpu_time = elapsed_time(timer)
   print *, S(2, 1:3)
   print *, "   host time    ==   ", real(cpu_time), " msec"
   print *, "   SPEEDUP      ==", cpu_time/(gpu_time/nLoops)

   ! warmup
   call convolve_cpu_omp(S, I, F)

   print *
   print *, "running convolve_cpu_omp on CPU"
   call init(timer)
   call start(timer)
   call convolve_cpu_omp(S, I, F)
   call stop(timer)

   cpu_time = elapsed_time(timer)
   print *, S(1, 1:3)
   print *, "   host time omp==   ", real(cpu_time), " msec"
   print *, "   SPEEDUP      ==", cpu_time/(gpu_time/nLoops)

!   do ii = 1, NX
!      do jj = 1, NY
!         if (abs(S(ii,jj) -1.0) > .000001) then
!            print *, "ERROR FOUND at", ii, jj, "=", S(ii,jj)
!            STOP
!         endif
!      end do
!   end do

end program test_convolve
