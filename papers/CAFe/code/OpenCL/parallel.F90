Module Parallel
!=====================================================================
! Module Parallel contains the Message Passing Interface (MPI) basic
! routines for the application.
!
!   ------------------------------------------------------------------
!
!   This software and ancillary information (herein called "SOFTWARE")
!   call Parallel Library is made available under the terms described
!   here.  The SOFTWARE has been approved for release with associated
!   LA-CC number 05-074.
!
!   Unless otherwise indicated, this SOFTWARE has been authored by an 
!   employee of the University of California, operator of Los Alamos
!   National Laboratory under Contract No. W-7405-ENG-36 with the U.S.
!   Department of Energy.  The U.S. Government has rights of use,
!   reproduce, and distribute this SOFTWARE.  The public may copy,
!   distribute, prepare derived works and publicly display this 
!   SOFTWARE without charge, provided that this Notice and any other
!   statements of authorship are reproduced on all copies.  Neither
!   the Government nor the University makes any warranty, express or 
!   implied, or assumes any liability or responsibility for the use
!   of this SOFTWARE.
! 
!   If SOFTWARE is modified to produce derivative works, such modified
!   SOFTWARE should be clearly marked, so as not to confuse it with
!   the version available from LANL.
!
!   ------------------------------------------------------------------
!
!   W. Weseloh, LA-CC-05-074, "Parallel Library"
!
!=====================================================================
 
Implicit None
Save
 
!... Basic MPI information
Integer :: my_id                  ! Rank of this processor
Integer :: numproc                ! Number of processors
 
!... Cartesian decomposition
Integer :: npex = 0               ! Number of processors X direction
Integer :: npey = 0               ! Number of processors Y direction
Integer :: npez = 0               ! Number of processors Z direction
 
!... Cartesian topology
Integer :: MPI_COMM_CART          ! Communicator with Cartesian topology
Integer :: Left, Right            ! Neighbor ranks (x-direction)
Integer :: Bottom, Top            ! Neighbor ranks (y-direction)
Integer :: Front, Back            ! Neighbor ranks (z-direction)
 
!... Generic Broadcast
Interface Parallel_Broadcast
   Module Procedure Broadcast_Integer
   Module Procedure Broadcast_Real
   Module Procedure Broadcast_Logical
   Module Procedure Broadcast_String
End Interface
 
Contains
 
Subroutine Parallel_Start 
!=====================================================================
! 
!        Parallel_Start 
!
! Subroutine Parallel_Start creates the parallel (MPI) process. Serial
! code may be executed before a call to Parallel_Start, however, this
! subroutine may be called only once per simulation.
!
! Author :
!        Wayne Weseloh, Los Alamos National Laboratory
!
! History :
!        2005-03-01, original version
!
! Externals :
!        MPI_INIT, MPI_COMM_RANK, MPI_COMM_SIZE
!
! References :
!        "Using MPI, Portable Parallel Programming with the Message-
!         Passing Interface", by W. Gropp, E. Lusk, A. Skejellum.
!
!=====================================================================
Implicit None
 
Include "mpif.h"
 
Integer :: ierror
!---------------------------------------------------------------------
 
!... Initialize MPI
Call MPI_INIT (ierror)
 
!... Rank (my_id) of the process
Call MPI_COMM_RANK (MPI_COMM_WORLD, my_id, ierror)
 
!... Number of processors (numproc)
Call MPI_COMM_SIZE (MPI_COMM_WORLD, numproc, ierror)
 
!... Initialize the Cartesian communicator (handle)
MPI_COMM_CART = MPI_COMM_NULL
 
End Subroutine Parallel_Start
 
Subroutine Parallel_End 
!=====================================================================
! 
!        Parallel_End
!
! Subroutine Parallel_End completes the parallel (MPI) process. Serial
! code may be executed after a call to Parallel_End. This subroutine
! should only be called only once per simulation.
!
! Author :
!        Wayne Weseloh, Los Alamos National Laboratory
!
! History :
!        2005-03-01, original version
!        2005-03-07, rev. 2, use MPI_COMM_NULL for checking
!
! Externals :
!        MPI_COMM_FREE, MPI_BARRIER, MPI_FINALIZE
!
! References :
!        "Using MPI, Portable Parallel Programming with the Message-
!         Passing Interface", by W. Gropp, E. Lusk, A. Skejellum.
!
!=====================================================================
Implicit None
 
Include "mpif.h"
 
Integer :: ierror
!---------------------------------------------------------------------
 
!... Release the Cartesian communicator (if it exists)
if (MPI_COMM_CART /= MPI_COMM_NULL) then
   Call MPI_COMM_FREE (MPI_COMM_CART, ierror)
endif
 
!... Synchronize
Call MPI_BARRIER (MPI_COMM_WORLD, ierror)
 
!... Finish MPI
Call MPI_FINALIZE (ierror)
 
End Subroutine Parallel_End
 
Subroutine Topology
!=====================================================================
! 
!        Topology
!
! Subroutine Topology creates a 3D Cartesian topology based on the
! number and distribution of processors.  A new communicator (handle)
! is created specifically for this topology (MPI_COMM_CART). The six
! neighbors are identified (Left,Right,Bottom,Top,Fromt,Back).
!
! Author :
!        Wayne Weseloh, Los Alamos National Laboratory
!
! History :
!        2005-03-01, original version
!
! Inputs :
!        npex     number of processors in the X-direction  [0-numproc]
!        npey     number of processors in the Y-direction  [0-numproc]
!        npez     number of processors in the Z-direction  [0-numproc]
!
! Outputs :
!        MPI_COMM_CART     communicator with new Cartesian topology     
!        Left              processor rank next to my_id (X+direction)        
!        Right             processor rank next to my_id (X-direction)        
!        Bottom            processor rank next to my_id (Y+direction)        
!        Top               processor rank next to my_id (Y-direction)        
!        Front             processor rank next to my_id (Z+direction)       
!        Back              processor rank next to my_id (Z-direction)        
!
! Externals :
!        MPI_DIMS_CREATE, MPI_CART_CREATE, MPI_COMM_RANK, 
!        MPI_CART_GET, MPI_CART_RANK, MPI_FINALIZE, MPI_INITIALIZED,
!        Dims_Check
!
! References :
!        "Using MPI, Portable Parallel Programming with the Message-
!         Passing Interface", by W. Gropp, E. Lusk, A. Skejellum.
!         (based on the code listed in oned.f and twod.f)
!        "MPI: A Message-Passing Interface Standard", version 1.1,
!         June 12, 1995 (see pages 189-190, Figure 6.1)
!
! Notes :
!        Parallel_Start must be called before this routine.
!        The Cartesian grid is defined as periodic.
!        The processor dimensions (npex,npey,npez) should be set
!        before the call to Topology.  However, the default values
!        of npex=npey=npez=0 does create an acceptable topology.  
!
!=====================================================================
Implicit None
 
Include "mpif.h"

Integer, parameter       :: ndim = 3         
Integer, Dimension(ndim) :: coords     
Integer, Dimension(ndim) :: dims       
Logical, Dimension(ndim) :: periods
 
Integer :: ierror
Logical :: active, reorder 
 
Integer, Dimension(ndim) :: neighbor
Integer :: i,j,k
!---------------------------------------------------------------------
 
!... Check on MPI initialization
Call MPI_INITIALIZED (active, ierror)
if (.NOT.active) then
   Write(unit=*,fmt='(/1x,"MPI has not been initialized (Parallel_Start)"/)') 
   STOP ' Error Exit [topology] ' 
endif
 
!... Arguments used to describe the Cartesian grid
dims   (:) = (/ npex,  npey,  npez /)  ! Number of processes in each dimension
periods(:) = (/.TRUE.,.TRUE.,.TRUE./)  ! Periodic in all directions
reorder    = .TRUE.                    ! Allows ranks to be reordered
 
!... Check dimensions
if (.NOT.Dims_Check(dims)) then
   Call MPI_FINALIZE (ierror)
   STOP 'Error exit [dims] ' 
endif
 
!... Create a 3D Cartesian topology and communicator
!---------------------------------------------------
 
!... Creates a balanced distribution of processors for a Cartesian topology
Call MPI_DIMS_CREATE (numproc, ndim, dims, ierror) 
 
!... Creates a Cartesian 3D topology (handle = MPI_COMM_CART)_
Call MPI_CART_CREATE (MPI_COMM_WORLD, ndim, dims, periods, reorder, MPI_COMM_CART, ierror)
 
!... Rank of this processor in the new communicator MPI_COMM_CART
Call MPI_COMM_RANK (MPI_COMM_CART, my_id, ierror)
 
!... Find the neighbors in the 3D Cartesian decomposition
!--------------------------------------------------------
 
!... Look up the ranks for the neighbors. Own process coordinates are (i,j,k)
!    and the neighbors are Left=(i-1,j,k), Right=(i+1,j,k), Bottom=(i,j-1,k),
!    Top=(i,j+1,k), Front=(i,j,k-1), and Back=(i,j,k+1).
 
Call MPI_CART_GET (MPI_COMM_CART, ndim, dims, periods, coords, ierror)
i = coords(1)
j = coords(2)
k = coords(3)
 
!... X-direction (Left)
neighbor = (/i-1,j,k/)
Call MPI_CART_RANK (MPI_COMM_CART, neighbor, Left,   ierror)

!... X+direction (Right)
neighbor = (/i+1,j,k/)
Call MPI_CART_RANK (MPI_COMM_CART, neighbor, Right,  ierror)
 
!... Y-direction (Bottom)
neighbor = (/i,j-1,k/)
Call MPI_CART_RANK (MPI_COMM_CART, neighbor, Bottom, ierror)
 
!... Y+direction (Top)
neighbor = (/i,j+1,k/)
Call MPI_CART_RANK (MPI_COMM_CART, neighbor, Top,    ierror)
 
!... Z-direction (Front)
neighbor = (/i,j,k-1/)
Call MPI_CART_RANK (MPI_COMM_CART, neighbor, Front,  ierror)
 
!... Z+direction (Back)
neighbor = (/i,j,k+1/)
Call MPI_CART_RANK (MPI_COMM_CART, neighbor, Back,   ierror)
 
End Subroutine Topology
 
Logical Function Dims_Check (dims)
!=====================================================================
! 
!        Dims_Check
!
! Subroutine Dims_Check examines the processor dimensions for 
! conflicts with the number of processors requested (numproc).
!
! Author :
!        Wayne Weseloh, Los Alamos National Laboratory
!
! History :
!        2005-03-01, original version
!
! Inputs :
!        dims     processor decomposition (i.e. npex,npey,npez)   
!
! Outputs :
!        Dims_Check     Logical, .TRUE. if no problems found.
!
! Externals :
!        MPI_FINALIZE
!
! References :
!        "Using MPI, Portable Parallel Programming with the Message-
!         Passing Interface", by W. Gropp, E. Lusk, A. Skejellum.
!
! Notes :
!        By default the dims are all zero, this is acceptable. 
!        If any of the dims are greater than the number of processors, 
!        then an error is generated and Dims_Check = .FALSE.  
!        If the product of the non-zero dims is greater than the
!        number of processors, then an error is generated and
!        Dims_Check = .FALSE.
!
!=====================================================================
Implicit None

 
Integer, Dimension(:), Intent(In)  :: dims
 
Integer :: ierror, n
!---------------------------------------------------------------------
 
!... If the dims are all zero, then accept the configuration
Dims_Check = ALL(dims(:) == 0)
if (Dims_Check) RETURN
 
!... Check on proposed Cartesian grid 
if (ANY(dims(:) > numproc)) then
   if (my_id == 0) then
      Write (unit=6,fmt='(1x,"*** Error - A processor dimension cannot ",      &
           &            "be greater than the number of processors ",i0)') numproc
   endif
   Call MPI_FINALIZE (ierror)
   STOP 'Error exit [numproc] ' 
endif
 
!... Check if the product of the dimensions exceeds numproc
n = PRODUCT(dims(:),mask=dims > 0)
if (n > numproc) then
   if (my_id == 0) then
      Write (unit=6,fmt='(1x,"*** Error - The product of the processor ",      &
           &            "dimensions exceeds the number of processors ",i0)') numproc
   endif
   Call MPI_FINALIZE (ierror)
   STOP 'Error exit [npex*npey*npez] ' 
endif
 
!... Passed all the tests
Dims_Check = .TRUE.
 
End Function Dims_Check

Subroutine Get_local_extents (nx,ny,nz, sx,ex, sy,ey, sz,ez, Error)
!=====================================================================
! 
!        Get_Local_Extents
!
! Subroutine Get_Local_Extents finds the lower and upper arrays bounds
! for each processor rank.
!
! Author :
!        Wayne Weseloh, Los Alamos National Laboratory
!
! History :
!        2005-03-01, original version
!
! Inputs :
!        nx,ny,nz     grid dimensions (0 < n < 2**31)   
!
! Outputs :
!        sx,ex      starting/ending X array bounds for rank = my_id 
!        sy,ey      starting/ending Y array bounds for rank = my_id 
!        sz,ez      starting/ending Z array bounds for rank = my_id
!        Error      logical flag, TRUE = extents tangled
!
! Externals :
!        MPI_CART_GET, MPI_ALLREDUCE, MPI_FINALIZE, Decomp1D
!
! References :
!        "Using MPI, Portable Parallel Programming with the Message-
!         Passing Interface", by W. Gropp, E. Lusk, A. Skejellum.
!
! Notes :
!         The array bounds DO NOT include the ghost cells.  The ghosts
!         are included by Dimension(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1)
!
!=====================================================================
Implicit None
 
Include "mpif.h"
 
Integer, Intent(In)  :: nx, ny, nz
Integer, Intent(Out) :: sx, ex, sy, ey, sz, ez
Logical, Intent(Out) :: Error
 
Integer, parameter       :: ndim = 3         
Integer, Dimension(ndim) :: coords     
Integer, Dimension(ndim) :: dims       
Logical, Dimension(ndim) :: periods
 
Integer :: ierror
Logical :: myError
!---------------------------------------------------------------------
 
!... Cartesian topology information associated with MPI_COMM_CART
Call MPI_CART_GET (MPI_COMM_CART, ndim, dims, periods, coords, ierror)
 
!... Decompose the global grid into its constituent parts
Call Decomp1D (nx, dims(1), coords(1), sx, ex)
Call Decomp1D (ny, dims(2), coords(2), sy, ey)
Call Decomp1D (nz, dims(3), coords(3), sz, ez)
 
!... Check the extents
myError = (ex < sx) .OR. (ey < sy) .OR. (ez < sz)
if (myError) then
   Write (unit=*,fmt='(1x,"Error(s) in computing extents, my_id = ",i0,   &
        &              2x,": sx,ex,sy,ey,sz,ez = ",6(i0,:,", "))')        &
        &              my_id,sx,ex,sy,ey,sz,ez
endif
 
!... Any processors have an error ?
Call MPI_ALLREDUCE (myError, Error, 1, MPI_LOGICAL, MPI_LOR,              &
                  & MPI_COMM_WORLD, ierror)
 
!... Update PE decomposition, if originally set to zero
if (npex == 0) npex = dims(1)
if (npey == 0) npey = dims(2)
if (npez == 0) npez = dims(3)
 
End Subroutine Get_local_extents

Subroutine Decomp1D (n, numdir, id, s, e)
!=====================================================================
! 
!        Decomp1D
!
! Subroutine Decomp1D computes the starting and ending array bounds 
! for each processor rank in one dimension.  
!
! Inputs :
!        n          grid dimension in the 1D direction
!        numdir     number of processors in the 1D direction
!        id         processor rank (i.e. my_id)
!
! Outputs :
!        s          starting array bound
!        e          ending   array bound
!
! References :
!        "Using MPI, Portable Parallel Programming with the Message-
!         Passing Interface", by W. Gropp, E. Lusk, A. Skejellum.
!        (see the MPE_DECOMP1D routine)
!
!=====================================================================
Implicit None
 
Integer, Intent(In)  :: n, numdir, id
Integer, Intent(Out) :: s, e
 
Integer :: nlocal, deficit
!---------------------------------------------------------------------
 
nlocal  = n / numdir
s       = id * nlocal + 1
deficit = MOD(n,numdir)
s       = s + MIN(id,deficit)
 
if (id < deficit) nlocal = nlocal + 1
 
e = s + nlocal - 1
if (e > n .OR. id == (numdir - 1)) e = n
 
End Subroutine Decomp1D

Subroutine Exchange3D (Array, sx,ex, sy,ey, sz,ez)
!=====================================================================
! 
!        Exchange3D
!
! Subroutine Exchange3D controls the exchange of information between
! processors for the variable "Array".  Each process sends data to its
! neighbors (Right/Left/Bottom/Top/Front/Back) and receives data from
! its neighbors (Left/Right/Top/Bottom/Back/Front).
!
! Author :
!        Wayne Weseloh, Los Alamos National Laboratory
!
! History :
!        2005-03-01, original version
!        2005-03-07, rev. 1, status is an array (MPI_STATUS_SIZE)
!
! Inputs :
!        Array    Array in need of updated ghost cell information
!        sx,ex    starting/ending X array bounds for rank = my_id
!        sy,ey    starting/ending Y array bounds for rank = my_id
!        sz,ez    starting/ending Z array bounds for rank = my_id
!
! Outputs :
!        Array    Array with the appropriate ghost cell information
!
! Externals :
!        MPI_SENDRECV
!
! References :
!        "Using MPI, Portable Parallel Programming with the Message-
!         Passing Interface", by W. Gropp, E. Lusk, A. Skejellum.
!         (based on the code exchng2 listed in twod.f)
!
! Notes :
!        The 2D slices of Array are converted to a 1D vector using
!        the Fortran 90 intrinsic RESHAPE and then sent to the
!        appropriate processor.  The vector is RESHAPEd on the other
!        side using the same intrinsic.  This avoids the issue of
!        creating and using MPI derived datatypes.
!        When SENDRECV is used, data flows simultaneously in both
!        directions (logically, at least) and cycles in the MPI
!        communication pattern DO NOT lead to deadlock. 
!
!=====================================================================
 
Use MPI
Implicit None
 
 
Integer, Intent(In) :: sx,ex,sy,ey,sz,ez
 
Real, Dimension(sx-1:ex+1,sy-1:ey+1,sz-1:ez+1), Intent(In Out) :: Array
 
!... local send/receive arrays
Real, Dimension((ey-sy+1)*(ez-sz+1)) :: xsend, xrecv
Real, Dimension((ez-sz+1)*(ex-sx+1)) :: ysend, yrecv
Real, Dimension((ex-sx+1)*(ey-sy+1)) :: zsend, zrecv

 
Integer :: ierror
Integer, Dimension(MPI_STATUS_SIZE) :: status
Integer :: mx, my, mz, n
!---------------------------------------------------------------------
 
!... number of elements
mx = ex - sx + 1
my = ey - sy + 1
mz = ez - sz + 1
 
!... X-direction 
!---------------
n = my * mz
xsend(:) = RESHAPE(source=Array(ex,sy:ey,sz:ez),shape=(/n/))
 
Call MPI_SENDRECV (xsend, n, MPI_REAL,  Right, 1, &
                 & xrecv, n, MPI_REAL,   Left, 1, &
                 & MPI_COMM_CART, status, ierror)
 
Array(sx-1,sy:ey,sz:ez) = RESHAPE(source=xrecv(:),shape=(/my,mz/))
 
!... X+direction
!---------------
n = my * mz
xsend(:) = RESHAPE(source=Array(sx,sy:ey,sz:ez),shape=(/n/))
 
Call MPI_SENDRECV (xsend, n, MPI_REAL,   Left, 2, &
                 & xrecv, n, MPI_REAL,  Right, 2, &
                 & MPI_COMM_CART, status, ierror)
 
Array(ex+1,sy:ey,sz:ez) = RESHAPE(source=xrecv(:),shape=(/my,mz/))
 
 
!... Y-direction
!---------------
n = mx * mz
ysend(:) = RESHAPE(source=Array(sx:ex,ey,sz:ez),shape=(/n/))
 
Call MPI_SENDRECV (ysend, n, MPI_REAL,    Top, 3, &
                 & yrecv, n, MPI_REAL, Bottom, 3, &
                 & MPI_COMM_CART, status, ierror)
 
Array(sx:ex,sy-1,sz:ez) = RESHAPE(source=yrecv(:),shape=(/mx,mz/))
 
!... Y+direction
!---------------
n = mx * mz
ysend(:) = RESHAPE(source=Array(sx:ex,sy,sz:ez),shape=(/n/))
 
Call MPI_SENDRECV (ysend, n, MPI_REAL, Bottom, 4, &
                 & yrecv, n, MPI_REAL,    Top, 4, &
                 & MPI_COMM_CART, status, ierror)
 
Array(sx:ex,ey+1,sz:ez) = RESHAPE(source=yrecv(:),shape=(/mx,mz/))
 
 
!... Z-direction
!---------------
n = mx * my
zsend(:) = RESHAPE(source=Array(sx:ex,sy:ey,ez),shape=(/n/))
 
Call MPI_SENDRECV (zsend, n, MPI_REAL,   Back, 5, &
                 & zrecv, n, MPI_REAL,  Front, 5, &
                 & MPI_COMM_CART, status, ierror)
 
Array(sx:ex,sy:ey,sz-1) = RESHAPE(source=zrecv(:),shape=(/mx,my/))
 
!... Z+direction
!---------------
n = mx * my
zsend(:) = RESHAPE(source=Array(sx:ex,sy:ey,sz),shape=(/n/))
 
Call MPI_SENDRECV (zsend, n, MPI_REAL,  Front, 6, &
                 & zrecv, n, MPI_REAL,   Back, 6, &
                 & MPI_COMM_CART, status, ierror)
 
Array(sx:ex,sy:ey,ez+1) = RESHAPE(source=zrecv(:),shape=(/mx,my/))
 
 
End Subroutine Exchange3D

Subroutine Parallel_Diag (lun, nx,ny,nz)
!=====================================================================
! 
!        Parallel_Diag
!
! Subroutine Parallel_Diag
!
!
! Author :
!        Wayne Weseloh, Los Alamos National Laboratory
!
! History :
!        2005-03-01, original version
!
! Inputs :
!        lun          logical unit number for all output (e.g. nfout)
!        nx,ny,nz     grid dimensions (global array bounds)
!
! Outputs :
!        Various MPI information and diagnostics
!
! Externals :
!        MPI_INITIALIZED, MPI_GATHER, MPI_BARRIER, Get_Local_Extents
!
! References :
!        "Using MPI, Portable Parallel Programming with the Message-
!         Passing Interface", by W. Gropp, E. Lusk, A. Skejellum.
!
!=====================================================================
Implicit None
 
Include "mpif.h"
 
Integer, Intent(In)  :: lun, nx,ny,nz
 
Integer :: i1,i2, j1,j2, k1,k2 
Integer :: n, ierror
Logical :: active, Error
Integer, Dimension(6) :: ext, nbr
Integer, Dimension(6*numproc) :: temp
Integer, Dimension(:,:), Allocatable :: bounds, neighbors
!---------------------------------------------------------------------
 
!... check on MPI initialization
Call MPI_INITIALIZED (active, ierror)
if (.NOT.active) then
   Write(unit=*,fmt='(/1x,"MPI has not been initialized (Parallel_Start)"/)') 
   STOP ' Error Exit [parallel_diag] ' 
endif
 
!... header
if (my_id == 0) then
   Write (unit=lun,fmt='(/1x,"< Parallel Diagnostics >")')
   Write (unit=lun,fmt='(/5x,"Numproc = ",i0)') numproc
endif
 
!... communicators
if (my_id == 0) then
   Write (unit=lun,fmt='(/1x,"Communicators (handles)")')
   Write (unit=lun,fmt='(/5x,"MPI_COMM_WORLD = ",i0)') MPI_COMM_WORLD
   Write (unit=lun,fmt='( 5x,"MPI_COMM_CART  = ",i0)') MPI_COMM_CART
   Write (unit=lun,fmt='( 5x,"MPI_COMM_NULL  = ",i0)') MPI_COMM_NULL
endif
 
!... processor decomposition
if (my_id == 0) then
   Write (unit=lun,fmt='(/1x,"Processor decomposition")')
   Write (unit=lun,fmt='(/5x,"Npex = ",i0)') npex
   Write (unit=lun,fmt='( 5x,"Npey = ",i0)') npey
   Write (unit=lun,fmt='( 5x,"Npez = ",i0)') npez
endif
 
!... local extents
Call Get_local_extents (nx,ny,nz, i1,i2, j1,j2, k1,k2, Error)
ext(1:6) = (/i1,i2, j1,j2, k1,k2/)
Call MPI_GATHER (ext,  6, MPI_INTEGER,           &
               & temp, 6, MPI_INTEGER,           &
               & 0, MPI_COMM_WORLD, ierror)
if (my_id == 0) then
   ALLOCATE(bounds(6,numproc))
   bounds(:,:) = RESHAPE(source=temp(1:6*numproc),shape=(/6,numproc/))
   Write (unit=lun,fmt='(/1x,"Subgrid bounds (without ghost cells)")')
   Write (unit=lun,fmt='(/5x,"id     sx:ex      sy:ey      sz:ez")') 
   do n = 1,numproc
      Write (unit=lun,fmt='(2x,i5,3x,3(i4,":",i4,2x))') (n-1),bounds(1:6,n)
   enddo
   DEALLOCATE(bounds)
   if (Error) then
      Write (unit=lun,fmt='(/5x,"*** Error(s) in the extents ***"/)')
   endif
endif
 
!... neighbors
nbr(1:6) = (/Right, Left, Bottom, Top, Front, Back/)
Call MPI_GATHER (nbr,  6, MPI_INTEGER,           &
               & temp, 6, MPI_INTEGER,           &
               & 0, MPI_COMM_WORLD, ierror)
if (my_id == 0) then
   ALLOCATE(neighbors(6,numproc))
   neighbors(:,:) = RESHAPE(source=temp(1:6*numproc),shape=(/6,numproc/))
   Write (unit=lun,fmt='(/1x,"Subgrid neighbors")') 
   Write (unit=lun,fmt='(/5x,"id   Right    Left  Bottom     Top   Front    Back")') 
   do n = 1,numproc
      Write (unit=lun,fmt='(2x,i5,6(4x,i4))') (n-1),neighbors(1:6,n)
   enddo
   DEALLOCATE(neighbors)
endif
 
!... synchronize
Call MPI_BARRIER (MPI_COMM_WORLD,ierror)
 
if (my_id == 0) then
   Write (unit=lun,fmt='(/1x,"< End Parallel Diagnostics >")')
endif

End Subroutine Parallel_Diag
 
Subroutine Broadcast_Integer (n)
!=====================================================================
! 
!        Broadcast_Integer
!
! Subroutine Broadcast_Integer
!
! Author :
!        Wayne Weseloh, Los Alamos National Laboratory
!
! History :
!        2005-03-22, original version
!
! Externals :
!        MPI_BCAST
!
! References :
!        "Using MPI, Portable Parallel Programming with the Message-
!         Passing Interface", by W. Gropp, E. Lusk, A. Skejellum.
!
!=====================================================================
Implicit None
 
Include "mpif.h"
 
Integer, Intent(In Out) :: n
 
Integer :: ierror
!---------------------------------------------------------------------
 
!... Broadcast an single integer 
Call MPI_BCAST (n, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror)
 
End Subroutine Broadcast_Integer
 

Subroutine Broadcast_Real (x)
!=====================================================================
! 
!        Broadcast_Real
!
! Subroutine Broadcast_Real
!
! Author :
!        Wayne Weseloh, Los Alamos National Laboratory
!
! History :
!        2005-03-22, original version
!
! Externals :
!        MPI_BCAST
!
! References :
!        "Using MPI, Portable Parallel Programming with the Message-
!         Passing Interface", by W. Gropp, E. Lusk, A. Skejellum.
!
!=====================================================================
Implicit None
 
Include "mpif.h"
 
Real, Intent(In Out) :: x
 
Integer :: ierror
!---------------------------------------------------------------------
 
!... Broadcast an single real 
Call MPI_BCAST (x, 1, MPI_REAL, 0, MPI_COMM_WORLD, ierror)
 
End Subroutine Broadcast_Real
 
Subroutine Broadcast_Logical (state)
!=====================================================================
! 
!        Broadcast_Logical
!
! Subroutine Broadcast_Logical
!
! Author :
!        Wayne Weseloh, Los Alamos National Laboratory
!
! History :
!        2005-03-22, original version
!
! Externals :
!        MPI_BCAST
!
! References :
!        "Using MPI, Portable Parallel Programming with the Message-
!         Passing Interface", by W. Gropp, E. Lusk, A. Skejellum.
!
!=====================================================================
 
Include "mpif.h"
 
Logical, Intent(In Out) :: state
 
Integer :: ierror
!---------------------------------------------------------------------
 
!... Broadcast an single logical 
Call MPI_BCAST (state, 1, MPI_LOGICAL, 0, MPI_COMM_WORLD, ierror)
 
End Subroutine Broadcast_Logical
 
Subroutine Broadcast_String (string)
!=====================================================================
! 
!        Broadcast_String
!
! Subroutine Broadcast_String
!
! Author :
!        Wayne Weseloh, Los Alamos National Laboratory
!
! History :
!        2005-03-22, original version
!
! Externals :
!        MPI_BCAST
!
! References :
!        "Using MPI, Portable Parallel Programming with the Message-
!         Passing Interface", by W. Gropp, E. Lusk, A. Skejellum.
!
!=====================================================================
Implicit None
 
Include "mpif.h"
 
Character(len=*), Intent(In Out) :: string
 
Integer :: ierror
!---------------------------------------------------------------------
 
!... Broadcast an character string
Call MPI_BCAST (string, LEN(string), MPI_CHARACTER, 0, MPI_COMM_WORLD, ierror)
 
End Subroutine Broadcast_String
 
End Module Parallel
