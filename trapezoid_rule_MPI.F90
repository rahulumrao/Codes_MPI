PROGRAM trapezoid_MPI
!************************************************************************
! A simple program ebmaded with MPI routines for finding area under the
! curve of a given function
! f(x) = {lim a -> b} int(f(x) dx)   :: f(x) = dsqrt(x - 1)
! Using ::-->  Trapezoidal Rule
! Written by :: Rahul Verma
!************************************************************************
IMPLICIT NONE
INCLUDE "mpif.h"
INTEGER :: i, n
REAL*8  :: a, b, h, x, integral, total
REAL    :: TrapZ
! MPI variables declaration
INTEGER :: ierr, rank, numproc, errorcode, namesize
INTEGER :: local_n
REAL*8  :: local_a, local_b
INTEGER,DIMENSION(MPI_STATUS_SIZE) :: stat

a = 2 ; b = 8                   ! lim a-> b 
n = 1024                        ! number of Trapezoids
h = (b - a)/n                   ! Grid size
! initializing MPI
CALL MPI_INIT(ierr)

! get numerical ID of every processor (rank)
CALL MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)

! get total number of processor
CALL MPI_COMM_SIZE(MPI_COMM_WORLD,numproc,ierr)

IF (MOD(numproc,2) .ne. 0) THEN
   PRINT*,"ERROR: number of processor is not even!"
   CALL MPI_ABORT (MPI_COMM_WORLD,errorcode,ierr)
ENDIF
! calculating local limits for every process
CALL para_range(a, b, n, h, numproc, rank, local_a, local_b, local_n) 
! computing value of function within new limits
CALL Trap(local_a, local_b, local_n, h, integral)

IF (rank .eq. 0) THEN
    total = (TrapZ(a) - TrapZ(b))*(h/2.0)  
    total = total + integral  ! adding value of integral in the total from the master         
! Receiving value of integral from every processor to 
! master processor (rank = 0) with tag id 0
DO i = 1, numproc-1
   CALL MPI_RECV(integral,1,MPI_REAL8,i,0,MPI_COMM_WORLD,stat,ierr)
   PRINT*,"recieved integral value",integral,"from processor",i
   total = total + integral
ENDDO
ELSE
! sending integral value to master porcess (rank = 0) 
! from every processor with tag id 0
  CALL MPI_SEND(integral,1,MPI_REAL8,0,0,MPI_COMM_WORLD,ierr)
ENDIF

IF (rank .eq. 0) THEN
WRITE(6,101)a,b,total
101 FORMAT ("Within the limit",F8.2,2X,"->",F8.2,2X, &
         & ",area under the function [f(x) = sqrt(x - 1)] is =",F8.2)
ENDIF

! shutting down MPI
CALL MPI_FINALIZE(ierr)

CONTAINS
!===================================================================================================
SUBROUTINE Trap (a, b, n, h, integral)
IMPLICIT NONE
INTEGER :: i, n
REAL*8  :: a, b, h, x, integral
REAL    :: TrapZ
! Calculating integral values at every grid point
DO i = 1, n
   x = a + i*h
   integral = integral + h*TrapZ(x)
!PRINT*,x,TrapZ(x)
ENDDO
END SUBROUTINE Trap
!===================================================================================================
SUBROUTINE para_range(a, b, n, h, numproc, rank, local_a, local_b, local_n)
IMPLICIT NONE
INTEGER :: numproc    ! total processor
INTEGER :: rank       ! numerial ID of each processor
INTEGER :: n          ! total number of grid
INTEGER :: local_n    ! local value of 'n' in each processor
INTEGER :: iwork      !  
REAL*8  :: a, b       ! function limit
REAL*8  :: h          ! grid size
REAL*8  :: local_a, local_b        ! Local value of 'a' and 'b' for each processor
iwork = MOD(n, numproc)
local_n = n/numproc
local_a = a + rank*local_n*h
IF (rank .eq. (numproc-1) .and. iwork .ne. 0) local_n = local_n + iwork
local_b = local_a + local_n*h
!PRINT*,'local a,b,n',local_a,local_b,local_n,rank
END SUBROUTINE para_range
END PROGRAM trapezoid_MPI
!===================================================================================================
FUNCTION TrapZ (fx)
IMPLICIT NONE
REAL*8 :: fx
REAL   :: TrapZ

TrapZ = dsqrt(fx - 1)

END FUNCTION TrapZ
!===================================================================================================

