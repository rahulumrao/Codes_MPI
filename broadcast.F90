PROGRAM trapezoid_bcast
!****************************************************************************************!
! A simple program with MPI routines for finding area under the curve of a given function!
! f(x) = {lim a -> b} int(f(x) dx)   :: f(x) = dsqrt(x - 1)                              !
! Using MPI_BCAST routine to broadcast data in each processor and then,                  !
! using MPI_REDUCE routine to collect the data from each processor followed              !
! by summing up all the values in master the process.                                    !
! Written by :- Rahul Verma                                                              !
!****************************************************************************************!
IMPLICIT NONE
INCLUDE "mpif.h"
INTEGER :: i, n
REAL*8  :: a, b, h, x, integral
REAL*8  :: TrapZ,total
! MPI variables declaration
INTEGER :: ierr, rank, numproc, errorcode
INTEGER :: local_n
REAL*8  :: local_a, local_b
INTEGER,DIMENSION(MPI_STATUS_SIZE) :: stat

! initialize MPI
CALL MPI_INIT(ierr)

! get rank of processor/processor ID (rank)
CALL MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)

! get total number of task(total numerb of processer)
CALL MPI_COMM_SIZE(MPI_COMM_WORLD,numproc,ierr)

IF (MOD(numproc,2) .ne. 0) THEN
PRINT*,"ERROR: number of processor is not even!"
CALL MPI_ABORT (MPI_COMM_WORLD,errorcode,ierr)
ENDIF
! reading data and broadcasting
CALL get_data(a ,b, n, h, rank)
!Create local variables for each processors
CALL para_range(a, b, n, h, numproc, rank, local_a, local_b, local_n)
!computing value of function within new limits
CALL Trap(local_a,local_b,local_n,h,integral)

! Recieve data from each processor to the master processor (rank = 0)
!MPI_REDUCE (sendbuf,recvbuf,count,datatype,op,root,comm,ierr)
CALL MPI_REDUCE (integral,total,1,MPI_REAL8,MPI_SUM,0,MPI_COMM_WORLD,ierr)

IF (rank .eq. 0) THEN
integral = (TrapZ(a) - trapZ(b))*(h/2.0)
total = total + integral ! adding value of integral in the total from the master
WRITE(6,101)a,b,total
101 FORMAT ("Within the limit",F8.2,2X,"->",F8.2,2X, &
         & ",area under the function [f(x) = sqrt(x - 1)] is =",F8.2)
ENDIF
! shutting the MPI process
CALL MPI_FINALIZE(ierr)

CONTAINS
!=========================================================================================
! subroutine to integrate function using trapezoid rule
SUBROUTINE Trap (a,b,n,h,integral)
IMPLICIT NONE
INTEGER :: i,n
REAL*8 :: a,b,h,x,integral
REAL*8 :: TrapZ
DO i = 1, n
   x = a + i*h
   integral = integral + h*TrapZ(x)
!PRINT*,x,integral
ENDDO
END SUBROUTINE Trap
!=========================================================================================
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
!=========================================================================================
SUBROUTINE get_data(a, b, n, h, rank)
IMPLICIT NONE
INTEGER :: n,rank,ierr
REAL*8 :: a,b,h

IF (rank .eq. 0) THEN
OPEN(11,FILE='input')
WRITE(6,*)'Reading the value of a,b, and n from File...'
READ(11,*)a,b,n
WRITE(6,'(A,F6.2,A,F6.2,2X,A,1X,I4)') "a =",a, ", b =",b, "and n =",n
ENDIF
h = (b - a)/n  ! Grid size
! Broadcast routine to send the input data in each process
!MPI_BCAST(buffer,count,datatype,root,comm,ierr)
CALL MPI_BCAST(a,1,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
CALL MPI_BCAST(b,1,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
CALL MPI_BCAST(h,1,MPI_REAL8,0,MPI_COMM_WORLD,ierr)
CALL MPI_BCAST(n,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
END SUBROUTINE get_data
END PROGRAM trapezoid_bcast
!=========================================================================================
! User defined function
FUNCTION TrapZ (x)
IMPLICIT NONE
REAL*8 :: TrapZ
REAL*8 :: x

TrapZ = dsqrt(x - 1)

END FUNCTION TrapZ
!=========================================================================================
