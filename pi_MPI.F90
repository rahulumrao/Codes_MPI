PROGRAM pi_MPI
!******************************************************************************!
! A simple program to compute vlue of PI using dart board algorithm            !
! Written by :: Rahul Verma                                                    !
!******************************************************************************!
IMPLICIT NONE
INCLUDE "mpif.h"
INTEGER :: i,ierr,circle_count
INTEGER,PARAMETER :: DARTS = 50000, ROUNDS = 100, MASTER = 0
INTEGER :: rank,numproc,status(MPI_STATUS_SIZE)
REAL*8 :: dboard
REAL*8 :: homepi, avepi, pisum , pi

avepi = 0

! initialize MPI
CALL MPI_INIT(ierr)
! get rank of processor/processor ID (rank)
CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
! get total number of task(total numerb of processer)
CALL MPI_COMM_SIZE(MPI_COMM_WORLD, numproc, ierr)

PRINT*,"task ID = ", rank
PRINT*,"numTask = ", numproc

DO i = 1 , ROUNDS
   homepi = dboard (DARTS)

CALL MPI_REDUCE (homepi, pisum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, MASTER, MPI_COMM_WORLD, ierr)
   IF (rank .eq. MASTER) THEN
     pi = pisum/numproc
     avepi = ((avepi * (i - 1)) + pi ) /i
     WRITE(*,32) DARTS*i, avepi
32 FORMAT('   After',i8,' throws, average value of pi = ',f10.8)
ENDIF
ENDDO

IF (rank .eq. MASTER) THEN
PRINT*
PRINT*,"Real value of PI: 3.1415926535897"
PRINT*
ENDIF

! shutting the MPI process
CALL MPI_FINALIZE(ierr)

END PROGRAM pi_MPI
!=======================================================================================
FUNCTION dboard(darts)
INTEGER :: i,circle_count, n, darts
REAL*8 dboard
REAL*8 :: r
REAL*8 :: x, y
n = darts
circle_count = 0
DO i = 1,n
   CALL RANDOM_NUMBER(r)
    x = (2.0 * r) - 1.0
   CALL RANDOM_NUMBER(r)
    y = (2.0 * r) - 1.0

IF ((x*x + y*y) .le. 1.0) THEN
   circle_count = circle_count + 1
ENDIF
END DO

pi = (4.0 * circle_count)/n
dboard = pi
END FUNCTION dboard
!=======================================================================================
