PROGRAM Hello
!===================================!
! A sample MPI Hello world program  !
! Written by Rahul Verma            !
!===================================!
IMPLICIT NONE
INCLUDE "mpif.h"                    ! MPI header file
INTEGER :: ierr,rank,numproc       

CALL MPI_INIT(ierr)                 ! Intialize MPI process

! Get the numerical ID of each processor
CALL MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr) 

! Get the total number of processor
CALL MPI_COMM_SIZE(MPI_COMM_WORLD,numproc,ierr)

WRITE(6,*)"Hello from the processor ",rank, "out of",numproc, " processors. "

CALL MPI_FINALIZE(ierr)             ! Finalize MPI process

END PROGRAM Hello
