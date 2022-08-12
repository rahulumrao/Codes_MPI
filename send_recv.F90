!=============================================================================!
! A MPI program to send and recieve data between process                      !
! Written By - Rahul Verma                                                    !
!=============================================================================!
PROGRAM send_recv

IMPLICIT NONE
INCLUDE "mpif.h"
INTEGER :: ierr, rank, numproc, errorcode, namesize
INTEGER,DIMENSION(MPI_STATUS_SIZE) :: status1
REAL  :: data0, data1
CHARACTER(MPI_MAX_PROCESSOR_NAME) :: hostname

! initialize MPI
CALL MPI_INIT(ierr)

! get numerical ID of processor
CALL MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierr)

! get total number of processor
CALL MPI_COMM_SIZE(MPI_COMM_WORLD,numproc,ierr)

! get hostname at which the code is running
CALL MPI_GET_PROCESSOR_NAME(hostname,namesize,ierr)
PRINT*,"Runnnig code from ",hostname(1:namesize)," with rank ", rank,"of processor", numproc

IF (MOD(numproc,2) .ne. 0) THEN
!MPI_ABORT (comm,errorcode,ierr) 
PRINT*,"ERROR: number of processor is not even!"
CALL MPI_ABORT (MPI_COMM_WORLD,errorcode,ierr) 
ENDIF

data0 = 100.0 ! initialize some random value to variable data

! send data from master processor to other processor
IF (rank .eq. 0) THEN
!MPI_SEND (buf,count,datatype,dest,tag,comm,ierr)
CALL MPI_SEND(data0,1,MPI_INT,1,0,MPI_COMM_WORLD,ierr)
PRINT*,"sending data value ",data0," from rank ",rank

CALL MPI_RECV(data0,1,MPI_INT,1,0,MPI_COMM_WORLD,status1,ierr)
PRINT*,"recevied data value ",data0," to rank ",rank
ENDIF

IF (rank .eq. 1) THEN
!MPI_RECV (buf,count,datatype,source,tag,comm,status,ierr)
CALL MPI_RECV(data1,1,MPI_INT,0,0,MPI_COMM_WORLD,status1,ierr)
PRINT*,"recevied data value ",data1," to rank ",rank

data1=data1+rank
CALL MPI_SEND(data1,1,MPI_INT,0,0,MPI_COMM_WORLD,ierr)
PRINT*,"sending data value ",data1," from rank ",rank
ENDIF

CALL MPI_FINALIZE(ierr)

END PROGRAM send_recv
