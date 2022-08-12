#===============================================================
#Makefile
#===============================================================

FC    = gfortran
MPIFC = mpif90

all:	hello.x			\
	trapezoid_rule.x	\
	send_recv.x		\
	trapezoid_rule_MPI.x	\
	broadcast.x		\
	pi_MPI.x

hello.x:  hello.F90
	$(MPIFC) hello.F90 -o hello.x

trapezoid_rule.x:  trapezoid_rule.F90
	$(FC) trapezoid_rule.F90 -o trapezoid_rule.x

send_recv.x:  send_recv.F90
	$(MPIFC) send_recv.F90 -o send_recv.x

trapezoid_rule_MPI.x: trapezoid_rule_MPI.F90
	$(MPIFC) trapezoid_rule_MPI.F90 -o trapezoid_rule_MPI.x

broadcast.x:  broadcast.F90
	$(MPIFC) broadcast.F90 -o broadcast.x

pi_MPI.x:  pi_MPI.F90
	$(MPIFC) pi_MPI.F90 -o pi_MPI.x

clean   :
	rm *.x
