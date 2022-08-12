# Check if you have MPI libraries avialable in your machine by running the following command :
which mpif90
# It should return something like --
/usr/local/bin/mpif90 \\
Or
$path/mpif90
# If it doesn't return this, then you may want to install the MPI library (below any)
1) OpenMPI  -- https://www.open-mpi.org/
2) MPICH    -- https://www.mpich.org/
3) MVAPICH  -- http://mvapich.cse.ohio-state.edu/
4) IntelMPI -- https://www.intel.com/content/www/us/en/developer/tools/oneapi/toolkits.html#hpc-kit
# From repository
sudo yum install openmpi-devel (CentOS/Fedora/Redhat)	\\
sudo apt-get install -y mpi    (Ubuntu/Linux Mint)	\\
sudo zypper install mpich      (OpenSUSE)		\\

# Once done, then just type "make", this will compile the program and "make clean" to clean executables

Makefile  		:- make file to compile all the FORTRAN programs \\
hello.F90		:- hello world program with MPI							\\
send_recv.F90		:- simple program just to check MPI_SEND/RECV routines				\\
trapezoid_rule.F90	:- serial version of program for Numerical Integration with Trapezoidal rule	\\
trapezoid_rule_MPI.F90  :- numerical Integration with Trapezoidal rule using MPI_SEND/RECV routines	\\
broadcast.F90		:- Numerical Integration with Trapezoidal rule using MPI_BCAST routines		\\
pi_MPI.F90		:- computes value of PI using dart algorithm					\\

# -----
Type "mpirun -n $NCPUS ./your-executable.x" to run the program in parallel

Written by : Rahul Verma (vrahul@iitk.ac.in)
