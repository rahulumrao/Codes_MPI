# Check if you have MPI libraries avialable in your machine by running the following command :
<pre> which mpif90 </pre>
# It should return something like --
<pre> /usr/local/bin/mpif90 
Or
$path/mpif90
</pre>
# If it doesn't return any of the above, then you may want to install the MPI library (below any)
<pre>
1) OpenMPI  -- https://www.open-mpi.org/
2) MPICH    -- https://www.mpich.org/
3) MVAPICH  -- http://mvapich.cse.ohio-state.edu/
4) IntelMPI -- https://www.intel.com/content/www/us/en/developer/tools/oneapi/toolkits.html#hpc-kit
</pre>
# From repository
<pre>
sudo yum install openmpi-devel (CentOS/Fedora/Redhat)	<br />
sudo apt-get install -y mpi    (Ubuntu/Linux Mint)	<br />
sudo zypper install mpich      (OpenSUSE)		<br />
</pre>
# Once done, then just type "make", this will compile the program and "make clean" to clean executables
<pre>
Makefile  		:- make file to compile all the FORTRAN programs 				<br />
hello.F90		:- hello world program with MPI							<br />
send_recv.F90		:- simple program just to check MPI_SEND/RECV routines				<br />
trapezoid_rule.F90	:- serial version of program for Numerical Integration with Trapezoidal rule	<br />
trapezoid_rule_MPI.F90  :- numerical Integration with Trapezoidal rule using MPI_SEND/RECV routines	<br />
broadcast.F90		:- Numerical Integration with Trapezoidal rule using MPI_BCAST routines		<br />
pi_MPI.F90		:- computes value of PI using dart algorithm					<br />
</pre>

Type "mpirun -n $NCPUS ./your-executable.x" to run the program in parallel
<pre>
# --------------------------------------------------------------------------
#	    Written by : Rahul Verma (vrahul@iitk.ac.in)
# --------------------------------------------------------------------------
</pre>
