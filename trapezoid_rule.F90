PROGRAM trapezoid
!***********************************************************************!
! A simple program for finding area under the curve of a given function !
! f(x) = {lim a -> b} int(f(x) dx)  :: f(x) = sqrt(x - 1)               !
! Using Trapezoidal Rule                                                !
! Written By :: Rahul Verma                                             !
!***********************************************************************!
IMPLICIT NONE
INTEGER :: i, n
REAL    :: TrapZ
REAL*8  :: a, b, h, x, integral

!Initial parameters
a = 2 ; b = 8           ! function fimits
n = 1024                ! number of trapeziods
h = (b - a)/n           ! grid size

! Calculating integral values at the limit
integral = (TrapZ(a) + TrapZ(b))*(h/2.0)

! Calculating integral values at every grid point
DO i = 1, n-1
     x = a + i*h
     integral = integral + h*TrapZ(x)
END DO
WRITE(6,101)a,b,integral
101 FORMAT ("Within the limit",F8.2,2X,"->",F8.2,2X, &
        & ",area under the function [f(x) = sqrt(x - 1)] is =",F8.2)
END PROGRAM trapezoid
!==========================================================================
FUNCTION TrapZ(x)
IMPLICIT NONE
REAL*8 :: x
REAL   :: TrapZ
 
TrapZ = dsqrt(x - 1)

END FUNCTION TrapZ
!==========================================================================

