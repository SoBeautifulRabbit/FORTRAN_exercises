!This program calculates an integral and investigates its deviation 
!from the accurate result.

!This function represents the integrand.
REAL(KIND=8) FUNCTION myfunc(x)
IMPLICIT NONE
REAL(KIND=8)::x
myfunc=x**2+dsin(x)
END FUNCTION

!This subroutine calculates the integral, evaluates the deviation from the
!accurate result and shows the result.
!parameter n is the number of trapezoids.
!diff is the deviation from the accurate result.
!a is the lower bound and b is the upper bound.
SUBROUTINE myintegrate(n)
IMPLICIT NONE
INTEGER::n,k
REAL(KIND=8),EXTERNAL::myfunc
REAL(KIND=8)::result=0D0,diff=0D0,h
REAL(KIND=8),PARAMETER::a=-2D0,b=2D0,kstandard=1.6D1/3D0

!h is the height of each trapezoid.
h=(b-a)/n

result=myfunc(a)+myfunc(b)
DO k=1,n-1
    result=result+2D0*myfunc(a+k*h)
END DO
result=result*h/2D0
diff=result-kstandard
WRITE(*,'("n=",I4,1X,"result=",F18.15,1X,"difference=",F18.15)') n,result,diff
END SUBROUTINE myintegrate

!The main program calls myintegrate 3 times to investigate how the parameter
!n influences the accuracy of the result. n is increased by a factor of 10
!each time.
PROGRAM ex0501
IMPLICIT NONE
INTEGER,PARAMETER::n1=50,n2=500,n3=5000
WRITE(*,*)
CALL myintegrate(n1)
CALL myintegrate(n2)
CALL myintegrate(n3)
WRITE(*,*)
WRITE(*,'("It is seen that when n is increased by a factor of 10, &
&the deviation from the accurate result is reduced to 1 percent of &
&the original.")')
WRITE(*,*)
WRITE(*,'("The reason behind it is that the error of the caculation raises &
&from replacing the original curve with a straight line (one side of the &
&trapezoid). The bigger the n, the smaller the difference from the &
&original shape of the curve, and thus the smaller error.")')
WRITE(*,*)
END PROGRAM

