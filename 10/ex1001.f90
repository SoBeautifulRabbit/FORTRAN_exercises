PROGRAM ex1001
! Calculate the value of a function using Lagrange interpolation method.

IMPLICIT NONE
REAL(KIND=8),PARAMETER :: PI=3.141592653589793
INTEGER,PARAMETER :: n1=4, n2=3
! n1 the number of given points. n2 the number of unknown points.

REAL(KIND=8) :: x(n1)= (/30.0D0, 45.0D0, 60.0D0, 90.0D0/), &
&               y(n1)= (/SQRT(3.0D0)/2.0D0, SQRT(2.0D0)/2.0D0, &
&                       1.0D0/2.0D0, 0.0D0/), &
&               t(n2)= (/47.0D0, 53.0D0, 79.0D0/), ty(n2)
! x,y the given points; t,ty the unknown points.

REAL(KIND=8),EXTERNAL :: la
INTEGER :: i

! convert deg->rad
x = x*PI/180.0D0
t = t*PI/180.0D0

! calculate with Lagrange interpolation method
DO i=1,n2
    ty(i) = la(t(i),n1,x,y)
END DO

WRITE(*,*)""    ! an empty line
WRITE(*,'(A)') "calculate  cos(47deg), cos(53deg), cos(79deg)"
WRITE(*,'(A)') "Lagrange interpolation method:"
WRITE(*,*) ty

! compare with the result calculated by internal function
WRITE(*,'(A)') "internal COS function:"
WRITE(*,*) COS(t)
WRITE(*,*)""    ! an empty line

END

REAL(KIND=8) FUNCTION la(t,n,x,y)
! this function returns the result calculate by Lagrange interpolation method
! t the x-coordinate of the unknown point, la the y-coordinate
! n the number of given points
! x,y the coordinate of the given points
IMPLICIT NONE

INTEGER,INTENT(IN) :: n
REAL(KIND=8), INTENT(IN) :: x(n), y(n), t
INTEGER :: i,j
REAL(KIND=8) :: temp(n)

DO i=1,n
    temp(i) = y(i)
    DO j=1,n
    IF (j==i) CYCLE
    temp(i) = temp(i) * (t-x(j))/(x(i)-x(j))
    END DO
END DO
la = SUM(temp)
END FUNCTION

