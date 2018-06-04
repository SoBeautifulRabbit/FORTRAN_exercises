PROGRAM ex0902
! Solve a nonlinear equation with Newton method
IMPLICIT NONE

INTEGER,PARAMETER :: MAX = 500  ! the maximum times of iteration
REAL(KIND=8),PARAMETER :: TOL = 10D-7, x0=1.5D0
! Initial value x0=1.5 because we want to find a root on the interval [1,2].

REAL(KIND=8) :: x1,x2,Dx  ! Dx the deviation
REAL(KIND=8),EXTERNAL :: func, dfunc
INTEGER :: i

! Newtion method
x2 = x0
DO i=1,MAX
    x1 = x2
    x2 = x1 - func(x1) / dfunc(x1)
    Dx = ABS(x2-x1)
    ! Terminate the iteration if the deviation is small enough.
    IF (Dx < TOL) THEN
        WRITE(*,'(A, I3)') "Iteration terminated at N= ", i
        EXIT
    END IF
END DO

WRITE(*,'(A)') "The result is:"
WRITE(*,'(A, F10.7)') "x= ", x2
END

REAL(KIND=8) FUNCTION func(x)
! The equation we want to solve is func=0
IMPLICIT NONE
REAL(KIND=8), INTENT(IN) :: x
func = x**3 + 2*x**2 + 10*x -20
END FUNCTION

REAL(KIND=8) FUNCTION dfunc(x)
! dfunc is the derivative of func
IMPLICIT NONE
REAL(KIND=8), INTENT(IN) :: x
dfunc = 3*x**2 +4*x +10
END FUNCTION








