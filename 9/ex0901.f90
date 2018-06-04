PROGRAM ex0901
! Solve the linear equations with Jacobi iterative method
IMPLICIT NONE

INTEGER,PARAMETER :: MAX = 500, N = 3
! MAX is the maximum times of iteration; N is the number of the unknowns.

REAL(KIND=8),PARAMETER :: A(3,3)=(/10D0, -1D0, 0D0, -1D0, 10D0, -4D0, &
&                                  0D0, -2D0, 10D0/), &
&                         B(3)= (/9D0, 7D0, 6D0/), X0(3)= (/0D0, 0D0, 0D0/), &
&                         TOL = 10D-8
REAL(KIND=8) :: X1(3), X2(3), s, SDX    ! SDX the standard deviation
INTEGER :: i, j, k

! Jacobi iterative method
X1 = X0
DO k=1,MAX
    X1=X2
    DO i=1,N
        s=0D0
        DO j=1,N
            IF (j == i) CYCLE
            s = s+ A(i,j) * X1(j)
        END DO
        X2(i) = (b(i)-s) / A(i,i)
    END DO
    SDX = 0D0
    DO i=1,N
        SDX = SDX + (X1(i) - X2(i))**2
    END DO
    SDX = SQRT(SDX)
    ! Terminate the iteration if the standard deviation is small enough.
    IF (SDX < TOL) THEN
        WRITE(*,'(A,I3)') "Iteration terminated at N= ", k
        EXIT
    END IF
END DO

WRITE(*,'(A)') "The result is:"
WRITE(*,'(A, f10.7)') "x1= ", X2(1)
WRITE(*,'(A, f10.7)') "x2= ", X2(2)
WRITE(*,'(A, f10.7)') "x3= ", X2(3)
WRITE(*,'(A, f11.8)') "standard deviation= ", SDX

END







