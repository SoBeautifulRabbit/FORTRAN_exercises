PROGRAM ex0601
! Evaluate pi using a Monte Carlo method.
IMPLICIT NONE
INTEGER :: m
DO m=1,7,2
    CALL evaluate_pi(10**m) ! see the subroutine further below
END DO

WRITE(*,'("Please wait a minute while the situation N=1000000000 is being&
& calculated...")')
WRITE(*,'("You may press Ctrl+C to terminate the program at any time...")')
CALL evaluate_pi(10**9)
END

SUBROUTINE init_random_seed()
! initialize the random seed for the program using the system time
IMPLICIT NONE
INTEGER :: i, n, clock
INTEGER, ALLOCATABLE :: seed(:)
CALL RANDOM_SEED(size = n)
ALLOCATE(seed(n))
CALL SYSTEM_CLOCK(COUNT=clock)
seed = clock + 41 * (/ (i - 1, i = 1, n) /)
CALL RANDOM_SEED(put = seed)
DEALLOCATE(seed)
END SUBROUTINE init_random_seed

SUBROUTINE evaluate_pi(n)
!evaluate pi 5 times for a given N, and print the result.

IMPLICIT NONE
INTEGER(KIND=8),INTENT(IN)::n
REAL,ALLOCATABLE :: x(:), y(:)
REAL,PARAMETER :: pi=3.1415926535898
REAL(KIND=8) :: counts, eva_pi, err_pi
INTEGER(KIND=8) :: i, j

DO i=1,5
    CALL init_random_seed()
    ALLOCATE(x(n))
    ALLOCATE(y(n))

    ! randomly deploy dots in a 1x1 square
    CALL RANDOM_NUMBER(x)
    CALL RANDOM_NUMBER(y)

    ! now x stores the square of the distance of the point from the origin
    x=x*x+y*y

    ! count the number of points that lies outside the circle and use it 
    ! to derive pi
    counts=COUNT(x<1)
    eva_pi=counts/n*4

    ! caluculate the error from the standard pi
    err_pi=eva_pi-pi
    DEALLOCATE(x)
    DEALLOCATE(y)

    WRITE(*,'("N= ",I10," experiment_",I1,"  evaluated_pi=",F10.7," error&
&=",F10.7)') n, i, eva_pi, err_pi

    ! pause for 1 second, so that the rand seed gets properly initialized
    CALL SLEEP(1)

END DO
END SUBROUTINE

