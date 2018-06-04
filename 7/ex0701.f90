PROGRAM ex0701
! By Li Dongxu 2015301510021

! create a magic square

IMPLICIT NONE
INTEGER::n
WRITE(*,'("Please input an odd number n (not too large): ",$)')
READ(*,*)n
IF (n>17) THEN
    WRITE(*,'("n is too large. Please choose a smaller n and try again")')
ELSE IF (0==MOD(n,2)) THEN
    WRITE(*,'("n is not an odd number. Please enter an odd number and try&
& again")')
ELSE
    CALL my_magic(n) ! see the subroutine further below
END IF
END

SUBROUTINE my_magic(n)
! calculate a magic square of order n and write it (on the screen)

IMPLICIT NONE
INTEGER,INTENT(IN):: n
INTEGER:: m, i, j
INTEGER,ALLOCATABLE:: A(:,:)
LOGICAL,EXTERNAL:: my_put_number ! see the function further below

! the first number will be 1 at the center of the first row
i=1
j=n/2+1
ALLOCATE(A(n,n))

! initialize a zero matrix and put in the first number
A=0
A(i,j)=1

! put the remaining numbers
DO m=2,n**2
    i=i-1
    j=j+1

    ! Loop until the function my_put_number returns true.
    DO WHILE (.NOT. my_put_number(A,i,j,m,n))
    END DO

END DO

! write the matrix (on the screen)
DO i=1,n
    DO j=1,n
        WRITE(*,'(1X,I4,$)')A(i,j)
    END DO
    WRITE(*,*)
END DO

DEALLOCATE(A)
END SUBROUTINE

LOGICAL FUNCTION my_put_number(A,i,j,m,n)
! This function tries to put number m at location (i,j) of matrix A.
! If succeeded, it returns true.
! If failed, it adjusts (i,j) and returns false, without doing anything to
! matrix A.

IMPLICIT NONE
INTEGER :: A(n,n)
INTEGER :: i,j
INTEGER,INTENT(IN):: m,n

! i,j could be larger than 5 according to the algorithm described below
! make sure i and j lies within 0~5.
! something like periodical boundary condition
i=MOD(i,n)
j=MOD(j,n)

! the smallest i and j could be zero according to the algorithm described below
! if such thing happens, adjust i and j as following:
IF (0==i) THEN
    i=n
END IF

IF (0==j) THEN
    j=n
END IF

! if (i,j) is not empty, adjust i and j as following, and return false.
IF (A(i,j)/=0) THEN
    i=i+2
    j=j-1
    my_put_number=.FALSE.

! if (i,j) is empty (=0), put the number m there and return true.
ELSE IF (0==A(i,j)) THEN
    A(i,j)=m
    my_put_number=.TRUE.
END IF

END FUNCTION
