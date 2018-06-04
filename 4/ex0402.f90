PROGRAM ex0402
!Fibonacci sequence
IMPLICIT NONE

INTEGER(KIND=8) :: a=1,b=1,c=0
INTEGER :: N,i,q,m
WRITE(*,'("How many terms do you want to get?(Enter an positive integer,<=48): ",\)')
READ(*,*) N
m=MOD(N,3)
q=N/3+1
DO i=1,q
    IF (i==q .AND. m==0) THEN
        WRITE(*,'(/)')
        EXIT
    END IF
    a=b+c
    WRITE(*,'(I11,\)')a
    IF (i==q .AND. m==1) THEN
        WRITE(*,'(/)')
        EXIT
    END IF
    b=c+a
    WRITE(*,'(I11,\)')b
    IF (i==q .AND. m==2) THEN
        WRITE(*,'(/)')
        EXIT
    END IF
    c=a+b
    WRITE(*,'(I11)')c
END DO
END
