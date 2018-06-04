PROGRAM ex0401
!Calculate a square-full integer
IMPLICIT NONE
INTEGER :: i=1
REAL::a=-100.0,b=0.0,c=0.0
DO
    b=SQRT(a+100)
    c=SQRT(a+268)
    IF (MOD(b,1.0)<0.00001 .AND. MOD(c,1.0)<0.00001)THEN
        WRITE(*,'("The No.",I1," Integer is: ",I5)') i,INT(a)
        i=i+1
    END IF
    IF (i>4) EXIT
    a=a+1
END DO
!WRITE(*,'("The integer is: ",I3)')INT(a)
END
