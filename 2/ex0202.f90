PROGRAM EX0201
REAL,PARAMETER :: PI=3.141593
INTEGER :: angle
REAL :: radian
WRITE (*,10) 'angle','radian','sin(angle)','cos(angle)'
10 FORMAT (T5'|','-----|----------|----------|-------&
&---|',/,T5,'|',A5,'|',3(A10,'|')&
&,/,T5,'|','-----|----------|----------|----------|')
DO angle=1,90
    radian=angle*PI/180
    WRITE(*,100) angle,radian,sin(radian),cos(radian)
    100 FORMAT (T5,'|',I3.2,'  |',(F5.2,'     |' )2(F5.3&
&,'     |'),/T5,'|',&
&'-----|----------|----------|----------|')
END DO
END
