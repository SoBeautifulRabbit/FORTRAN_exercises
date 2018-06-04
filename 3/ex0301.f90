PROGRAM EX0301
!Created on Mar. 29 by LDX 2015301510021
!Input a date and output the number of the date of the year.

IMPLICIT NONE
INTEGER:: YEAR,MONTH,DAY,NUM

!The following constants are calculated previously.
!It help improve the efficiency of the program.
!M_i=the preceding number of days of i_th month.
!For example, M3=59 means that there are 59 days before March.
INTEGER,PARAMETER::M1=0,M2=31,M3=59,M4=90,M5=120,M6=151,M7=181,&
&M8=212,M9=243,M10=273,M11=304,M12=334

WRITE(*,*) "Please input a date: yyyy mm dd"
READ(*,*) YEAR,MONTH,DAY

SELECT CASE(MONTH)
CASE (1)
    NUM=M1
CASE (2)
    NUM=M2
CASE (3)
    NUM=M3
CASE (4)
    NUM=M4
CASE (5)
    NUM=M5
CASE (6)
    NUM=M6
CASE (7)
    NUM=M7
CASE (8)
    NUM=M8
CASE (9)
    NUM=M9
CASE (10)
    NUM=M10
CASE (11)
    NUM=M11
CASE (12)
    NUM=M12
CASE DEFAULT
    WRITE(*,*) "Sorry! Invalid month input."
    STOP
END SELECT

NUM=NUM+DAY

IF (MONTH>2) THEN  !Consider leap year if month>2
    IF (MOD(YEAR,100)==0) THEN !Whether the year is centurial year or not
        IF (MOD(YEAR,400)==0) THEN !central leap year
            NUM=NUM+1
        END IF
    ELSE
        IF (MOD(YEAR,4)==0) THEN !ordinary leap year
            NUM=NUM+1
        END IF
    END IF
END IF

WRITE(*,'(I2,"-",I2," is the number ",I3," day of the year ",I4)')&
&MONTH,DAY,NUM,YEAR
END
