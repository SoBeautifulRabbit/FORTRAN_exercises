MODULE global
! some global varibles
IMPLICIT NONE
INTEGER,PARAMETER:: N=100000, length_f=19   ! 19 bytes for a formatted "double"
END MODULE


PROGRAM ex0801
! file operations

USE global
IMPLICIT NONE

REAL(KIND=8):: a(N),b, c(2*N), ave_f, ave_unf
! a: numbers to be written
! c: numbers read from a file

REAL(KIND=8),EXTERNAL :: my_average
REAL :: id_r(2*N), time_f, time_unf, time_start, time_end
INTEGER:: err1, length_unf, id_i(2*N), i
! id_i stores the rec_number of elements to be read from the file.


INQUIRE(IOLENGTH=length_unf) b   ! get the length (in bytes) for an unformatted
                                 ! "double"
CALL init_random_seed()
CALL RANDOM_NUMBER(a)

! formatted file
OPEN(UNIT=8, FILE="formatted_100000.dat", ACCESS="DIRECT", FORM="FORMATTED", &
&IOSTAT=err1, ACTION="WRITE", RECL=length_f)

CALL deal_with_error(err1)

DO i=1,N
    WRITE(UNIT=8,FMT='(F19.16)',REC=i, IOSTAT=err1) a(i) 
    ! The first digit of F19.16 is actually empty.
END DO

CALL deal_with_error(err1)

CLOSE(8)

! We could have replaced the DO loop above with the following statement:
! WRITE(8,*) a
! provided that we remove "RECL=length_f" in the OPEN statement.
! But that is not a safe choice. We had better not do that.

! unformatted file
OPEN(UNIT=9, FILE="unformatted_100000.dat", ACCESS="DIRECT", &
&FORM="UNFORMATTED", IOSTAT=err1, ACTION="WRITE", RECL=length_unf)

CALL deal_with_error(err1)

DO i=1,N
    WRITE(UNIT=9, REC=i, IOSTAT=err1) a(i)
END DO

CALL deal_with_error(err1)

CLOSE(9)

! id_i stores the rec_number of elements to be read from the file.
CALL init_random_seed()
CALL random_number(id_r)
id_r=id_r*N
id_i=INT(id_r)

! read from formatted file
CALL CPU_TIME(time_start)
OPEN(UNIT=10, FILE="formatted_100000.dat", STATUS="OLD", ACTION="READ", &
&ACCESS="DIRECT", FORM="FORMATTED", IOSTAT=err1, RECL=length_f)

CALL deal_with_error(err1)

DO i=1,2*N
    READ(UNIT=10,FMT='(F19.16)',REC=id_i(i)+1, IOSTAT=err1) c(i)
    ! +1, because id_i(i)=0 is outside range
END DO

CALL deal_with_error(err1)

CLOSE(10)
CALL CPU_TIME(time_end)
time_f = time_end - time_start
ave_f=my_average(c)

! id_i stores the rec_number of elements to be read from the file.
CALL init_random_seed()
CALL random_number(id_r)
id_r=id_r*N
id_i=INT(id_r)

! read from unformatted file
CALL CPU_TIME(time_start)
OPEN(UNIT=11, FILE="unformatted_100000.dat", STATUS="OLD", ACTION="READ", &
&ACCESS="DIRECT", FORM="UNFORMATTED", IOSTAT=err1, RECL=length_unf)

CALL deal_with_error(err1)

DO i=1,2*N
    READ(UNIT=11,REC=id_i(i)+1, IOSTAT=err1) c(i)
    ! +1, because id_i(i)=0 is outside range
END DO

CALL deal_with_error(err1)

CLOSE(11)
CALL CPU_TIME(time_end)
time_unf = time_end - time_start
ave_unf=my_average(c)

! write the results to a file
OPEN(UNIT=12, FILE="results.dat", FORM="FORMATTED", ACCESS="SEQUENTIAL", &
IOSTAT=err1)

CALL deal_with_error(err1)

WRITE(UNIT=12, FMT='(A)') "Formatted file:"
WRITE(UNIT=12, FMT='(A,F19.16)') "average= ", ave_f
WRITE(UNIT=12, FMT='(A,F9.6,A)') "time= ", time_f, " seconds"
WRITE(UNIT=12, FMT='(A)') " "
WRITE(UNIT=12, FMT='(A)') "Unformatted file:"
WRITE(UNIT=12, FMT='(A,F19.16)') "average= ", ave_unf
WRITE(UNIT=12, FMT='(A,F9.6,A)') "time= ", time_unf, " seconds"
WRITE(UNIT=12, FMT='(A)') " "
IF (time_unf < time_f) WRITE(UNIT=12, FMT='(A)')"The unformatted file processes&
& faster than the formatted file!"
CLOSE(12)
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

REAL(KIND=8) FUNCTION my_average(a)
! returns the average of numbers in array a.
! a contains a large number of small numbers

! The following algorithm avoids add a small number to a large number,
! so it preserves the accuracy.

USE global

IMPLICIT NONE
REAL(KIND=8),INTENT(IN):: a(2*N)
REAL(KIND=8):: b(2*N)
INTEGER :: i, j, m, c(8)

my_average=0
b=a
c=0
c(1)=2*N

DO i=2,10
    m=NINT(2*N/(10.0**(i-1)))  ! use 2.0 instead of 2
    IF (MOD(m,10) /= 0) EXIT
    c(i)=m
END DO

DO j=2,8
    DO i=1, c(j)
        b(i)=b(i+c(j)*0)+b(i+c(j)*1)+b(i+c(j)*2)+b(i+c(j)*3)+b(i+c(j)*4)+&
            &b(i+c(j)*5)+b(i+c(j)*6)+b(i+c(j)*7)+b(i+c(j)*8)+b(i+c(j)*9)
    END DO
END DO

DO j=2,8
    IF (c(j)==0) THEN
        DO i=1,c(j-1)
            my_average=my_average+b(i)
        END DO
        EXIT
    END IF
END DO
my_average=my_average/(2*N)
END FUNCTION

SUBROUTINE deal_with_error(err1)
! Stop the program if there is any error in file operation.

IMPLICIT NONE

INTEGER:: err1

IF (err1 /=0) THEN
    WRITE(*,*) "An error occured in file operation!"
    STOP
END IF
END SUBROUTINE

