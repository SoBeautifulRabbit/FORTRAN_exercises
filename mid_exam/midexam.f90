PROGRAM midexam
!mid-term exam. Print Yang Hui triangle
!BY Li Dongxu 2015301510021  Apr.26  f89

INTEGER::i,j  !the counting indexs for loops

!Use two dynamic arrays to store (temporarily) each line of the triangle
INTEGER,ALLOCATABLE::row_a(:),row_b(:) 

!The first two rows need to be printed seperately.
WRITE(*,'(I4)')1
ALLOCATE(row_a(2))
row_a=(/1,1/) !Initialize row_a.
WRITE(*,'(2I4)')row_a

!Use a nested loop to derive line 3 to line 10.
DO i=3,10

    !row_b is allocated according to the length of the line.
    ALLOCATE(row_b(i))
    
    !Initialize row_b to unity, and the first element and the last
    !element will remain unchanged in the rest of the program.
    row_b=1
    
    !The inner loop derives the rest of the elements in the line.
    DO j=2,i-1
        row_b(j)=row_a(j-1)+row_a(j)
    END DO

    !Write a line.
    WRITE(*,'(10I4)') row_b

    !Necessary preparation to begin a new loop:
    !copy the value of row_b to row_a, then deallocate row_b.
    DEALLOCATE(row_a)
    ALLOCATE(row_a(i))
    row_a=row_b
    DEALLOCATE(row_b)

END DO

DEALLOCATE(row_a) !This line is optional.
!If ommited, the system will take care of the memory deallocation.

END

