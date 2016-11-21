PROGRAM Thomas
  IMPLICIT NONE
  INTEGER :: N

  write(*,*)"Enther the size of the matrix"
  read(*,*) N
  call algorithm(N)
CONTAINS
    SUBROUTINE algorithm(N)
      INTEGER,INTENT(IN) :: N
      INTEGER :: i
      REAL,DIMENSION(0:N) :: x,a,b,c,d
      do i=1,N
        if(i==1) then
          write(*,*) "Type the value for b(",i,")"
          read(*,*) b(i)
          write(*,*) "Type the value for c(",i,")"
          read(*,*) c(i)
          write(*,*) "Type the value for d(",i,")"
          read(*,*) d(i)
        else 
          if(i==N) then
            write(*,*) "Type the value for a(",i,")"
            read(*,*) a(i)
            write(*,*) "Type the value for b(",i,")"
            read(*,*) b(i)
            write(*,*) "Type the value for d(",i,")"
            read(*,*) d(i)
          else
            write(*,*) "Type the value for a(",i,")"
            read(*,*) a(i)
            write(*,*) "Type the value for b(",i,")"
            read(*,*) b(i)
            write(*,*) "Type the value for c(",i,")"
            read(*,*) c(i)
            write(*,*) "Type the value for d(",i,")"
            read(*,*) d(i)
          end if
        end if
      end do

      do i=2,N
        b(i) = b(i) - (a(i)/b(i-1))*c(i-1)
        d(i) = d(i) - (a(i)/b(i-1))*d(i-1)
      end do
      x(N) = d(N)/b(N)
      do i=N-1,1,-1
        x(i) = (d(i)-c(i)*x(i+1))/b(i)
      end do

      do i=1,N
        write(*,*) "x(",i,") = ",x(i)
      end do
    END SUBROUTINE algorithm
END PROGRAM Thomas
