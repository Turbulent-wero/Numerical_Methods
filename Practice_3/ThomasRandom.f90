PROGRAM Thomas
  IMPLICIT NONE
  INTEGER :: N
  REAL :: y,z

  write(*,*)"Enther the size of the matrix"
  read(*,*) N
  write(*,*)"Enter the upper bound"
  read(*,*) y
  write(*,*)"Enter the lower bound"
  read(*,*) z
  call algorithm(N,y,z)
CONTAINS
    SUBROUTINE algorithm(N,y,z)
      INTEGER,INTENT(IN) :: N
      REAL,INTENT(IN) :: y,z
      INTEGER :: i
      REAL,DIMENSION(0:N) :: x,a,b,c,d
      do i=1,N
        if(i==1) then
          b(i) = (rand(0)*(y-z))+z
          write(*,*) b(i)
          c(i) = (rand(0)*(y-z))+z
          write(*,*) c(i)
          d(i) = (rand(0)*(y-z))+z
          write(*,*) d(i)
        else 
          if(i==N) then
            a(i) = (rand(0)*(y-z))+z
            write(*,*) a(i)
            b(i) = (rand(0)*(y-z))+z
            write(*,*) b(i)
            d(i) = (rand(0)*(y-z))+z
            write(*,*) d(i)
          else
            a(i) = (rand(0)*(y-z))+z
            write(*,*) a(i)
            b(i) = (rand(0)*(y-z))+z
            write(*,*) b(i)
            c(i) = (rand(0)*(y-z))+z
            write(*,*) c(i)
            d(i) = (rand(0)*(y-z))+z
            write(*,*) d(i)
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
