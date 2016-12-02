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
        a(i)=-1
        b(i)=4
        c(i)=-1
        x(i)=cos(3.14159265/i)
        write(*,*) x(i)
      end do
        d(1)=b(1)*x(1)+c(1)*x(1+1)
        d(N)=a(N)*x(N-1)+b(N)*x(N)
      do i=2,N-1
        d(i)=a(i)*x(i-1)+b(i)*x(i)+c(i)*x(i+1)
      end do
      do i=1,N
        write(*,*) "d(",i,") = ",d(i)
      end do
    END SUBROUTINE algorithm
END PROGRAM Thomas
