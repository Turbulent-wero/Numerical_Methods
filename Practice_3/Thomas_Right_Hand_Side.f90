PROGRAM Thomas_Right_Hand_Side
  IMPLICIT NONE
  INTEGER :: i,N
  REAL*8,DIMENSION(0:10000) :: a,b,c,d,x

  write(*,*)"Enther the size of the matrix  (between 1 and 10000):"
  read(*,*) N
  do i=1,N
    a(i)=-1
    b(i)=4
    c(i)=-1
    x(i)=i
  end do
  call Thomas(x,a,b,c,d,i,N)

CONTAINS
    SUBROUTINE Thomas(x,a,b,c,d,i,N)
    INTEGER,INTENT(INOUT) :: i,N
    REAL*8,DIMENSION(0:N) :: x,a,b,c,d
    d(1)=b(1)*x(1)+c(1)*x(1+1)
    d(N)=a(N)*x(N-1)+b(N)*x(N)
    do i=2,N-1
      d(i)=a(i)*x(i-1)+b(i)*x(i)+c(i)*x(i+1)
    end do
    do i=1,N
      write(*,*) "d(",i,") = ",d(i)
    end do
    END SUBROUTINE Thomas
END PROGRAM Thomas_Right_Hand_Side
