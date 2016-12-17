PROGRAM Thomas_Random_Matrix
  IMPLICIT NONE
  INTEGER :: i,N
  REAL*8 :: y,z
  REAL*8,DIMENSION(0:10000) :: a,b,c,d,x
  write(*,*)"Enther the size of the matrix (between 1 and 10,000)"
  read(*,*) N
  write(*,*)"Enter the upper bound"
  read(*,*) y
  write(*,*)"Enter the lower bound"
  read(*,*) z

call matrix(a,b,c,d,i,N,y,z)
call Thomas(x,a,b,c,d,N,i)
CONTAINS
    SUBROUTINE matrix(a,b,c,d,i,N,y,z)
      INTEGER,INTENT(INOUT) :: N,i
      REAL*8, INTENT(INOUT) :: y,z
      REAL*8,DIMENSION(0:N),INTENT (INOUT) :: a,b,c,d
      do i=1,N
	a(i) = (rand(0)*(y-z))+z        
	write(*,*) a(i)
	b(i) = (rand(0)*(y-z))+z
        write(*,*) b(i)
        c(i) = (rand(0)*(y-z))+z
        write(*,*) c(i)
        d(i) = (rand(0)*(y-z))+z
        write(*,*) d(i)
      end do
    END SUBROUTINE matrix
    SUBROUTINE Thomas(x,a,b,c,d,N,i)
      INTEGER,INTENT(INOUT) :: N,i
      REAL*8,DIMENSION(0:N),INTENT(INOUT) :: x,a,b,c,d
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
    END SUBROUTINE Thomas
END PROGRAM Thomas_Random_Matrix


