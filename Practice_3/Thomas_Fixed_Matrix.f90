PROGRAM Thomas_Fixed_Matrix
  IMPLICIT NONE
  INTEGER :: i,N=4
  REAL*8,DIMENSION(0:1000) :: x,a,b,c,d,Xex
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
  do i=1,N
    write(*,*)"Type the value of the exact x(",i,")"
    read(*,*) Xex(i)
  end do
  call Thomas(x,a,b,c,d,N,i)
  call error(i,N,x,Xex)
CONTAINS
    SUBROUTINE Thomas(x,a,b,c,d,N,i)
      INTEGER,INTENT(INOUT) :: N,i
      REAL*8,DIMENSION(0:1000),INTENT(INOUT) :: x,a,b,c,d
    
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
    SUBROUTINE error(i,N,x,Xex)
      INTEGER,INTENT(INOUT) :: i,N
      REAL*8,DIMENSION(0:1000),INTENT(INOUT) :: x,Xex
      REAL*8,DIMENSION(0:1000) :: e
      do i=1,N
	e(i) = abs(x(i)-Xex(i))
	write(*,*)"Error at ",i,"=",e(i)
      end do
    END SUBROUTINE error
END PROGRAM Thomas_Fixed_Matrix
