PROGRAM Newmann
  IMPLICIT NONE
  INTEGER :: N,M,j,counter=10
  REAL*8 :: pi,dx
  REAL*8,DIMENSION(0:100000) :: x,a,b,c,d,U,Uex,f
  OPEN(UNIT=1,FILE="Newmann.txt",ACTION="WRITE")
  pi = 4.d0*datan(1.d0)
  do M=10,10000,10
    if(M==counter) then
    dx = pi/M
    do j=0,M+1		
      x(j) = j*dx		!Mesh
      Uex(j) = cos(pi*x(j))	!Exact solution
      f(j) = (-(pi)**2)*(cos(pi*x(j)))
    end do     	
    do j=0,M+1	 		!Matrix
      if(j==0) then
        b(j) = -2
        c(j) = 2
        d(j) = (dx**2)*f(j)
        else if(j==M+1) then
          a(j) = 2
          b(j) = -2*(1+dx)
          d(j) = (dx**2)*f(j)-2*dx
        else
          a(j) = 1
	  b(j) = -2
  	  c(j) = 1  
  	  d(j) = (dx**2)*f(j)
      end if
!write(1,*) a(j),b(j),c(j),d(j)
    end do
    N=M+1
    call Thomas(U,a,b,c,d,N,j)	!Call Thomas algorithm subroutine
    call error(j,N,U,Uex,dx)	!Call error subroutine
    counter=counter*10
    end if
  end do
CONTAINS
    SUBROUTINE Thomas(U,a,b,c,d,N,j)
      INTEGER,INTENT(INOUT) :: N,j
      REAL*8,DIMENSION(0:1000),INTENT(INOUT) :: U,a,b,c,d
    
      do j=2,N
        b(j) = b(j) - (a(j)/b(j-1))*c(j-1)
        d(j) = d(j) - (a(j)/b(j-1))*d(j-1)
      end do
      U(N) = d(N)/b(N)
      do j=N-1,1,-1
        U(j) = (d(j)-c(j)*U(j+1))/b(j)
      end do
!do j=0,N
 !write(1,*) x(j),Uex(j),U(j)!"x(",j,") = ",
!end do
    END SUBROUTINE Thomas
    SUBROUTINE error(j,N,U,Uex,dx)
      INTEGER,INTENT(INOUT) :: j
      INTEGER,INTENT(IN) :: N
      REAL*8,INTENT(INOUT) :: dx
      REAL*8,DIMENSION(0:1000),INTENT(INOUT) :: U,Uex
      REAL*8 :: e
      e=0
      do j=1,N
	e = e + (abs(Uex(j+1)-U(j-1)))**2
      end do
      e = (e*dx)**.5
      write(1,*) dx/pi,e
    END SUBROUTINE error
END PROGRAM Newmann
