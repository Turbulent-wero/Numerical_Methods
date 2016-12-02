PROGRAM Dirichlet
  IMPLICIT NONE
  INTEGER :: N,M,j,counter=10
  REAL*8 :: pi,dx
  REAL*8,DIMENSION(0:100000) :: x,a,b,c,d,U,Uex,f
  OPEN(UNIT=1,FILE="Dirichlet.txt",ACTION="WRITE")
  pi = 4.d0*datan(1.d0)
  do M=10,10000,10
    if(M==counter) then
    dx = pi/M
    do j=0,M		
      x(j) = j*dx		!Mesh
      Uex(j) = cos(x(j))	!Exact solution
      f(j) = -cos(x(j))
    end do
    U(0) = 1		!Lower boundary condition
    U(M) = -1		!Upper boundary condition         	
    do j=1,M-1 		!Matrix
      if(j==1) then
        b(j) = -2
        c(j) = 1
        d(j) = (dx**2)*f(j)-U(0)
        else if(j==M-1) then
          a(j) = 1
          b(j) = -2
          d(j) = (dx**2)*f(j)-U(M)
        else
          a(j) = 1
	  b(j) = -2
  	  c(j) = 1  
  	  d(j) = (dx**2)*f(j)
      end if
    end do
    N=M-1
    call Thomas(U,a,b,c,d,N,j)	!Call Thomas algorithm subroutine
    call error(j,M,U,Uex,dx)	!Call error subroutine
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
        U(j) = (d(j)-c(j)*x(j+1))/b(j)
      end do
!      do j=1,N
!        write(*,*) "x(",j,") = ",x(j)
!      end do
    END SUBROUTINE Thomas
    SUBROUTINE error(j,M,U,Uex,dx)
      INTEGER,INTENT(INOUT) :: j
      INTEGER,INTENT(IN) :: M
      REAL*8,INTENT(INOUT) :: dx
      REAL*8,DIMENSION(0:1000),INTENT(INOUT) :: U,Uex
      REAL*8 :: e=0
      do j=0,M
	e = e + (abs(Uex(j)-U(j)))**2
      end do
      e = (e*dx)**.5
      write(1,*) dx,e
    END SUBROUTINE error
END PROGRAM Dirichlet
