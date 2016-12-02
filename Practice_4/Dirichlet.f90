PROGRAM Dirichlet
  IMPLICIT NONE
  Integer*8 :: N,cont=10
  Real*8 :: dx,pi
  OPEN(UNIT=1,FILE="Dirichlet.txt",ACTION="WRITE")
!   read(*,*)N
  do N=10,10000,10
  if(N==cont) then
    pi = 4.d0*datan(1.d0)
    dx = pi/N
    call algorithm(N,dx)
    cont=cont*10
  end if
  end do
! call error
CONTAINS
  SUBROUTINE algorithm(N,dx)
    INTEGER*8,INTENT(IN) :: N
    REAL*8,INTENT(IN) :: dx
    INTEGER :: i
    REAL*8,DIMENSION(0:N) :: x,a,b,c,d,U,Uex,f
    REAL*8 :: error
    U(0) = 1
    U(N) = -1
    do i=0,N
      x(i) = i*dx			!Mesh
      Uex(i) = cos(x(i))		!Exact Solution
    end do
    do i=1,N-1
      f(i) = -cos(x(i))			!Value of f(i)
      a(i) = 1				!Set or Matrix "A"
      b(i) = -2	
      c(i) = 1	
      if(i==1) then			!Right hand side "d"
        d(i) = (dx**2)*(f(i))-U(0)
        else if(i==N-1) then
          d(i) = (dx**2)*(f(i))-U(N)
        else
          d(i) = (dx**2)*(f(i))
      end if
    end do
!    write(*,*) d(1),d(2),d(N-1)
!Thomas Algorithm    
    do i=2,N-1
      b(i) = b(i) - (a(i)/b(i-1))*c(i-1)
      d(i) = d(i) - (a(i)/b(i-1))*d(i-1)
!      write(*,*)a(i),b(i)
    end do
    U(N-1) = d(N-1)/b(N-1)
    do i=N-2,1,-1
      U(i) = (d(i)-c(i)*U(i+1))/b(i)
    end do
!Error
    error = 0
    do i=0,N
      error = error + (Uex(i) - U(i))**2
    end do
    error = (dx*error)**.5
    write(1,*) log(dx/pi),log(error)
!    write(1,*) dx/pi,error
  END SUBROUTINE algorithm

    

END PROGRAM Dirichlet
