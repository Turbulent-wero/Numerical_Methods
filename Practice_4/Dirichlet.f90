PROGRAM Thomas
  IMPLICIT NONE
  Integer*8 :: N
  Real*8 :: dx,pi
  READ(*,*) N
  pi = 4.d0*datan(1.d0)
  dx = pi/N
  call algorithm(N,pi)
! call error
CONTAINS
  SUBROUTINE algorithm(N,pi)
    INTEGER*8,INTENT(IN) :: N
    REAL*8,INTENT(IN) :: pi
    INTEGER :: i
    REAL*8,DIMENSION(0:N) :: x,a,b,c,d,U,Uex,f
    U(0)=1
    U(N)=-1
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
!Thomas Algorithm    
    do i=2,N-1
      b(i) = b(i) - (a(i)/b(i-1))*c(i-1)
      d(i) = d(i) - (a(i)/b(i-1))*d(i-1)
    end do
!    U(N) = d(N)/b(N)
    do i=N-1,1,-1
      U(i) = (d(i)-c(i)*U(i+1))/b(i)
    end do
    do i=0,N
      write(*,*) x(i),Uex(i),U(i)
    end do
  END SUBROUTINE algorithm
!Error
    

END PROGRAM Thomas