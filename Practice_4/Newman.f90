PROGRAM Newman
  IMPLICIT NONE
  Integer*8 :: N
  Real*8 :: dx,pi
  OPEN(UNIT=2,FILE="Newman.txt",ACTION="WRITE")
  READ(*,*) N
  pi = 4.d0*datan(1.d0)
  dx = pi/N
  call algorithm(N,dx)
! call error
CONTAINS
  SUBROUTINE algorithm(N,dx)
    INTEGER*8,INTENT(IN) :: N
    REAL*8,INTENT(IN) :: dx
    INTEGER :: i
    REAL*8,DIMENSION(0:N) :: x,a,b,c,d,U,Uex,f
    REAL*8 :: error
    do i=0,N
      x(i) = i*dx			!Mesh
      Uex(i) = cos(x(i))		!Exact Solution
    end do
    do i=0,N
      f(i) = -cos(x(i))			!Value of f(i)
      if(i==0) then			!Set of Matrix "A"
        b(i) = -2
        c(i) = 2
      else if (i==N) then
          a(i) = 2
          b(i) = -2*(1+dx)
        else
          a(i) = 1				
          b(i) = -2	
          c(i) = 1	
      end if
      if(i==N) then			!Right hand side "d"
        d(i) = (dx**2)*(f(i))/2-dx
        else 
          d(i) = (dx**2)*(f(i))
      end if
    end do
!    write(*,*) d(1),d(2),d(N-1)
!Thomas Algorithm    
    do i=1,N-1
      b(i) = b(i) - (a(i)/b(i-1))*c(i-1)
      d(i) = d(i) - (a(i)/b(i-1))*d(i-1)
!      write(*,*)a(i),b(i)
    end do
    
    U(N) = d(N)/b(N)
    do i=N-1,1,-1
      U(i) = (d(i)-c(i)*U(i+1))/b(i)
    end do
!Error
    error = 0
    do i=0,N
      write(*,*) x(i),Uex(i),U(i)
      error = error + (Uex(i) - U(i))**2
    end do
    error = (dx*error)**.5
    write(2,*) log(dx/pi),log(error)
  END SUBROUTINE algorithm

    

END PROGRAM Newman
