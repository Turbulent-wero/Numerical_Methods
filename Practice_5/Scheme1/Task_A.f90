PROGRAM TestA
  IMPLICIT NONE
  real*8 :: time=0.d0,pi=4.d0*datan(1.d0),tmax
  integer :: i,j,iter=0
  real*8,dimension(0:10000) :: t,x,Um,Umplus,Umminus
  READ(*,*)M,dt
  time=0.d0
  iter=0
  dx = pi/M
  stab = dt/(dx**2)
  if(stab<=.5) then
    do i=0,M
      x(i) = i*dx
      t(i) = i*dt
      Um(i) = sin(x(i))
    end do
    do iter=0,itermax
      time = iter*dt
      Umplus(0) = 0
      do j=1,M-1
	Umplus(j) = a*Um(j-1)+b*Um(j)+c*Um(j+1)
      end do
      Umplus(M) = 0
      Um(iter) = Umplus
    end do
END PROGRAM TestA
