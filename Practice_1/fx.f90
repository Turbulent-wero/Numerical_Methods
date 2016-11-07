program fx
implicit none
integer :: i
integer,parameter :: N=4
real :: deltax, HR=1, LR=-1
real,dimension(0:N) :: x,y

 deltax = (HR-LR)/N
 do i = 0,N
  x(i) = -1 + i*deltax
  y(i) = 1/(1+25*x(i)*x(i)) 
  print*,x(i),y(i)
 end do

end program fx

 
