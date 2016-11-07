program homework
implicit none
integer :: i,j
integer,parameter :: N=9, P=1000
real :: HR=1, LR=-1, deltax, deltax2, temp
real,dimension(0:N) :: x,alpha
real,dimension(0:P) :: x2,point

OPEN(UNIT=1,FILE="P9.txt",ACTION="WRITE")

 deltax=(HR-LR)/N
 do i = 0,N
  x(i) = LR+i*deltax
  alpha(i)=1/(1+25*(x(i)*x(i)))
  print*, x(i), alpha(i)
 end do 
 do i = 1,N
  do j = N,i,-1
   alpha(j) = (alpha(j)-alpha(j-1))/(x(j)-x(j-i))
   end do
  print*, alpha(i)
 end do
 deltax2=(HR-LR)/P
 print*, deltax2
  do i = 0,P
   x2(i) = LR+i*deltax2
   !WRITE(1,*) x2(i)
  end do 
  do i=0,P
   temp = alpha(N)
   do j=N,1,-1
    temp = temp*(x2(i)-x(j-1))+alpha(j-1)
   end do
   point(i) = temp
   !print*, x2(i),"  ",point(i)
   WRITE(1,*) x2(i),point(i)
  end do

CLOSE(UNIT=1)
end program homework
