Program Fourier
IMPLICIT NONE
  INTEGER :: N
  OPEN(UNIT=1,FILE="Fourier.txt",ACTION="WRITE")
  WRITE(*,*) "Ingrese el valor de N"
  READ(*,*)N
  CALL FS(N)
CONTAINS
  SUBROUTINE FS(N)
  INTEGER,INTENT(IN) :: N
  INTEGER :: i,j
  REAL*8 :: dx,a=-3.14159265,b=3.14159265,f
  dx=(b-a)/100
  do i=0,100
    f=0
    a=a+dx
    do j=1,N
      f = f + (1/j)*((4*2)/b)*sin(j*a)
    end do
    WRITE(1,*) a,f
  end do
  END SUBROUTINE
     
END PROGRAM Fourier
