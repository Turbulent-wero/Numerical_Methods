program Gauss
    IMPLICIT NONE
    INTEGER :: i,j,k
    REAL :: Area, a=1,b=6,integral, error
    REAL,DIMENSION(1:7) :: x,y
 
    OPEN(UNIT=1,FILE="PointsWeights.txt",ACTION="READ")
    OPEN(UNIT=2,FILE="GaussResults.txt",ACTION="WRITE")
    OPEN(UNIT=3,FILE="Gauss.txt",ACTION="WRITE")
    OPEN(UNIT=4,FILE="ErrorGauss.txt",ACTION="WRITE")

    integral = (2*b-(b**.5*(cos(2*b**.5)))+((sin(2*b**.5))/2)) - (2*a-(a**.5*(cos(2*a**.5)))+((sin(2*a**.5))/2))
    !integral = log(b+2)-log(a+2)
    print*,integral
    WRITE(2,*) "f(x) = 2+sin(2x^.5)"

    do i = 2,7
    Area = 0
      do j = 1,i
        READ(1,*) x(j),y(j)
        print*, x(j),y(j)
        Area = Area + (2+sin(2*(x(j)**.5)))
        Area = Area*y(j)
        !Area = Area + (1/(x(j)+2))*y(j)
        !WRITE(2,*) Area
      end do
      print*,"                                                   ",Area
      error = abs(integral-Area)
      WRITE(2,*) "N =",i,"Integral =",Area,"Error =",error
      WRITE(2,*) "-----------------------------------"
      WRITE(4,*)i,error
    end do
    CLOSE(UNIT=1)
    CLOSE(UNIT=2)
    CLOSE(UNIT=3)
    CLOSE(UNIT=4)

END PROGRAM Gauss







