program Num_Int
    IMPLICIT NONE
    INTEGER :: i,j,N
    REAL :: Arear, Areat, Aream, Areas, a=0, b=1, h, integral
    REAL :: errorr, errort, errorm, errors
    OPEN(UNIT=1,FILE="Equation1.txt",ACTION="WRITE")
    OPEN(UNIT=2,FILE="Rectangular.txt",ACTION="WRITE")
    OPEN(UNIT=3,FILE="Trapezoidal.txt",ACTION="WRITE")
    OPEN(UNIT=4,FILE="Midpoint.txt",ACTION="WRITE")
    OPEN(UNIT=7,FILE="Simpson.txt",ACTION="WRITE")
    OPEN(UNIT=8,FILE="ErrorRectangular.txt",ACTION="WRITE")
    OPEN(UNIT=9,FILE="ErrorTrapezoidal.txt",ACTION="WRITE")
    OPEN(UNIT=10,FILE="ErrorMidpoint.txt",ACTION="WRITE")
    OPEN(UNIT=11,FILE="ErrorSimpson.txt",ACTION="WRITE")
    N = 1
    integral = ((b**5)/5-(b**4)/4) - ((a**5)/5-(a**4)/4)
    print*,integral
    WRITE(1,*) "f(x) = x^4-x^3",integral
    do i = 1,5 
      h = (b-a)/N
      print*, "N =",N
      WRITE(1,*) "N =",N
      Arear = 0
      Areat = 0
      Aream = 0
      Areas = 0
      do j = 0, N
        CALL rectangular(N,j,h,a,Arear)
        CALL trapezoidal(N,j,h,a,Areat)
        CALL mid_point(N,j,h,a,Aream)
        CALL simpson(N,j,h,a,Areas)
      end do
      errorr = abs(integral-arear)
      errort = abs(integral-areat)
      errorm = abs(integral-aream)
      errors = abs(integral-areas)
      WRITE(1,*) "Rectangular = ", Arear,"Error =", errorr
      WRITE(1,*) "Trapezoidal = ", Areat,"Error =", errort
      WRITE(1,*) "Mid Point   = ", Aream,"Error =", errorm
      WRITE(1,*) "Simpson     = ", Areas,"Error =", errors
      WRITE(8,*)N,errorr
      WRITE(9,*)N,errort
      WRITE(10,*)N,errorm
      WRITE(11,*)N,errors
      N = N*2
      WRITE(1,*) "----------------------------------------------------"
    end do
    CLOSE(UNIT=1)
CONTAINS
    SUBROUTINE rectangular(N,j,h,a,Arear)
        IMPLICIT NONE
        INTEGER,INTENT(IN) :: N,j
        REAL,INTENT(INOUT) :: Arear,h,a
        REAL,DIMENSION(0:N) :: x,r
        x(j) = a+j*h
        if(j/=N) then
          r(j) = x(j)**4-x(j)**3
          !print*, "r(",j,") = ",r(j)
          Arear = Arear + h*r(j)
          !print*,"Area (",j,") = ",Arearect
        end if
        !print*,"-------------------------------------------"
        if(N==16) then
          WRITE(2,*)x(j),Arear
        end if
    END SUBROUTINE rectangular

    SUBROUTINE trapezoidal(N,j,h,a,Areat)
        IMPLICIT NONE
        INTEGER,INTENT(IN) :: N,j
        REAL,INTENT(INOUT) :: Areat,h,a
        REAL,DIMENSION(0:N) :: x,r
        x(j) = a+j*h
        r(j) = x(j)**4-x(j)**3
        !print*, "r(",j,") = ",r(j)
        if(j==0 .OR. j==N) then
            Areat = Areat + h*(r(j)/2)
        else
        !print*,"Area (",j,") = ",Areat
        Areat = Areat + h*r(j)  
        !print*, "Area total = ",Areat
        !print*,"-------------------------------------------" 
        if(N==16) then
          WRITE(3,*)x(j),Areat
        end if           
        end if
    END SUBROUTINE trapezoidal

    SUBROUTINE mid_point(N,j,h,a,Aream)
        IMPLICIT NONE
        INTEGER,INTENT(IN) :: N,j
        REAL,INTENT(INOUT) :: Aream,h,a
        REAL,DIMENSION(0:N) :: x,x1
        x(j) = a+j*h
        x1(j) = x(j)+h
        !print*,x(j)
        if(j<N) then
          Aream = Aream + h*(((x(j)+x1(j))/2)**4-((x(j)+x1(j))/2)**3)  
          !print*, "r(",j,") = ",r(j)
        end if
        !print*,"Area (",j,") = ",Area
        !print*, "Area total = ",Aream
        !print*,"-------------------------------------------"
        if(N==16) then
          WRITE(4,*)x(j),Aream
        end if
    END SUBROUTINE mid_point

    SUBROUTINE simpson(N,j,h,a,Areas)
        IMPLICIT NONE
        INTEGER,INTENT(IN) :: N,j
        REAL,INTENT(INOUT) :: Areas,h,a
        REAL,DIMENSION(0:N) :: x,x1,x2
        if(N==1) then
          Areas = 0
        end if
        x(j) = a+j*h
        x1(j) = x(j)+h
        x2(j) = x1(j)+h
        if(MOD(j,2)==0 .AND. j<N) then
          Areas = Areas + (h/3)*(x(j)**4-x(j)**3+4*(x1(j)**4-x1(j)**3)+x2(j)**4-x2(j)**3)
        end if
        !print*,"Area (",j,") = ",Area
        !print*, "Area total = ",Areas
        !print*,"-------------------------------------------"
        if(N==16) then
          WRITE(7,*)x(j),Areas
        end if
    END SUBROUTINE simpson
END PROGRAM Num_Int
