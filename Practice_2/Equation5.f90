program Num_Int
    IMPLICIT NONE
    INTEGER :: i,j,N
    REAL :: Arear, Areat, Aream, Areas, a=2, b=7, h, integral
    REAL :: errorr, errort, errorm, errors
    OPEN(UNIT=1,FILE="Equation5.txt",ACTION="WRITE")
    N = 1
    integral = log(b)-log(a)
    print*,integral
    WRITE(1,*) "f(x) = 1/x"
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
          r(j) = 1/x(j)
          !print*, "r(",j,") = ",r(j)
          Arear = Arear + h*r(j)
          !print*,"Area (",j,") = ",Arearect
        end if
        !print*,"-------------------------------------------"
    END SUBROUTINE rectangular

    SUBROUTINE trapezoidal(N,j,h,a,Areat)
        IMPLICIT NONE
        INTEGER,INTENT(IN) :: N,j
        REAL,INTENT(INOUT) :: Areat,h,a
        REAL,DIMENSION(0:N) :: x,r
        x(j) = a+j*h
        r(j) = 1/x(j)
        !print*, "r(",j,") = ",r(j)
        if(j==0 .OR. j==N) then
            Areat = Areat + h*(r(j)/2)
        else
        !print*,"Area (",j,") = ",Areat
        Areat = Areat + h*r(j)  
        !print*, "Area total = ",Areat
        !print*,"-------------------------------------------"            
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
          Aream = Aream + h*(1/((x(j)+x1(j))/2))  
          !print*, "r(",j,") = ",r(j)
        end if
        !print*,"Area (",j,") = ",Area
        !print*, "Area total = ",Aream
        !print*,"-------------------------------------------"
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
          Areas = Areas + (h/3)*(1/x(j)+4*(1/x1(j))+1/x2(j))
        end if
        !print*,"Area (",j,") = ",Area
        !print*, "Area total = ",Areas
        !print*,"-------------------------------------------"
    END SUBROUTINE simpson
END PROGRAM Num_Int
