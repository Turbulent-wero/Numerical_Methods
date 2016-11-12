program Num_Int
    IMPLICIT NONE
    INTEGER :: i,j,N
    REAL :: Arear, Areat, Aream, Areas, a=0, b=1, h, integral
    REAL :: errorr, errort, errorm, errors
    OPEN(UNIT=1,FILE="Num_Int.txt",ACTION="WRITE")
    N = 1
    integral = (3*b+b*b+(5*b*b*b)/3) - (3*a+a*a+(5/3)*a*a*a)
    print*,integral
    do i = 1,5 
      h = (b-a)/N
      print*, "N =",N
      WRITE(1,*) "f(x) = 3+2x+5x²"
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
          r(j) = 3+2*x(j)+5*x(j)*x(j)
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
        r(j) = 3+2*x(j)+5*x(j)*x(j)
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
        REAL,DIMENSION(0:N) :: x
        x(j) = (a+h)/2 + h*j
        !print*,x(j)
        if(j<N) then
          Aream = Aream + h*(3+2*x(j)+5*x(j)*x(j))  
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
          Areas = Areas + (h/3)*((3+2*x(j)+5*(x(j)**2))+4*((3+2*x1(j)+5*(x1(j)**2)))+(3+2*x2(j)+5*(x2(j)**2)))
        end if
        !print*,"Area (",j,") = ",Area
        !print*, "Area total = ",Areas
        !print*,"-------------------------------------------"
    END SUBROUTINE simpson

    SUBROUTINE gauss(N
        IMPLICIT NONE
        
        
END PROGRAM Num_Int