program Num_Int
    IMPLICIT NONE
    INTEGER :: i,N
    REAL :: Area, a=0, b=1, h
    

    !OPEN(UNIT=1,FILE="Num_Int.txt",ACTION="WRITE")
    N = 1
    do i = 1,5 
      h = (b-a)/N
      print*, "N =",N
      !WRITE(1,*) "N =",N,"                |"
      CALL rectangular(N,h,a,Area)
      !WRITE(1,*) "Rectangular = ", Area,"|"
      CALL trapezoidal(N,h,a,Area)
      !WRITE(1,*) "Trapezoidal = ", Area,"|"
      CALL mid_point(N,h,a,Area)
      !WRITE(1,*) "Mid Point   = ", Area,"|"
      CALL simpson(N,h,a,Area)
      !WRITE(1,*) "Simpson     = ", Area,"|"
      N = N*2
      !WRITE(1,*) "---------------------------"
    end do
    !CLOSE(UNIT=1)
CONTAINS
    SUBROUTINE rectangular(N,h,a,Area)
        IMPLICIT NONE
        INTEGER,INTENT(IN) :: N
        REAL,INTENT(INOUT) :: Area,h,a
        INTEGER :: j
        REAL,DIMENSION(0:N) :: x,r
        Area = 0
        do j = 0, N
          x(j) = a + j*h
          !print*, "x(",j,") = ",x(j)
          if(j/=N) then
            r(j) = 3+2*x(j)+5*x(j)*x(j)
            !print*, "r(",j,") = ",r(j)
            Area = Area + r(j)
            !print*,"Area (",j,") = ",Arearect
          end if
        end do
        Area = h*Area
        print*, "Area total = ",Area
        !print*,"-------------------------------------------"
    END SUBROUTINE rectangular

    SUBROUTINE trapezoidal(N,h,a,Area)
        IMPLICIT NONE
        INTEGER,INTENT(IN) :: N
        REAL,INTENT(INOUT) :: Area,h,a
        INTEGER :: j
        REAL,DIMENSION(0:N) :: x,r
        Area = 0
        do j = 0, N
          x(j) = a + j*h
          !print*, "x(",j,") = ",x(j)
          r(j) = 3+2*x(j)+5*x(j)*x(j)
          !print*, "r(",j,") = ",r(j)
          if(j/=0 .AND. j/=N) then
              Area = Area + r(j)
          end if
          !print*,"Area (",j,") = ",Area
        end do
        Area = r(0)/2 + Area + r(N)/2  
        Area = h*Area
        print*, "Area total = ",Area
        !print*,"-------------------------------------------"
    END SUBROUTINE trapezoidal

    SUBROUTINE mid_point(N,h,a,Area)
        IMPLICIT NONE
        INTEGER,INTENT(IN) :: N
        REAL,INTENT(INOUT) :: Area,h,a
        INTEGER :: j
        REAL,DIMENSION(0:N) :: x,r
        Area = 0
        do j = 0, N
          x(j) = a + j*h
          !print*, "x(",j,") = ",x(j)
          !print*, "r(",j,") = ",r(j)
          if(j/=0) then
              r(j) = 3+2*((x(j)+x(j-1))/2)+5*((x(j)+x(j-1))/2)*((x(j)+x(j-1))/2)    
              Area = Area + r(j)
          end if
          !print*,"Area (",j,") = ",Area
        end do
        Area = h*Area
        print*, "Area total = ",Area
        !print*,"-------------------------------------------"
    END SUBROUTINE mid_point

    SUBROUTINE simpson(N,h,a,Area)
        IMPLICIT NONE
        INTEGER,INTENT(IN) :: N
        REAL,INTENT(INOUT) :: Area,h,a
        INTEGER :: j
        REAL,DIMENSION(0:N) :: x,r
        Area = 0
        do j = 0, N
          x(j) = a + j*h
          !print*, "x(",j,") = ",x(j)
          !print*, "r(",j,") = ",r(j)
          if(MOD(j,2)==0 .AND. j/=0) then
              r(j) = (3+2*x(j)+5*x(j)*x(j))+4*((3+2*x(j-1)+5*x(j-1)*x(j-1)))+(3+2*x(j-2)+5*x(j-2)*x(j-2))
              Area = Area + r(j)
          end if
          !print*,"Area (",j,") = ",Area
        end do
        Area = (h/3)*Area
        print*, "Area total = ",Area
        print*,"-------------------------------------------"
    END SUBROUTINE simpson
END PROGRAM Num_Int
