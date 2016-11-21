program test_random_number
            INTEGER :: i
            REAL,DIMENSION(0:4) :: r
            CALL RANDOM_NUMBER(r)
            do i=0,4
              print*,r(i)
            end do
          end program
