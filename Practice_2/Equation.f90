!program equation
!implicit none
!character(len=10) :: eq,res
!integer :: i
!integer,dimension(1:10) :: pos
!character,dimension(1:10) :: cambio
!
!  print*,"Type an equation"
!  read(*,*) eq
!  res = scan(eq,"1")
!  print*,res
!   do i=48,57
!     res = char(i)
!     print*, res
!   end do
!end program equation

  program read_val
            integer value
            character(len=10) string
            
            read(*,*) string
      
          
            ! Convert a string to a numeric value
            read (string,'(I10)') value
            print *, value
          
          end program read_val

