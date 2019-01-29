program counting
  implicit none
  
  integer, parameter :: n = 50000000
  integer :: i,j,counter
  real, allocatable :: a(:), b(:)

  allocate(a(n),b(n))

  counter = 0
  j = 0

  a = (/ (i, i = 1, n) /)
  b = 0
  do i = 1, n
     if(a(i) >= 10) then
        counter = counter + 1
        j = j + 1
        b(j) = counter
     end if
  end do

end program counting
