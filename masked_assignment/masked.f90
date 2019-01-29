program masked
  use worker_module
  use iso_fortran_env, only : real64
  implicit none
  
  integer, parameter :: n = 50000000
  integer :: i
  real(real64), allocatable :: a(:), b(:),c(:)

  allocate(a(n),b(n),c(n))

  b = (/ (i, i = 1, n) /)

  c = 0.0

  do i = 1, n
     a(i) = i + 2.0
  end do

   call where_all(a,b,c)
   write(*,*) c(10)

  !call if_then_all(a,b,c)
  !write(*,*) c(10)

  ! c = 0.0
  !call where_sqrt(a,b,c)
  !write(*,*) c(10)

end program masked
