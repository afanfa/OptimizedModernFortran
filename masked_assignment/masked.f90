program masked
  use worker_module
  use iso_fortran_env, only : real64
  implicit none
  
  integer, parameter :: n = 100000
  integer :: counter, i
  real(real64), allocatable :: a(:), b(:),c(:)

  allocate(a(n),b(n),c(n))

  b = (/ (i, i = 1, n) /)

  c = 0.0
  counter = 0

  do i = 1, n
     a(i) = i + 2.0
  end do

  call where_all(a,b,c)
  write(*,*) c(10)
  c = 0.0
  c(1000) = 1.0
  call do_counter_partial_sum(c,counter)
  call do_counter_count(c,counter)
  write(*,*) counter


end program masked
