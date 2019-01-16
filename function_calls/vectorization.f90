program vector_1
  use worker_module
  use iso_fortran_env, only : real64
  implicit none

  integer, parameter :: n = 1000000
  integer :: i,k = 1
  real(real64),dimension(n) :: a, b, c
  real(real64) :: tmp

  do i = 1, n
     a(i) = 1.0/n
     b(i) = 2.0
  end do

  do i = 1, n
     b(i) = a(i) + b(i)
  end do

  tmp = 0.0
  do i = 1, n - 1
     tmp = tmp + a(i)
  end do

  do i = 1, n
     c(i) = my_scalar_add(a(i),b(i))
  end do

  do concurrent (i=1:n)
     c(i) = my_scalar_add(a(i),b(i))
  end do

end program vector_1
