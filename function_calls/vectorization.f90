program vector_1
  use worker_module
  use iso_fortran_env, only : real64
  implicit none

  integer, parameter :: n = 1000000
  integer :: i,k = 1
  real(real64),dimension(n) :: a, b, c, d
  real(real64) :: tmp, pi

  d = (/ (i, i = 1, n ) /)

  do i = 1, n
     c(i) = my_scalar_add(a(i),b(i))
  end do

  do i = 1, n
     c(i) = my_scalar_add_pure(a(i),b(i))
  end do

  do i = 1, n
     call my_scalar_add_elemental_value(c(i), a(i), b(i))
  end do

  do i = 1, n
     c(i) = my_scalar_add_elemental(a(i),b(i))
  end do

  pi = 3.1415_real64
  tmp = 1.0
  
  do i = 1, n
     c(i) = my_scalar_add_elemental(pi,tmp)
  end do

  c = my_scalar_add_elemental(a(i),b(i))

  do i = 1, n
     c(i) = nice_function_pure(pi,tmp)
  end do

contains

  pure function nice_function_pure(x1, x2) result(res)
    implicit none
    real(real64),intent(in) :: x1, x2
    real(real64) :: res

    res = x1 + x2

  end function nice_function_pure

end program vector_1
