program vector_dep
  use worker_module
  use iso_fortran_env, only : real32
  implicit none

  integer, parameter :: n = 1000000
  integer :: i,k=1
  real(real32),dimension(n) :: a
  real(real32) :: c

  a = (/ (i, i = 1, n ) /)

  call vec_dep(a,c,k,n)

end program vector_dep
