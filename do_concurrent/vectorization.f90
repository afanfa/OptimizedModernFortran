program vector_dep
  use worker_module
  use iso_fortran_env, only : real32
  implicit none

  integer, parameter :: n = 10000000
  integer :: i,k=1
  real(real32),dimension(n) :: a
  real(real32) :: c

  a = (/ (i, i = 1, n ) /)
  c = 0.5

  call vec_dep(a,c,k,n)
  !call vec_dep_dir(a,c,k,n)
  !call vec_dep_do_concurrent(a,c,k,n)

  write(*,*) a(10)

end program vector_dep
