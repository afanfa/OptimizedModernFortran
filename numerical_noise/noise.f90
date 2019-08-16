program noise
  use iso_fortran_env, only : real32, real64
  implicit none

  integer, parameter :: n = 100000
  real(real32), allocatable :: a(:),b(:)
  real(real32) :: redux
  integer :: i

  allocate(a(n),b(n))

  do i = 1, n
     a(i) = i + 314.1415
     b(i) = 1/(1+10000*i)
  end do

  redux = 0.0
!dir$ novector
  do i = 1, n
     redux = redux + a(i) + b(i)
  end do

  write( *, "(F24.8)") redux

end program noise
