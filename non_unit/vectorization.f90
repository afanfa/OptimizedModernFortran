program vector_1
  use iso_fortran_env, only : real64
  implicit none

  integer, parameter :: n = 9999000
  integer :: i
  real(real64),allocatable :: a_a(:),a_b(:)

  allocate(a_a(n),a_b(n))

  do i = 1, n
     a_a(i) = 1.0/n
     a_b(i) = 2.0
  end do

  do i = 1, n, 2
     a_b(i) = a_a(i) + a_b(i)
  end do

  write(*,*) a_b(10)

end program vector_1
