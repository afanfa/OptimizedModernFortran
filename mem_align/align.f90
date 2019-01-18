program mem_alignment
  use iso_fortran_env, only : real64
  implicit none

  integer, parameter :: n = 10
  integer :: i,k = 1
  real(real64),dimension(n) :: a, b
  real(real64),allocatable :: a_a(:), a_b(:)
  real(real64) :: tmp, rec

  allocate(a_a(n),a_b(n))

  do i = 1, n
     a(i) = 1.0/n
     b(i) = 2.0
  end do

  do i = 1, n
     a_a(i) = 1.0/n
     a_b(i) = 2.0
  end do  

end program mem_alignment
