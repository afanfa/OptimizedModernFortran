program mem_alignment
  use iso_fortran_env, only : real32, real64
  implicit none

  integer, parameter :: n = 16
  integer :: i,k = 1
  real(real64),dimension(n) :: a, b
  real(real64),allocatable :: a_a(:), a_b(:)
  real(real32),dimension(n) :: a_s, b_s
  real(real32),allocatable :: a_a_s(:), a_b_s(:)
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

  deallocate(a_a,a_b)

  allocate(a_a_s(n),a_b_s(n))

  do i = 1, n
     a_s(i) = 1.0/n
     b_s(i) = 2.0
  end do  

  do i = 1, n
     a_a_s(i) = 1.0/n
     a_b_s(i) = 2.0
  end do

end program mem_alignment
