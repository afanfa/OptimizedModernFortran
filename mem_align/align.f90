program mem_alignment
  use iso_fortran_env, only : real32, real64
  implicit none

  integer, parameter :: n = 10000000
  integer :: i,k = 1
  real(real64),dimension(n) :: a, b
  real(real64),allocatable :: a_a(:), a_b(:)
 ! real(real32),dimension(n) :: a_s, b_s
 ! real(real32),allocatable :: a_a_s(:), a_b_s(:)
  real(real64) :: tmp, rec
  integer :: j, iter=1000

  allocate(a_a(n),a_b(n))

  rec = 1/n

  do j=1,iter
    do i = 1, n
       a(i) = 1.0 - 3.1415 + 2*(n - i + 1.0_real64) * rec
       b(i) = 2.0 + 6.283 + 5*(n - i + 1.0_real64)
    end do
  end do

  ! do j = 1,iter
  !    do i = 1, n
  !       a_a(i) = 1.0 - 3.1415 + 2*( n - i + 1.0_real64) * rec
  !       a_b(i) = 2.0 + 6.283 + 5*(n - i + 1.0_real64)
  !    end do
  ! end do

  ! do j = 1,iter
  !    do i = 1, n-1
  !       a(i) = 1.0 - 3.1415 + 2*( n - i + 1.0_real64) * rec
  !       a(i) = 2.0 + 6.283 + 5*(n - i + 1.0_real64)
  !    end do
  ! end do

end program mem_alignment
