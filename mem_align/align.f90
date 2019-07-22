program mem_alignment
  use iso_fortran_env, only : real32, real64
  implicit none

  integer, parameter :: n = 2048
  integer :: i,k = 1
  real(real32),dimension(n) :: a, b
  real(real32),allocatable :: a_a(:), a_b(:)
  !dir$ attributes align:64 :: a_a, a_b

  real(real32) :: tmp, rec, c
  real(real64) :: time_Start, time_end
  integer :: j, iter=10000000, ierr

  allocate(a_a(n),a_b(n))

  rec = 1/n
  c = 0.0

  call cpu_time(time_start)

  do j=1,iter
    do i = 1, n
       a(i) = 1.0 - 3.1415 + 2*(n - i + 1.0) * rec + c
       b(i) = 2.0 + 6.283 + 5*(n - i + 1.0) + c
    end do
    c = c + 1
  end do

  call cpu_time(time_end)

  write(*,*) a(10), 'TIme:',time_end - time_start

  ! c = 0.0

  ! call cpu_time(time_start)

  ! do j = 1,iter
  !    do i = 1, n
  !       a_a(i) = 1.0 - 3.1415 + 2*( n - i + 1.0) * rec + c
  !       a_b(i) = 2.0 + 6.283 + 5*(n - i + 1.0) + c
  !    end do
  !   c = c + 1
  ! end do

  ! call cpu_time(time_end)

  ! write(*,*) a_a(10), 'TIme:',time_end - time_start

!To be commented when explaining vectorization
  ! do j = 1,iter
  !    do i = 1, n-1
  !       a(i) = 1.0 - 3.1415 + 2*( n - i + 1.0_real64) * rec
  !       a(i) = 2.0 + 6.283 + 5*(n - i + 1.0_real64)
  !    end do
  ! end do

end program mem_alignment
