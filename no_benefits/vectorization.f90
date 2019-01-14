program no_benefits
  use iso_fortran_env, only : real64
  implicit none

  integer, parameter :: n = 1990000!999000004
  integer :: i,j
  real(real64),allocatable:: a(:), b(:), c(:)
  real(real64) :: rec

  allocate(a(n),b(n),c(n))

  write(*,*) "Memory allocated (MB): ",8.0*n*3/1024**2

  rec = 1.0/n

  do i = 1, n
     a(i) = rec
     b(i) = 2.0
     c(i) = i
  end do

!dir$ novector
 do i = 1, n
    c(i) = c(i) + a(i) * b(i) * 2*rec
 end do

  write(*,*) c(100),c(1000)

end program no_benefits
