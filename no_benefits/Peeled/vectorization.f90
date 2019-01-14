program no_benefits
  use iso_fortran_env, only : real32
  implicit none

  integer, parameter :: n = 20000*8!999000004
  integer :: i,j
  real(real32),allocatable:: a(:), b(:), c(:)
  real(real32) :: rec

  allocate(a(n),b(n),c(n))

  write(*,*) "Memory allocated (MB): ",8.0*n*3/1024**2

  rec = 1.0/n

  do i = 1, n
     a(i) = rec
     b(i) = 2.0
     c(i) = i
  end do

 do i = 1, n
    do j =1, n-3 
       c(i) = c(i) + a(j) * b(j) * 2*rec
    end do
 end do

  write(*,*) c(100),c(1000)

end program no_benefits
