program strength_red
  use iso_fortran_env, only : real64
  implicit none

  integer, parameter :: n = 19900000
  integer :: i,j
  real(real64), dimension(n) :: a,b
  real(real64) :: rec, pi = 3.1415, r

  a = (/ (i, i = 1, n) /)
  b = (/ (2*i, i = 1, n) /)

  do j = 1, 1000
     do i = 1, n
        a(i) = b(i) / pi
     end do
  end do

  ! rec = 1/pi

  ! do j = 1, 1000
  !    do i = 1, n
  !       a(i) = b(i) * rec
  !    end do
  ! end do
  
  write(*,*) a(100)

end program strength_red
