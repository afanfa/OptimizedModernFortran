program convolve
  implicit none

  integer, parameter :: n = 91000000
  integer, parameter :: kernel_size = 5
  real, dimension(n) :: y
  real, dimension(n) :: x
  real, dimension(kernel_size) :: h
  integer :: i,j,k

  do i = 1, kernel_size
     h(i) = (-1)**(i)
  end do

  h(3) = 5

  write(*,*) 'Kernel',h

  y = 0.0

  do i = 1, n
     x(i) = i
  end do

!Valid convolution
!dir$ novector
  do i=kernel_size, n
     j=i
!dir$ novector
     do k=1,kernel_size
        y(i) = y(i) + x(j)*h(k)
        j = j-1
     end do
  end do
  
  ! write(*,*) y
               
end program convolve
