program mandelbrot
  implicit none

  real :: x, y
  complex :: z, c
  real, parameter :: x_max=0.5, x_min = -2.0, y_max = 1.25, y_min = -1.25
  integer, parameter :: m = 1000, n = 50000, max_iter = 50000
  real, dimension(m,n) :: final_matrix
  integer :: i, j, iter
  logical :: diverge
  
  final_matrix = 0.0

  do i = 1, m
     do j = 1, n
        diverge = .false.
        x = (real(j-1) * x_max + real(m-j) * x_min) / real(m-1)
        y = (real(i-1) * y_max + real(n-i) * y_min) / real(n-1)

        c = CMPLX(x,y)
        z = c

        do iter = 1, max_iter
           z = z ** 2 + c
        end do
        
        if (abs(z) > n) diverge = .true.

        if(diverge .eqv. .false.) final_matrix(i,j) = 1.0
     end do
  end do

  write(*,*) final_matrix(100,100)
  
end program mandelbrot
