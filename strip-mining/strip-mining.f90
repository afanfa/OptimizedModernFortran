program strip_mining
  use iso_fortran_env, only : real64
  implicit none

  integer, parameter :: n = 1048576, m=65536
  integer, parameter :: tile = 32
  integer :: i,j,jj
  real(real64),allocatable:: a(:), b(:), c(:)
  real(real64) :: rec

  allocate(a(n),b(m),c(n))

  rec = 1.0/n

  do i = 1, n
     a(i) = rec
     c(i) = i
  end do

  do i = 1, m
     b(i) = i*2.0
  end do

  do jj = 1, m, tile
     do i = 1, n
        do j = jj, jj + (tile - 1)
           c(i) = c(i) + a(i) * b(j) * 2*rec
        end do
     end do
  end do

  write(*,*) c(100),c(1000)

end program strip_mining
