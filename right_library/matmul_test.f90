program matmul_test
  use iso_fortran_env, only : real64
  implicit none

  integer, parameter :: n = 3000
  real(real64),allocatable :: a(:,:),b(:,:),c(:,:)
  integer :: i,j

  allocate(a(n,n),b(n,n),c(n,n))

  do i = 1, n
     do j = 1, n
        a(i,j) = (i+j) * 0.5
        b(i,j) = (i+j) * 0.25
     end do
  end do

  !c = matmul(a,b)

  call dgemm('n','n',n,n,n,1.d0,a,n,b,n,0.d0,c,n)

  write(*,*) c(100,100)

end program matmul_test
