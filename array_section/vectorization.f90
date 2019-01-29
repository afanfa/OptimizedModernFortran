program vector_1
  use worker_module
  use iso_fortran_env, only : real64
  implicit none

  integer, parameter :: n = 1000, m = 100
  integer :: i,j,k
  real(real64),allocatable  :: a(:,:,:),b(:,:,:) ! deferred-shape arrays

  allocate(a(n,n,n),b(n,n,n))

  do i = 1, n
     do j = 1, n
        do k = 1, n
           a(i,j,k) = 1.0/n + j - k
           b(i,j,k) = 2.0 - j + k
        end do
     end do
  end do

  !a_e = a
  !b_e = b

  do i = 1, n
     !call my_matrix_add(a(:,:,i),b(i,:,:))
     !call my_matrix_add_intent(a(:,:,i),b(i,:,:))
     !call my_matrix_add_explicit(a(:,:,i),b(i,:,:),n)
     !call my_matrix_add_explicit_intent(a(:,:,i),b(i,:,:),n)
     !call my_matrix_add(a(:,:,i),b(:,:,i))
     call my_matrix_add_allocatable(a(:,:,i),b(i,:,:))
  end do

end program vector_1
