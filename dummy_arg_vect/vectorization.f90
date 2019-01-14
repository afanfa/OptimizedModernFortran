program vector_1
  use worker_module
  use iso_fortran_env, only : real64
  implicit none

  integer, parameter :: n = 10000
  integer :: i
  real(real64),dimension(n) :: a, b, c
  real(real64),allocatable,target :: a_a(:),a_b(:)
  real(real64),pointer :: p_a(:),p_b(:)
  real(real64),pointer,contiguous :: pc_a(:),pc_b(:)

  allocate(a_a(n),a_b(n))

  do i = 1, n
     a(i) = 1.0/n
     b(i) = 2.0
  end do

  a_a = a
  a_b = b

  call my_vector_add(n,a,b)
  call my_vector_add_assumed(a,b)
  call my_vector_add_assumed_contiguous(a,b)
  call my_vector_add_allocatable(a_a,a_b)

  p_a(1:n) => a_a(1:n)
  p_b(1:n) => a_b(1:n)

  pc_a(1:n) => a_a(1:n)
  pc_b(1:n) => a_b(1:n)

  call my_vector_add_pointer(p_a,p_b)
  call my_vector_add_pointer_contiguous(pc_a,pc_b)
  call my_vector_add_assumed_contiguous(p_a,p_b)

end program vector_1
