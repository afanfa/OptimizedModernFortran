module worker_module
use iso_fortran_env, only: real64
contains

  subroutine my_matrix_add(a, b)
    implicit none

    integer :: i, j
    real(real64) :: a(:,:), b(:,:) ! assumed-shape

    do i = 1,size(a,1)
       do j =1,size(a,2)
          a(i,j) = a(i,j) + b(i,j)
       end do
    end do

  end subroutine my_matrix_add

  subroutine my_matrix_add_intent(a, b)
    implicit none

    integer :: i, j
    real(real64), intent(inout) :: a(:,:) !assumed-shape
    real(real64), intent(in) :: b(:,:)

    do i = 1,size(a,1)
       do j =1,size(a,2)
          a(i,j) = a(i,j) + b(i,j)
       end do
    end do

  end subroutine my_matrix_add_intent

  subroutine my_matrix_add_explicit(a, b, n)
    implicit none

    integer :: i, j, n
    real(real64),dimension(n,n) :: a, b !explicit-shape

    do i = 1,n
       do j =1,n
          a(i,j) = a(i,j) + b(i,j)
       end do
    end do

  end subroutine my_matrix_add_explicit

  subroutine my_matrix_add_explicit_intent(a, b, n)
    implicit none

    integer :: i, j, n
    real(real64),dimension(n,n) :: a
    real(real64),dimension(n,n),intent(in) :: b

    do i = 1,n
       do j =1,n
          a(i,j) = a(i,j) + b(i,j)
       end do
    end do

  end subroutine my_matrix_add_explicit_intent

  subroutine my_matrix_add_allocatable(a, b)
    implicit none

    integer :: i, j
    real(real64),allocatable :: a(:,:), b(:,:)

    do i = 1,size(a,1)
       do j =1,size(a,2)
          a(i,j) = a(i,j) + b(i,j)
       end do
    end do

  end subroutine my_matrix_add_allocatable

end module worker_module
