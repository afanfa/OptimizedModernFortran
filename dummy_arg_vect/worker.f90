module worker_module
use iso_fortran_env, only: real64
contains

  subroutine my_vector_add(n, a, b)
    implicit none

    integer :: i,n
    real(real64),dimension(n) :: a, b

    do i = 1, n
       a(i) = a(i) + b(i)
    end do

  end subroutine my_vector_add

  subroutine my_vector_add_assumed(a, b)
    implicit none

    integer :: i
    real(real64) :: a(:), b(:)

    do i = 1,size(a)
       a(i) = a(i) + b(i)
    end do

  end subroutine my_vector_add_assumed

  subroutine my_vector_add_assumed_contiguous(a, b)
    implicit none

    integer :: i
    real(real64), contiguous :: a(:), b(:)

    do i = 1,size(a)
       a(i) = a(i) + b(i)
    end do
    
  end subroutine my_vector_add_assumed_contiguous

  subroutine my_vector_add_allocatable(a, b)
    implicit none

    integer :: i
    real(real64),allocatable :: a(:), b(:)

    do i = 1,size(a)
       a(i) = a(i) + b(i)
    end do

  end subroutine my_vector_add_allocatable

  subroutine my_vector_add_pointer(a, b)
    implicit none

    integer :: i
    real(real64), pointer :: a(:), b(:)

    do i = 1,size(a)
       a(i) = a(i) + b(i)
    end do

  end subroutine my_vector_add_pointer

  subroutine my_vector_add_pointer_contiguous(a, b)
    implicit none

    integer :: i
    real(real64), pointer,contiguous :: a(:), b(:)

    do i = 1,size(a)
       a(i) = a(i) + b(i)
    end do

  end subroutine my_vector_add_pointer_contiguous

end module worker_module
