module worker_module
use iso_fortran_env, only: real64

contains

  subroutine where_all(a,b,c)
    implicit none
    real(real64), allocatable :: a(:), b(:),c(:)
    
    where(a >= b)
       c = a + 10.0 / b
    end where
    
  end subroutine where_all

  subroutine do_counter_partial_sum(c,counter)
    implicit none
    real(real64), allocatable :: c(:)
    integer, intent(out) :: counter
    integer :: i,n
    
    counter = 0
    n = size(c,1)

!Partial sums inhibit vectorization
    do i = 1, n
       if(c(i) > 0.0) counter = counter + 1
    end do
    
  end subroutine do_counter_partial_sum

  subroutine do_counter_count(c,counter)
    implicit none
    real(real64), allocatable :: c(:)
    integer, intent(out) :: counter

    counter = count(c > 0.0)
    
  end subroutine do_counter_count

end module worker_module
