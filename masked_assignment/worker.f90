module worker_module
use iso_fortran_env, only: real64

contains

  subroutine where_all(a,b,c)
    implicit none
    real(real64), allocatable :: a(:), b(:),c(:)
    
    where(a >= b)
       c = (a - 10.0) * b
    end where
    
  end subroutine where_all

  subroutine if_then_all(a,b,c)
    implicit none
    real(real64), allocatable :: a(:), b(:),c(:)
    integer :: n,i

    n = size(a,1)

    do i = 1, n
       if(a(i) > b(i)) then
          c(i) = (a(i) - 10.0) * b(i)
       end if
    end do
    
  end subroutine if_then_all

  subroutine where_sqrt(a,b,c)
    implicit none
    real(real64), allocatable :: a(:), b(:), c(:)
    
    where(a >= b)
       c = sqrt(a)
    elsewhere
       c = 0.0
    end where
    
  end subroutine where_sqrt

end module worker_module
