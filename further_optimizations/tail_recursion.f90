program fibo
  use iso_fortran_env, only : real64
  implicit none

  integer, parameter :: fib = 45
  
  write(*,*) fibonacci(fib)
  
contains

  function fibonacci(n) result(res)
    integer, intent(in) :: n
    integer :: res

    res = fibo_naive(n)
    !res = fibo_tail(0,1,n)

  end function fibonacci

  recursive function fibo_naive(n) result(res)
    integer, intent(in) :: n
    integer :: res
    
    if(n <= 0) then
       res = 0
       return
    end if
    
    if(n == 1) then
       res = 1
       return
    end if

    res = fibo_naive(n-1) + fibo_naive(n-2)

  end function fibo_naive

  recursive function fibo_tail(prev2,prev1,n) result(res)
    integer,intent(in) :: prev2,prev1,n
    integer :: res

    if(n == 0) then
       res = prev2
       return
    end if

    res = fibo_tail(prev1,prev1+prev2,n-1)

  end function fibo_tail

end program fibo
