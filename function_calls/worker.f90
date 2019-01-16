module worker_module
use iso_fortran_env, only: real64
contains

  pure function my_scalar_add(x1, x2) result(res)
    implicit none
    real(real64),intent(in) :: x1, x2
    real(real64) :: res

    res = x1 + x2

  end function my_scalar_add

end module worker_module
