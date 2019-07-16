module worker_module
use iso_fortran_env, only: real64
contains

  function my_scalar_add(x1, x2) result(res)
    implicit none
    real(real64),intent(in) :: x1, x2
    real(real64) :: res

    res = x1 + x2

  end function my_scalar_add

  pure function my_scalar_add_pure(x1, x2) result(res)
    implicit none
    real(real64),intent(in) :: x1, x2
    real(real64) :: res

    res = x1 + x2

  end function my_scalar_add_pure

  elemental subroutine my_scalar_add_elemental_value(res, x1, x2)
    implicit none
    real(real64),value,intent(in) :: x1, x2
    real(real64),intent(out) :: res

    res = x1 + x2

  end subroutine my_scalar_add_elemental_value

  elemental function my_scalar_add_elemental(x1, x2) result(res)
    implicit none
    real(real64),intent(in) :: x1, x2
    real(real64) :: res

    res = x1 + x2

  end function my_scalar_add_elemental

end module worker_module
