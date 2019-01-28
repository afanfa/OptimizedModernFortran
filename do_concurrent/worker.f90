module worker_module
use iso_fortran_env, only: real32
contains

! Vectorization illegal if k < 0
  subroutine vec_dep(a, c, k, n)
    implicit none

    integer :: i, k, n
    real(real32),dimension(n) :: a
    real(real32),intent(in) :: c

    do i = 1, n-1
       a(i) = a(i+k) * c
    end do

  end subroutine vec_dep

  subroutine vec_dep_dir(a, c, k, n)
    implicit none

    integer :: i, k, n
    real(real32),dimension(n) :: a
    real(real32),intent(in) :: c

!dir$ ivdep
    do i = 1, n-1
       a(i) = a(i+k) * c
    end do

  end subroutine vec_dep_dir

  subroutine vec_dep_do_concurrent(a, c, k, n)
    implicit none

    integer :: i, k, n
    real(real32),dimension(n) :: a
    real(real32),intent(in) :: c

    do concurrent (i = 1:n-1)
       a(i) = a(i+k) * c
    end do
    
  end subroutine vec_dep_do_concurrent

end module worker_module
