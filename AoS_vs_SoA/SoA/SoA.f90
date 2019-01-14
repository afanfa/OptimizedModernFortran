program AoS
  USE IFPORT
  use iso_fortran_env, only : real32, real64
  implicit none

  type charge
     real(real32),allocatable :: x(:),y(:),z(:),q(:) ! Coordinates and value of the charge
  end type charge

  type(charge) :: charges
  real(real32), allocatable :: potential(:)
  integer, parameter :: m = 2048, n_trials = 10
  integer :: i,j
  real(real32) :: Rx, Ry, Rz
  real(real64) :: end_time, start_time

  allocate(potential(m*m), charges%x(m), charges%y(m), charges%z(m), charges%q(m))

  call srand(12345)

  do i = 1, m
     charges%x(i) = rand()
     charges%y(i) = rand()
     charges%z(i) = rand()
     charges%q(i) = rand()
  end do

  do i = 1, n_trials
     potential = 0.0
     call cpu_time(start_time)
     do j = 1, m*m
        Rx = real(mod(j,m))
        Ry = real(j/m)
        Rz = 0.0
        potential(j) = calculate_electric_potential(charges, Rx, Ry, Rz)
     end do
     call cpu_time(end_time)
     write(*,*) 'Trial ',i,' test ',potential(100), 'time ', end_time - start_time
  end do

contains

  function calculate_electric_potential(charges, Rx, Ry, Rz) result(phi)
    implicit none

    real(real32), intent(in) :: Rx, Ry, Rz
    type(charge),intent(in) :: charges
    real(real32) :: dx,dy,dz
    real(real32) :: phi
    integer :: i

    phi = 0.0

    do i = 1, size(charges%x)
       dx = charges%x(i) - Rx
       dy = charges%y(i) - Ry
       dz = charges%z(i) - Rz
       phi = phi - charges%q(i) / sqrt(dx**2 + dy**2 + dz**2)
    end do

  end function calculate_electric_potential

end program AoS
