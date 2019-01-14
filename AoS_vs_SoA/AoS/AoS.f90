program AoS
  USE IFPORT
  use iso_fortran_env, only : real32, real64
  implicit none

  type charge
     real(real32) :: x,y,z,q ! Coordinates and value of the charge
  end type charge

  type(charge), allocatable :: charges(:)
  real(real32), allocatable :: potential(:)
  integer, parameter :: m = 2048, n_trials = 10
  integer :: i,j
  real(real32) :: Rx, Ry, Rz
  real(real64) :: end_time, start_time

  allocate(charges(m), potential(m*m))

  call srand(12345)

  do i = 1, m
     charges(i)%x = rand()
     charges(i)%y = rand()
     charges(i)%z = rand()
     charges(i)%q = rand()
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
    type(charge),allocatable,intent(in) :: charges(:)
    real(real32) :: dx,dy,dz
    real(real32) :: phi
    integer :: i

    phi = 0.0

    do i = 1, size(charges)
       dx = charges(i)%x - Rx
       dy = charges(i)%y - Ry
       dz = charges(i)%z - Rz
       phi = phi - charges(i)%q / sqrt(dx**2 + dy**2 + dz**2)
    end do

  end function calculate_electric_potential

end program AoS
