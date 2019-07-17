  include 'mkl_vsl.f90'
program binning
  use mkl_vsl
  use mkl_vsl_type
  use iso_fortran_env, only : real64
  implicit none

  type(VSL_STREAM_STATE) :: stream
  integer,parameter :: n = 900000000
  real :: group_width
  real(real64) :: mu, sigma, max_age, min_age
  real(real64), allocatable :: samples(:)
  integer :: method,seed,brng, n_bins
  integer, allocatable :: bins(:)
  integer :: i,j,err

  brng = VSL_BRNG_MT19937
  method = VSL_RNG_METHOD_GAUSSIAN_ICDF
  seed = 2398
 
  mu = 35.0
  sigma = 3.0

  allocate(samples(n))

  samples = 0.0

  err = vslnewstream( stream, brng,  seed )

  err = vdrnggaussian( method, stream, n, samples, mu, sigma )

  err = vsldeletestream( stream )

  ! Binning starts
  max_age = maxval(samples)
  min_age = minval(samples)
  write(*,*) 'Max age',max_age,'min_age',min_age
  group_width = 5.0
  n_bins = floor(max_age/group_width + 0.1)

  allocate(bins(n_bins))

  bins = 0

  do i = 1, n
     
     j = int( samples(i) / group_width)

     bins(j) = bins(j) + 1 

  end do

  write(*,*) bins

 
end program binning
