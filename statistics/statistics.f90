  include 'mkl_vsl.f90'
program statistics
  use mkl_vsl
  use mkl_vsl_type
  use iso_fortran_env, only : real64
  implicit none

  type(VSL_STREAM_STATE) :: stream
  integer,parameter :: n = 65536
  real(real64) :: mean_samples, stddev_samples
  real(real64) :: mu, sigma, max_age, min_age
  real(real64), allocatable :: samples(:)
  integer :: method,seed,brng, n_bins
  integer, allocatable :: bins(:)
  integer :: i,j,err,n_trials,k

  brng = VSL_BRNG_MT19937
  method = VSL_RNG_METHOD_GAUSSIAN_BOXMULLER
  seed = 2398
 
  mu = 35.0
  sigma = 3.0

  allocate(samples(n))

  samples = 0.0

  err = vslnewstream( stream, brng,  seed )

  err = vdrnggaussian( method, stream, n, samples, mu, sigma )

  err = vsldeletestream( stream )

  n_trials = 10000

  ! Compute mean and stddev

  mean_samples = 0.0
  stddev_samples = 0.0

  do i = 1, n
     mean_samples = mean_samples + samples(i)
  end do

  mean_samples = mean_samples / n

  do i = 1, n
     stddev_samples = stddev_samples + (samples(i) - mean_samples) ** 2
  end do

  stddev_samples = sqrt( 1.0/(n-1) * stddev_samples)

  write(*,*) 'mu',mu,'sigma',sigma,'mean',mean_samples,'stddev',stddev_samples
 
end program statistics
