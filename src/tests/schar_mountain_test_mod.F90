module schar_mountain_test_mod

  ! This test case is from the test 2.1 in Ullrich et al. (2012) for DCMIP-2012.

  use const_mod

  implicit none

  real(r8), parameter :: ztop = 3.0e4_r8    ! Height position of the model top (m)
  real(r8), parameter :: ptop = 32.818e2_r8 ! Pressure at the model top at the equator (Pa)
  real(r8), parameter :: h0   = 250.0_r8    ! Maximum Schar-type mountain height (m)
  real(r8), parameter :: d    = 5.0e3_r8    ! Schar-type mountain half-width (m)
  real(r8), parameter :: xi   = 4.0e3_r8    ! Schar-type mountain wavelength (m)
  real(r8), parameter :: peq  = 1.0e5_r8    ! Reference surface pressure at the equator (Pa)
  real(r8), parameter :: teq  = 300.0_r8    ! Reference surface temperature at the equator (K)
  real(r8), parameter :: ueq  = 20.0_r8     ! Reference zonal wind velocity (m s-1)

  ! Rayleigh damping parameters
  real(r8), parameter :: zh   = 2.0e4_r8    ! Height of the Rayleigh damped layer (m)
  real(r8), parameter :: tau0 = 25.0_r8     ! Rayleigh friction time scale (s)

contains

end module schar_mountain_test_mod
