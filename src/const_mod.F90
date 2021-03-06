module const_mod

  use, intrinsic :: ieee_arithmetic

#ifdef REAL_KIND
  integer, parameter :: r8 = REAL_KIND
#else
  integer, parameter :: r8 = 8
#endif

  real(r8), parameter :: pi       = atan(1.0_r8) * 4.0_r8
  real(r8), parameter :: pi2      = pi * 2
  real(r8), parameter :: pi05     = pi * 0.5_r8
  real(r8), parameter :: deg      = 180.0_r8 / pi
  real(r8), parameter :: rad      = pi / 180.0_r8
  real(r8), parameter :: omega    = 2.0_r8 * pi / 86400.0_r8  ! s-1
  real(r8), parameter :: radius   = 6.37122d6                 ! m
  real(r8), parameter :: g        = 9.80616_r8                ! m2 s-2
  real(r8), parameter :: eps      = epsilon(1.0_r8)
  real(r8), parameter :: inf      = huge(1.0_r8)

  real(r8), parameter :: Rd       = 287.04_r8                 ! J kg-1 K-1
  real(r8), parameter :: Rv       = 461.497_r8                ! J kg-1 K-1
  real(r8), parameter :: cp       = 1004.0_r8                 ! J kg-1 K-1
  real(r8), parameter :: cv       = 717.0_r8                  ! J kg-1 K-1
  real(r8), parameter :: Rd_o_cp  = Rd / cp
  real(r8), parameter :: cp_o_cv  = cp / cv
  real(r8), parameter :: cv_o_cp  = cv / cp
  
end module const_mod
