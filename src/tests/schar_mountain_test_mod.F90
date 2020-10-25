module schar_mountain_test_mod

  ! This test case is from the test 2.1 in Ullrich et al. (2012) for DCMIP-2012.

  use const_mod

  implicit none

  private

  public schar_mountain_test_zs
  public schar_mountain_test_z
  public schar_mountain_test_p
  public schar_mountain_test_t
  public schar_mountain_test_u
  public schar_mountain_test_v
  public schar_mountain_test_rayleigh_damp

  real(r8), parameter :: ztop = 3.0e4_r8    ! Height position of the model top (m)
  real(r8), parameter :: ptop = 32.818e2_r8 ! Pressure at the model top at the equator (Pa)
  real(r8), parameter :: xc   = 0.0_r8      ! X of Schar-type mountain centerpoint (m)
  real(r8), parameter :: yc   = 0.0_r8      ! Y of Schar-type mountain centerpoint (m)
  real(r8), parameter :: h0   = 250.0_r8    ! Maximum Schar-type mountain height (m)
  real(r8), parameter :: d    = 5.0e3_r8    ! Schar-type mountain half-width (m)
  real(r8), parameter :: xi   = 4.0e3_r8    ! Schar-type mountain wavelength (m)
  real(r8), parameter :: peq  = 1.0e5_r8    ! Reference surface pressure at the equator (Pa)
  real(r8), parameter :: teq  = 300.0_r8    ! Reference surface temperature at the equator (K)
  real(r8), parameter :: ueq  = 20.0_r8     ! Reference zonal wind velocity (m s-1)
  real(r8), parameter :: cs   = 0.0_r8      ! Equatorial surface wind shear (for sheared flow) (m-1)

  ! Rayleigh damping parameters
  real(r8), parameter :: zh   = 2.0e4_r8    ! Height of the Rayleigh damped layer (m)
  real(r8), parameter :: tau0 = 25.0_r8     ! Rayleigh friction time scale (s)

contains

  real(r8) function schar_mountain_test_zs(x, y) result(res)

    real(r8), intent(in) :: x
    real(r8), intent(in) :: y

    res = h0 * exp(-(x - xc)**2 / d**2) * cos(pi * (x - xc) / xi)**2

  end function schar_mountain_test_zs

  real(r8) function schar_mountain_test_z(x, y, p) result(res)

    real(r8), intent(in) :: x
    real(r8), intent(in) :: y
    real(r8), intent(in) :: p

    res = Rd * teq / g * log(peq / p)

  end function schar_mountain_test_z

  real(r8) function schar_mountain_test_p(x, y, z) result(res)

    real(r8), intent(in) :: x
    real(r8), intent(in) :: y
    real(r8), intent(in) :: z

    res = peq * exp(-g * z / Rd / teq)

  end function schar_mountain_test_p

  real(r8) function schar_mountain_test_t(x, y, z, p) result(res)

    real(r8), intent(in) :: x
    real(r8), intent(in) :: y
    real(r8), intent(in), optional :: z
    real(r8), intent(in), optional :: p

    res = teq

  end function schar_mountain_test_t

  real(r8) function schar_mountain_test_u(x, y, z, p) result(res)

    real(r8), intent(in) :: x
    real(r8), intent(in) :: y
    real(r8), intent(in), optional :: z
    real(r8), intent(in), optional :: p

    real(r8) zz

    if (present(z)) then
      zz = z
    else if (present(p)) then
      zz = schar_mountain_test_z(x, y, p)
    end if
    res = ueq * sqrt(2 * teq / teq * cs * zz + teq / teq)

  end function schar_mountain_test_u

  real(r8) function schar_mountain_test_v(x, y, z, p) result(res)

    real(r8), intent(in) :: x
    real(r8), intent(in) :: y
    real(r8), intent(in), optional :: z
    real(r8), intent(in), optional :: p

    res = 0.0_r8

  end function schar_mountain_test_v

  subroutine schar_mountain_test_rayleigh_damp(x, y, z, p, u, v)

    real(r8), intent(in) :: x
    real(r8), intent(in) :: y
    real(r8), intent(in), optional :: z
    real(r8), intent(in), optional :: p
    real(r8), intent(inout) :: u
    real(r8), intent(inout) :: v

    real(r8) zz, f

    if (present(z)) then
      zz = z
    else if (present(p)) then
      zz = schar_mountain_test_z(x, y, p)
    end if
    if (zz > zh) then
      f = sin(pi05 * (zz - zh) / (ztop - zh))**2
    else
      f = 0.0_r8
    end if

    u = u - f / tau0 * (u - schar_mountain_test_u(x, y, z, p))
    v = v - f / tau0 * (v - schar_mountain_test_v(x, y, z, p))

  end subroutine schar_mountain_test_rayleigh_damp

end module schar_mountain_test_mod
