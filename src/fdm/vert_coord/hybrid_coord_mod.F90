module hybrid_coord_mod

  use flogger
  use namelist_mod
  use const_mod
  use hybrid_coord_test_mod
  use hybrid_coord_ecmwf_mod
  use mesh_mod

  implicit none

  private

  public hybrid_coord_init
  public hybrid_coord_final
  public hybrid_coord_calc_ph
  public hybrid_coord_calc_ph_eta
  public hybrid_coord_calc_dphdt_eta

  real(r8), allocatable, dimension(:) :: hyai
  real(r8), allocatable, dimension(:) :: hybi
  real(r8), allocatable, dimension(:) :: hyam
  real(r8), allocatable, dimension(:) :: hybm

  character(10) :: template = '' ! Template:
                                 ! - ecmwf_l50

  real(r8) :: p0 = 1d5 ! Reference pressure (Pa)

  namelist /hybrid_coord/ &
    hyai, hybi, hyam, hybm, p0, template

contains

  subroutine hybrid_coord_init(num_eta, namelist_path)

    integer, intent(in) :: num_eta
    character(*), intent(in) :: namelist_path

    integer ierr, k

    if (allocated(hyai)) deallocate(hyai)
    if (allocated(hybi)) deallocate(hybi)
    if (allocated(hyam)) deallocate(hyam)
    if (allocated(hybm)) deallocate(hybm)

    allocate(hyai(num_eta+1)); hyai = 0
    allocate(hybi(num_eta+1)); hybi = 0
    allocate(hyam(num_eta  )); hyam = 0
    allocate(hybm(num_eta  )); hybm = 0

    open(10, file=namelist_path, status='old')
    read(10, nml=hybrid_coord, iostat=ierr)
    close(10)

    if (ierr /= 0) then
      call log_error('No hybrid_coord parameters in ' // trim(namelist_path) // '!')
    end if

    select case (template)
    case ('test_l15')
      call hybrid_coord_test_l15(p0, hyai, hybi)
    case ('test_l26')
      call hybrid_coord_test_l26(p0, hyai, hybi)
    case ('test_l30')
      call hybrid_coord_test_l30(p0, hyai, hybi)
    case ('ecmwf_l50')
      call hybrid_coord_ecmwf_l50(p0, hyai, hybi)
    case default
    end select

    if (all(hyam == 0)) then
      do k = 1, num_eta
        hyam(k) = 0.5d0 * (hyai(k) + hyai(k+1))
        hybm(k) = 0.5d0 * (hybi(k) + hybi(k+1))
      end do
    end if

    do k = 1, num_eta
      global_mesh%full_eta(k) = hyam(k) + hybm(k)
    end do
    do k = 1, num_eta + 1
      global_mesh%half_eta(k) = hyai(k) + hybi(k)
    end do

  end subroutine hybrid_coord_init

  subroutine hybrid_coord_final()

    deallocate(hyai)
    deallocate(hybi)
    deallocate(hyam)
    deallocate(hybm)

  end subroutine hybrid_coord_final

  pure real(r8) function hybrid_coord_calc_ph(k, phs) result(res)

    integer, intent(in) :: k
    real(r8), intent(in) :: phs

    res = hyai(k) * p0 + hybm(k) * phs

  end function hybrid_coord_calc_ph

  pure real(r8) function hybrid_coord_calc_ph_eta(k, phs) result(res)

    integer, intent(in) :: k
    real(r8), intent(in) :: phs

    res = hyai(k) * p0 + hybi(k) * phs

  end function hybrid_coord_calc_ph_eta

  pure real(r8) function hybrid_coord_calc_dphdt_eta(k, dphsdt) result(res)

    integer, intent(in) :: k
    real(r8), intent(in) :: dphsdt

    res = hybi(k) * dphsdt

  end function hybrid_coord_calc_dphdt_eta

end module hybrid_coord_mod
