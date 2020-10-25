module test_mod

  use const_mod
  use namelist_mod
  use formula_mod
  use mesh_mod
  use static_mod
  use state_mod
  use schar_mountain_test_mod
  use vert_coord_mod

  implicit none

  private

  public test_init
  public test_apply_initial_condition
  public test_apply_forcing

  interface test_apply_initial_condition
    module procedure test_apply_initial_condition_cgrid_lorenz
  end interface test_apply_initial_condition

  interface test_apply_forcing
    module procedure test_apply_forcing_cgrid_lorenz
  end interface test_apply_forcing

  interface
    real(r8) function test_ic_zs_interface(x, y)
      import r8
      real(r8), intent(in) :: x
      real(r8), intent(in) :: y
    end function test_ic_zs_interface

    real(r8) function test_ic_z_interface(x, y, p)
      import r8
      real(r8), intent(in) :: x
      real(r8), intent(in) :: y
      real(r8), intent(in) :: p
    end function test_ic_z_interface

    real(r8) function test_ic_p_interface(x, y, z)
      import r8
      real(r8), intent(in) :: x
      real(r8), intent(in) :: y
      real(r8), intent(in) :: z
    end function test_ic_p_interface

    real(r8) function test_ic_t_interface(x, y, z, p)
      import r8
      real(r8), intent(in) :: x
      real(r8), intent(in) :: y
      real(r8), intent(in), optional :: z
      real(r8), intent(in), optional :: p
    end function test_ic_t_interface

    real(r8) function test_ic_u_interface(x, y, z, p)
      import r8
      real(r8), intent(in) :: x
      real(r8), intent(in) :: y
      real(r8), intent(in), optional :: z
      real(r8), intent(in), optional :: p
    end function test_ic_u_interface

    real(r8) function test_ic_v_interface(x, y, z, p)
      import r8
      real(r8), intent(in) :: x
      real(r8), intent(in) :: y
      real(r8), intent(in), optional :: z
      real(r8), intent(in), optional :: p
    end function test_ic_v_interface

    real(r8) function test_fc_u_interface(u, x, y, z, p)
      import r8
      real(r8), intent(in) :: u
      real(r8), intent(in) :: x
      real(r8), intent(in) :: y
      real(r8), intent(in), optional :: z
      real(r8), intent(in), optional :: p
    end function test_fc_u_interface

    real(r8) function test_fc_v_interface(v, x, y, z, p)
      import r8
      real(r8), intent(in) :: v
      real(r8), intent(in) :: x
      real(r8), intent(in) :: y
      real(r8), intent(in), optional :: z
      real(r8), intent(in), optional :: p
    end function test_fc_v_interface
  end interface

  procedure(test_ic_zs_interface), pointer :: ic_zs
  procedure(test_ic_z_interface ), pointer :: ic_z
  procedure(test_ic_p_interface ), pointer :: ic_p
  procedure(test_ic_t_interface ), pointer :: ic_t
  procedure(test_ic_u_interface ), pointer :: ic_u
  procedure(test_ic_v_interface ), pointer :: ic_v
  procedure(test_fc_u_interface ), pointer :: fc_u
  procedure(test_fc_v_interface ), pointer :: fc_v

contains

  subroutine test_init()

    select case (test_case)
    case ('schar_mountain_test')
      ic_zs => schar_mountain_test_ic_zs
      ic_z  => schar_mountain_test_ic_z
      ic_p  => schar_mountain_test_ic_p
      ic_t  => schar_mountain_test_ic_t
      ic_u  => schar_mountain_test_ic_u
      ic_v  => schar_mountain_test_ic_v
      fc_u  => schar_mountain_test_fc_u
      fc_v  => schar_mountain_test_fc_v
    case default
      print *, 'Unknown test case ' // trim(test_case) // '!'
      stop 1
    end select

  end subroutine

  subroutine test_apply_initial_condition_cgrid_lorenz(mesh, static, state)

    type(mesh_type), intent(in) :: mesh
    type(static_type), intent(inout) :: static
    type(state_cgrid_lorenz_type), intent(inout) :: state

    integer i, j, k

    do j = mesh%full_y_ibeg, mesh%full_y_iend
      do i = mesh%full_x_ibeg, mesh%full_x_iend
        static%gzs(i,j) = ic_zs(mesh%full_x(i), mesh%full_y(j)) * g
        state %phs(i,j) = ic_p (mesh%full_x(i), mesh%full_y(j), 0.0_r8)
      end do
    end do

    do k = mesh%half_eta_ibeg, mesh%half_eta_iend
      do j = mesh%full_y_ibeg, mesh%full_y_iend
        do i = mesh%full_x_ibeg, mesh%full_x_iend
          state%ph_eta(i,j,k) = vert_coord_calc_ph_eta(k, state%phs(i,j))
          state%gz_eta(i,j,k) = ic_z(mesh%full_x(i), mesh%full_y(j), state%ph_eta(i,j,k))
        end do
      end do
    end do

    do k = mesh%full_eta_ibeg, mesh%full_eta_iend
      do j = mesh%full_y_ibeg, mesh%full_y_iend
        do i = mesh%full_x_ibeg, mesh%full_x_iend
          state%ph(i,j,k) = 0.5_r8 * (state%ph_eta(i,j,k) + state%ph_eta(i,j,k+1))
          state%t (i,j,k) = ic_t(mesh%full_x(i), mesh%full_y(j), p=state%ph(i,j,k))
          state%pt(i,j,k) = potential_temperature(state%t(i,j,k), state%ph(i,j,k))
        end do
      end do
    end do

    do k = mesh%full_eta_ibeg, mesh%full_eta_iend
      do j = mesh%full_y_ibeg, mesh%full_y_iend
        do i = mesh%half_x_ibeg, mesh%half_x_iend
          state%u(i,j,k) = ic_u(mesh%half_x(i), mesh%full_y(j), p=state%ph(i,j,k))
        end do
      end do
    end do

    do k = mesh%full_eta_ibeg, mesh%full_eta_iend
      do j = mesh%half_y_ibeg, mesh%half_y_iend
        do i = mesh%full_x_ibeg, mesh%full_x_iend
          state%v(i,j,k) = ic_v(mesh%full_x(i), mesh%half_y(j), p=state%ph(i,j,k))
        end do
      end do
    end do

  end subroutine test_apply_initial_condition_cgrid_lorenz

  subroutine test_apply_forcing_cgrid_lorenz(mesh, state)

    type(mesh_type), intent(in) :: mesh
    type(state_cgrid_lorenz_type), intent(inout) :: state

    integer i, j, k

    do k = mesh%full_eta_ibeg, mesh%full_eta_iend
      do j = mesh%full_y_ibeg, mesh%full_y_iend
        do i = mesh%half_x_ibeg, mesh%half_x_iend
          state%u(i,j,k) = fc_u(state%u(i,j,k), mesh%half_x(i), mesh%full_y(j), p=state%ph(i,j,k)) ! FIXME: We need ph on u grid.
        end do
      end do
    end do

    do k = mesh%full_eta_ibeg, mesh%full_eta_iend
      do j = mesh%half_y_ibeg, mesh%half_y_iend
        do i = mesh%full_x_ibeg, mesh%full_x_iend
          state%v(i,j,k) = fc_v(state%v(i,j,k), mesh%full_x(i), mesh%half_y(j), p=state%ph(i,j,k)) ! FIXME: We need ph on v grid.
        end do
      end do
    end do

  end subroutine test_apply_forcing_cgrid_lorenz

end module test_mod
