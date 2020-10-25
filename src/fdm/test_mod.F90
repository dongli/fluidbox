module test_mod

  use const_mod
  use namelist_mod
  use mesh_mod
  use state_mod
  use schar_mountain_test_mod

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
    real(r8) function test_zs_interface(x, y)
      import r8
      real(r8), intent(in) :: x
      real(r8), intent(in) :: y
    end function test_zs_interface

    real(r8) function test_z_interface(x, y, p)
      import r8
      real(r8), intent(in) :: x
      real(r8), intent(in) :: y
      real(r8), intent(in) :: p
    end function test_z_interface

    real(r8) function test_p_interface(x, y, z)
      import r8
      real(r8), intent(in) :: x
      real(r8), intent(in) :: y
      real(r8), intent(in) :: z
    end function test_p_interface

    real(r8) function test_t_interface(x, y, z, p)
      import r8
      real(r8), intent(in) :: x
      real(r8), intent(in) :: y
      real(r8), intent(in), optional :: z
      real(r8), intent(in), optional :: p
    end function test_t_interface

    real(r8) function test_u_interface(x, y, z, p)
      import r8
      real(r8), intent(in) :: x
      real(r8), intent(in) :: y
      real(r8), intent(in), optional :: z
      real(r8), intent(in), optional :: p
    end function test_u_interface

    real(r8) function test_v_interface(x, y, z, p)
      import r8
      real(r8), intent(in) :: x
      real(r8), intent(in) :: y
      real(r8), intent(in), optional :: z
      real(r8), intent(in), optional :: p
    end function test_v_interface
  end interface

  procedure(test_zs_interface), pointer :: ic_zs
  procedure(test_z_interface ), pointer :: ic_z
  procedure(test_p_interface ), pointer :: ic_p
  procedure(test_t_interface ), pointer :: ic_t
  procedure(test_u_interface ), pointer :: ic_u
  procedure(test_v_interface ), pointer :: ic_v

contains

  subroutine test_init()

    select case (test_case)
    case ('schar_mountain_test')
      ic_zs => schar_mountain_test_zs
      ic_z  => schar_mountain_test_z
      ic_p  => schar_mountain_test_p
      ic_t  => schar_mountain_test_t
      ic_u  => schar_mountain_test_u
      ic_v  => schar_mountain_test_v
    case default
      print *, 'Unknown test case ' // trim(test_case) // '!'
      stop 1
    end select

  end subroutine

  subroutine test_apply_initial_condition_cgrid_lorenz(mesh, state)

    type(mesh_type), intent(in) :: mesh
    type(state_cgrid_lorenz_type), intent(inout) :: state

  end subroutine test_apply_initial_condition_cgrid_lorenz

  subroutine test_apply_forcing_cgrid_lorenz(mesh, state)

    type(mesh_type), intent(in) :: mesh
    type(state_cgrid_lorenz_type), intent(inout) :: state

  end subroutine test_apply_forcing_cgrid_lorenz

end module test_mod
