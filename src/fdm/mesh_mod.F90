module mesh_mod

  use const_mod

  implicit none

  private

  public mesh_type

  type mesh_type
    integer :: nx = 0
    integer :: ny = 0
    integer :: nz = 0
    integer full_x_ibeg
    integer full_x_iend
    integer half_x_ibeg
    integer half_x_iend
    integer full_y_ibeg
    integer full_y_iend
    integer half_y_ibeg
    integer half_y_iend
    integer full_z_ibeg
    integer full_z_iend
    integer half_z_ibeg
    integer half_z_iend
    real(r8), allocatable, dimension(:) :: full_x
    real(r8), allocatable, dimension(:) :: half_x
    real(r8), allocatable, dimension(:) :: full_y
    real(r8), allocatable, dimension(:) :: half_y
    real(r8), allocatable, dimension(:) :: full_eta
    real(r8), allocatable, dimension(:) :: half_eta
  contains
    procedure :: init => mesh_init
    procedure :: clear => mesh_clear
    final :: mesh_final
  end type mesh_type

contains

  subroutine mesh_init(this, nx, ny, nz)

    class(mesh_type), intent(inout) :: this
    integer, intent(in) :: nx
    integer, intent(in) :: ny
    integer, intent(in) :: nz

    call this%clear()

  end subroutine mesh_init

  subroutine mesh_clear(this)

    class(mesh_type), intent(inout) :: this

    if (allocated(this%full_x  )) deallocate(this%full_x  )
    if (allocated(this%half_x  )) deallocate(this%half_x  )
    if (allocated(this%full_y  )) deallocate(this%full_y  )
    if (allocated(this%half_y  )) deallocate(this%half_y  )
    if (allocated(this%full_eta)) deallocate(this%full_eta)
    if (allocated(this%half_eta)) deallocate(this%half_eta)

  end subroutine mesh_clear

  subroutine mesh_final(this)

    type(mesh_type), intent(inout) :: this

    call this%clear()

  end subroutine mesh_final

end module mesh_mod
