module state_mod

  use const_mod
  use mesh_mod

  implicit none

  private

  public state_cgrid_lorenz_type

  type state_cgrid_lorenz_type
    real(r8), allocatable, dimension(:,:,:) :: u
    real(r8), allocatable, dimension(:,:,:) :: v
    real(r8), allocatable, dimension(:,:,:) :: w
    real(r8), allocatable, dimension(:,:,:) :: pt
    real(r8), allocatable, dimension(:,:  ) :: phs
    real(r8), allocatable, dimension(:,:,:) :: gz
    real(r8), allocatable, dimension(:,:,:) :: gz_eta
    real(r8), allocatable, dimension(:,:,:) :: ph
    real(r8), allocatable, dimension(:,:,:) :: ph_eta
  contains
    procedure :: init => state_cgrid_lorenz_init
    procedure :: clear => state_cgrid_lorenz_clear
    final :: state_cgrid_lorenz_final
  end type state_cgrid_lorenz_type

contains

  subroutine state_cgrid_lorenz_init(this, mesh)

    class(state_cgrid_lorenz_type), intent(inout) :: this
    type(mesh_type), intent(in) :: mesh

    call this%clear()

    allocate(this%u     (mesh%half_x_ibeg:mesh%half_x_iend,mesh%full_y_ibeg:mesh%full_y_iend,mesh%full_z_ibeg:mesh%full_z_iend))
    allocate(this%v     (mesh%full_x_ibeg:mesh%full_x_iend,mesh%half_y_ibeg:mesh%half_y_iend,mesh%full_z_ibeg:mesh%full_z_iend))
    allocate(this%w     (mesh%full_x_ibeg:mesh%full_x_iend,mesh%full_y_ibeg:mesh%full_y_iend,mesh%half_z_ibeg:mesh%half_z_iend))
    allocate(this%pt    (mesh%full_x_ibeg:mesh%full_x_iend,mesh%full_y_ibeg:mesh%full_y_iend,mesh%full_z_ibeg:mesh%full_z_iend))
    allocate(this%phs   (mesh%full_x_ibeg:mesh%full_x_iend,mesh%full_y_ibeg:mesh%full_y_iend                                  ))
    allocate(this%gz    (mesh%full_x_ibeg:mesh%full_x_iend,mesh%full_y_ibeg:mesh%full_y_iend,mesh%full_z_ibeg:mesh%full_z_iend))
    allocate(this%gz_eta(mesh%full_x_ibeg:mesh%full_x_iend,mesh%full_y_ibeg:mesh%full_y_iend,mesh%half_z_ibeg:mesh%half_z_iend))
    allocate(this%ph    (mesh%full_x_ibeg:mesh%full_x_iend,mesh%full_y_ibeg:mesh%full_y_iend,mesh%full_z_ibeg:mesh%full_z_iend))
    allocate(this%ph_eta(mesh%full_x_ibeg:mesh%full_x_iend,mesh%full_y_ibeg:mesh%full_y_iend,mesh%half_z_ibeg:mesh%half_z_iend))

  end subroutine state_cgrid_lorenz_init

  subroutine state_cgrid_lorenz_clear(this)

    class(state_cgrid_lorenz_type), intent(inout) :: this

    if (allocated(this%u     )) deallocate(this%u     )
    if (allocated(this%v     )) deallocate(this%v     )
    if (allocated(this%w     )) deallocate(this%w     )
    if (allocated(this%pt    )) deallocate(this%pt    )
    if (allocated(this%phs   )) deallocate(this%phs   )
    if (allocated(this%gz    )) deallocate(this%gz    )
    if (allocated(this%gz_eta)) deallocate(this%gz_eta)
    if (allocated(this%ph    )) deallocate(this%ph    )
    if (allocated(this%ph_eta)) deallocate(this%ph_eta)

  end subroutine state_cgrid_lorenz_clear

  subroutine state_cgrid_lorenz_final(this)

    type(state_cgrid_lorenz_type), intent(inout) :: this

    call this%clear()

  end subroutine state_cgrid_lorenz_final

end module state_mod
