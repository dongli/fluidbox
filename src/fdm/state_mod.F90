module state_mod

  use const_mod
  use mesh_mod

  implicit none

  private

  type state_cgrid_lorenz_type
    real(r8), allocatable, dimension(:,:,:) :: u
    real(r8), allocatable, dimension(:,:,:) :: v
    real(r8), allocatable, dimension(:,:,:) :: w
    real(r8), allocatable, dimension(:,:,:) :: pt
    real(r8), allocatable, dimension(:,:,:) :: phs
    real(r8), allocatable, dimension(:,:,:) :: gz
    real(r8), allocatable, dimension(:,:,:) :: gz_eta
    real(r8), allocatable, dimension(:,:,:) :: ph
    real(r8), allocatable, dimension(:,:,:) :: ph_eta
  contains
    procedure :: init => state_cgrid_lorenz_init
  end type state_cgrid_lorenz_type

contains

  subroutine state_cgrid_lorenz_init(this, mesh)

    class(state_cgrid_lorenz_type), intent(out) :: this
    type(mesh_type), intent(in) :: mesh

  end subroutine state_cgrid_lorenz_init

end module state_mod
