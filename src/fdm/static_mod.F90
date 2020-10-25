module static_mod

  use const_mod
  use mesh_mod

  implicit none

  type static_type
    real(r8), allocatable, dimension(:,:) :: gzs
  contains
    procedure :: init => static_init
    procedure :: clear => static_clear
    final :: static_final
  end type static_type

contains

  subroutine static_init(this, mesh)

    class(static_type), intent(inout) :: this
    type(mesh_type), intent(in) :: mesh

    call this%clear()

  end subroutine static_init

  subroutine static_clear(this)

    class(static_type), intent(inout) :: this

    if (allocated(this%gzs)) deallocate(this%gzs)

  end subroutine static_clear

  subroutine static_final(this)

    type(static_type), intent(inout) :: this

    call this%clear()

  end subroutine static_final

end module static_mod
