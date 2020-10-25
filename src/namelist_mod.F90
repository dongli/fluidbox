module namelist_mod

  implicit none

  integer nx
  integer ny
  integer nz

  real(8) :: start_x = 0
  real(8) ::   end_x = 1
  real(8) :: start_y = 0
  real(8) ::   end_y = 1

  character(30) :: test_case = ''

  namelist /fluidbox/ &
  start_x           , &
    end_x           , &
  start_y           , &
    end_y           , &
  test_case

contains

  subroutine namelist_init(file_path)

    character(*), intent(in) :: file_path

    open(10, file=file_path, status='old')
    read(10, nml=fluidbox)
    close(10)

  end subroutine namelist_init

end module namelist_mod
