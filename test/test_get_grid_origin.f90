program test_get_grid_origin

  use bmif_3_0, only: BMI_FAILURE
  use bmiheatf
  use fixtures, only: config_file, status, no_extensions, &
       supported_extensions

  implicit none

  integer, parameter :: grid_id = 0
  integer, parameter :: rank = 2
  double precision, dimension(rank), parameter :: &
       expected_origin = [0.0, 0.0]

  type (bmi_heat) :: m
  double precision, dimension(2) :: grid_origin
  integer :: i

  status = m%initialize(config_file, no_extensions, supported_extensions)
  status = m%get_grid_origin(grid_id, grid_origin)
  status = m%finalize()

  do i = 1, rank
     if (grid_origin(i).ne.expected_origin(i)) then
        stop BMI_FAILURE
     end if
  end do
end program test_get_grid_origin
