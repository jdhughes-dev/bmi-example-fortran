program test_get_grid_face_edges

  use bmif_3_0, only: BMI_FAILURE
  use bmiheatf
  use fixtures, only: config_file, status, no_extensions, &
       supported_extensions

  implicit none

  integer, parameter :: grid_id = 0
  integer, parameter :: count = 2
  integer, dimension(count), parameter :: expected_conn = [-1, -1]

  type (bmi_heat) :: m
  integer, dimension(count) :: conn
  integer :: i, fail_status

  status = m%initialize(config_file, no_extensions, supported_extensions)
  fail_status = m%get_grid_face_edges(grid_id, conn)
  status = m%finalize()

  print *, conn
  print *, expected_conn

  do i = 1, count
     if (conn(i) /= expected_conn(i)) then
        stop BMI_FAILURE
     end if
  end do

  if (fail_status /= BMI_FAILURE) then
     stop BMI_FAILURE
  end if
end program test_get_grid_face_edges
