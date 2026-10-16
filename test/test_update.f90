program test_update

  use bmif_3_0, only: BMI_FAILURE
  use bmiheatf
  use fixtures, only: config_file, status, tolerance, no_extensions, &
       supported_extensions

  implicit none

  double precision, parameter :: expected_time = 0.333333d0

  type (bmi_heat) :: m
  double precision :: time

  status = m%initialize(config_file, no_extensions, supported_extensions)
  status = m%update()
  status = m%get_current_time(time)
  status = m%finalize()

  print *, time
  print *, expected_time

  if (abs(time - expected_time) > tolerance) then
     stop BMI_FAILURE
  end if
end program test_update
