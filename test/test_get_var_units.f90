program test_get_var_units

  use bmif_3_0, only: BMI_FAILURE, BMI_MAX_UNITS_NAME
  use bmiheatf
  use fixtures, only: config_file, status, no_extensions, &
       supported_extensions

  implicit none

  character (len=*), parameter :: var_name = "plate_surface__temperature"
  character (len=*), parameter :: expected_units = "K"

  type (bmi_heat) :: m
  integer :: var_index
  character (len=BMI_MAX_UNITS_NAME) :: var_units

  status = m%initialize(config_file, no_extensions, supported_extensions)
  status = m%get_var_index(var_name, var_index)
  status = m%get_var_units(var_index, var_units)
  status = m%finalize()

  if (var_units.ne.expected_units) then
     stop BMI_FAILURE
  end if
end program test_get_var_units
