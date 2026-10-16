program test_get_var_type

  use bmif_3_0, only: BMI_FAILURE, BMI_MAX_TYPE_NAME
  use bmiheatf
  use fixtures, only: config_file, status, no_extensions, &
       supported_extensions

  implicit none

  character (len=*), parameter :: var_name = "plate_surface__temperature"
  character (len=*), parameter :: expected_type = "real"

  type (bmi_heat) :: m
  integer :: var_index
  character (len=BMI_MAX_TYPE_NAME) :: var_type

  status = m%initialize(config_file, no_extensions, supported_extensions)
  status = m%get_var_index(var_name, var_index)
  status = m%get_var_type(var_index, var_type)
  status = m%finalize()

  if (var_type.ne.expected_type) then
     stop BMI_FAILURE
  end if
end program test_get_var_type
