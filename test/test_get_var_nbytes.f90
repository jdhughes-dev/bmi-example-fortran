program test_get_var_nbytes

  use bmif_3_0, only: BMI_FAILURE
  use bmiheatf
  use fixtures, only: config_file, status, no_extensions, &
       supported_extensions

  implicit none

  character (len=*), parameter :: var_name = "plate_surface__temperature"
  integer, parameter :: expected_nbytes = 20*10*4

  type (bmi_heat) :: m
  integer :: var_index
  integer :: var_nbytes

  status = m%initialize(config_file, no_extensions, supported_extensions)
  status = m%get_var_index(var_name, var_index)
  status = m%get_var_nbytes(var_index, var_nbytes)
  status = m%finalize()

  if (var_nbytes.ne.expected_nbytes) then
     stop BMI_FAILURE
  end if
end program test_get_var_nbytes
