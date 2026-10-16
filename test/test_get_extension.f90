program test_get_extension

  use bmif_3_0, only: BMI_FAILURE, bmi_extension
  use bmiheatf
  use fixtures, only: config_file, status, no_extensions, &
       supported_extensions

  implicit none

  type (bmi_heat), target :: m
  class (bmi_extension), pointer :: ext

  status = m%initialize(config_file, no_extensions, supported_extensions)

  status = m%get_extension("not_an_extension", ext)
  if ((status /= BMI_FAILURE).or.associated(ext)) then
     stop 1
  end if

  status = m%finalize()
end program test_get_extension
