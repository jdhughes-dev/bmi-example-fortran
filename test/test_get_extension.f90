program test_get_extension

  use bmif_3_0, only: BMI_SUCCESS, BMI_FAILURE, bmi_extension, bmi_pointers
  use bmiheatf
  use fixtures, only: config_file, status, no_extensions, &
       supported_extensions

  implicit none

  type (bmi_heat), target :: m
  class (bmi_extension), pointer :: ext

  status = m%initialize(config_file, no_extensions, supported_extensions)

  status = m%get_extension("bmi:pointers", ext)
  if ((status /= BMI_SUCCESS).or.(.not.associated(ext))) then
     stop 2
  end if
  select type (ext)
  class is (bmi_pointers)
  class default
     stop 3
  end select

  status = m%get_extension("not_an_extension", ext)
  if ((status /= BMI_FAILURE).or.associated(ext)) then
     stop 1
  end if

  status = m%finalize()
end program test_get_extension
