program test_get_varset_member_count

  use bmif_3_0, only: BMI_SUCCESS, BMI_FAILURE
  use bmiheatf
  use fixtures, only: config_file, status, no_extensions, &
       supported_extensions

  implicit none

  integer, parameter :: expected_input_count = 3
  integer, parameter :: expected_output_count = 1

  type (bmi_heat) :: m
  integer :: count

  status = m%initialize(config_file, no_extensions, supported_extensions)

  status = m%get_varset_member_count("input", count)
  if ((status /= BMI_SUCCESS).or.(count /= expected_input_count)) then
     stop 1
  end if

  status = m%get_varset_member_count("output", count)
  if ((status /= BMI_SUCCESS).or.(count /= expected_output_count)) then
     stop 2
  end if

  status = m%get_varset_member_count("not_a_set", count)
  if (status /= BMI_FAILURE) then
     stop 3
  end if

  status = m%finalize()
end program test_get_varset_member_count
