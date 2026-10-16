program test_get_varset_members

  use bmif_3_0, only: BMI_SUCCESS, BMI_FAILURE, BMI_MAX_VAR_NAME
  use bmiheatf
  use fixtures, only: config_file, status

  implicit none

  type (bmi_heat) :: m
  character (len=BMI_MAX_VAR_NAME) :: expected_input(3), expected_output(1)
  character (len=BMI_MAX_VAR_NAME), pointer :: names(:)
  integer :: i

  expected_input(1) = "plate_surface__temperature"
  expected_input(2) = "plate_surface__thermal_diffusivity"
  expected_input(3) = "model__identification_number"
  expected_output(1) = "plate_surface__temperature"

  status = m%initialize(config_file)

  status = m%get_varset_members("input", names)
  if ((status /= BMI_SUCCESS).or.(size(names) /= size(expected_input))) then
     stop 1
  end if
  do i = 1, size(names)
     write(*,*) trim(names(i))
     if (names(i) /= expected_input(i)) then
        stop 2
     end if
  end do

  status = m%get_varset_members("output", names)
  if ((status /= BMI_SUCCESS).or.(size(names) /= size(expected_output))) then
     stop 3
  end if
  do i = 1, size(names)
     write(*,*) trim(names(i))
     if (names(i) /= expected_output(i)) then
        stop 4
     end if
  end do

  status = m%get_varset_members("not_a_set", names)
  if (status /= BMI_FAILURE) then
     stop 5
  end if

  status = m%finalize()
end program test_get_varset_members
