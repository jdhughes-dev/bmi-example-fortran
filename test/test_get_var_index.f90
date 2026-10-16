program test_get_var_index

  use bmif_3_0, only: BMI_SUCCESS, BMI_FAILURE, BMI_MAX_VAR_NAME
  use bmiheatf
  use fixtures, only: config_file, status, no_extensions, &
       supported_extensions

  implicit none

  type (bmi_heat) :: m
  character (len=BMI_MAX_VAR_NAME), pointer :: names(:)
  character (len=BMI_MAX_VAR_NAME) :: units
  integer :: indices(3)
  integer :: i, j

  status = m%initialize(config_file, no_extensions, supported_extensions)

  ! Every input variable has an index, and no two share one.
  status = m%get_varset_members("input", names)
  do i = 1, size(names)
     status = m%get_var_index(names(i), indices(i))
     write(*,*) trim(names(i)), indices(i)
     if (status /= BMI_SUCCESS) then
        stop 1
     end if
     do j = 1, i - 1
        if (indices(j) == indices(i)) then
           stop 2
        end if
     end do
  end do

  ! The index refers to the named variable.
  status = m%get_var_units(indices(1), units)
  if (units /= "K") then
     stop 3
  end if

  status = m%get_var_index("not_a_variable", i)
  if (status /= BMI_FAILURE) then
     stop 4
  end if

  status = m%finalize()
end program test_get_var_index
