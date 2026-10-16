! Read a model variable by reference through the bmi:pointers extension.
program extension_ex

  use bmif_3_0, only: BMI_SUCCESS, BMI_MAX_EXTENSION_NAME, &
       bmi_extension, bmi_pointers
  use bmiheatf
  use testing_helpers, only: print_array, supported_extensions
  implicit none

  ! m is a target so the extension object stays valid after get_extension.
  type (bmi_heat), target :: m
  class (bmi_extension), pointer :: ext
  character (len=BMI_MAX_EXTENSION_NAME) :: requested(1)
  integer :: s, i, j, temperature_index, grid_id, dims(2)
  real, pointer :: temperature(:)
  double precision :: time

  requested(1) = "bmi:pointers"
  s = m%initialize("", requested, supported_extensions)
  write (*,"(a)") "Supported extensions:"
  do i = 1, size(supported_extensions)
     write (*,"(2a)") "- ", trim(supported_extensions(i))
  end do
  if (size(supported_extensions) /= 1) then
     stop 1
  end if
  s = m%get_var_index("plate_surface__temperature", temperature_index)

  s = m%get_extension("bmi:pointers", ext)
  if (s /= BMI_SUCCESS) then
     stop 2
  end if

  ! The caller needs only the abstract bmi_pointers type, not heat_pointers.
  select type (ext)
  class is (bmi_pointers)
     s = ext%get_value_ptr_float(temperature_index, temperature)
  class default
     stop 3
  end select

  s = m%get_var_grid(temperature_index, grid_id)
  s = m%get_grid_shape(grid_id, dims)

  ! The pointer tracks the model state across updates.
  do j = 1, 3
     s = m%get_current_time(time)
     write (*,"(a, f6.1)") "Current time:", time
     call print_array(temperature, dims)
     s = m%update()
  end do

  s = m%finalize()
end program extension_ex
