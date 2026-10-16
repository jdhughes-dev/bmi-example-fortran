! Test the set_value and set_value_at_indices functions.
program set_value_ex

  use bmif_3_0, only: BMI_MAX_VAR_NAME
  use bmiheatf
  use testing_helpers
  implicit none

  type (bmi_heat) :: m
  integer :: temperature_index
  integer :: var_index
  integer :: s, i, j, grid_id
  character (len=BMI_MAX_VAR_NAME), pointer :: names(:)
  integer :: grid_size, dims(2), locations(3)
  real :: values(3)
  real, allocatable :: z(:), y(:)
  character(len=30) :: rowfmt

  write (*,"(a)",advance="no") "Initializing..."
  s = m%initialize("", no_extensions, supported_extensions)
  s = m%get_var_index("plate_surface__temperature", temperature_index)
  write (*,*) "Done."

  s = m%get_varset_members("output", names)
  write (*,"(a, a)") "Output variables: ", names

  s = m%get_var_index(names(1), var_index)
  s = m%get_var_grid(var_index, grid_id)
  s = m%get_grid_shape(grid_id, dims)
  write(rowfmt,'(a,i4,a)') '(', dims(2), '(1x,f6.1))'
  s = m%get_grid_size(grid_id, grid_size)
  write(*,'(a,i8)') 'Grid size: ', grid_size

  write (*, "(a)") "Initial values:"
  allocate(z(grid_size))
  s = m%get_value(temperature_index, z)
  call print_array(z, dims)

  write (*,"(a)",advance="no") "Setting new values..."
  z = 42.0
  s = m%set_value(temperature_index, z)
  write (*,*) "Done."
  write (*, "(a)") "New values:"
  allocate(y(grid_size))
  s = m%get_value(temperature_index, y)
  call print_array(y, dims)

  write (*, "(a)") "Set values at three locations:"
  locations = [21, 41, 62]
  values = [-1.0, -1.0, -1.0]
  write (*,*) "Locations: ", locations
  write (*,*) "Values: ", values
  s = m%set_value_at_indices(temperature_index, locations, values)
  write (*, "(a)") "New values:"
  s = m%get_value(temperature_index, y)
  call print_array(y, dims)

  write (*,"(a)", advance="no") "Finalizing..."
  deallocate(z, y)
  s = m%finalize()
  write (*,*) "Done"

end program set_value_ex
