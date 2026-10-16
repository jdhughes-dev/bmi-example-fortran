! Test the BMI get_var_* and get_grid_* functions.
program vargrid_test

  use bmif_3_0, only: BMI_MAX_VAR_NAME
  use bmiheatf
  use testing_helpers, only: no_extensions, supported_extensions
  implicit none

  type (bmi_heat) :: m
  integer :: var_index
  integer :: s, i
  character (len=BMI_MAX_VAR_NAME), pointer :: names(:)
  integer :: grid_id
  character (len=BMI_MAX_VAR_NAME) :: astring
  integer :: asize
  double precision, dimension(2) :: darray
  integer, dimension(2) :: iarray

  write (*,"(a)",advance="no") "Initializing..."
  s = m%initialize("", no_extensions, supported_extensions)
  write (*,*) "Done."

  s = m%get_varset_members("output", names)
  write (*,"(a30)") "Output variables:"
  do i = 1, size(names)
     write (*,"(a30, 1x, a)") "-", names(i)
  end do

  s = m%get_var_index(names(1), var_index)
  s = m%get_var_grid(var_index, grid_id)
  write (*,"(a30, i3)") "Grid id:", grid_id

  s = m%get_grid_type(grid_id, astring)
  write (*,"(a30, 1x, a30)") "Grid type:", astring
  s = m%get_grid_origin(grid_id, darray)
  write (*,"(a30, 1x, 2(f8.2))") "Grid origin:", darray
  s = m%get_grid_rank(grid_id, asize)
  write (*,"(a30, i3)") "Grid rank:", asize
  s = m%get_grid_shape(grid_id, iarray)
  write (*,"(a30, 2(1x, i3))") "Grid shape:", iarray
  s = m%get_grid_size(grid_id, asize)
  write (*,"(a30, i8)") "Grid size:", asize
  s = m%get_grid_spacing(grid_id, darray)
  write (*,"(a30, 1x, 2(f8.2))") "Grid spacing:", darray

  s = m%get_var_itemsize(var_index, asize)
  write (*,"(a30, i8, 1x, a)") "Item size:", asize, "bytes"
  s = m%get_var_nbytes(var_index, asize)
  write (*,"(a30, i8, 1x, a)") "Variable size:", asize, "bytes"
  s = m%get_var_type(var_index, astring)
  write (*,"(a30, 1x, a30)") "Variable type:", astring
  s = m%get_var_units(var_index, astring)
  write (*,"(a30, 1x, a30)") "Variable units:", astring

  write (*,"(a)", advance="no") "Finalizing..."
  s = m%finalize()
  write (*,*) "Done"

end program vargrid_test
