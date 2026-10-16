program test_get_value_ptr

  use bmif_3_0, only: BMI_SUCCESS, BMI_FAILURE, BMI_MAX_EXTENSION_NAME, &
       bmi_extension, bmi_pointers
  use bmiheatf
  use fixtures, only: status, print_array, supported_extensions

  implicit none

  character (len=256) :: config_file
  character (len=*), parameter :: var_name = "plate_surface__temperature"
  integer, parameter :: rank = 2
  integer, parameter, dimension(rank) :: shape = (/ 10, 5 /)
  real, parameter, dimension(shape(2)) :: &
       expected = (/ 0.0, 0.0, 0.0, 0.0, 0.0 /)
  integer :: retcode

  ! Get the path to the config file from command line params
  call get_command_argument(1, config_file)

  retcode = run_test()
  if (retcode.ne.BMI_SUCCESS) then
     stop BMI_FAILURE
  end if

contains

  function run_test() result(code)
    type (bmi_heat), target :: m
    class (bmi_extension), pointer :: ext
    character (len=BMI_MAX_EXTENSION_NAME) :: requested(1)
    real, pointer :: tref(:)
    integer :: i, j
    integer :: code

    requested(1) = "bmi:pointers"
    status = m%initialize(trim(config_file), requested, &
         supported_extensions)
    status = m%get_extension("bmi:pointers", ext)
    select type (ext)
    class is (bmi_pointers)
       status = ext%get_value_ptr_float(var_name, tref)
    class default
       code = BMI_FAILURE
       return
    end select

    ! Visual inspection.
    call print_array(tref, shape)
    do i = 1, shape(2)
       write(*,*) tref((i-1)*shape(1)+1)
    end do

    code = BMI_SUCCESS
    do i = 1, shape(2)
       if (tref((i-1)*shape(1)+1).ne.expected(i)) then
          code = BMI_FAILURE
          exit
       end if
    end do

    status = m%finalize()
  end function run_test

end program test_get_value_ptr
