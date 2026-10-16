program test_initialize

  use bmif_3_0, only: BMI_SUCCESS, BMI_MAX_EXTENSION_NAME
  use bmiheatf
  use fixtures, only: status, no_extensions, &
       supported_extensions

  implicit none

  character (len=*), parameter :: config_file1 = ""
  character (len=256) :: config_file2

  type (bmi_heat) :: m
  character (len=BMI_MAX_EXTENSION_NAME) :: requested(1)
  integer :: status1, status2, status3

  ! Get the path to the config file from command line params
  call get_command_argument(1, config_file2)

  status1 = m%initialize(config_file1, no_extensions, supported_extensions)
  status = m%finalize()
  if (status1.ne.BMI_SUCCESS) then
     stop 1
  end if
  if (size(supported_extensions).ne.0) then
     stop 3
  end if

  status2 = m%initialize(trim(config_file2), no_extensions, &
       supported_extensions)
  status = m%finalize()
  if (status2.ne.BMI_SUCCESS) then
     stop 2
  end if

  ! An unknown extension is not reported as supported.
  requested(1) = "not_an_extension"
  status3 = m%initialize(config_file1, requested, supported_extensions)
  status = m%finalize()
  if (status3.ne.BMI_SUCCESS) then
     stop 4
  end if
  if (size(supported_extensions).ne.0) then
     stop 5
  end if
end program test_initialize
