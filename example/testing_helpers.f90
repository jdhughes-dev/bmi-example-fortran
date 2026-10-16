module testing_helpers

  use bmif_3_0, only: BMI_MAX_EXTENSION_NAME
  implicit none

  ! Request no extensions from initialize.
  character (len=BMI_MAX_EXTENSION_NAME) :: no_extensions(0)
  character (len=BMI_MAX_EXTENSION_NAME), allocatable :: &
       supported_extensions(:)

contains

  ! Prints a rank 1 array to the console.
  subroutine print_array(array, dims)
    integer :: dims(2)
    real, dimension(product(dims)) :: array
    integer :: i, j

    do j = 1, dims(1)
       do i = 1, dims(2)
          write (*,"(f6.1)", advance="no") array(j + dims(1)*(i-1))
       end do
       write (*,*)
    end do
  end subroutine print_array

end module testing_helpers
