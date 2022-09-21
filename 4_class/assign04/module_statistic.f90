module module_statistic

  use iso_fortran_env
  use module_sorter

  implicit none

  private

  public :: mean, stddev, pct90, pct95, pct99

  contains

  real function mean(x)
    implicit none
    real, dimension(:), intent(in) :: x
    ! ---------------
    ! Not implemented
    ! ---------------
    !
    ! You can use the intrinsic Fortran SUM
    !
    ! https://gcc.gnu.org/onlinedocs/gfortran/SUM.html
    !
  end function mean

  real function stddev(x)
    implicit none
    real, dimension(:), intent(in) :: x
    ! ---------------
    ! Not implemented
    ! ---------------
    !
    ! You can use the intrinsic Fortran SUM
    !
    ! https://gcc.gnu.org/onlinedocs/gfortran/SUM.html
    !
  end function stddev

  real function pct90(x)
    implicit none
    real, dimension(:), intent(in) :: x
    ! ---------------
    ! Not implemented
    ! ---------------
    !
    ! The percentile can be calculated using the nearest-rank method
    !
    !         https://en.wikipedia.org/wiki/Percentile
    !
    ! We need to order the array in ascending order
    ! Use the sort function in module_sorter for this task
    ! The 50th percentile of a dataset is the element in the middle of
    ! the ordered set of the elements in the dataset.
    ! The 10th percentile of a dataset of 20 elements is the element in
    ! position (10 * 20)/100 = 2 of the ordered set of the original
    ! array elements.
    ! To round up the numbers you can use the intrinsic fortran function
    ! ceiling. e.g. ceiling(3.2) will be equal to 4
    !
    ! NOTE: THE INPUT ARRAY MUST NOT BE CHANGED IN THE FUNCTION
    !
  end function pct90

  real function pct95(x)
    implicit none
    real, dimension(:), intent(in) :: x
    ! ---------------
    ! Not implemented
    ! ---------------
    !
    ! The percentile can be calculated using the nearest-rank method
    !
    !         https://en.wikipedia.org/wiki/Percentile
    !
    ! We need to order the array in ascending order
    ! Use the sort function in module_sorter for this task
    ! The 50th percentile of a dataset is the element in the middle of
    ! the ordered set of the elements in the dataset.
    ! The 10th percentile of a dataset of 20 elements is the element in
    ! position (10 * 20)/100 = 2 of the ordered set of the original
    ! array elements.
    ! To round up the numbers you can use the intrinsic fortran function
    ! ceiling. e.g. ceiling(3.2) will be equal to 4
    !
    ! NOTE: THE INPUT ARRAY MUST NOT BE CHANGED IN THE FUNCTION
    !
  end function pct95

  real function pct99(x)
    implicit none
    real, dimension(:), intent(in) :: x
    ! ---------------
    ! Not implemented
    ! ---------------
    !
    ! The percentile can be calculated using the nearest-rank method
    !
    !         https://en.wikipedia.org/wiki/Percentile
    !
    ! We need to order the array in ascending order
    ! Use the sort function in module_sorter for this task
    ! The 50th percentile of a dataset is the element in the middle of
    ! the ordered set of the elements in the dataset.
    ! The 10th percentile of a dataset of 20 elements is the element in
    ! position (10 * 20)/100 = 2 of the ordered set of the original
    ! array elements.
    ! To round up the numbers you can use the intrinsic fortran function
    ! ceiling. e.g. ceiling(3.2) will be equal to 4
    !
    ! NOTE: THE INPUT ARRAY MUST NOT BE CHANGED IN THE FUNCTION
    !
  end function pct99

end module module_statistic
