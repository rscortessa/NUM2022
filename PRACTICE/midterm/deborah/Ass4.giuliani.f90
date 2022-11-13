!
! Reference implementation for exercise four numerical method 1 (2021 course)
!
! 1) Find all elements common to both the arrays and print a message
!    indicating, for each common element, the index occupied by the first
!    and second array.
! 2) Create a new array with the common elements (arr3).
! 3) Calculate the median of the arrays (arr1, arr2, arr3)
! 4) Calculate the standard deviation of the arrays (arr1, arr2, arr3)
! 5) Calculate the root mean square difference (average difference) between
!    the two arrays (arr1 and arr2)
!
! Author : Graziano Giuliani
! Date   : 01/25/2021
!
program assignement_four
  use module_ass4
  implicit none
  ! Fragment of code from exercise text
  real(8), dimension(100) :: arr1, arr2
  real(4), dimension(100) :: rind
  integer :: i , i1 , i2
  ! End fragment of code from exercise text

  integer, dimension(100) :: ind
  real(8), allocatable, dimension(:) :: arr3

  ! Fragment of code from exercise text
  call random_number(arr1)
  call random_number(arr2)
  call random_number(rind)
  arr1 = arr1 * 1000_8
  arr2 = arr2 * 1000_8
  ind = min(max(int(rind * 100_4),1),100)
  do i = 1, ind(1)
    i1 = ind(i)
    i2 = abs(ind(i)-100)
    arr2(i1) = arr1(i2)
  end do
  ! End fragment of code from exercise text

  call common_elements(arr1, arr2, arr3)
  print*, "The common elements are: ", arr3
  print*, "Median of arr1: ", median(arr1)
  print*, "Median of arr2: ", median(arr2)
  print*, "Median of arr3: ", median(arr3)
  print*, "Standard deviation of arr1: ", std(arr1)
  print*, "Standard deviation of arr2: ", std(arr2)
  print*, "Standard deviation of arr3: ", std(arr3)
  print*, "Root mean square difference between arr1 and arr2: ", rmsd(arr1,arr2)
end program assignement_four
