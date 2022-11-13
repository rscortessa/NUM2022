!
! Reference implementation for exercise three Numerical Method 1 (2021 course)
! Write a program that, taken a positive integer number N>=1 as
! input, calculates the N-step element of the sequence T of the
! modify Tribonacci numbers defined by:
!
!   T(0) = 0
!   T(1) = 1
!   T(2) = 2
!   T(K) = (T(K-1) + T(K-2) + T(K-3))/2 (For K >=3)
!
! Author : Graziano Giuliani
! Date   : 01/22/2021
!
program assignement_three
  implicit none

  integer :: N
  real :: T

  write (6, '(a)') 'Calculate the tribonacci number of positive number N'
  write (6, '(a)', advance='no') 'Enter number N : '
  read (5,*) N
  if ( N < 0 ) then
    write(0,*) 'Error: the N number must be > 0'
    stop 'error'
  end if

  T = tribonacci(N)

  write (6, '(a,f8.4)') 'Tribonacci number is : ',T

  contains

  real recursive function tribonacci(n) result(res)
    implicit none
    integer , intent(in) :: n
    if ( n >= 3 ) then
      res = (tribonacci(n-1)+tribonacci(n-2)+tribonacci(n-3))/2.0
    else
      res = real(n)
    end if
  end function tribonacci

end program assignement_three
