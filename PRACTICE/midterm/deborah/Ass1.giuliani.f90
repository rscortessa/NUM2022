!
! Reference implementation for exercise one Numerical Method 1 (2021 course)
!
! Author : Graziano Giuliani
! Date   : 01/18/2021
!
! Assignement one:
! Write a Fortran program to be able to accomplish the following tasks:
! 1) calculate the following equation
!   x = -b/(2a + 3c**a)
!   (where a, b and c are read from the screen at execution time)
! 2) write on the screen a, b, c and x.
! 3) calculate y = a + b + c + x
! 4) write on the screen y
!
program assignement_one
  implicit none

  real :: a , b , c
  real :: x
  real :: y

  ! Read the data from user input

  write (6, '(a)', advance='no') 'Enter number a : '
  read (*,*) a
  write (6, '(a)', advance='no') 'Enter number b : '
  read (*,*) b
  write (6, '(a)', advance='no') 'Enter number c : '
  read (*,*) c

  ! Compute requested
  x = -b/(2.0*a + 3.0*c**a)

  ! Write requested
  write (6, *) 'The input a is : ', a
  write (6, *) 'The input b is : ', b
  write (6, *) 'The input c is : ', c
  write (6, *) 'The intermediate x is : ', x

  ! Compute requested final
  y = a + b + c + x

  ! Write on screen
  write (6, *) 'The computed result y is : ', y

end program assignement_one
