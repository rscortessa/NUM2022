!
! Reference implementation for exercise two Numerical Method 1 (2021 course)
!
! Author : Graziano Giuliani
! Date   : 01/20/2021
!
! Write a program that takes a positive integer number A as an input and
! calculates the maximum number N such that the sum of the first even
! numbers up to N is greater than A.
! Example, if A equal to 23:
!                2=2
!               2+4=6
!              2+4+6=12
!             2+4+6+8=20
!           2+4+6+8+10=30
! In this case N=10
! The program must to be able to take the integer number A as input during
! running time, assign to a variable and print on the screen N as a result.
! Optional
! Write a program that takes a positive integer number A as an input and
! calculates the maximum prime number N such that the sum of the first prime
! numbers up to N is greater than A.
! Example, if A equal to 23:
!                 1=1
!                1+2=3
!               1+2+3=6
!              1+2+3+5=11
!             1+2+3+5+7=18
!            1+2+3+5+7+11=29
! In this case N=11
! The program must to be able to take the integer number A as input during
! running time, assign to a variable and print on the screen N as a result.
!
program assignement_two
  implicit none

  integer :: A
  integer :: thesum, N, i
  logical :: isprime

  write (6, '(a)') 'Calculate the maximum number N such that the sum of&
                   & the first even numbers up '
  write (6, '(a)') 'to N is greater than A'
  write (6, *)
  write (6, '(a)', advance='no') 'Enter number A : '
  read (*,*) A

  if ( A <= 0 ) then
    write(0,*) 'Error: the A number must be >= 0'
    stop 'error'
  end if

  thesum = 2
  N = 2
  do while ( thesum < A )
    N = N + 2
    thesum = thesum + N
  end do

  write (6, '(a,i8)') 'The number N is : ', N
  write (6, *)

  write (6, '(a)') 'Calculate the maximum number N such that the sum of&
                   & the first prime numbers up '
  write (6, '(a)') 'to N is greater than A'
  write (6, *)
  write (6, '(a)', advance='no') 'Enter number A : '
  read (*,*) A

  if ( A <= 0 ) then
    write(0,*) 'Error: the A number must be >= 0'
    stop 'error'
  end if

  thesum = 1
  N = 1
  do while ( thesum < A )
    N = N + 1
    isprime = .true.
    do i = 2 , N - 1
      if ( mod(N,i) == 0 ) isprime = .false.
    end do
    if ( isprime ) thesum = thesum + N
  end do

  write (6, '(a,i8)') 'The number N is : ', N
  write (6, *)

end program assignement_two
