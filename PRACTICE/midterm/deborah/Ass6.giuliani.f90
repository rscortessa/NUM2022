!
! Reference implementation for exercise six Numerical Method 1 (2021 course)
!
! Root finding exercise
! Find a real root of f(x) = exp(x) - 2 with eps = 1.0e-6
! 1) Read at running time two initial guesses and check if the root
!    is bracketed
! 2) Write a program with
!        Subroutine Bisection(.....) and RegulaFalsi(....)
!        Function func(x)
! 3) Write the output in a file formatted as follow:
!    count   a     f(a)    b    f(b)     c    f(c)
!       1   ..     ....   ..    ....    ..    ....
!       2   ..     ....   ..    ....    ..    ....
!     ...
!
! Author : Graziano Giuliani
! Date   : 01/02/2021
!
program assignement_six
  implicit none
  real , parameter :: eps = 1.0e-6
  real :: a , b

  write(6,'(a)',advance='no') 'Please enter initial guess a : '
  read(5,*) a
  write(6,'(a)',advance='no') 'Please enter initial guess b : '
  read(5,*) b

  call bisection(a,b)

  write(6,*)

  call regulafalsi(a,b)

  contains

  subroutine bisection(aa,bb)
    implicit none
    real , intent(in) :: aa , bb
    real :: a , b , c
    real :: fa , fb , fc
    integer :: ic
    a = aa
    b = bb
    do ic = 1 , 100
      fa = func(a)
      fb = func(b)
      c = (a+b)/2.0
      fc = func(c)
      write(6,'(i4,6f12.6)') ic , a , fa , b , fb , c , fc
      if ( abs(fc) < eps ) then
        exit
      end if
      if ( fc * fa < 0.0 ) then
        b = c
      end if
      if ( fc * fb < 0.0 ) then
        a = c
      end if
    end do
  end subroutine bisection

  subroutine regulafalsi(aa,bb)
    implicit none
    real , intent(in) :: aa , bb
    real :: a , b , c
    real :: fa , fb , fc
    integer :: ic
    a = aa
    b = bb
    do ic = 1 , 100
      fa = func(a)
      fb = func(b)
      c = (fb*a - fa*b)/(fb-fa)
      fc = func(c)
      write(6,'(i4,6f12.6)') ic , a , fa , b , fb , c , fc
      if ( abs(fc) < eps ) then
        exit
      end if
      if ( fc * fa < 0.0 ) then
        b = c
      end if
      if ( fc * fb < 0.0 ) then
        a = c
      end if
    end do
  end subroutine regulafalsi

  pure real function func(x)
    implicit none
    real , intent(in) :: x
    func = exp(x) - 2.0
  end function func

end program assignement_six
