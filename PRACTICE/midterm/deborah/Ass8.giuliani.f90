!
! Reference implementation for exercise eight Numerical Method 1 (2021 course)
!
! Root finding exercise
!
! Author : Graziano Giuliani
!
! 1) Find a real root of f(x) = sqrt(x)cos(x**2) using the Secant method.
! 2) Write a mixed Newton-Bisection method subroutine. Whenever
!    Newton is not converging fast enough or is failing, take a
!    bisection step (optional)
! 3) First guess: of your choice, Accuracy: eps = 1e-8
! 4) Write a program with:
!      *) Subroutine Secant(...)
!      *) Subroutine New_Bisec(...)
!      *) Function f(x) and f'(x)
!
! Date   : 05/02/2021
!
program assignement_eight
  implicit none
  double precision , parameter :: eps = 1.0e-12
  character(len=*) , parameter :: datafile = 'guesses.txt'
  character(len=*) , parameter :: outfile1 = 'giuliani_secant.txt'
  character(len=*) , parameter :: outfile2 = 'giuliani_new_bisec.txt'
  double precision , dimension(:) , allocatable :: x0, x1
  double precision :: swap
  integer :: iunit , ierr
  integer :: iguess , numguesses

  open(newunit=iunit,file=datafile,status='old',form='formatted', &
          iostat=ierr, action='read')
  if ( ierr /= 0 ) then
    write(0,*) 'Error opening file ',datafile
    stop 'I/O erorr'
  end if

  read(iunit,fmt=*,iostat=ierr) numguesses
  if ( ierr /= 0 ) then
    write(0,*) 'Error reading from file ',datafile
    stop 'I/O errorr'
  end if

  if ( numguesses <= 0 ) then
    write(0,*) 'The number of guesses must be positive!'
    stop 'Logical errorr'
  end if

  allocate(x0(numguesses))
  allocate(x1(numguesses))

  do iguess = 1 , numguesses
    read(iunit,fmt=*,iostat=ierr) x0(iguess), x1(iguess)
    if ( ierr /= 0 ) then
      write(0,*) 'Error reading from file ',datafile,' at line ',iguess
      stop 'I/O error'
    end if
    if ( x0(iguess) > x1(iguess) ) then
      swap = x0(iguess)
      x0(iguess) = x1(iguess)
      x1(iguess) = swap
    end if
    if ( x0(iguess) < 0.0 ) then
      write(0,*) 'Error : function is defined only for x >= 0'
      stop 'Argument error'
    end if
  end do
  close(iunit)

  open(newunit=iunit,file=outfile1,status='replace',form='formatted', &
          iostat=ierr, action='write')
  if ( ierr /= 0 ) then
    write(0,*) 'Error opening file ',outfile1
    stop 'I/O erorr'
  end if

  do iguess = 1 , numguesses
    call secant(x0(iguess),x1(iguess),iunit)
  end do
  close(iunit)

  open(newunit=iunit,file=outfile2,status='replace',form='formatted', &
          iostat=ierr, action='write')
  if ( ierr /= 0 ) then
    write(0,*) 'Error opening file ',outfile2
    stop 'I/O erorr'
  end if

  do iguess = 1 , numguesses
    call new_bisec(x0(iguess),x1(iguess),iunit)
  end do
  close(iunit)

  contains

  subroutine secant(x0,x1,iunit)
    implicit none
    double precision , intent(in) :: x0 , x1
    integer , intent(in) :: iunit
    double precision :: xm2 , xm1 , x
    double precision :: fxm1 , fxm2
    !integer , parameter :: maxcycle = 100
    integer :: ic
    xm2 = x0
    xm1 = x1
    do! ic = 1 , maxcycle
      fxm1 = func(xm1)
      fxm2 = func(xm2)
      x = xm1 - fxm1 * (xm1-xm2)/(fxm1-fxm2)
      if ( abs(x-xm1) < eps ) then
        exit
      end if
      xm2 = xm1
      xm1 = x
    end do
    !if ( ic >= maxcycle ) then
    !  write(iunit,'(2f14.4,a)') x0, x1, ': Convergence not reached!!!!'
    !  return
    !end if
    write(iunit,'(i4,2f14.4,f14.8)') ic , x0 , x1 , x
  end subroutine secant

  subroutine bisection(aa,bb,iunit)
    implicit none
    double precision , intent(in) :: aa, bb
    integer , intent(in) :: iunit
    double precision :: a , b , c
    double precision :: fa , fb , fc
    integer :: ic
    a = aa
    b = bb
    do ic = 1 , 100
      fa = func(a)
      fb = func(b)
      c = (a+b)/2.0d0
      fc = func(c)
      if ( abs(fc) < eps ) then
        exit
      end if
      if ( fc * fa < 0.0d0 ) then
        b = c
      end if
      if ( fc * fb < 0.0d0 ) then
        a = c
      end if
    end do
    write(iunit,'(i4,2f14.4,f14.8)') ic , aa , bb , c
  end subroutine bisection

  subroutine new_bisec(x0,x1,iunit)
    implicit none
    double precision , intent(in) :: x0 , x1
    integer , intent(in) :: iunit
    double precision :: x , xp1
    double precision :: fx , fp , dist
    integer , parameter :: maxcycle = 100
    integer :: ic
    x = x0
    dist = 0.0d0
    do ic = 1 , maxcycle
      fx = func(x)
      fp = deriv(x)
      if ( abs(fp) <= 1.0d-3 ) then
        write(iunit,'(f14.4,a)') x0, ': Zero derivative !!!!'
        call bisection(x0,x1,iunit)
        return
      end if
      xp1 = x - (fx/fp)
      dist = abs(x-xp1)
      if ( dist < eps .or. abs(func(xp1)) < epsilon(1.0d0) ) then
        exit
      end if
      x = xp1
    end do
    if ( ic >= maxcycle ) then
      ! Try secant method
      write(iunit,'(f14.4,a)') x0, ': Convergence not reached!!!!'
      call bisection(x0,x1,iunit)
      return
    end if
    write(iunit,'(i4,f14.4,f14.8)') ic, x0 , xp1
  end subroutine new_bisec

  pure double precision function func(x)
    implicit none
    double precision , intent(in) :: x
    func = sqrt(x) * cos(x*x)
  end function func

  pure double precision function deriv(x)
    implicit none
    double precision , intent(in) :: x
    deriv = 0.5d0 * cos(x*x)/sqrt(x) - 2.0d0 * sqrt(x) * sin(x*x) * x
  end function deriv

end program assignement_eight
