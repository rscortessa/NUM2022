!
! Reference implementation for exercise seven Numerical Method 1 (2021 course)
!
! Root finding exercise
!
! Find a real root of f(x) = x**2 - 2 with precision eps=1.0e-8
! 1) Write a program that includes a Subroutine “Newton” and two
!    functions f(x) and f’(x)
! 2) The program should be able to read a number of guesses from a
!    file “guesses.txt” where in the first row it is written the number
!    of guesses to read from the file. The guesses have to be assigned
!    to an allocatable array.
! 3) For each guess in “guesses.txt”, the program have to write, in a
!    new file “YourFirstName_Newton.txt”, the initial guess, the root
!    and the number of iterations needed to find that particular root.
!
! Author : Graziano Giuliani
! Date   : 03/02/2021
!
program assignement_seven
  implicit none
  double precision , parameter :: eps = 1.0e-8
  character(len=*) , parameter :: datafile = 'guesses.txt'
  character(len=*) , parameter :: outfile = 'giuliani_newton.txt'
  double precision , dimension(:) , allocatable :: x0
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

  do iguess = 1 , numguesses
    read(iunit,fmt=*,iostat=ierr) x0(iguess)
    if ( ierr /= 0 ) then
      write(0,*) 'Error reading from file ',datafile,' at line ',iguess
      stop 'I/O errorr'
    end if
  end do
  close(iunit)

  open(newunit=iunit,file=outfile,status='replace',form='formatted', &
          iostat=ierr, action='write')
  if ( ierr /= 0 ) then
    write(0,*) 'Error opening file ',outfile
    stop 'I/O erorr'
  end if
  write(iunit,'(a)') '# firstguess iteration found_root'

  do iguess = 1 , numguesses
    call newton(x0(iguess))
  end do
  close(iunit)

  contains

  subroutine newton(xx)
    implicit none
    double precision , intent(in) :: xx
    double precision :: x , xp1
    double precision :: fx , fp
    integer , parameter :: maxcycle = 100
    integer :: ic
    x = xx
    do ic = 1 , maxcycle
      fx = func(x)
      fp = deriv(x)
      xp1 = x - (fx/fp)
      if ( abs(x-xp1) < eps .or. abs(func(xp1)) < epsilon(1.0d0) ) then
        exit
      end if
      x = xp1
    end do
    if ( ic >= maxcycle ) then
      write(iunit,'(f14.4,a)') xx, ': Convergence not reached!!!!'
      return
    end if
    write(iunit,'(f14.4,i4,f14.8)') xx , ic , xp1
  end subroutine newton

  pure double precision function func(x)
    implicit none
    double precision , intent(in) :: x
    func = x * x - 2.0d0
  end function func

  pure double precision function deriv(x)
    implicit none
    double precision , intent(in) :: x
    deriv = 2.0d0 * x
  end function deriv

end program assignement_seven
