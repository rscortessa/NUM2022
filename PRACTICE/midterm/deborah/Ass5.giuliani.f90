!
! Reference implementation for exercise five Numerical Method 1 (2021 course)
!
! Write a program that reads, from the file data.dat, the monthly
! precipitation data for two different years for Boulder (Colorado,
! US) and calculates:
!
! 1) Seasonal means for Winter 2002 and 2013
! 2) Annual mean for 2013
! 3) The Annual and Seasonal means calculated in point 1 and 2
!    have to be written in a file called “YourFirstName.dat”
! 4) Take care of the right formatting writing in the output file
!    only the significative decimals
!
! Author : Graziano Giuliani
! Date   : 01/27/2021
!
program assignement_five
  implicit none
  integer :: iunit, ierr
  integer , parameter :: maxline = 256
  integer , parameter :: numline = 12
  integer , parameter :: nmonths = 12
  character (len=*), parameter :: datafile = 'data.dat'
  character (len=*), parameter :: outfile = 'giuliani.dat'
  character (len=maxline) :: fline
  integer :: il , im
  integer , dimension(12) :: idx
  character(len=3) , dimension(12) :: month
  real , dimension(12) :: prp2012
  real , dimension(12) :: prp2013
  character(len=3) , dimension(12) :: season
  real , dimension(4) :: smean2012 , smean2013

  open(newunit=iunit, file=datafile,status='old',form='formatted', &
          iostat=ierr, action='read')
  if ( ierr /= 0 ) then
    write(0,*) 'Error opening file ',datafile
    stop 'I/O erorr'
  end if

  ! skip two lines
  read(iunit,'(a)',iostat=ierr) fline
  if ( ierr /= 0 ) then
    write(0,*) 'Error redding from file ',datafile
    stop 'I/O erorr'
  end if
  read(iunit,'(a)',iostat=ierr) fline
  if ( ierr /= 0 ) then
    write(0,*) 'Error redding from file ',datafile
    stop 'I/O erorr'
  end if

  do il = 1 , numline
    read(iunit,'(i2,6x,a3,6x,f4.2,6x,f4.2,6x,a3)',iostat=ierr) &
            idx(il), month(il), prp2012(il), prp2013(il), season(il)
    if ( ierr /= 0 ) then
      write(0,*) 'Error redding data line from file ',datafile
      stop 'I/O erorr'
    end if
  end do
  close(iunit)

  do im = 1 , nmonths
    if ( season(im) == 'Win' ) then
      smean2012(1) = smean2012(1) + prp2012(im)
      smean2013(1) = smean2013(1) + prp2013(im)
    end if
    if ( season(im) == 'Spr' ) then
      smean2012(2) = smean2012(2) + prp2012(im)
      smean2013(2) = smean2013(2) + prp2013(im)
    end if
    if ( season(im) == 'Sum' ) then
      smean2012(3) = smean2012(3) + prp2012(im)
      smean2013(3) = smean2013(3) + prp2013(im)
    end if
    if ( season(im) == 'Fal' ) then
      smean2012(4) = smean2012(4) + prp2012(im)
      smean2013(4) = smean2013(4) + prp2013(im)
    end if
  end do

  smean2012 = smean2012/3.0
  smean2013 = smean2013/3.0

  open(newunit=iunit, file=outfile,status='replace',form='formatted', &
          iostat=ierr, action='write')
  if ( ierr /= 0 ) then
    write(0,*) 'Error opening file ',outfile
    stop 'I/O erorr'
  end if

  write(iunit,'(a)') '# Statistics for Boulder Colorado'
  write(iunit,'(a1,6a6)') '#', [ 'year',' Win',' Spr',' Sum',' Fal','Mean' ]
  write(iunit,'(i7,4f6.2,f6.2)') 2012, smean2012, sum(prp2012)/12.0
  write(iunit,'(i7,4f6.2,f6.2)') 2013, smean2013, sum(prp2013)/12.0
  close(iunit)

  write(6,*) 'Done'

end program assignement_five
