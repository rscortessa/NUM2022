program linreg
  implicit none
  integer , parameter :: nregion = 10
  integer , parameter :: nyears = 100
  integer , parameter :: nslice = 3
  integer , parameter :: slicelen = 30
  real(8), dimension(nyears) :: x
  real(8), dimension(nslice,nregion) :: means
  real(8), dimension(nslice,nregion) :: sigmas
  character(len=6) :: namefile1
  character(len=3),dimension(nregion) :: region
  integer :: i , k
  integer :: unitnum
  integer , dimension(2,3) , parameter :: bd = &
          reshape([1,36,71,30,65,100],[2,3])
  means = -99
  sigmas = -99
  do i = 1 , nregion
    write(namefile1,'(i0.2,a)') i,".dat"
    open(newunit=unitnum,file=namefile1)
    read(unit=unitnum, fmt=*) region(i)
    print * , namefile1, ': ', region(i)
    do k = 1 , nyears
      read(unit=unitnum, fmt=*) x(k)
    end do
    close(unitnum)
    means(1,i) = compute_mean(x,1,30)
    means(2,i) = compute_mean(x,36,65)
    means(3,i) = compute_mean(x,71,100)
    sigmas(1,i) = compute_sigma(x,means(1,i),1,30)
    sigmas(2,i) = compute_sigma(x,means(2,i),36,65)
    sigmas(3,i) = compute_sigma(x,means(3,i),71,100)
  end do
  open(newunit=unitnum,file="analysis.dat")
  write(unitnum,fmt="(a,10(a,8x))") "REGIONS:    ",region
  do i = 1, nslice
    write(unit=unitnum,fmt="(a,10(f8.2,3x))") "SIGMA: ", sigmas(i,:)
    write(unit=unitnum,fmt="(a,10(f8.2,3x))") "MEAN:  ", means(i,:)
  end do
  close(unitnum)

  contains

  real(8) function compute_mean(stat,k1,k2) result(mean)
    implicit none
    real(8), dimension(:) , intent(in) :: stat
    integer, intent(in) :: k1 , k2
    integer :: i
    real(8) :: sum1
    sum1 = 0.0d0
    do i = k1 , k2
      sum1 = sum1 + stat(i)
    end do
    mean = sum1/real(k2-k1+1)
  end function

  real(8) function compute_sigma(stat,mu,k1,k2) result(sigma)
    implicit none
    real(8), dimension(:), intent(in) :: stat
    real(8), intent(in) :: mu
    integer , intent(in) :: k1 , k2
    real(8) :: sum1
    integer :: i
    sum1 = 0.0d0
    do i = k1 , k2
      sum1 = sum1 + (stat(i)-mu)**2
    end do
    sigma = sqrt(sum1/real(k2-k1))
  end function

end program linreg
