PROGRAM assig13
  USE random
  IMPLICIT NONE
  INTEGER :: l,m,seed,u,v,c,i,ios
  REAL :: a,b,dx
  REAL, DIMENSION(5000) :: de
  REAL, DIMENSION(5000,2) :: density
  REAL, DIMENSION(5000,1) :: d
  REAL, DIMENSION(:,:), ALLOCATABLE :: hist
 
  m=5000
  u=10
  v=40
  c=70 
  seed=10
  a=-10.0
  b=10.0
  d=0
  dx=(b-a)/(m*1.0)
  CALL rejection(m,"data.dat",seed,a,b)
  d=store(m,1,"data.dat")
  de=d(:,1)
  !SORTING THE ARRAY
  
  CALL sort(de)
   !CALCULATE THE EMPIRICAL DISTRIBUTION
  CALL CEDF(de,density)
  !CALCULATE THE HISTOGRAM
  
  l=diaconis(de)
  print*,l
  OPEN(UNIT=10,IOSTAT=ios,file="dummy.txt")
  PRINT*,ios
  CALL histogram(de,hist,l,10)
  CLOSE(10)
  DEALLOCATE(hist)
   OPEN(UNIT=u,IOSTAT=ios,FILE="density.dat",STATUS='replace',ACTION='write')
!  OPEN(UNIT=c,IOSTAT=ios,FILE="real.dat",STATUS='replace',ACTION='write')
  
  DO i=1,m
   !  WRITE(u,*)density(i,:)
    ! WRITE(c,*)a+i*dx,f(a+i*dx)
  END DO
  CLOSE(u)
  !CLOSE(v)
  !CLOSE(c)
  !CALL KDE("kernel.dat",de,100000,-10.,10.)
 
  
END PROGRAM assig13
