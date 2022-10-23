PROGRAM assig13
  USE random
  IMPLICIT NONE
  INTEGER :: l,m,seed,u,v,c,i,ios
  REAL :: a,b,dx

  REAL, DIMENSION(:,:), ALLOCATABLE :: hist
  REAL, DIMENSION(5000) :: de
  REAL, DIMENSION(5000,2) :: density
  REAL, DIMENSION(5000,1) :: d
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
  ALLOCATE(hist(l,2))
  CALL histogram(de,hist)
  OPEN(UNIT=u,IOSTAT=ios,FILE="density.dat",STATUS='replace',ACTION='write')
  OPEN(UNIT=v,IOSTAT=ios,FILE="histogram.dat",STATUS='replace',ACTION='write')
  OPEN(UNIT=c,IOSTAT=ios,FILE="real.dat",STATUS='replace',ACTION='write')
  DO i=1,m
     WRITE(u,*)density(i,:)
     WRITE(c,*)a+i*dx,f(a+i*dx)
     IF(i<=l) THEN
        PRINT*,"GOOD2"
        WRITE(v,*)hist(i,:)
     ENDIF
  END DO
  CLOSE(v)
  CLOSE(u)
  CLOSE(c)
  CALL KDE("kernel.dat",de,100000,-10.,10.)
  DEALLOCATE(hist)
  
END PROGRAM assig13
