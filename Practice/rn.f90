PROGRAM random
  USE randomvar
  REAL, DIMENSION(:), ALLOCATABLE :: a
  INTEGER :: n,rep,steps,u,ios
  steps=2
  rep=10
  u=10
  ALLOCATE(a(2*steps+1))
  DO i=1,2*steps+1
     a(i)=0
  END DO

  CALL randomwalk(steps,rep,a)
  OPEN(UNIT=u, IOSTAT=ios,FILE="central.txt",STATUS="replace",ACTION="write")
  CALL printer(a,u,steps)
  CLOSE(u)
END PROGRAM random
