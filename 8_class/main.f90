PROGRAM numerical
  USE secantmethod 
  INTEGER, DIMENSION(3) ::ios,u,m,n
  REAL :: a,b,c,d,eps
  u=(/10,20,30/)
  ios=(/0,0,0/)
  PRINT*,'Give an estimate of the interval and the precision:'
  WRITE(6,'(a)',advance='no')"Xo="
  READ*,a
  WRITE(6,'(a)',advance='no')"X1="
  READ*,b
  WRITE(6,'(a)',advance='no')"precision="
  READ*,eps
  OPEN(UNIT=u(1),IOSTAT=ios(1),FILE='Secantmethod.txt',STATUS='new',ACTION='write')
  OPEN(UNIT=u(2),IOSTAT=ios(2),FILE='Bisection.txt',STATUS='new',ACTION='write')

  OPEN(UNIT=u(3),IOSTAT=ios(3),FILE='Iterations.txt',STATUS='new',ACTION='write')
  IF(ios(1)==0) THEN
     CALL secant(a,b,eps,0,u(1))
     PRINT*,a,b
     ELSE
        PRINT*,'Delete your textfiles'
     ENDIF
  IF(ios(2)==0) THEN
     CALL bisection(a,b,eps,0,u(2),1.0)
     PRINT*,a,b
     ELSE
        PRINT*,'Delete your textfiles'
     ENDIF
  CLOSE(u(1))
  CLOSE(u(2))

  
  CLOSE(u(3))
     
END PROGRAM numerical

