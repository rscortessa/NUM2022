PROGRAM numerical
  USE newphson
  REAL :: eps
  INTEGER :: u,ios,i
  CHARACTER(12) :: is
  REAL, DIMENSION(3) :: interv
  interv=(/-1.0,0.5,3.2/)
  eps=1.D-7
  is="123"
  u=0
  OPEN(UNIT=u,IOSTAT=ios,FILE='root.txt',STATUS='new',ACTION='write')
   IF(ios==0) THEN
        CALL raphson(-5.0,eps,0,u,0)
     ELSE
        PRINT*,'Delete your textfiles'
     ENDIF
     CLOSE(u)

     eps=1.D-6
  DO i=1,3
     u=10*i
     OPEN(UNIT=u,IOSTAT=ios,FILE='Singularpoint'//is(i:i)//'.txt',STATUS='new',ACTION='write')
     IF(ios==0) THEN
        CALL raphson(interv(i),eps,0,u,1)
     ELSE
        PRINT*,'Delete your textfiles'
     ENDIF
     CLOSE(u)
  END DO
END PROGRAM numerical
