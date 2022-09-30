PROGRAM numerical
  USE newphson
  REAL :: eps
  INTEGER :: u,ios,i
  CHARACTER(12) :: is
  REAL, DIMENSION(3) :: interv
  interv=(/-3.0,0.25,1.1/)
  eps=1.D-7
  DO i=1,3
     u=10*i
     is=TRIM(is)
     Write(is,*)i
     PRINT*,is
     OPEN(UNIT=u,IOSTAT=ios,FILE='Root'//TRIM(is)//'.txt',STATUS='new',ACTION='write')
     IF(ios==0) THEN
        CALL raphson(interv(i),eps,0,u)
     ELSE
        PRINT*,'Delete your textfiles'
     ENDIF
     CLOSE(u)
  END DO
END PROGRAM numerical

