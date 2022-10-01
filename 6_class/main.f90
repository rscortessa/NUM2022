PROGRAM ass6
  USE numerical
  REAL :: a,b,c,d,eps,root
  INTEGER :: u,v,ios
  a=-2
  b=2
  c=a
  d=b
  eps=1.D-7
  u=10
  v=20
  
  OPEN(u,IOSTAT=ios,FILE='bisection.txt',STATUS='new',ACTION='write')
  OPEN(v,IOSTAT=ios,FILE='false_point.txt',STATUS='new',ACTION='write')
  
  IF (ios/=0) THEN
     PRINT*,"Remove txt files"
     STOP
  ELSE
     call bisection(c,d,eps,0,u,1.0)
     c=a
     d=b
     call bisection(c,d,eps,0,v,0.0)
  ENDIF
  
  CLOSE(u)
  CLOSE(v)
END PROGRAM ass6
  
