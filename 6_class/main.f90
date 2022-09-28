PROGRAM ass6
  USE numerical
  REAL :: a,b,eps,root
  INTEGER :: u,v,ios
  a=-1
  b=1
  eps=1.D-7
  u=10
  v=20
  
  OPEN(u,IOSTAT=ios,FILE='bisection.txt',STATUS='new',ACTION='write')
  call bisection(a,b,eps,0,u)
  CLOSE(u)
END PROGRAM ass6
  
