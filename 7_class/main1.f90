PROGRAM ass6
  USE numerical
  REAL, DIMENSION(3) :: a,b
  REAL :: c,d,eps,root
  INTEGER :: u,v,ios,i
  CHARACTER (len=3) :: asas
  a=(/-0.6,0.5,3.5/)
  b=(/-0.4,1.5,4.0/)
  asas="123"
  DO i=1,3
  c=a(i)
  d=b(i)
  PRINT*,c,d
  eps=1.D-5 
  u=10
  v=20

  OPEN(u,IOSTAT=ios,FILE="bisection"//asas(i:i)//".txt",STATUS='new',ACTION='write')
  OPEN(v,IOSTAT=ios,FILE="false_point"//asas(i:i) //".txt",STATUS='new',ACTION='write')
  
  IF (ios/=0) THEN
     PRINT*,"Remove txt files"
     STOP
  ELSE
     call bisection(c,d,eps,0,u,1.0)
     c=a(i)
     d=b(i)
#call bisection(c,d,eps,0,v,0.0)
  ENDIF
  
  CLOSE(u)
  CLOSE(v)

  END DO
END PROGRAM ass6
  
