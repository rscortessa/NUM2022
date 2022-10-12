PROGRAM numint1
  USE simpleint
  REAL ::a,b,eps,sum
  INTEGER :: n,ios,method,i
  INTEGER, DIMENSION(4) :: u
  CHARACTER(50),DIMENSION(4) :: name
  name(1)='simple.txt'
  name(2)='middle.txt'
  name(3)='trapezoid.txt'
  name(4)="simpson.txt"
  PRINT*,'Choose precision an interval and a number of steps (a,b,n,eps)'
  READ*,a,b,n,eps
  u=(/10,20,30,40/)
  sum=0
  method=0
  DO i=1,4
  method=i-1
  OPEN(UNIT=u(i),IOSTAT=ios,FILE=name(i),STATUS='replace',ACTION='write')
  sum=integration(a,b,n,u(i),eps,sum,method)  
  CLOSE(u(i))
  END DO


END PROGRAM numint1


