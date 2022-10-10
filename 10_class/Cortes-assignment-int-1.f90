PROGRAM numint1
  USE simpleint
  REAL ::a,b,eps,sum
  INTEGER :: n,ios,method
  INTEGER, DIMENSION(3) :: u
  CHARACTER(50),DIMENSION(3) :: name
  name(1)='simple.txt'
  name(2)='middle point'
  name(3)='trapezoid rule'
  PRINT*,'Choose precision an interval and a number of steps (a,b,n,eps)'
  READ*,a,b,n,eps
  u=(/10,20,30/)
  sum=0
  method=0
  
  OPEN(UNIT=u,IOSTAT=ios,FILE='simplenumint.txt',STATUS='replace',ACTION='write')
  sum=integration(a,b,n,u,eps,sum,method)  
  CLOSE(u)
END PROGRAM numint1


