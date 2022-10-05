PROGRAM numint1
  USE simpleint
  REAL ::a,b,eps,sum
  INTEGER :: n,u,ios
  PRINT*,'Choose precision an interval and a number of steps (a,b,n,eps)'
  READ*,a,b,n,eps
  u=10
  sum=0
  OPEN(UNIT=u,IOSTAT=ios,FILE='simplenumint.txt',STATUS='replace',ACTION='write')

  sum=simint(a,b,n,u,eps,sum)
  CLOSE(u)
END PROGRAM numint1


