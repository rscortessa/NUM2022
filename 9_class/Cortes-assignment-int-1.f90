
MODULE simpleint
CONTAINS
  REAL FUNCTION f(x) RESULT(fx)
    REAL :: x
    fx=EXP(x) !Define the function

  END FUNCTION f

  RECURSIVE FUNCTION simint(a,b,n,u,eps,antsum,converg,j) RESULT(sum)
    IMPLICIT NONE
    REAL,INTENT(INOUT) :: a,b,eps,converg
    REAL,INTENT(INOUT) ::antsum
    INTEGER,INTENT(INOUT) :: n,u
    INTEGER,INTENT(IN) :: j
    REAL :: sum
    INTEGER :: i
    REAL :: dx
    dx=(b-a)/(1.0*n)
    sum=0
    DO i=1,n
       sum=sum+dx*f(a+dx*(i))
    END DO
    IF(ABS(sum-antsum)<eps) THEN
       WRITE(u,*)sum,n,ABS(sum-EXP(b)+EXP(a))
       RETURN
    ELSE
       IF(ABS(converg)-ABS(sum-antsum)<0 .and. j/=0) THEN
          PRINT*,"The maximum convergence has already been reached"
          sum=antsum
          RETURN
       ELSE
    WRITE(u,*)sum,n,ABS(sum-EXP(b)+EXP(a))
    converg=sum-antsum    
    antsum=sum
    n=2*n
    sum=simint(a,b,n,u,eps,antsum,converg,j+1)
    ENDIF
    ENDIF
    
  END FUNCTION simint

END MODULE simpleint



PROGRAM numint1
  USE simpleint
  REAL ::a,b,eps,sum,c
  INTEGER :: n,u,ios
  PRINT*,'Choose precision an interval and a number of steps (a,b,n,eps)'
  READ*,a,b,n,eps
  u=10
  sum=0
  c=0
  OPEN(UNIT=u,IOSTAT=ios,FILE='simplenumint.txt',STATUS='replace',ACTION='write')
  sum=simint(a,b,n,u,eps,sum,c,0)
  PRINT*,"Value=",sum," n=",n," error=",ABS(c)
  CLOSE(u)
END PROGRAM numint1


