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
    INTEGER,INTENT(IN) :: n,u,j
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
    sum=simint(a,b,2*n,u,eps,antsum,converg,j+1)
    ENDIF
    ENDIF
    
  END FUNCTION simint

END MODULE simpleint


  
    
  
