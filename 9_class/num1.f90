MODULE simpleint
CONTAINS
  REAL FUNCTION f(x) RESULT(fx)
    REAL :: x
    fx=EXP(x) !Define the function

  END FUNCTION f

  RECURSIVE FUNCTION simint(a,b,n,u,eps,antsum) RESULT(sum)
    IMPLICIT NONE
    REAL,INTENT(IN) :: a,b,eps
    REAL,INTENT(INOUT) ::antsum
    INTEGER,INTENT(IN) :: n,u
    REAL :: sum
    INTEGER :: i
    REAL :: dx
    dx=(b-a)/(1.0*n)
    sum=0
    DO i=1,n
       sum=sum+dx*f(a+dx*(i))
    END DO
    WRITE(u,*)sum,n,sum-EXP(1.0)+1.0000
    PRINT*,sum,n
    IF(ABS(sum-antsum)<eps) THEN
       RETURN
    ELSE
    antsum=sum
    sum=simint(a,b,2*n,u,eps,antsum)
    ENDIF

    
  END FUNCTION simint

END MODULE simpleint


  
    
  
