MODULE newphson
  IMPLICIT NONE
CONTAINS
   REAL FUNCTION f(x) RESULT(fx)
    REAL :: x
    fx=2*EXP(x)-2*x**3-3 ! DEFINE THE FUNCTION                                                                                       
  END FUNCTION f

  REAL FUNCTION fp(x) RESULT(fpx)
    REAL :: x
    fpx=2*EXP(x)-6*x**2 ! DEFINE THE FUNCTION                                                                                      
  END FUNCTION fp

  RECURSIVE SUBROUTINE raphson(x,eps,n,u)
    IMPLICIT NONE
    REAL, INTENT(IN) :: x,eps
    REAL :: yp,x1
    INTEGER, INTENT(IN) :: n,u
    WRITE(u,*)n,x
    yp=fp(x)
    IF(ABS(yp)<TINY(x)) THEN
       PRINT*,yp,'The root cannot be determined'
       STOP
    ENDIF
    x1=x-f(x)/yp
    IF(ABS(x1-x)<eps) THEN
       STOP
    ELSE
       CALL raphson(x1,eps,n+1,u)
    ENDIF
    END SUBROUTINE raphson
END MODULE newphson
    


  
