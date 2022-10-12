MODULE newphson
  IMPLICIT NONE
CONTAINS
   REAL FUNCTION f(x,n) RESULT(fx)
     REAL :: x
     INTEGER :: n
     IF (n==0) THEN
        fx=2*EXP(x)-2*x**3-3 ! DEFINE THE FUNCTION
     ELSE
        IF(n==1) THEN
           fx=2*EXP(x)-6*x**2
        ELSE
           fx=2*EXP(x)-12*x
        ENDIF
     ENDIF

     
  END FUNCTION f

  RECURSIVE SUBROUTINE raphson(x,eps,n,u,m)
    IMPLICIT NONE
    REAL, INTENT(IN) :: x,eps
    REAL :: yp,x1
    INTEGER, INTENT(IN) :: n,u,m
    WRITE(u,*)n,x
    yp=f(x,m+1)
    IF(ABS(yp)<TINY(x)) THEN
       PRINT*,yp,'The root cannot be determined'
       STOP
    ENDIF
    x1=x-f(x,m)/yp
    IF(ABS(x1-x)<eps) THEN
       return
    ELSE
       CALL raphson(x1,eps,n+1,u,m)
    ENDIF
    END SUBROUTINE raphson
END MODULE newphson
    


  
