MODULE apx
  IMPLICIT NONE
CONTAINS
   REAL FUNCTION f(x,avg) RESULT(fx)
    REAL :: x,avg
    fx=10*SIN(4*ATAN(1.0)*((x-109.5)/183.0))+12.2-avg ! DEFINE THE FUNCTION                                                                                                                                                                            
  END FUNCTION f

  RECURSIVE SUBROUTINE secant(xn,xn1,eps,n,u,avg)
    IMPLICIT NONE
    REAL, INTENT(IN) :: xn,xn1,eps
    REAL :: xn2
    INTEGER, INTENT(IN) :: u
    REAL ,INTENT(IN) :: avg
    INTEGER, INTENT(INOUT) :: n
    WRITE(u,*)n,xn1
    IF(ABS(f(xn1,avg)-f(xn,avg))<TINY(xn)) THEN
       PRINT*,'The root cannot be determined'
       STOP
    ENDIF
    xn2=xn1-f(xn1,avg)*(xn1-xn)/(f(xn1,avg)-f(xn,avg))
    IF(ABS(xn2-xn1)<eps) THEN
       RETURN
    ELSE
       n=n+1
       CALL secant(xn1,xn2,eps,n,u,avg)
    ENDIF
  END SUBROUTINE secant
END MODULE apx
