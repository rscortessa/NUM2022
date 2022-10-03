MODULE secantmethod

  IMPLICIT NONE
CONTAINS
   REAL FUNCTION f(x) RESULT(fx)
    REAL :: x
    fx=2*EXP(x)-2*x**2-4 ! DEFINE THE FUNCTION                                                                                       
  END FUNCTION f

  RECURSIVE SUBROUTINE secant(xn,xn1,eps,n,u)
    IMPLICIT NONE
    REAL, INTENT(INOUT) :: xn,xn1,eps
    REAL :: xn2
    INTEGER, INTENT(IN) :: n,u
    WRITE(u,*)n,xn1
    IF(ABS(f(xn1)-f(xn))<TINY(xn)) THEN
       PRINT*,'The root cannot be determined'
       STOP
    ENDIF
    xn2=xn1-f(xn1)*(xn1-xn)/(f(xn1)-f(xn))
    IF(ABS(xn2-xn1)<eps) THEN
       RETURN
    ELSE
       CALL secant(xn1,xn2,eps,n+1,u)
    ENDIF
  END SUBROUTINE secant

    
  RECURSIVE SUBROUTINE bisection(a,b,eps,i,u,m) 
    IMPLICIT NONE
    REAL, INTENT(IN) :: a,b,eps,m
    INTEGER, INTENT(IN) ::i,u
    INTEGER :: j
    REAL :: c
    IF (a>b .or. eps<0) THEN !INITIAL COMDITIONS
       PRINT*,'Define a good interval or precision'
       STOP
    ENDIF
    c=((a+b)/2)*m+(1.0-m)*(f(b)*a-f(a)*b)/(f(b)-f(a))
    WRITE(u,*) i,c
    
    IF(ABS(b-c)<eps) THEN !TERMINATION CONDITION
       RETURN
    ENDIF
    
    IF(SIGN(1.0,f(b))*SIGN(1.0,f(c))<0) THEN
       CALL bisection(c,b,eps,i+1,u,m)
    ELSE
       CALL bisection(a,c,eps,i+1,u,m)
    ENDIF
  END SUBROUTINE bisection

END MODULE secantmethod
    


  
