MODULE numerical
  IMPLICIT NONE
CONTAINS
  REAL FUNCTION f(x) RESULT(fx)
    REAL :: x
    fx=x ! DEFINE THE FUNCTION
  END FUNCTION f
  
  RECURSIVE SUBROUTINE bisection(a,b,eps,i,u)
    IMPLICIT NONE
    REAL, INTENT(IN) :: a,b,eps
    INTEGER, INTENT(IN) ::i,u
    INTEGER :: j
    REAL :: c
    IF (a>b .or. eps<0) THEN !INITIAL COMDITIONS
       PRINT*,'Define a good interval or precision'
       STOP
    ENDIF
    c=(a+b)/2
    PRINT*,i,c
    WRITE(u,'(i3, f7.3)')i,c
    IF(b-c<eps) THEN !TERMINATION CONDITION
       STOP
    ENDIF
    
    IF(SIGN(1.0,f(b))*SIGN(1.0,f(c))<0) THEN
       CALL bisection(c,b,eps,i+1,u)
    ELSE
       CALL bisection(a,c,eps,i+1,u)
    ENDIF
  END SUBROUTINE bisection
END MODULE numerical
     
    
    
    
    

    
