MODULE simpleint
CONTAINS
  REAL FUNCTION f(x) RESULT(fx)
    REAL :: x
    fx=EXP(x) !Define the function

  END FUNCTION f

  RECURSIVE SUBROUTINE simple(n,dx,a,int,op)
    IMPLICIT NONE
    REAL,INTENT(IN) :: dx
    REAL, INTENT(INOUT) :: a,int
    INTEGER,INTENT(INOUT) :: n
    INTEGER, INTENT(IN) :: op
    IF(n==0) THEN
       RETURN
    ELSE
       n=n-1
       int=int+f(a+dx/2.0*op)*dx !op=0 -> simple integration, op=1 middle point
       a=a+dx
       CALL simple(n,dx,a,int,op)
    ENDIF
  END SUBROUTINE simple

  
  RECURSIVE SUBROUTINE simpson(n,dx,a,int)
    IMPLICIT NONE
    REAL, INTENT(IN) :: dx,a
    REAL,INTENT(INOUT) :: int
    INTEGER,INTENT(INOUT) :: n
    INTEGER :: m,l
    REAL :: sum1,sum2,a1,a2,dx1
    int=(f(a)+f(a+n*dx))*dx/(3.0)
    dx1=2*dx
    m=(n-1)/2
    l=n-1-m
    a1=a+dx
    a2=a+2*dx
    sum1=0
    sum2=0
    PRINT*,l,m
    CALL simple(l,dx1,a1,sum1,0)
    CALL simple(m,dx1,a2,sum2,0)
    int=int+(sum2+2*sum1)/3.0
    RETURN

  END SUBROUTINE SIMPSON
  
  RECURSIVE SUBROUTINE trapezoid(n,dx,a,int)
    IMPLICIT NONE
    REAL, INTENT(IN) :: dx,a
    REAL,INTENT(INOUT) :: int
    INTEGER,INTENT(IN) :: n
    INTEGER :: m
    REAL :: a1
    int=(f(a)+f(a+n*dx))*dx/2.0
    m=n-1
    a1=a+dx
    Call simple(m,dx,a1,int,0)
  END SUBROUTINE trapezoid
  
  RECURSIVE SUBROUTINE nmethods(n,dx,a,int,op)
    IMPLICIT NONE
    REAL, INTENT(INOUT) :: dx,a,int
    INTEGER,INTENT(INOUT) :: n,op
    int=0
    IF(op==3) THEN
       CALL simpson(n,dx,a,int)
       RETURN
    ENDIF
    
    IF(op==2) THEN
       CALL trapezoid(n,dx,a,int)
       RETURN
    ENDIF
    
    CALL simple(n,dx,a,int,op)
  END SUBROUTINE nmethods


  
  
  RECURSIVE FUNCTION integration(a,b,n,u,eps,antsum,anteps,method) RESULT(sum)
    IMPLICIT NONE
    REAL,INTENT(IN) :: a,b,eps
    REAL,INTENT(INOUT) ::antsum,anteps
    INTEGER,INTENT(IN) :: n,u,method
    REAL :: sum,auxeps
    INTEGER :: m,mt
    REAL :: dx,c
    dx=(b-a)/(1.0*n)
    c=a
    m=n
    sum=0.0
    mt=method
    CALL nmethods(m,dx,c,sum,mt)
    WRITE(u,*)sum,n,sum-EXP(b)+1.0000
    auxeps=ABS(sum-antsum)
    PRINT*,anteps,auxeps
    IF(auxeps<eps .OR. anteps< auxeps ) THEN
       RETURN
    ELSE
       antsum=sum
       anteps=auxeps
       mt=method
       sum=integration(a,b,2*n,u,eps,antsum,anteps,mt)
    ENDIF    
  END FUNCTION integration

END MODULE simpleint


  
    
  
