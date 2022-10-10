MODULE simpleint
CONTAINS
  REAL FUNCTION f(x) RESULT(fx)
    REAL :: x
    fx=EXP(x) !Define the function

  END FUNCTION f

  RECURSIVE SUBROUTINE simple(n,dx,a,int,op)
    IMPLICIT NONE
    REAL, INTENT(INOUT) :: dx,a,int
    INTEGER,INTENT(INOUT) :: n
    INTEGER :: op
    IF(op==3) THEN
       int=(f(a)+f(a+n*dx))*dx/2.0
       op=0
       n=n-2
       a=a+dx
       Call simple(n,dx,a,int,op)
    ENDIF
    IF(n==0) THEN
       RETURN
    ELSE
       n=n-1
       int=int+f(a+dx/2.0*op)*dx !op=0 -> simple integration, op=1 middle point
       a=a+dx
       CALL simple(n,dx,a,int,op)
    ENDIF
  END SUBROUTINE simple


  
  
  RECURSIVE FUNCTION integration(a,b,n,u,eps,antsum,method) RESULT(sum)
    IMPLICIT NONE
    REAL,INTENT(IN) :: a,b,eps
    REAL,INTENT(INOUT) ::antsum
    INTEGER,INTENT(IN) :: n,u
    INTEGER,INTENT(INOUT) :: method
    REAL :: sum
    INTEGER :: m
    REAL :: dx,c
    method=0
    dx=(b-a)/(1.0*n)
    c=a
    m=n
    sum=0
    CALL simple(m,dx,c,sum,method)
    WRITE(u,*)sum,n,sum-EXP(1.0)+1.0000
    IF(ABS(sum-antsum)<eps) THEN
       RETURN
    ELSE
    antsum=sum
    sum=integration(a,b,2*n,u,eps,antsum,method)
    ENDIF

    
  END FUNCTION integration

END MODULE simpleint


  
    
  
