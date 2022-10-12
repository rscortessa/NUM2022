MODULE euler
  USE functions
  IMPLICIT NONE
CONTAINS
  SUBROUTINE eulerint(a,b,n,x0,name,improv)
    REAL, INTENT(IN) :: a,b,x0
    INTEGER, INTENT(IN) :: n
    LOGICAL, INTENT(IN) :: improv
    CHARACTER(len=*) name
    ! Variables needed in the code.

    REAL :: dx,x1,x2,xm,t,fexact,diff
    INTEGER :: i,ios,u
    u=10
    dx=(b-a)/(n*1.0)
    x1=x0
    x2=0
    xm=0
    t=a
    fexact=h(a)
    diff=ABS(fexact-x1)
    
    WRITE(u,*)t,x1,fexact,diff
    OPEN(UNIT=u,IOSTAT=ios,FILE=name,STATUS="replace",ACTION="write")
    IF (improv .eqv. .FALSE.) THEN
       DO i=1,n
          x2=x1+f(x1,t)*dx
          x1=x2
          t=t+dx
          fexact=h(t)
          diff=ABS(fexact-x1)
          WRITE(u,*)t,x1,fexact,diff
       END DO
    ELSE
       DO i=1,n
          xm=x1+f(x1,t)*dx/2.0
          x2=x1+f(xm,t+dx/2.0)*dx
          x1=x2
          t=t+dx
          fexact=h(t)
          diff=ABS(fexact-x1)
          WRITE(u,*)t,x1,fexact,diff
       END DO
    END IF
    CLOSE(u)
  END SUBROUTINE eulerint
END MODULE euler
