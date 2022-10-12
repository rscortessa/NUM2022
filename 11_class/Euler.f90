MODULE euler
  USE function
  IMPLICIT NONE
  SUBROUTINE eulerint(a,b,n,x0,name,u,improv)
    REAL, INTENT(IN) :: a,b,x0,fexact,diff
    INTEGER, INTENT(IN) :: n,u
    LOGICAL, INTENT(IN) :: improv
    CHARACTER(len=*) name
    ! Variables needed in the code.

    REAL :: h,x1,x2,xm,t
    INTEGER :: i
    h=(b-a)/(n*1.0)
    x1=x0
    x2=0
    xm=0
    t=a
    fexact=h(a)
    diff=ABS(fexact-x1)
    
    WRITE(u,*)t,x1,fexact,diff
    OPEN(UNIT=u,IOSTAT=ios,FILE=name,STATUS="replace",ACTION="write")
    IF (improv == .FALSE.) THEN
       DO i=1,n
          x2=x1+f(x1,t)*h
          x1=x2
          t=t+h
          fexact=h(t)
          diff=ABS(fexact-x1)
          WRITE(u,*)t,x1,fexact,diff
       END DO
    ELSE
       DO i=1,n
          xm=x1+f(x1,t)*h/2.0
          x2=x1+f(xm,t+h/2.0)*h
          x1=x2
          t=t+h
          fexact=h(t)
          diff=ABS(fexact-x1)
          WRITE(u,*)t,x1,fexact,diff
       END DO
    END IF
    CLOSE(u)
  END SUBROUTINE eulerint
END MODULE euler
