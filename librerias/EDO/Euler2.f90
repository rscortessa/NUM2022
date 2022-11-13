MODULE euler
  USE lotka
  IMPLICIT NONE
CONTAINS
  SUBROUTINE eulerint(a,b,n,x0,y0,name,improv)
    REAL, INTENT(IN) :: a,b,x0,y0
    INTEGER, INTENT(IN) :: n
    LOGICAL, INTENT(IN) :: improv
    CHARACTER(len=*) name
    ! Variables needed in the code.

    REAL :: dx,x1,x2,y1,y2,ym,xm,t,xexact,yexact,diffx,diffy
    INTEGER :: i,ios,u
    u=10
    dx=(b-a)/(n*1.0)
    x1=x0
    x2=0
    y1=y0
    y2=0
    ym=0
    xm=0
    t=a
    xexact=hx(a)
    yexact=hy(a)
    diffx=ABS(xexact-x1)
    diffy=ABS(yexact-y1)
    WRITE(u,*)t,x1,y1,xexact,diffx,yexact,diffy
    OPEN(UNIT=u,IOSTAT=ios,FILE=name,STATUS="replace",ACTION="write")
    IF (improv .eqv. .FALSE.) THEN
       DO i=1,n
          x2=x1+fx(x1,y1,t)*dx
          y2=y1+fy(x1,y1,t)*dx
          x1=x2
          y1=y2
          t=t+dx
          xexact=hx(t)
          yexact=hy(t)
          diffx=ABS(xexact-x1)
          diffy=ABS(yexact-y1)
          WRITE(u,*)t,x1,y1
       END DO
    ELSE
       DO i=1,n
          xm=x1+fx(x1,y1,t)*dx/2.0
          ym=y1+fy(x1,y1,t)*dx/2.0
          x2=x1+fx(xm,ym,t+dx/2.0)*dx
          y2=y1+fy(xm,ym,t+dx/2.0)*dx
          x1=x2
          y1=y2
          t=t+dx
          xexact=hx(t)
          yexact=hy(t)
          diffx=ABS(xexact-x1)
          diffy=ABS(yexact-y1)
          WRITE(u,*)t,x1,y1
       END DO
    END IF
    CLOSE(u)
  END SUBROUTINE eulerint
END MODULE euler
