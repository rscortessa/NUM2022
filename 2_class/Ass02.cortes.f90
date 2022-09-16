PROGRAM pi
  IMPLICIT NONE
  INTEGER :: m,j
  REAL :: sum,eps,pis
  WRITE(6,'(a)',advance="no") "Assign a first value = "
  READ*,m
  eps=1.0E-3
  sum=0
  pis=4.0*atan(1.0)
  DO j=0,m,1
     sum=sum+4*(-1)**j/(2.0*j+1)
  END DO
  DO
     IF (ABS(sum-pis)<eps.OR.m >100) THEN
        PRINT*,"The final value is",sum," Precision",ABS(sum-pis)
        EXIT
     ELSE
        m=m+1
        sum=sum+4*(-1)**m/(2.0*m+1)
     END IF
  END DO
  END PROGRAM pi
 
