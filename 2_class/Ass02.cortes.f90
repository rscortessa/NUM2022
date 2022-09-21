PROGRAM pi
  IMPLICIT NONE
  INTEGER :: m,j
  REAL :: sum,eps,pis
  DO
     WRITE(6,'(a)',advance="no") "Assign a first value = "
     READ*,m
     IF (m<=1) THEN
        PRINT*, "Insert a value of m greater than 1"
     ELSE
        EXIT
     END IF
  END DO
  eps=1.0E-7
  sum=0
  pis=4.0*atan(1.0)
  DO j=0,m,1
     sum=sum+4*(-1)**j/(2.0*j+1)
  END DO
  DO
     IF (ABS(sum-pis)<eps) THEN
        PRINT*,"The final value is",sum," m=",m," Precision",ABS(sum-pis)
        EXIT
     ELSE
        m=m+1
        sum=sum+4*(-1)**m/(2.0*m+1)
     END IF
  END DO
  END PROGRAM pi
 
