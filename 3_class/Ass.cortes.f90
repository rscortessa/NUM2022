INTEGER FUNCTION checkin(x,y) RESULT(isit)
  REAL, INTENT(IN) :: x,y
  REAL             ::z
  z=x**2+y**2
  IF (z<=1) THEN
     isit=1
  ELSE
     isit=0
  END IF
END FUNCTION checkin

PROGRAM pi_value
  IMPLICIT NONE
  REAL     :: x,y,pi_exp,pi_teo,eps
  INTEGER  ::i,j,sum
  INTEGER, EXTERNAL  ::checkin
  sum=0
  j=0
  i=0
  DO
     i=i+1
     CALL RANDOM_NUMBER(x)
     CALL RANDOM_NUMBER(y)
     j=checkin(x,y)
     sum=sum+j
     pi_exp=4.0*(sum*1.0)/(i*1.0)
     pi_teo=3.1415 !The value to reach is set
     eps=pi_exp-pi_teo
     PRINT*,"Fraction",pi_exp," Error",eps  ," P. inside",sum," P. total",i
     IF (ABS(eps)<=9.99D-5 .and. eps>0) THEN !We are approaching from the top, in order to get the four digit
        PRINT*, "Runs",i
        EXIT
     END IF
     IF (i>1000000) THEN
        PRINT*, "There is still not convergence"
        EXIT
     END IF
  END DO
END PROGRAM pi_value
     
        


        
