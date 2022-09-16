PROGRAM fibonacci
  INTEGER :: x,y
  REAL :: phi_est
  x=1 !a_1
  y=1 !a_2
  PRINT *,1
  PRINT *,1
  DO i=3,50,1
     x=x+y ! a_i
     y=x+y ! a_(i+1)
     phi_est=y/(1.0*x)
     PRINT *,x
     PRINT *,y
     PRINT *,phi_est
  END DO
 END PROGRAM fibonacci
