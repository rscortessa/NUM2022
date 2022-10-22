
PROGRAM assig13
  USE random
  IMPLICIT NONE
  INTEGER :: m,seed
  REAL :: a,b
  m=5000
  seed=1
  a=-10.0
  b=10.0
  CALL rejection(m,"data.dat",seed,a,b)
  
END PROGRAM assig13
