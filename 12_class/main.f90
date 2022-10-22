PROGRAM assig12
  USE random
  IMPLICIT NONE
  INTEGER :: seed,n
  REAL,DIMENSION(1) :: x,y
  PRINT*,"Assign a seed and the number of iterations"
  READ*,seed,n
  CALL rejection(n,"rejection.txt",seed)
  CALL inversemethod(n,"inverse.txt",seed)
  CALL gaussian(x,y,1,1.0)
  PRINT*,x,y
END PROGRAM assig12
