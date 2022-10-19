PROGRAM assig12
  USE random
  IMPLICIT NONE
  INTEGER :: seed,n
  PRINT*,"Assign a seed and the number of iterations"
  READ*,seed,n
  CALL rejection(n,"rejection.txt",seed)
  CALL inversemethod(n,"inverse.txt",seed)
END PROGRAM assig12
