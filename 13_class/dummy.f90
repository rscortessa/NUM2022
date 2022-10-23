PROGRAM dummy
  USE data
  IMPLICIT NONE
  REAL, DIMENSION(3) :: A
  REAL :: min,max
  A=(/3,5,1/)
  CALL calculate(A,min,max,.FALSE.)
 
  
END PROGRAM dummy
