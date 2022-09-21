MODULE ORDERING
  IMPLICIT NONE
CONTAINS
  SUBROUTINE initialize_vector(cool_vec,m)
    INTEGER, INTENT(INOUT) :: m
    REAL, DIMENSION(m) , INTENT(INOUT) :: cool_vec
    CALL RANDOM_NUMBER(cool_vec)
  END SUBROUTINE initialize_vector
END MODULE ORDERING
   
    
  
