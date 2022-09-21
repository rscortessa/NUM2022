

MODULE ORDERING
  IMPLICIT NONE
CONTAINS
  SUBROUTINE initialize_vector(cool_vec,m)
    INTEGER, INTENT(INOUT) :: m
    REAL, DIMENSION(m) , INTENT(INOUT) :: cool_vec
    CALL RANDOM_NUMBER(cool_vec)
  END SUBROUTINE initialize_vector

  SUBROUTINE up_down(cool_vec,m)
    INTEGER, INTENT(INOUT) :: m
    INTEGER :: i,j
    REAL, DIMENSION(m), INTENT(INOUT) :: cool_vec
    REAL(8) :: aux1
    DO i=1,m
       aux1=cool_vec(i)
       DO j=i+1,m
          IF (aux1<cool_vec(j)) THEN
             cool_vec(i)=cool_vec(j)
             cool_vec(j)=aux1
             aux1=cool_vec(i)
          END IF
       END DO
    END DO
  END SUBROUTINE up_down
  
  SUBROUTINE down_up(cool_vec,m)
    INTEGER, INTENT(INOUT) :: m
    INTEGER :: i,j
    REAL :: aux1
    REAL, DIMENSION(m), INTENT(INOUT) :: cool_vec
    DO i=1,m
       aux1=cool_vec(i)
       DO j=i+1,m
          IF (aux1>cool_vec(j)) THEN
             cool_vec(i)=cool_vec(j)
             cool_vec(j)=aux1
             aux1=cool_vec(i)
          END IF
       END DO
    END DO
    END SUBROUTINE down_up
END MODULE ORDERING


    
  
