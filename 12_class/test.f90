MODULE random
  IMPLICIT NONE
CONTAINS
  SUBROUTINE initialize(o)
    INTEGER, INTENT(IN) :: o
    INTEGER :: n,i
    INTEGER,ALLOCATABLE :: seed(:)
    REAL :: r
    CALL random_seed(size=n)
    ALLOCATE(seed(n))
    seed = 987654321
    ! putting
    ! arbitrary seed to all elements
    CALL random_seed(put=seed)
    DEALLOCATE(seed)

    IF (o==1) THEN
      DO i=1,3
         CALL RANDOM_NUMBER(r)
         PRINT*,r
      END DO
    ENDIF
   
  END SUBROUTINE initialize

END MODULE random

PROGRAM test
  USE random
  INTEGER :: o
  o=1
  CALL initialize(o)
  o=0
  PRINT*,"Now outside"
  CALL initialize(o)
  DO i=1,3
     CALL RANDOM_NUMBER(r)
     PRINT*,r
  END DO
END PROGRAM test
