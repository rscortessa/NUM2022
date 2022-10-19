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
    seed = o
    ! putting
    ! arbitrary seed to all elements
    CALL random_seed(put=seed)
    DEALLOCATE(seed)   
  END SUBROUTINE initialize

  REAL FUNCTION finv(x) RESULT(y)
    y=x**(1/3)
  END FUNCTION finv
  
  REAL FUNCTION f(x) RESULT(y)
    y=x**3
  END FUNCTION f

  SUBROUTINE rejection(n,filename,seed)
    
    INTEGER, INTENT(IN) :: n
    REAL, INTENT(IN) :: seed
    REAL :: fmax
    CALL initialize(seed)
    
END MODULE random



