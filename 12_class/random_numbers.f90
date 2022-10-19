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
    REAL, INTENT(IN) :: x
    y=x**(1/3.0)
  END FUNCTION finv
  
  REAL FUNCTION f(x) RESULT(y)
    REAL, INTENT(IN) :: x
    y=3*x**2
  END FUNCTION f

  
  SUBROUTINE rejection(n,filename,seed)  
    INTEGER, INTENT(IN) :: n
    INTEGER, INTENT(IN) :: seed
    REAL :: fmax,x,r,s
    INTEGER :: i,ios,u
    CHARACTER(len=*) filename
    CALL initialize(seed)
    fmax=3.0 !choose your max
    i=0
    u=10
    OPEN(UNIT=u,IOSTAT=ios,FILE=filename,STATUS='replace',ACTION='write')
    DO WHILE(i<n)
       CALL RANDOM_NUMBER(x)
       CALL RANDOM_NUMBER(r)
       s=f(x)/fmax
       IF (s>=r) THEN
          i=i+1
          WRITE(u,*)x
       ENDIF      
    END DO
    CLOSE(u)
  END SUBROUTINE rejection

  SUBROUTINE inversemethod(n,filename,seed)  
    INTEGER, INTENT(IN) :: n
    INTEGER, INTENT(IN) :: seed
    REAL :: fmax,x,r
    INTEGER :: i,ios,u
    CHARACTER(len=*) filename
    CALL initialize(seed)
    fmax=3.0 !choose your max
    u=10
    OPEN(UNIT=u,IOSTAT=ios,FILE=filename,STATUS='replace',ACTION='write')
    DO i=1,n
       CALL RANDOM_NUMBER(x)
       WRITE(u,*)f(x)
    END DO
    CLOSE(u)
  END SUBROUTINE inversemethod


  
END MODULE random



