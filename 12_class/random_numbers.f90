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
    REAL :: x
    INTEGER :: i,ios,u
    CHARACTER(len=*) filename
    CALL initialize(seed)
    u=10
    OPEN(UNIT=u,IOSTAT=ios,FILE=filename,STATUS='replace',ACTION='write')
    DO i=1,n
       CALL RANDOM_NUMBER(x)
       WRITE(u,*)f(x)
    END DO
    CLOSE(u)
  END SUBROUTINE inversemethod

  
  SUBROUTINE gaussian(u,v,seed,sigma)  
    REAL, DIMENSION(:), INTENT(INOUT) :: u,v
    INTEGER, INTENT(IN) :: seed
    REAL, INTENT(IN) :: sigma
    REAL :: x,y
    INTEGER :: i,ios,n
    CALL initialize(seed)
    n=size(u)
    DO i=1,n
       CALL RANDOM_NUMBER(x)
       CALL RANDOM_NUMBER(y)
       x=SQRT(-2.0*sigma**2*LOG(1.0-x))
       y=8*ATAN(1.d0)
       u(i)=x*SIN(y)
       v(i)=x*COS(y)
    END DO
  END SUBROUTINE gaussian
  
END MODULE random



