MODULE random
  USE data
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

  FUNCTION av(T)
    REAL, DIMENSION(:), INTENT(IN) :: T
    REAL, DIMENSION(2):: av
    INTEGER :: i,len
    av(1)=0
    av(2)=0
    len=size(T)
    DO i=1,len
       av(1)=av(1)+T(i)
    END DO
    av(1)=av(1)/(len*1.0)
    DO i=1,len
       av(2)=av(2)+(T(i)-av(1))**2
    END DO
    av(2)=SQRT(av(2)/(len*1.0))
  END FUNCTION av
    
  REAL FUNCTION finv(x) RESULT(y)
    REAL, INTENT(IN) :: x
    y=x**(1/3.0)
  END FUNCTION finv

  REAL FUNCTION N(x,s,u) RESULT(y)
    REAL, INTENT(IN) :: x,s,u
    y=1/(SQRT(8.*ATAN(1.D0))*s)*EXP(-(x-u)**2/(2.0*s**2))
  END FUNCTION N

  REAL FUNCTION f(x) RESULT(y)
    REAL, INTENT(IN) :: x
    y= 16.0/395.0*(15*x**2*N(x,0.25,-0.5)+13*N(x,0.3,-1.5)+7*N(x,1.,3.))
  END FUNCTION f

  
  SUBROUTINE rejection(n,filename,seed,a,b)  
    INTEGER, INTENT(IN) :: n,seed
    REAL, INTENT(IN) :: a,b
    REAL :: fmax,x,r,s
    INTEGER :: i,ios,u
    CHARACTER(len=*) filename
    CALL initialize(seed)
    fmax=17.5 !choose your max
    i=0
    u=10
    OPEN(UNIT=u,IOSTAT=ios,FILE=filename,STATUS='replace',ACTION='write')
    DO WHILE(i<n)
       CALL RANDOM_NUMBER(x)
       x=a+(b-a)*x
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

  !CUMULATIVE EMPIRICAL DISTRIBUTION FUNCTION
  SUBROUTINE CEDF(T,dens)
    REAL, DIMENSION (:), INTENT(INOUT) :: T
    REAL, DIMENSION(:,:), INTENT(INOUT) :: dens
    INTEGER :: j,M
    M=size(T)
    dens(:,1)=T
    DO j=1,M
       dens(j,2)=j
    END DO
    dens(:,2)=dens(:,2)/(M*1.0)
  END SUBROUTINE CEDF

  INTEGER FUNCTION diaconis(T) RESULT(N)
    REAL, DIMENSION (:), INTENT(INOUT) :: T
    REAL :: dx,min,max
    INTEGER :: M
    M=size(T)
    min=T(1)
    max=T(M)
    !COMPUTE THE NUMBER OF BINS AND ITS DISTANCE
    dx=(T(M*3/4)-T(M/4))*2/(M*1.0)**(1/3.0)
    N=FLOOR((max-min)/(dx))+1
  END FUNCTION diaconis
  
  
  SUBROUTINE histogram(T,l,u)
    REAL, DIMENSION (:), INTENT(IN) :: T
    INTEGER, INTENT(IN) ::l,u
    REAL, DIMENSION(:,:), ALLOCATABLE :: hist
    REAL :: dx,min,max,x
    INTEGER :: M,N,j,i
    !INITIALIZE VARIABLES
    ALLOCATE(hist(l,2))
    max=MAXVAL(T)
    min=MINVAL(T)
    N=l
    M=size(T)
    dx=(max-min)/(N*1.0)
    !CREATE THE HISTOGRAM
    hist=0
    x=min+dx
    i=1
    j=1
    hist(1,1)=x-0.5*dx
    DO WHILE(j<=M)
       IF(x>T(j)) THEN
          hist(i,2)=hist(i,2)+1.0
          j=j+1
       ELSE
          i=i+1
          x=x+dx
          hist(i,1)=x-0.5*dx
       ENDIF
    END DO
    hist(:,2)=hist(:,2)/(M*dx)
    DEALLOCATE(hist)
    DO i=1,N
       WRITE(u,*) hist(i,:)
    END DO
  END SUBROUTINE histogram
  

  REAL FUNCTION kernel(x,s,points) RESULT(px)
    REAL, INTENT(IN) :: x,s
    REAL, DIMENSION(:), INTENT(IN) :: points
    INTEGER :: len,i
    px=0
    len=size(points)
    DO i=1,len
       px=px+N(x,s,points(i))
    END DO
    px=px/(len*1.0)
  END FUNCTION kernel

  SUBROUTINE KDE(filename,T,N,a,b)
    REAL, DIMENSION(:), INTENT(INOUT) :: T
    REAL, INTENT(IN) :: a,b
    INTEGER, INTENT(IN) :: N
    INTEGER :: u,ios,len,i
    CHARACTER(len=*) filename
    REAL, DIMENSION(2) :: sigma
    REAL :: x,dx,fx
    !INITIALIZE VALUES
    u=10
    len=size(T)
    sigma=av(T)
    sigma(1)=(T(3*len/4)-T(len/4))/1.34
    IF (sigma(1) > sigma(2)) THEN
       sigma(1)=sigma(2)
    ENDIF
    dx=(b-a)/(N*1.0)
    !CREATE THE LOOP
    OPEN(UNIT=u,IOSTAT=ios,FILE=filename,STATUS='replace',ACTION='write')
    DO i=0,N-1
       x=a+i*dx
       fx=kernel(x,sigma(1),T)
       WRITE(u,*)x,fx
    END DO
    CLOSE(u)
  END SUBROUTINE KDE
  
  
END MODULE random



