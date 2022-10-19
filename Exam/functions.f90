MODULE temperature
  IMPLICIT NONE
CONTAINS
   INTEGER FUNCTION initialize(name) RESULT(i)
    IMPLICIT NONE
    INTEGER :: ios,u
    CHARACTER(len=*),INTENT(IN) :: name
    CHARACTER(128) :: check
    PRINT*,name
    u=10
    ios=0
    i=0
    OPEN(UNIT=u,IOSTAT=ios, FILE=name,STATUS='old',ACTION='read')
    DO
       READ(u,*,IOSTAT=ios)check
           IF (ios==0) THEN
              i=i+1
           ELSE
              EXIT
           ENDIF
        END DO
    CLOSE(u)
  END FUNCTION initialize

    SUBROUTINE store(D,T,prop)
    IMPLICIT NONE
    REAL ,DIMENSION(:), INTENT(INOUT)  :: D
    REAL ,DIMENSION(:), INTENT(INOUT)  :: T
    REAL :: prop
    REAL :: A,B
    INTEGER :: i,j,ios,u

    i = size(D)
    ios=0
    j=1
    u=10
    prop=0
    OPEN(UNIT=u,IOSTAT=ios, FILE="temperature_2010_Trieste.dat",STATUS='old',ACTION='read')
    DO WHILE(j<=i)
       READ(u,*,IOSTAT=ios) A,B
       IF (ios==0) THEN
          D(j)=A
          T(j)=B
          prop=prop+T(j)
          j=j+1
       ENDIF
    END DO
    CLOSE(u)
    prop=prop/(365*1.0)
  END SUBROUTINE store

    SUBROUTINE calculate(T,prop,max,min)
    IMPLICIT NONE
    REAL ,DIMENSION(:), INTENT(INOUT)  :: T
    REAL ,INTENT(INOUT) :: prop,max,min
    INTEGER :: length,i
    length=size(T)
    max=prop
    min=prop
    DO i=0,length
       IF(T(i)>max) THEN
          max=T(i)
       ENDIF

       IF(T(i)<min) THEN
          min=T(i)
       ENDIF
    END DO
  END SUBROUTINE calculate
    
       
  
  
END MODULE temperature
