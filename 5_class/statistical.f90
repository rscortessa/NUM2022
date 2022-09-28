MODULE STATISTICS
  IMPLICIT NONE
CONTAINS
  INTEGER FUNCTION initialize(name,a) RESULT(i)
    INTEGER :: ios,u
    INTEGER, INTENT(IN) :: a
    CHARACTER(a),INTENT(IN) :: name
    CHARACTER(100) :: check
    PRINT*,name
    u=10
    ios=0
    i=0
    OPEN(UNIT=u,IOSTAT=ios, FILE=name,STATUS='old',ACTION='read')
    DO
       READ(u,*,IOSTAT=ios)check
           IF (ios==0) THEN
              IF(check(1:1)/='#') THEN
                 i=i+1
              END IF
           ELSE
              EXIT
           ENDIF
        END DO
    CLOSE(u)
  END FUNCTION initialize

  
  SUBROUTINE store(T,W,i)

    INTEGER, INTENT(INOUT) :: i
    REAL ,DIMENSION(i), INTENT(INOUT)  :: T
    REAL ,DIMENSION(i), INTENT(INOUT)  :: W
    REAL :: A,B
    INTEGER :: j,ios,u
    
    ios=0
    j=1
    u=10
    
    OPEN(UNIT=u,IOSTAT=ios, FILE='numerical.dat',STATUS='old',ACTION='read')
    DO WHILE(j<=i)
       READ(u,'(T6f7.5,T14f9.3)',IOSTAT=ios)A,B
       IF (ios==0) THEN
          T(j)=A
          W(j)=B
          j=j+1
       ENDIF
    END DO
    CLOSE(u)
  END SUBROUTINE store

  SUBROUTINE average(T,W,i,Ta,Wa)
    INTEGER, INTENT(INOUT) :: i
    REAL ,DIMENSION(i), INTENT(INOUT)  :: T,W
    REAL ,INTENT(INOUT) :: Ta,Wa
    INTEGER ::j
    Ta=0
    Wa=0
    DO j=1,i
       Ta=Ta+T(i)
       Wa=Wa+W(i)
    END DO
    Ta=Ta/i
    Wa=Wa/i
  END SUBROUTINE average
  
END MODULE STATISTICS
