MODULE STATISTICS
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
              IF(check(1:1)/='#') THEN
                 i=i+1
              END IF
           ELSE
              EXIT
           ENDIF
        END DO
    CLOSE(u)
  END FUNCTION initialize


  SUBROUTINE store(T,W)
    IMPLICIT NONE
    REAL ,DIMENSION(:), INTENT(INOUT)  :: T
    REAL ,DIMENSION(:), INTENT(INOUT)  :: W
    REAL :: A,B
    INTEGER :: i,j,ios,u

    i = size(T)
    ios=0
    j=1
    u=10

    OPEN(UNIT=u,IOSTAT=ios, FILE='numerical.dat',STATUS='old',ACTION='read')
    DO WHILE(j<=i)
       READ(u,'(T6,f7.5,T14,f9.3)',IOSTAT=ios) A,B
       IF (ios==0) THEN
          W(j)=A
          T(j)=B
          j=j+1
       ENDIF
    END DO
    CLOSE(u)
  END SUBROUTINE store

  SUBROUTINE average(T,W,Ta,Wa)
    IMPLICIT NONE
    REAL ,DIMENSION(:), INTENT(IN)  :: T,W
    REAL ,INTENT(OUT) :: Ta, Wa
    INTEGER :: i, j
    real(8) :: sumT, sumW
    sumT = 0.0d0
    sumW = 0.0d0
    i = size(T)
    DO j = 1, i
       sumT = sumT+T(j)
       sumW = sumW+W(j)
    END DO
    Ta = real(sumT/dble(i))
    Wa = real(sumW/dble(i))
  END SUBROUTINE average

END MODULE STATISTICS
