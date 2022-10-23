
MODULE data
  IMPLICIT NONE
CONTAINS
   INTEGER FUNCTION init(name) RESULT(i)
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
  END FUNCTION init

  FUNCTION store(m,n,name)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: m,n
    CHARACTER(len=*),INTENT(IN) :: name
    REAL ,DIMENSION(m,n) :: store
    REAL, DIMENSION(n) :: A
    INTEGER :: j,ios,u
    ios=0
    j=1
    u=10
    OPEN(UNIT=u,IOSTAT=ios, FILE=name,STATUS='old',ACTION='read')
    DO WHILE(j<=m)
       READ(u,*,IOSTAT=ios) A
       IF (ios==0) THEN
          store(j,:)=A
          j=j+1
       ENDIF
    END DO
    CLOSE(u)
  END FUNCTION store

  SUBROUTINE sort(T)
    IMPLICIT NONE
    REAL ,DIMENSION(:), INTENT(INOUT)  :: T
    REAL, DIMENSION(:), ALLOCATABLE :: Taux
    LOGICAL ,DIMENSION(:), ALLOCATABLE :: sorting
    INTEGER :: length,i
    length=size(T)
    ALLOCATE(sorting(length))
    ALLOCATE(Taux(length))
    Taux=0
    sorting=.TRUE.
    DO i=1,length
       Taux(i)=MINVAL(T,sorting)
       sorting(MINLOC(T,sorting))=.FALSE.
    END DO
    T=Taux
    DEALLOCATE(Taux,sorting)
  END SUBROUTINE sort
    
END MODULE data
