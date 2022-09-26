PROGRAM READING
  REAL, DIMENSION(:), ALLOCATABLE :: T
  REAL, DIMENSION(:), ALLOCATABLE :: W
  INTEGER :: i,j,m,u,Ta,Wa,ios
  CHARACTER(100) :: check
  u=10
  i=0
  j=0
  OPEN(UNIT=u,IOSTAT=ios,FILE='numerical.dat',STATUS='old',ACTION='read')
  DO
     READ(u,'(a100)',IOSTAT=ios) check
     IF (ios==0) THEN
        i=i+1
        IF (check(1:1) == '#') THEN
           j=j+1
        ENDIf 
     ELSE
        EXIT
     ENDIF
  END DO

  ALLOCATE(T(i-j))
  ALLOCATE(W(i-j))

  DO m=1,i
     READ(u,IOSTAT=ios,FILE=)
  
  PRINT*,i
  
#  USE STATISTICS(T,W,Ta,Wa)
  

  
END PROGRAM READING
