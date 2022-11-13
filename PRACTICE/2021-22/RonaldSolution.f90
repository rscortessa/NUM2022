PROGRAM test
  USE data
  USE random
  IMPLICIT NONE
  REAL, DIMENSION(:,:), ALLOCATABLE ::precip
  REAL, DIMENSION(10,6) :: stats
  REAL, DIMENSION(10,2) :: diff
  REAL, DIMENSION(100,1) :: aux
  CHARACTER(3),DIMENSION(10) :: names
  CHARACTER(len=6) :: name
  INTEGER :: i,years,u,ios
  years=100
  ALLOCATE(precip(10,years))
  DO i=1,10
     WRITE(name,"(i0.2,a)") i,".dat"
     OPEN(unit=50,FILE=name,STATUS="OLD",ACTION="READ")
     READ(50,*)names(i)
     close(50)
     aux=store(years,1,name)
     precip(i,:)=aux(:,1)
     stats(i,1:2)=av(precip(i,1:30)) 
     stats(i,3:4)=av(precip(i,36:65))
     stats(i,5:6)=av(precip(i,71:100))
  END DO
  u=10
  OPEN(UNIT=u,IOSTAT=ios,FILE="FINAL.dat",STATUS="REPLACE",ACTION="WRITE")
  DO i=1,6
     WRITE(u,*) stats(:,i)
  END DO
  CLOSE(u)
  DO i=1,10
     diff(i,1)=ABS(stats(i,1)-stats(i,3))
     diff(i,2)=ABS(stats(i,5)-stats(i,1))
     IF(diff(i,1)>diff(1,2)) THEN
        PRINT*,"The maximum difference in",names(i),"happened between 36-50 and 01-30 :",diff(i,1)
     ELSE
        PRINT*,"The maximum difference in ",names(i)," happened between 71-2000 and 01-30 :",diff(i,2)
     ENDIF

     
  END DO
  
END PROGRAM test
