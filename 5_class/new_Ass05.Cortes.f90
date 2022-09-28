PROGRAM READING
  USE STATISTICS
  REAL, DIMENSION(:), ALLOCATABLE :: T
  REAL, DIMENSION(:), ALLOCATABLE :: W
  REAL :: Ta,Wa
  INTEGER :: i

  i = initialize('numerical.dat')

  ALLOCATE(T(i))
  ALLOCATE(W(i))

  CALL store(T,W)

  CALL average(T,W,Ta,Wa)

  PRINT*,"T average=",Ta, " Precipitation average=",Wa

END PROGRAM READING
