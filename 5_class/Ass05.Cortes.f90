PROGRAM READING
  USE STATISTICS
  REAL, DIMENSION(:), ALLOCATABLE :: T
  REAL, DIMENSION(:), ALLOCATABLE :: W
  REAL :: A,B,Ta,Wa
  INTEGER :: i

  i=initialize('numerical.dat',13)

  ALLOCATE(T(i))
  ALLOCATE(W(i))

  CALL store(T,W,i)

  CALL average(T,W,i,Ta,Wa)

  PRINT*,Ta,Wa
  
END PROGRAM READING
