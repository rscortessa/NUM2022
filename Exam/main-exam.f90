PROGRAM main
  USE temperature
  USE apx
  IMPLICIT NONE
  REAL, DIMENSION(:), ALLOCATABLE :: a
  REAL, DIMENSION(:), ALLOCATABLE :: b
  REAL, DIMENSION(3) :: prop  !Maximum, Minimum, average
  INTEGER :: tam,u,ios,n,m
  REAL :: eps
  eps=1.D-4
  n=0
  m=0
  tam=initialize("temperature_2010_Trieste.dat")
  PRINT*,tam
  ALLOCATE(a(tam))
  ALLOCATE(b(tam))
  CALL store(a,b,prop(3))
  CALL calculate(b,prop(3),prop(1),prop(2))
  PRINT*,"max=",prop(1),"min=",prop(2),'av=',prop(3)
  u=20
  ios=0
  OPEN(UNIT=u,IOSTAT=ios,FILE='APRIL.txt',STATUS="replace",ACTION='write')
  CALL secant(90.0,120.0,eps,n,u,prop(3))
  CLOSE(u)
  u=30
  OPEN(UNIT=u,IOSTAT=ios,FILE='OCTOBER.txt',STATUS="replace",ACTION='write')
  CALL secant(272.0,303.0,eps,m,u,prop(3))
  CLOSE(u)
END PROGRAM MAIN
