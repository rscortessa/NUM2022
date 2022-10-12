PROGRAM class11
  USE functions
  USE euler
  REAL :: a,b,x0
  INTEGER ::i
  CHARACTER(len=50) :: name
  CHARACTER(len=3) :: j
  INTEGER,DIMENSION(3) :: n
  n=(/10,100,200/)
  a=0.0
  b=10.0
  x0=h(a)
  PRINT*, "THe function is going to be plotted from ",a," to ",b
  DO i=1,3
     WRITE(j,"(i1)")i
     name='euler'//TRIM(j)//'.txt'
     CALL eulerint(a,b,n(i),x0,name,.false.)
     name='euler-improv'//TRIM(j)//'.txt'
     CALL eulerint(a,b,n(i),x0,name,.true.)
  END DO
END PROGRAM class11
