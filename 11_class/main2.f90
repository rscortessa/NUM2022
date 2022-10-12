PROGRAM class11
  USE lotka
  USE euler
  REAL :: a,b,x0,y0
  INTEGER ::i
  CHARACTER(len=50) :: name
  INTEGER :: n
  n=100
  a=0.0
  b=10.0
  x0=1.0
  y0=0.6
  PRINT*, "THe function is going to be plotted from ",a," to ",b
  name='lotkaeuler.txt'
  CALL eulerint(a,b,n,x0,y0,name,.false.)
  name='lotkaeuler-improv.txt'
  CALL eulerint(a,b,n,x0,y0,name,.true.)
END PROGRAM class11
