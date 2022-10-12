PROGRAM class11
  USE euler
  REAL :: a,b,x0
  INTEGER :: n
  a=0
  b=1
  x0=h(a)
  PRINT*, "Choose the number of points"
  READ*,n
  PRINT*, "THe function is going to be plotted from ",a," to ",b
  
END PROGRAM class11
