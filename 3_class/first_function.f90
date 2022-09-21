REAL(8) FUNCTION mynorm(xv,yv,zv) RESULT(res)
  REAL(8), INTENT(IN) :: xv,yv,zv !
  REAL(8)             :: a
  a = xv**2+yv**2+zv**2
  res = SQRT(a)
END FUNCTION mynorm

PROGRAM norm1
  IMPLICIT NONE
  REAL(8) :: a,x,y,z
  REAL(8), EXTERNAL :: mynorm
  x = 1.0
  y = 1.0
  z = 1.0
  a = mynorm(x,y,z)
  PRINT*, a
  END PROGRAM norm1
