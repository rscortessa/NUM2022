
MODULE functions
  CONTAINS
  REAL FUNCTION f(x,t) RESULT(r)
    REAL, INTENT(IN) :: x,t
    r=x
  END FUNCTION F
  
  REAL FUNCTION h(t) RESULT(r)
    REAL, INTENT(IN) :: t
    r=EXP(t)
  END FUNCTION h
  
END MODULE functions

MODULE lotka
  CONTAINS
  REAL FUNCTION fx(x,y,t) RESULT(r)
    REAL, INTENT(IN) :: x,y,t
    REAL :: a
    a=1
    b=2
    r=b*x-a*x*y
    
  END FUNCTION Fx

   REAL FUNCTION fy(x,y,t) RESULT(r)
    REAL, INTENT(IN) :: x,y,t
    REAL :: b
    b=1
    r=b*x*y-y
  END FUNCTION Fy
  
  REAL FUNCTION hx(t) RESULT(r)
    REAL, INTENT(IN) :: t
    r=0
  END FUNCTION hx

  REAL FUNCTION hy(t) RESULT(r)
    REAL, INTENT(IN) :: t
    r=0
  END FUNCTION hy
  
END MODULE lotka
