
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
