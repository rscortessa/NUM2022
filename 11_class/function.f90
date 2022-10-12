MODULE function
  REAL FUNCTION f(x,t) RESULT(r)
    REAL, INTENT(INT) :: x,t
    r=x*2+t
  END FUNCTION F
  
  REAL FUNCTION h(t) RESULT(r)
    REAL, INTENT(INT) :: t
    r=t
  END FUNCTION h
END MODULE function
