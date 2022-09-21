PROGRAM assignment_4
  USE ORDERING
  REAL, DIMENSION(:), ALLOCATABLE :: cool_vec
  INTEGER :: m
  WRITE(6,'(a)',advance="no") "Assign a first value = "
  READ*,m
  ALLOCATE(cool_vec(m))
  CALL Initialize_vector(cool_vec,m)
  PRINT*,"Original array= ",cool_vec
END PROGRAM assignment_4
