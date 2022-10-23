MODULE randomvar
  IMPLICIT NONE
CONTAINS
  INTEGER function randomstep(n) RESULT(p)
    INTEGER, INTENT(in) :: n
    INTEGER :: i
    REAL :: step
    p=0
    DO i=1,n
       CALL RANDOM_NUMBER(step)
       PRINT*,step,p
    IF(step-0.5000>0.0000) THEN
       p=p+1
    ELSE
       p=p-1
    ENDIF
    END DO
  END function randomstep

  RECURSIVE SUBROUTINE randomwalk(steps,rep,dis)
    REAL,DIMENSION(:), INTENT(INOUT) :: dis
    INTEGER,INTENT(INOUT) :: steps
    INTEGER,INTENT(INOUT) :: rep
    INTEGER :: i
    IF(rep==0) THEN
       RETURN
    ELSE
       i=randomstep(steps)+steps+1
       PRINT*,i,rep
       dis(i)=dis(i)+1.0
       rep=rep-1
       CALL randomwalk(steps,rep,dis)
    END IF
  END SUBROUTINE randomwalk

  SUBROUTINE printer(dis,u,steps)
    REAL,DIMENSION(:), INTENT(INOUT) :: dis
    INTEGER,INTENT(IN) :: u,steps
    INTEGER :: i
    PRINT*,dis
    DO i=-steps,steps
       WRITE(u,*)i,dis(i+steps)
    END DO

    
  END SUBROUTINE PRINTER
    
END MODULE randomvar
