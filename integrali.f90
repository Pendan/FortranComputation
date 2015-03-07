PROGRAM INTEGRALE
IMPLICIT NONE
REAL(KIND(0.D0)),PARAMETER :: PI4=ACOS(SQRT(2.D0)/2.D0)
REAL :: CASU,SUMMA=0.D0,ERRORE=0.D0,SUMMA1=0.D0
INTEGER :: I=0

WRITE (*,*) "Inserire numero di iterazioni:"

IF (I.LE.0) THEN
	READ (*,*) I
END IF


DO I=1,I

CALL RANDOM_NUMBER(CASU)
SUMMA=SUMMA+(1/(1+CASU**2))
SUMMA1=SUMMA/I
ERRORE=((PI4-SUMMA1)/PI4)*100
WRITE (*,*) "N.Conteggi:", I, "Errore:", ABS(ERRORE),"%"

END DO

SUMMA=SUMMA/I

WRITE (*,*) 
WRITE (*,*) 
WRITE (*,*) 
WRITE (*,*) "Valore reale:", PI4
WRITE (*,*) "Valore trovato:", SUMMA
WRITE (*,*) "Errore:", ABS(PI4-SUMMA), "Errore Percentuale:"
WRITE (*,*) "Errore Percentuale:", ABS(ERRORE),"%"
READ(*,*)
END PROGRAM