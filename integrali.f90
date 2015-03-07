PROGRAM INTEGRALE
IMPLICIT NONE
REAL(KIND(0.D0)),PARAMETER :: PI4=ACOS(SQRT(2.D0)/2.D0)
REAL :: CASU,ERRORE=0.D0,SUMMA1=0.D0
REAL*8 :: SUMMA=0.D0
INTEGER :: I=0,CON=0,SCELTA=0

1 call system('cls')
SUMMA=0.D0
ERRORE=0.D0
SUMMA1=0.D0
CASU=0.D0
WRITE (*,*) "Mono-Integral Calculator v0.01, di Daniele Scarinci"
WRITE (*,*)
WRITE (*,"(A)") "Inserire: 1 = Versione grafica 2 = Versione normale 3 = Versione veloce"
WRITE (*,"(A)") "Premere qualsiasi altro tasto per uscire."

READ (*,*) SCELTA


IF (SCELTA.EQ.1) THEN

WRITE (*,"(A)",ADVANCE="NO") "Inserire numero di iterazioni:"

READ (*,*) CON



DO I=1,CON

CALL RANDOM_NUMBER(CASU)
SUMMA=SUMMA+(1/(1+CASU**2))
SUMMA1=SUMMA/I
ERRORE=((PI4-SUMMA1)/PI4)*100
WRITE (*,*) "N.Conteggi:", I, "Errore:", ABS(ERRORE),"%"
IF (I.LE.(CON/10)) THEN
WRITE (*,*) "<---------->"
ELSE IF (I.LE.(2*CON/10)) THEN
WRITE (*,*) "<=--------->"
ELSE IF (I.LE.(3*CON/10)) THEN
WRITE (*,*) "<==-------->"
ELSE IF (I.LE.(4*CON/10)) THEN
WRITE (*,*) "<===------->"
ELSE IF (I.LE.(5*CON/10)) THEN
WRITE (*,*) "<====------>"
ELSE IF (I.LE.(6*CON/10)) THEN
WRITE (*,*) "<=====----->"
ELSE IF (I.LE.(7*CON/10)) THEN
WRITE (*,*) "<======---->"
ELSE IF (I.LE.(8*CON/10)) THEN
WRITE (*,*) "<=======--->"
ELSE IF (I.LE.(9*CON/10)) THEN
WRITE (*,*) "<========-->"
ELSE IF (I.LE.(10*CON/10)) THEN
WRITE (*,*) "<=========->"
ELSE 
WRITE (*,*) "<==========>"
END IF

call system('cls')




END DO

SUMMA=SUMMA/CON

WRITE (*,*) 
WRITE (*,*) 
WRITE (*,*) 
WRITE (*,*) "Valore reale:", PI4
WRITE (*,*) "Valore trovato:", SUMMA
WRITE (*,*) "Errore:", ABS(PI4-SUMMA)
WRITE (*,*) "Errore Percentuale:", ABS(ERRORE),"%"
WRITE (*,*)
WRITE (*,*) "Premere Invio per continuare"
READ (*,*)
GOTO 1


ELSE IF (SCELTA.EQ.2) THEN

WRITE (*,"(A)",ADVANCE="NO") "Inserire numero di iterazioni:"


READ (*,*) CON



DO I=1,CON

CALL RANDOM_NUMBER(CASU)
SUMMA=SUMMA+(1/(1+CASU**2))
SUMMA1=SUMMA/I
ERRORE=((PI4-SUMMA1)/PI4)*100
WRITE (*,*) "N.Conteggi:", I, "Errore:", ABS(ERRORE),"%"

END DO

SUMMA=SUMMA/CON

WRITE (*,*) 
WRITE (*,*) 
WRITE (*,*) 
WRITE (*,*) "Valore reale:", PI4
WRITE (*,*) "Valore trovato:", SUMMA
WRITE (*,*) "Errore:", ABS(PI4-SUMMA)
WRITE (*,*) "Errore Percentuale:",ABS(ERRORE),"%"
WRITE (*,*)
WRITE (*,*) "Premere Invio per continuare"
READ (*,*)
GOTO 1

ELSE IF (SCELTA.EQ.3) THEN

WRITE (*,"(A)",ADVANCE="NO") "Inserire numero di iterazioni:"


READ (*,*) CON



DO I=1,CON

CALL RANDOM_NUMBER(CASU)
SUMMA=SUMMA+(1/(1+CASU**2))


END DO

SUMMA=SUMMA/CON

WRITE (*,*) 
WRITE (*,*) 
WRITE (*,*) 
WRITE (*,*) "Valore reale:", PI4
WRITE (*,*) "Valore trovato:", SUMMA
WRITE (*,*) "Errore:", ABS(PI4-SUMMA)
WRITE (*,*) "Errore Percentuale:", ABS(((PI4-SUMMA)/PI4)*100),"%"
WRITE (*,*)
WRITE (*,*) "Premere Invio per continuare"
READ (*,*)
GOTO 1

ELSE

GOTO 2

END IF

2 WRITE(*,*)

END PROGRAM