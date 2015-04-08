!*************************************************
!*                                               *
!*  PROGRAMMA FORTRAN CALCOLO INTEGRALE          *
!*  Di Daniele Scarinci                          *
!*  daniele.scarinci@studenti.unicam.it          *
!*                                               *
!* Il programma è stato realizzato usando Linux, *
!* qualche lettera accentata potrebbe causare    *
!* problemi su di un pc con windows              *
!*                                               *
!*************************************************




PROGRAM INTEGRALE
IMPLICIT NONE
REAL(KIND(0.D0)),PARAMETER :: PI4=ACOS(SQRT(2.D0)/2.D0)
REAL(KIND(0.D0)) :: CASU,A,B,SUMMA,MC_INTEGRAL,MC_RECTANGLE,MC_TRAP,MC_CAVSIMP
REAL :: start, finish
INTEGER :: CON=0,SCELTA=0,PARI

! Il compilatore prende informazioni sul sistema operativo e manda il comando per pulire lo schermo

1 WRITE(*,*)
!DEC$ IF DEFINED(_WIN32)
call system('cls')				
!DEC$ ELSEIF DEFINED(_LINUX)
call system('clear')



WRITE (*,"(A)")		" /*******************************************************************\"
WRITE (*,"(A)")		" |                                                                   |"
WRITE (*,"(A)")		" |         Integral Calculator v0.1, di Daniele Scarinci             |"
WRITE (*,"(A)")		" |                                                                   |"
WRITE (*,"(A)")		" |  Modalità: 1-> Metodo Montecarlo    2-> Metodo dei rettangoli     |"
WRITE (*,"(A)") 	" |            3-> Metodo dei trapezi   4-> Metodo Cavalieri-Simpson  |"
WRITE (*,"(A)")		" |                  5-> Confronto tra i metodi	                     |"
WRITE (*,"(A)")		" |                                                                   |"
WRITE (*,"(A)") 	" |                  Premere 0 per uscire.                            |"
WRITE (*,"(A)")		" |	                                                 	     |"
WRITE (*,"(A)")		" \*******************************************************************/"



A=-1
B=0
CON=0
casu=0

! Prende i dati in ingresso e la modalità in cui eseguire il programma

write(*,*)
WRITE (*,33,ADVANCE="NO") "Inserire num. intervalli (se dispari sarà aumentato di 1): "
DO WHILE(CON.LE.0)
11 READ (*,*,ERR=11) CON
END DO

!Controlla se è dispari, in caso affermativo aggiunge uno al valore 
!(stiamo lavorando con gli intervalli)

PARI=MOD(CON+1,2)
IF (PARI.EQ.0) THEN
CON=CON+1
END IF


WRITE (*,33,ADVANCE="NO") "Inserire punto iniziale intervallo: "
33 FORMAT(1X,A)
12 READ (*,*,ERR=12) A


WRITE (*,33,ADVANCE="NO") "Inserire punto finale intervallo ( > punto iniziale): "
13 READ (*,*,ERR=13) B
DO WHILE(B.LE.A)
14 READ (*,*,ERR=14) B
END DO

WRITE (*,"(A)")
WRITE (*,33,ADVANCE="NO") "Scelta modalità programma (5 per confronto): "
10 READ (*,*,ERR=10) SCELTA

! Pulisce lo schermo

!DEC$ IF DEFINED(_WIN32)
call system('cls')				
!DEC$ ELSEIF DEFINED(__LINUX)
call system('clear')




! Esecuzione modalità Metodo di Montecarlo

IF (SCELTA.EQ.1) THEN
SUMMA=0.D0

WRITE (*,*) 
WRITE (*,*) 
WRITE (*,42) "Iterazioni: ", CON
WRITE (*,52) "Intervallo di integrazione: ", INT(A), ",", INT(B)
SUMMA=MC_INTEGRAL(A,B,CON)
WRITE (*,99) "Valore trovato: ", SUMMA
99 FORMAT (3X,A30,F25.12)
WRITE (*,*)
WRITE (*,*) "Premere Invio per continuare"
READ (*,*)
GOTO 1



! Esecuzione modalità Metodo Rettangoli

else IF (SCELTA.EQ.2) THEN

SUMMA=0.D0

WRITE (*,*) 
WRITE (*,*) 
WRITE (*,42) "Iterazioni: ", CON
WRITE (*,52) "Intervallo di integrazione: ", INT(A), ",", INT(B)
SUMMA=MC_RECTANGLE(A,B,CON)
WRITE (*,99) "Valore trovato: ", SUMMA
WRITE (*,*)
WRITE (*,*) "Premere Invio per continuare"
READ (*,*)

GOTO 1



! Esecuzione modalità Metodo dei trapezi

else IF (SCELTA.EQ.3) THEN

SUMMA=0.D0

WRITE (*,*) 
WRITE (*,*) 
WRITE (*,42) "Iterazioni: ", CON
WRITE (*,52) "Intervallo di integrazione: ", INT(A), ",", INT(B)
SUMMA=MC_TRAP(A,B,CON)
WRITE (*,99) "Valore trovato: ", SUMMA
WRITE (*,*)
WRITE (*,*) "Premere Invio per continuare"
READ (*,*)

GOTO 1



! Esecuzione modalità Metodo di Cavalieri Simpson

else IF (SCELTA.EQ.4) THEN
SUMMA=0.D0

WRITE (*,*) 
WRITE (*,*) 
WRITE (*,42) "Iterazioni: ", CON
WRITE (*,52) "Intervallo di integrazione: ", INT(A), ",", INT(B)
SUMMA=MC_CAVSIMP(A,B,CON)
WRITE (*,99) "Valore trovato: ", SUMMA
WRITE (*,*)
WRITE (*,*) "Premere Invio per continuare"
READ (*,*)

GOTO 1



! eseguo il confronto tra i vari metodi chiamando a turno tutte le subroutine

else IF (SCELTA.EQ.5) THEN

SUMMA=0.D0

WRITE (*,*) 
WRITE (*,*) 
WRITE (*,42) "Iterazioni: ", CON
42 FORMAT(3X,A30,12X,I9)

WRITE (*,52) "Intervallo di integrazione: ", INT(A), ",", INT(B)
52 FORMAT(3X,A30,12X,I4,A,I4)

WRITE (*,*) 


WRITE (*,*) 

!fornisce una media molto rozza del tempo utilizzato dalla cpu per computare l'integrale

call cpu_time(start)
SUMMA=MC_INTEGRAL(A,B,CON)
call cpu_time(finish)

WRITE (*,72) "Metodo di Montecarlo:"
72 FORMAT(3X,A)
WRITE (*,82)  SUMMA, "Tempo: ", finish-start, "Err: ",1/Sqrt(1.D0*CON)
82 FORMAT(3X,F25.15,1X,A,F6.3,1X,A,F17.15)
WRITE (*,*)
call cpu_time(start)
SUMMA=MC_RECTANGLE(A,B,CON)
call cpu_time(finish)

WRITE (*,72) "Metodo dei rettangoli: "
WRITE (*,82) SUMMA, "Tempo: ", finish-start, "Err: ",1.D0/(CON)
WRITE (*,*)
call cpu_time(start)
SUMMA=MC_TRAP(A,B,CON)
call cpu_time(finish)

WRITE (*,72) "Metodo dei trapezi: "
WRITE (*,82) SUMMA, "Tempo: ", finish-start, "Err: ",1.D0/(CON**2.D0)
WRITE (*,*)
call cpu_time(start)
SUMMA=MC_CAVSIMP(A,B,CON)
call cpu_time(finish)

WRITE (*,72) "Cavalieri-Simpson: " 
WRITE (*,82) SUMMA, "Tempo: ", finish-start, "Err: ",1.D0/(CON**4.D0)
WRITE (*,*)
WRITE (*,*) "Premere Invio per continuare"

READ (*,*)

GOTO 1



! Per uscire dal programma

ELSE IF (SCELTA.EQ.0) THEN
CON=0
GOTO 2


! Gestisce le eccezioni

ELSE

GOTO 10

END IF

! Esce dal programma

2 WRITE(*,*)

! Come sopra

!DEC$ IF DEFINED(_WIN32)
call system('cls')
!DEC$ ELSEIF DEFINED(__LINUX)
call system('clear')

END PROGRAM INTEGRALE


! Funzione per il calcolo col metodo di Montecarlo


Function MC_INTEGRAL(lower,upper,NP)



Real(kind(0.d0)),intent(in) :: lower,upper
Real(kind(0.d0)) :: my_funct,MC_INTEGRAL
Integer,intent(in) :: NP
Real(kind(0.d0)),allocatable :: casu(:)
Integer :: i


allocate(casu(NP))
call random_number(casu)
casu=lower+(upper-lower)*casu

do i=1,NP
	casu(i)=my_funct(casu(i))
end do


MC_INTEGRAL=1.d0*(SUM(casu)/NP)*(upper-lower)
deallocate(casu)

end function MC_INTEGRAL


! Funzione per il calcolo col metodo dei rettangoli


Function MC_RECTANGLE(lower,upper,NP)


Real(kind(0.d0)),intent(in) :: lower,upper
Real(kind(0.d0)) :: my_funct,MC_RECTANGLE,argomento,passo
Integer,intent(in) :: NP
Real(kind(0.d0)),allocatable :: casu(:)
Integer :: i


allocate(casu(NP))
passo=1.d0*(upper-lower)/(NP)


do i=1,NP
	argomento=lower+i*passo
	casu(i)=my_funct(argomento)
end do


MC_RECTANGLE=1.d0*(SUM(casu))*passo

deallocate(casu)

end function MC_RECTANGLE


! Funzione per il calcolo col metodo dei trapezi


Function MC_TRAP(lower,upper,NP)



Real(kind(0.d0)),intent(in) :: lower,upper
Real(kind(0.d0)) :: my_funct,MC_TRAP,argomento,passo
Integer,intent(in) :: NP
Real(kind(0.d0)),allocatable :: casu(:)
Integer :: i,temp


allocate(casu(NP+1))
passo=1.d0*(upper-lower)/(NP)


do i=1,NP+1
	argomento=lower+(i-1)*passo
	casu(i)=my_funct(argomento)
end do
temp=NP-1
MC_TRAP=((casu(1)/2.D0)+(SUM(casu(2:NP)))+(casu(NP+1)/2.D0))*passo

deallocate(casu)

end function MC_TRAP

! Funzione per il calcolo col metodo di Cavalieri-Simpson

Function MC_CAVSIMP(lower,upper,NP)


Real(kind(0.d0)),intent(in) :: lower,upper
Real(kind(0.d0)) :: my_funct,MC_CAVSIMP,argomento,passo,casupari,casudispari
Integer,intent(in) :: NP
Real(kind(0.d0)),allocatable :: casu(:)
Integer :: i,parte,temp


allocate(casu(NP+1))
passo=1.d0*(upper-lower)/(NP)


do i=1,NP+1
	argomento=lower+(i-1)*passo
	casu(i)=my_funct(argomento)
end do
casupari=0.d0
casudispari=0.d0

temp=NP-1

do i=2,NP
	parte=mod(i,2)
	IF (parte.EQ.0) THEN
	casupari=casupari+casu(i)
	ELSE
	casudispari=casudispari+casu(i)
	end if
	
end do

MC_CAVSIMP=1.D0*(casu(1)+casu(NP+1)+ 4.D0*casupari+ 2.D0*casudispari)*(passo/3)

deallocate(casu)

end function MC_CAVSIMP


! Funzione che restituisce la funzione di cui vogliamo calcolare l'integrale


function my_funct(x)
	implicit none
	Real(kind(0.d0)) :: my_funct
	Real(kind(0.d0)), Intent(in) :: x

		!Cambiare questa per cambiare la funzione da integrare

		my_funct=x**2.d0

end function my_funct



