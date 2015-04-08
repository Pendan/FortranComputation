!*************************************************
!*                                               *
!*  PROGRAMMA FORTRAN CENTRO MASSA/INERZIA       *
!*  Di Daniele Scarinci                          *
!*  daniele.scarinci@studenti.unicam.it          *
!*                                               *
!*                                               *
!*************************************************


PROGRAM CENTROMASSA

   IMPLICIT NONE

   INTEGER                    :: NP,i
   REAL(KIND(0.D0))           :: RAGGIO,RAGGIO_P,RAGGIO_G,YCM,X,DENSITY
   CHARACTER                  :: RISP
   REAL(KIND(0.D0))           :: RISy,RISx,RISi,semicirc,semicircx,corona,coronax,DENSITYcorona
   REAL(KIND(0.D0))           :: inerziaCircTop,coronaCircTop,coronaCircOr,RIStop,RISor
   EXTERNAL                   :: DENSITY,DENSITYcorona
   REAL(KIND(0.D0)), PARAMETER:: Pi=4.D0*DATAN(1.D0)



! Pulisce lo schermo

1 WRITE(*,*)
  !DEC$ IF DEFINED(_WIN32)
   call system('cls')				
  !DEC$ ELSEIF DEFINED(_LINUX)
   call system('clear')

! Inizializzazione variabili

   NP=0
   RAGGIO=0.D0
   RAGGIO_P=-1.D0
   RISy=0.D0
   RISx=0.D0
   RISi=0.D0



WRITE (*,"(A)")		" /*******************************************************************\"
WRITE (*,"(A)")		" |                                                                   |"
WRITE (*,"(A)")		" |              Center of Mass v0.1, di Daniele Scarinci             |"
WRITE (*,"(A)") 	" |                                                                   |"
WRITE (*,"(A)")		" |                                                                   |"
WRITE (*,"(A)")		" \*******************************************************************/"
WRITE(*,*)



! Riceve i dati in ingresso

DO WHILE(NP.LE.0)
   11 WRITE (*,"(3X,A27)",ADVANCE="NO") "Inserire num. iterazioni: "
      READ (*,*,ERR=11) NP
END DO


DO WHILE(RAGGIO.LE.0)
   12 WRITE (*,"(3X,A27)",ADVANCE="NO") "Inserire raggio: "
      READ (*,*,ERR=12) RAGGIO
END DO


DO WHILE(RAGGIO_P.GE.RAGGIO.OR.RAGGIO_P.LT.0)
   13 WRITE (*,"(3X,A27)",ADVANCE="NO") "Inserire raggio minore: "
      READ (*,*,ERR=13) RAGGIO_P
END DO

! Pulisce lo schermo

  WRITE(*,*)
  !DEC$ IF DEFINED(_WIN32)
   call system('cls')				
  !DEC$ ELSEIF DEFINED(_LINUX)
   call system('clear')

! Stampa a schermo i risultati

WRITE(*,*)

   WRITE(*,42) "Num. Iterazioni = ",NP
   42 FORMAT(3X,A25,I20)
   WRITE(*,102) "Raggio = ",RAGGIO
   WRITE(*,102) "Raggio interno = ",RAGGIO_P
   102 FORMAT(3X,A25,F20.10,1X,A15,1X,F20.10)

WRITE(*,*)

   WRITE(*,52) "Semicirconferenza: "
WRITE(*,*)
   52 FORMAT(3X,A)
   
      RISy=semicirc(RAGGIO,DENSITY,NP)
      RISx=semicircx(RAGGIO,DENSITY,NP)
      RISi=inerziaCircTop(RAGGIO,DENSITY,NP)

   WRITE(*,72) "Ycm = ", RISy
   WRITE(*,72) "Xcm = ", RISx
   72 FORMAT(3X,A25,F20.10,1X,A25,1X,F15.10)
   WRITE(*,82) "Inerzia dalla cima = ",RISi
   82 FORMAT(3X,A25,F20.10,1X,A25,1X,F15.10)

WRITE(*,*)

   WRITE(*,52) "Corona circolare: "
WRITE(*,*)
      RISy=corona(RAGGIO,RAGGIO_P,DENSITYcorona,NP)
      RISx=coronax(RAGGIO,RAGGIO_P,DENSITYcorona,NP)
      RIStop=coronaCircTop(RAGGIO,RAGGIO_P,DENSITYcorona,NP)
      RISor=coronaCircOr(RAGGIO,RAGGIO_P,DENSITYcorona,NP)
   WRITE(*,72) "Ycm = ", RISy
   WRITE(*,72) "Xcm = ", RISx
   WRITE(*,82) "Inerzia dalla cima = ",RIStop
   WRITE(*,82) "Inerzia dall'origine = ",RISor

! Chiede se ricominciare od uscire

   WRITE(*,*)
123   WRITE(*,"(A)",ADVANCE="NO") "Ricominciare? (y/n)"
      READ (*,*,ERR=123) RISP
      IF (RISP.EQ."y".or.RISP.EQ."Y") THEN
         GOTO 1
      ELSE IF (RISP.EQ."n".or.RISP.EQ."N") THEN
         GOTO 999
      ELSE
         GOTO 123
      END IF

999   WRITE(*,*)
     !DEC$ IF DEFINED(_WIN32)
      call system('cls')				
     !DEC$ ELSEIF DEFINED(_LINUX)
      call system('clear')

END PROGRAM CENTROMASSA







! Funzione per il calcolo della coordinata ycm del centro di massa della circonferenza

Function semicirc(RAGGIO,funden,NP)


Real(kind(0.d0)),intent(in) :: RAGGIO
Real(kind(0.d0)) :: funden,semicirc
Integer,intent(in) :: NP
Real(kind(0.d0)),allocatable :: casu(:),densu(:),cocasu(:)
Integer :: i
REAL(KIND(0.D0)), PARAMETER:: Pi=4.D0*DATAN(1.D0)


 allocate(casu(NP))
 allocate(cocasu(NP))
 allocate(densu(NP))
 casu=0
 densu=0
 cocasu=0

! Genero NP numeri casuali tra 0 ed 1

   call random_number(casu)

! Trasformo l'intervallo da 0 ad 1 in quello di nostro interesse

   casu=Pi*casu


   forall (i=1:NP) cocasu(i)=Cos(casu(i))

   forall (i=1:NP) casu(i)=Sin(casu(i))

   ! Calcolo densità

   do i=1,NP
      densu(i)=funden(RAGGIO*cocasu(i),RAGGIO*casu(i))
   end do

   ! Valore di ritorno della funzione, ycm

   semicirc=1.d0*((RAGGIO*sum(casu*densu))/sum(densu))

 deallocate(casu)
 deallocate(cocasu)
 deallocate(densu)

end function semicirc






! Funzione per il calcolo della coordinata xcm del centro di massa della circonferenza

Function semicircx(RAGGIO,funden,NP)


Real(kind(0.d0)),intent(in) :: RAGGIO
Real(kind(0.d0)) :: funden,semicircx
Integer,intent(in) :: NP
Real(kind(0.d0)),allocatable :: casu(:),densu(:),cocasu(:)
Integer :: i
REAL(KIND(0.D0)), PARAMETER:: Pi=4.D0*DATAN(1.D0)


 allocate(casu(NP))
 allocate(cocasu(NP))
 allocate(densu(NP))
 densu=0
 cocasu=0

! Genero NP numeri casuali tra 0 ed 1

   call random_number(casu)

! Trasformo l'intervallo da 0 ad 1 in quello di nostro interesse

   casu=Pi*casu

   forall (i=1:NP) cocasu(i)=Cos(casu(i))

   forall (i=1:NP) casu(i)=Sin(casu(i))

   ! Calcolo densità

   do i=1,NP
      densu(i)=funden(RAGGIO*cocasu(i),RAGGIO*casu(i))
   end do

   ! Valore di ritorno della funzione, ycm

   semicircx=1.d0*((RAGGIO*sum(cocasu*densu))/sum(densu))

 deallocate(casu)
 deallocate(cocasu)
 deallocate(densu)

end function semicircx





! Funzione per il calcolo della coordinata ycm del centro di massa della corona circolare

Function corona(RAGGIO,RAGGIO_P,funden,NP)


Real(kind(0.d0)),intent(in) :: RAGGIO,RAGGIO_P
Real(kind(0.d0)) :: funden,corona
Integer,intent(in) :: NP
Real(kind(0.d0)),allocatable :: casu(:),densu(:),cocasu(:)
Integer :: i
REAL(KIND(0.D0)), PARAMETER:: Pi=4.D0*DATAN(1.D0)


allocate(casu(NP))
allocate(cocasu(NP))
allocate(densu(NP))

densu=0
cocasu=0
casu=0

! Prendiamo NP punti che cadono nell'intervallo di nostro interesse ed escludiamo quelli al di fuori

Do i=1,NP
   Do 
     call random_number(casu(i))
     call random_number(cocasu(i))

     ! Li rendiamo numeri casuali non da 0 a 1 ma nell'intervallo di nostro interesse
      casu(i)=(RAGGIO)*casu(i)
      cocasu(i)=-RAGGIO+(2*RAGGIO)*cocasu(i)
     ! Se il numero cade nella corona sferica lo prendiamo, altrimenti lo rigeneriamo
      if (funden(cocasu(i),casu(i),RAGGIO,RAGGIO_P).GT.0.D0) exit
   end do

end do


! Calcolo centro di massa Ycm

do i=1,NP
   densu(i)=funden(cocasu(i),casu(i),RAGGIO,RAGGIO_P)
end do


 corona=1.d0*((sum(casu*densu))/sum(densu))

deallocate(casu)
deallocate(cocasu)
deallocate(densu)

end function corona








! Funzione per il calcolo della coordinata xcm del centro di massa della corona circolare

Function coronax(RAGGIO,RAGGIO_P,funden,NP)


Real(kind(0.d0)),intent(in) :: RAGGIO,RAGGIO_P
Real(kind(0.d0)) :: funden,coronax
Integer,intent(in) :: NP
Real(kind(0.d0)),allocatable :: casu(:),densu(:),cocasu(:)
Integer :: i
REAL(KIND(0.D0)), PARAMETER:: Pi=4.D0*DATAN(1.D0)

allocate(casu(NP))
allocate(cocasu(NP))
allocate(densu(NP))
densu=0
cocasu=0
casu=0

! Prendiamo NP punti che cadono nell'intervallo di nostro interesse ed escludiamo quelli al di fuori

Do i=1,NP
   Do 
     call random_number(casu(i))
     call random_number(cocasu(i))

     ! Li rendiamo numeri casuali non da 0 a 1 ma nell'intervallo di nostro interesse
        casu(i)=(RAGGIO)*casu(i)
        cocasu(i)=-RAGGIO+(2*RAGGIO)*cocasu(i)
     ! Se il numero cade nella corona sferica lo prendiamo, altrimenti lo rigeneriamo
        if (funden(cocasu(i),casu(i),RAGGIO,RAGGIO_P).GT.0) exit
   end do
end do



! Calcolo centro di massa Xcm

do i=1,NP
   densu(i)=funden(cocasu(i),casu(i),RAGGIO,RAGGIO_P)
end do


 coronax=1.d0*((sum(cocasu*densu))/sum(densu))

deallocate(casu)
deallocate(cocasu)
deallocate(densu)

end function coronax








! Funzione per il calcolo del momento di inerzia della circonferenza

function inerziaCircTop(RAGGIO,funden,NP)
   implicit none
   
   Real(kind(0.d0)),intent(in) :: RAGGIO
   Real(kind(0.d0)) :: funden,inerziaCircTop
   Integer,intent(in) :: NP
   Real(kind(0.d0)),allocatable :: casu(:),densu(:),cocasu(:)
   Integer :: i
   REAL(KIND(0.D0)), PARAMETER:: Pi=4.D0*DATAN(1.D0)

   allocate(casu(NP))
   allocate(cocasu(NP))
   allocate(densu(NP))
   
   casu=0
   densu=0
   cocasu=0
   call random_number(casu)

   casu=Pi*casu

   forall (i=1:NP) cocasu(i)=Cos(casu(i))
   
   forall (i=1:NP) casu(i)=Sin(casu(i))


   do i=1,NP
      densu(i)=funden(RAGGIO*cocasu(i),RAGGIO*casu(i))
   end do


      inerziaCircTop=1.d0*(Pi*(raggio**3.D0)/NP)*SUM((cocasu**2.D0+(1-casu)**2.D0)*densu)

   deallocate(casu)
   deallocate(cocasu)
   deallocate(densu)

end function inerziaCircTop








! Funzione per il calcolo del momento di inerzia della corona circolare rispetto alla "cima"

Function coronaCircTop(RAGGIO,RAGGIO_P,funden,NP)


Real(kind(0.d0)),intent(in) :: RAGGIO,RAGGIO_P
Real(kind(0.d0)) :: funden,coronaCircTop
Integer,intent(in) :: NP
Real(kind(0.d0)),allocatable :: casu(:),densu(:),cocasu(:)
Integer :: i
REAL(KIND(0.D0)), PARAMETER:: Pi=4.D0*DATAN(1.D0)


allocate(casu(NP))
allocate(cocasu(NP))
allocate(densu(NP))
densu=0
cocasu=0
casu=0

! Prendiamo NP punti che cadono nell'intervallo di nostro interesse ed escludiamo quelli al di fuori

Do i=1,NP
   Do 
     call random_number(casu(i))
     call random_number(cocasu(i))

     ! Li rendiamo numeri casuali non da 0 a 1 ma nell'intervallo di nostro interesse
        casu(i)=(RAGGIO)*casu(i)
        cocasu(i)=-RAGGIO+(2*RAGGIO)*cocasu(i)
     ! Se il numero cade nella corona sferica lo prendiamo, altrimenti lo rigeneriamo
     if (funden(cocasu(i),casu(i),RAGGIO,RAGGIO_P).GT.0) exit
   end do
end do

do i=1,NP
   densu(i)=funden(cocasu(i),casu(i),RAGGIO,RAGGIO_P)
end do


 coronaCircTop=1.d0*(Pi*(RAGGIO**2.d0-RAGGIO_P**2.d0)/2*(1.d0/NP)*sum(((RAGGIO-casu)**2.d0+(cocasu)**2)*densu))

deallocate(casu)
deallocate(cocasu)
deallocate(densu)

end function coronaCircTop







! Funzione per il calcolo del momento di inerzia della corona circolare rispetto all'origine

Function coronaCircOr(RAGGIO,RAGGIO_P,funden,NP)

Real(kind(0.d0)),intent(in) :: RAGGIO,RAGGIO_P
Real(kind(0.d0)) :: funden,coronaCircOr
Integer,intent(in) :: NP
Real(kind(0.d0)),allocatable :: casu(:),densu(:),cocasu(:)
Integer :: i
REAL(KIND(0.D0)), PARAMETER:: Pi=4.D0*DATAN(1.D0)


allocate(casu(NP))
allocate(cocasu(NP))
allocate(densu(NP))
densu=0
cocasu=0
casu=0

! Prendiamo NP punti che cadono nell'intervallo di nostro interesse ed escludiamo quelli al di fuori

Do i=1,NP
   Do 
     call random_number(casu(i))
     call random_number(cocasu(i))

     ! Li rendiamo numeri casuali non da 0 a 1 ma nell'intervallo di nostro interesse
        casu(i)=(RAGGIO)*casu(i)
        cocasu(i)=-RAGGIO+(2*RAGGIO)*cocasu(i)
     ! Se il numero cade nella corona sferica lo prendiamo, altrimenti lo rigeneriamo
     if (funden(cocasu(i),casu(i),RAGGIO,RAGGIO_P).GT.0) exit
   end do
end do


do i=1,NP

densu(i)=funden(cocasu(i),casu(i),RAGGIO,RAGGIO_P)
end do


coronaCircOr=1.d0*(((Pi*(RAGGIO**2.d0-RAGGIO_P**2.d0)/2)/NP)*sum((casu**2.d0+cocasu**2.d0)*densu))

deallocate(casu)
deallocate(cocasu)
deallocate(densu)

end function coronaCircOr






! Funzione che forinisce la densità per la circonferenza

function DENSITY(X,Y)
	implicit none
	Real(kind(0.d0)) :: density
	Real(kind(0.d0)), Intent(in) :: x,y

		!Cambiare questa per cambiare la funzione densità

		density=2.d0

end function DENSITY






! Funzione che forinisce la densità per la corona circolare

function DENSITYcorona(X,Y,RAGGIO,RAGGIO_P)
	implicit none
	Real(kind(0.d0)) :: DENSITYcorona,s
	Real(kind(0.d0)), Intent(in) :: x,y,RAGGIO,RAGGIO_P

		
                s=x**2.D0+y**2.D0
                if(s.LE.RAGGIO**2.d0.AND.S.GT.RAGGIO_P**2.d0) THEN
                        ! Cambiare questa per cambiare la funzione densità
		        densitycorona=2.d0
		        ! Se il punto è al di fuori della corona circolare la densità è zero
                else  
                        densitycorona=0.d0
                end if

end function DENSITYcorona
