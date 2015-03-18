PROGRAM USO_SUBROUTINE
IMPLICIT NONE

!Programma che chiama la funzione sotto e gli fa calcolare l'integrale da a a b diviso in np punti

INTEGER :: NP,C=7,D=9
REAL(KIND(0.D0)) :: INTEGRAL_VALUE,A,B,MC_INTEGRAL

! Integral from a to b in NP points, always of 1/(1+x^2) 
WRITE (*,*)
WRITE (*,*) "Inserire a,b ed np: "
10 READ (*,*,ERR=10) A,B,NP

INTEGRAL_VALUE=MC_INTEGRAL(A,B,NP)


WRITE (*,*) INTEGRAL_VALUE
!prove per vedere come si comporta con tipi diversi

!INTEGRAL_VALUE=MC_INTEGRAL(C,D,NP)

!INTEGRAL_VALUE=MC_INTEGRAL(1.,C,7000)
!per cambiare l'ordine
!INTEGRAL_VALUE=MC_INTEGRAL(upper=10,npoints=1000,lower=7)

END PROGRAM USO_SUBROUTINE

Real(kind(0.d0)) Function MC_INTEGRAL(lower,upper,NP)
implicit none
!W colud have given a value to MC_INTEGRAL inside instead of calling it before
!Il tipo di lower upper e npoints sono dati dal programma che chiama la funzione
!Sono chiamati dumme arguments (argomenti a salve)

!intent(in) vuol dire che tratta il valore come una costante

Real(kind(0.d0)),intent(in) :: lower,upper
Integer,intent(in) :: NP
Real(kind(0.d0)),allocatable :: casu(:)
!si poteva fare anche
!Real(kind(0.d0)) :: MC_INTEGRAL


allocate(casu(NP))
call random_number(casu)
casu=lower+(upper-lower)*casu
casu=1.d0/(1.d0+casu**2.d0)
MC_INTEGRAL=1.d0*(SUM(casu)/NP)
deallocate(casu)

end function MC_INTEGRAL
