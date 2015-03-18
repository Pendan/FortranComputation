Real(kind(0.d0) Function MC_INTEGRAL (lower,upper,NP)

!W colud have given a value to MC_INTEGRAL inside instead of calling it before
!Il tipo di lower upper e npoints sono dati dal programma che chiama la funzione
!Sono chiamati dumme arguments (argomenti a salve)

!intent(in) vuol dire che tratta il valore come una costante

Real(kind(0.d0)),intent(in) :: lower,upper
Integer,intent(in) :: NP
Real(kind(0.d0)),allocatable :: casu(:)
Integer :: i


allocate(casu(NP))
call random_number(casu)

forall(i=1:NP) casu(i)=my_funct(casu(i))

end function MC_INTEGRAL
