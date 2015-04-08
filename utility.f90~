module utility
	implicit none
	integer,parameter :: dp=kind(0.d0)

	subroutine subtract(magg,mino,ris)
		implicit none
		integer,intent(in) :: mino
		integer,intent(out):: ris
		integer,optional,intent(in)::magg
		if(present(magg)) then
			ris=magg-mino
		else
			ris=mino
		end if
	end subroutine subtract

end module utility
