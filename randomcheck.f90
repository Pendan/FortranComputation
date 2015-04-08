PROGRAM random_number_check

IMPLICIT NONE

integer, parameter :: DP=(kind(0.d0))
real :: x
 character :: c
integer, allocatable :: seed_vect(:),org_seed(:)
integer :: seed_size,i

call date_and_time(c)
write(*,*) c
call random_seed(size=seed_size)
allocate(seed_vect(seed_size), org_seed(seed_size))
org_seed=12345678
seed_vect=org_seed
call random_seed(put=seed_vect)


write(*,*) seed_size


do i=1,10
 call random_number(x)
 call random_seed(get=seed_vect)
 write(*,*) i,x
 write(*,*) seed_vect(1:seed_size/2)
 write(*,*) seed_vect(seed_size/2+1:seed_size)
end do

end program 
