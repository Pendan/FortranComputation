        PROGRAM random_number_check

        IMPLICIT NONE

        integer, parameter :: DP=(kind(0.d0))
        real :: x
        integer, allocatable :: seed_vect(:)
        integer :: seed_size


        call random_seed(size=seed_size)
        allocate(seed_vect(seed_size))
        seed_vect=12345678
        call random_seed(put=seed_vect)
        call random_number(x)
        call random_seed(get=seed_vect)

 
        end program 
