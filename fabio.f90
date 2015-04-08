! GPL LICENSE QUALCOSA

PROGRAM SEMI_CIRCUNFERENCE

    IMPLICIT NONE

        INTEGER, PARAMETER::     DP=KIND(0.D0)
        INTEGER::                N_POINTS
        REAL(DP)::               R
        REAL(DP)::               R_SMALL
        REAL(DP)::               Ycm
        REAL(DP)::               Xcm
        REAL(DP)::               IM_top
        REAL(DP)::               YMC_SEMI_CIRC
        REAL(DP)::               XMC_SEMI_CIRC
        REAL(DP)::               IM_SEMI_CIRC
        REAL(DP)::               YMC_HALF_CIRCULAR_SECTOR
        REAL(DP)::               DENSITY
        EXTERNAL::               DENSITY
        REAL(DP)::               DENSITY_SUP
        EXTERNAL::               DENSITY_SUP
        REAL(DP), PARAMETER::    PiG = ACOS(-1.D0)

1         WRITE(6,3,ADVANCE='NO') 'Please enter radius (R) and NUMBER OF ITERATIONS and R_SMALL:'
          READ(5,*,ERR=1,END=1) R, N_POINTS, R_SMALL
          WRITE(*,*)
3         FORMAT(10X,A,T50,F14.8)

             IF(N_POINTS.LT.1.D0) THEN
                WRITE(6,3) 'NUMBER OF ITERATIONS must be GREATER than 0' 
                WRITE(*,*)
                GOTO 1
             END IF
	     
             IF(R.LE.10**(-6)) THEN
                WRITE(6,3) 'Radius (R) must be GREATER than ZERO' 
                WRITE(*,*)
                GOTO 1
             END IF
	     
             IF(R_SMALL.GE.R) THEN
                WRITE(6,3) 'R_SMALL must be LESSER than R' 
                WRITE(*,*)
                GOTO 1
             END IF
	     

        Ycm = YMC_SEMI_CIRC(DENSITY, R, N_POINTS)
        Xcm = XMC_SEMI_CIRC(DENSITY, R, N_POINTS)
	
        WRITE(6,3) 'SEMI CIRCUNFERENCE'
        WRITE(*,*)
        WRITE(6,3) 'CENTER OF MASS'
        WRITE(6,3) 'Xcm:', Xcm
        WRITE(6,3) 'Theoric Xcm:', 0.D0
        WRITE(6,3) 'Ycm:', Ycm
        WRITE(6,3) 'Theoric Ycm :', (2 / PiG) * R
        WRITE(6,3) 'ERROR :', ABS(Ycm - (2 / PiG) * R)
        WRITE(*,*)
	
        IM_top = IM_SEMI_CIRC(DENSITY, R, N_POINTS)

        WRITE(6,3) 'INERTIAL MOMENTUM'
        WRITE(6,3) 'IM_top:', IM_top
        WRITE(*,*)
	
	
        Ycm = YMC_HALF_CIRCULAR_SECTOR(DENSITY_SUP, R, R_SMALL, N_POINTS)

        WRITE(6,3) 'HALF DISK'
        WRITE(*,*)
        WRITE(6,3) 'CENTER OF MASS'
        WRITE(6,3) 'Ycm:', Ycm
        WRITE(6,3) 'Theoric Ycm :', (4 /(3 * PiG)) * R
        WRITE(6,3) 'ERROR :', ABS(Ycm - (4 /(3 * PiG)) * R)

END PROGRAM SEMI_CIRCUNFERENCE

    FUNCTION YMC_SEMI_CIRC(MY_FUNCT, RADIUS, N_ITERS)
    
      IMPLICIT NONE
    
        INTEGER, PARAMETER::     DP=KIND(0.D0)
        INTEGER::                I
        INTEGER, INTENT(IN)::    N_ITERS
        REAL(DP)::               YMC_SEMI_CIRC
        REAL(DP)::               MY_FUNCT
        REAL(DP), INTENT(IN)::   RADIUS
        REAL(DP), ALLOCATABLE::  CASU_SIN(:)
        REAL(DP), ALLOCATABLE::  CASU_COS(:)
        REAL(DP), ALLOCATABLE::  CASU_DENSITY(:)
        REAL(DP), PARAMETER::    PiG = ACOS(-1.D0)
    
     ALLOCATE(CASU_SIN(N_ITERS))
     ALLOCATE(CASU_DENSITY(N_ITERS))
     ALLOCATE(CASU_COS(N_ITERS))

            CALL RANDOM_NUMBER(CASU_SIN)
            CASU_SIN = (PiG) * CASU_SIN          !It maps a random number generated in [0,1] to one in [0,PiG]
	    
            CASU_DENSITY = 0
	    
                 DO I=1,N_ITERS
                   CASU_SIN(I) = SIN(CASU_SIN(I))
                 END DO
		 
                 DO I=1,N_ITERS
                   CASU_COS(I) = SQRT(1 - CASU_SIN(I)*CASU_SIN(I))
                 END DO
		 
                 DO I=1,N_ITERS
                   CASU_DENSITY(I) = MY_FUNCT(RADIUS*CASU_COS(I), RADIUS * CASU_SIN(I))
                 END DO
	    
            YMC_SEMI_CIRC = (RADIUS * SUM(CASU_SIN * CASU_DENSITY)) / (SUM(CASU_DENSITY))   
    
    DEALLOCATE(CASU_SIN)
    DEALLOCATE(CASU_DENSITY)
    DEALLOCATE(CASU_COS)
    
    END FUNCTION YMC_SEMI_CIRC
    
    
    FUNCTION XMC_SEMI_CIRC(MY_FUNCT, RADIUS, N_ITERS)
    
      IMPLICIT NONE
    
        INTEGER, PARAMETER::     DP=KIND(0.D0)
        INTEGER::                I
        INTEGER, INTENT(IN)::    N_ITERS
        REAL(DP)::               XMC_SEMI_CIRC
        REAL(DP)::               MY_FUNCT
        REAL(DP), INTENT(IN)::   RADIUS
        REAL(DP), ALLOCATABLE::  CASU_SIN(:)
        REAL(DP), ALLOCATABLE::  CASU_COS(:)
        REAL(DP), ALLOCATABLE::  CASU_DENSITY(:)
        REAL(DP), PARAMETER::    PiG = ACOS(-1.D0)
    
     ALLOCATE(CASU_SIN(N_ITERS))
     ALLOCATE(CASU_DENSITY(N_ITERS))
     ALLOCATE(CASU_COS(N_ITERS))

            CALL RANDOM_NUMBER(CASU_SIN)
            CASU_SIN = (PiG) * CASU_SIN          !It maps a random number generated in [0,1] to one in [0,PiG]
	    
            CASU_DENSITY = 0
	    
                 DO I=1,N_ITERS
                   CASU_SIN(I) = SIN(CASU_SIN(I))
                 END DO
		 
                 DO I=1,N_ITERS
                   CASU_COS(I) = SQRT(1 - CASU_SIN(I)*CASU_SIN(I))
                 END DO
		 
                 DO I=1,N_ITERS
                   CASU_DENSITY(I) = MY_FUNCT(RADIUS*CASU_COS(I), RADIUS * CASU_SIN(I))
                 END DO
	    
            XMC_SEMI_CIRC = (RADIUS * SUM(CASU_COS * CASU_DENSITY)) / (SUM(CASU_DENSITY))   
    
    DEALLOCATE(CASU_SIN)
    DEALLOCATE(CASU_DENSITY)
    DEALLOCATE(CASU_COS)
    
    END FUNCTION XMC_SEMI_CIRC


    
    FUNCTION YMC_HALF_CIRCULAR_SECTOR(MY_FUNCT, RADIUS, RADIUS_SMALL, N_ITERS)
    
      IMPLICIT NONE
    
        INTEGER, PARAMETER::     DP=KIND(0.D0)
        INTEGER::                I
        INTEGER, INTENT(IN)::    N_ITERS
        REAL(DP)::               YMC_HALF_CIRCULAR_SECTOR
        REAL(DP)::               MY_FUNCT
        REAL(DP), INTENT(IN)::   RADIUS
        REAL(DP), INTENT(IN)::   RADIUS_SMALL
        REAL(DP), ALLOCATABLE::  CASU_X(:)
        REAL(DP), ALLOCATABLE::  CASU_Y(:)
        REAL(DP), ALLOCATABLE::  DENSITY_VEC(:)
        REAL(DP), PARAMETER::    PiG = ACOS(-1.D0)
    
     ALLOCATE(CASU_X(N_ITERS))
     ALLOCATE(CASU_Y(N_ITERS))
     ALLOCATE(DENSITY_VEC(N_ITERS))
     DO I=1,N_ITERS
       DO
            CALL RANDOM_NUMBER(CASU_X(I))
               CASU_X(I) = -RADIUS + (2 * RADIUS) * CASU_X(I)
            CALL RANDOM_NUMBER(CASU_Y(I))
               CASU_Y(I) = (RADIUS) * CASU_Y(I)
            IF(MY_FUNCT(RADIUS, RADIUS_SMALL, CASU_X(I), CASU_Y(I)).GT.0.D0) EXIT
       END DO
    END DO
	    
            DENSITY_VEC = 0.D0
	    
            DO I=1,N_ITERS
                DENSITY_VEC(I) = MY_FUNCT(RADIUS, RADIUS_SMALL, CASU_X(I), CASU_Y(I))
            END DO
	    
            YMC_HALF_CIRCULAR_SECTOR = (SUM(CASU_Y*DENSITY_VEC)) / (SUM(DENSITY_VEC))   
    
    DEALLOCATE(CASU_X)
    DEALLOCATE(CASU_Y)
    DEALLOCATE(DENSITY_VEC)
    
    
    END FUNCTION YMC_HALF_CIRCULAR_SECTOR

    FUNCTION IM_SEMI_CIRC(MY_FUNCT, RADIUS, N_ITERS)
    
      IMPLICIT NONE
    
        INTEGER, PARAMETER::     DP=KIND(0.D0)
        INTEGER::                I
        INTEGER, INTENT(IN)::    N_ITERS
        REAL(DP)::               IM_SEMI_CIRC
        REAL(DP)::               MY_FUNCT
        REAL(DP), INTENT(IN)::   RADIUS
        REAL(DP), ALLOCATABLE::  CASU_SIN(:)
        REAL(DP), ALLOCATABLE::  CASU_COS(:)
        REAL(DP), ALLOCATABLE::  CASU_DENSITY(:)
        REAL(DP), PARAMETER::    PiG = ACOS(-1.D0)
    
     ALLOCATE(CASU_SIN(N_ITERS))
     ALLOCATE(CASU_DENSITY(N_ITERS))
     ALLOCATE(CASU_COS(N_ITERS))

            CALL RANDOM_NUMBER(CASU_SIN)
            CASU_SIN = (PiG) * CASU_SIN          !It maps a random number generated in [0,1] to one in [0,PiG]
	    
            CASU_DENSITY = 0
	    
                 DO I=1,N_ITERS
                   CASU_SIN(I) = SIN(CASU_SIN(I))
                 END DO
		 
                 DO I=1,N_ITERS
                   CASU_COS(I) = SQRT(1 - CASU_SIN(I)*CASU_SIN(I))
                 END DO
		 
                 DO I=1,N_ITERS
                   CASU_DENSITY(I) = MY_FUNCT(RADIUS*CASU_COS(I), RADIUS * CASU_SIN(I))
                 END DO
	    
            IM_SEMI_CIRC = ((PiG*RADIUS**3.D0)/N_ITERS)*(SUM((CASU_COS**2.D0+(1-CASU_SIN)**2.D0)*CASU_DENSITY))   
    
    DEALLOCATE(CASU_SIN)
    DEALLOCATE(CASU_DENSITY)
    DEALLOCATE(CASU_COS)
    
    END FUNCTION IM_SEMI_CIRC


    
    
    
    FUNCTION DENSITY(X,Y)
	
        IMPLICIT NONE
	
        INTEGER, PARAMETER ::     DP = KIND(0.D0)
        REAL(DP) ::               DENSITY
        REAL(DP), INTENT(IN) ::   X,Y
		 	
            DENSITY = 2.D0                            
	    
    END FUNCTION DENSITY

    FUNCTION DENSITY_SUP(RADIUS,RADIUS_SMALL,X,Y)
	
        IMPLICIT NONE
	
        INTEGER, PARAMETER ::     DP = KIND(0.D0)
        REAL(DP) ::               DENSITY_SUP
        REAL(DP), INTENT(IN) ::   X, Y, RADIUS, RADIUS_SMALL
        
	
        IF(X**2.D0 + Y**2.D0 .LE. RADIUS**2.D0 .AND. X**2.D0 + Y**2.D0 .GT. RADIUS_SMALL**2.D0) THEN
           DENSITY_SUP = 2.D0
        ELSE
           DENSITY_SUP = 0
        END IF
	
    END FUNCTION DENSITY_SUP
  						
