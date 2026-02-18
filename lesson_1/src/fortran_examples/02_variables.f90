PROGRAM main
! Declarations
IMPLICIT NONE   ! In Fortran, by default the type of variables whose names start with I...N is INTEGER,
                ! and REAL otherwise. It is a legacy of the past and it is strongly advised
                ! to put an implicit none statement in your program and in each module. That statement was added in the Fortran 90 standard.
INTEGER :: age
REAL :: height
COMPLEX :: z
CHARACTER(LEN=10) :: name
LOGICAL :: isStudent

    ! Initializations
    age = 25
    height = 1.75
    z = (3.0, 4.0)
    name = 'Alice'
    isStudent = .TRUE.

    ! Printing
    PRINT *, 'Age:', age
    PRINT *, 'Height:', height
    PRINT *, 'The complex number z is:', z
    PRINT *, 'Name:', name
    PRINT *, 'Is Student:', isStudent
END PROGRAM main