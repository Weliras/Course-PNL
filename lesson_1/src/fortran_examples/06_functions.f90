!-------------------------------
! File: functions_example.f90
!-------------------------------
PROGRAM test_functions
INTEGER :: n, result

    ! Input value
    n = 7
    ! Call the function
    result = square(n)

    PRINT *, 'The square of', n, 'is', result
END PROGRAM test_functions

! Function to compute square
INTEGER FUNCTION square(x)
INTEGER, INTENT(IN) :: x

    square = x * x
END FUNCTION square
