PROGRAM main
IMPLICIT NONE
! --- Declarations ---
! Arithmetic operations (integer and real)
INTEGER :: a_int, b_int, sum, diff, prod, int_div
REAL :: x_real, y_real, exp_result
! Relational operations
INTEGER :: m, n
LOGICAL :: rel_result
! Logical operations
LOGICAL :: l1, l2, log_result
! Type conversions
INTEGER :: i
REAL :: r
DOUBLE PRECISION :: d
COMPLEX :: c
! Combined logical and arithmetic operation
INTEGER :: x_val, y_val
LOGICAL :: combined_check

    ! --- Executable statements ---

    ! Arithmetic operations
    a_int = 7
    b_int = 2
    sum = a_int + b_int
    diff = a_int - b_int
    prod = a_int * b_int
    int_div = a_int / b_int      ! Integer division

    x_real = 3.0
    y_real = 2.0
    exp_result = x_real ** y_real

    PRINT *, 'Sum:', sum
    PRINT *, 'Difference:', diff
    PRINT *, 'Product:', prod
    PRINT *, 'Integer Division:', int_div
    PRINT *, 'Exponentiation:', exp_result

    ! Relational operations
    m = 5
    n = 3

    rel_result = (m == n)
    PRINT *, 'm == n:', rel_result
    rel_result = (m /= n)
    PRINT *, 'm /= n:', rel_result
    rel_result = (m > n)
    PRINT *, 'm > n:', rel_result

    ! Logical operations
    l1 = .TRUE.
    l2 = .FALSE.

    log_result = l1 .AND. l2
    PRINT *, 'l1 AND l2:', log_result
    log_result = l1 .OR. l2
    PRINT *, 'l1 OR l2:', log_result
    log_result = .NOT. l1
    PRINT *, 'NOT l1:', log_result

    ! Type conversions
    i = 7
    r = REAL(i)        ! Convert integer to real
    d = DBLE(r)        ! Convert real to double precision
    c = CMPLX(r)       ! Convert real to complex

    PRINT *, 'Integer i:', i
    PRINT *, 'Converted to Real:', r
    PRINT *, 'Converted to Double Precision:', d
    PRINT *, 'Converted to Complex:', c

    ! Combined logical and arithmetic operation
    x_val = 10
    y_val = 5

    combined_check = ((x_val + y_val) > 12) .AND. (y_val /= 0) .AND. ((REAL(x_val) / REAL(y_val)) >= 4.3)
    PRINT *, 'Combined operation result:', combined_check

END PROGRAM main
