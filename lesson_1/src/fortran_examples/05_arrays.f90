PROGRAM main
INTEGER :: i
INTEGER :: arr(5)
INTEGER :: sum

    ! Initialize array
    arr = (/1, 2, 3, 4, 5/)

    ! Compute sum
    sum = 0
    DO i = 1, SIZE(arr)
        sum = sum + arr(i)
    END DO
    PRINT *, 'Sum of array elements:', sum

    ! Multiply each element by 2
    DO i = 1, SIZE(arr)
        arr(i) = arr(i) * 2
    END DO
    PRINT *, 'Array after multiplying by 2:', arr
END PROGRAM main
