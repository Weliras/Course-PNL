PROGRAM main
INTEGER :: i

    DO i = 1, 10
        ! stop after reaching 8th iteration
        IF (i >= 8) EXIT

        IF (MOD(i,2) == 0) THEN
            PRINT *, i, 'is even'
        ELSE
            PRINT *, i, 'is odd'
        END IF

    END DO
END PROGRAM main
