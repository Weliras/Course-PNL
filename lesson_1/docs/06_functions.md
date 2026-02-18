# Lesson 1.6: Functions, Subprograms, Subroutines

## Overview

This lesson introduces functions, subprograms, and subroutines in Fortran, which are reusable blocks of code. These structures help break programs into manageable parts, promote code reuse, and simplify debugging.

- Functions: return a value
- Subroutines: perform tasks without returning a value
- Calling subprograms
- Passing arguments
- Scope and local variables

## 1. Functions

A function performs a task and returns a single value. Functions are called within expressions.

Syntax:
```fortran
<type> FUNCTION <name> ( <arguments> )
<declarations>
    <body>
    <name> = expression   ! Assign result to function name
    RETURN
END FUNCTION <name>
```

Example of function to compute a square:

```fortran
PROGRAM test_function
INTEGER :: n, result

    n = 5
    result = square(n)
    PRINT *, 'Square of', n, 'is', result
END PROGRAM test_function

INTEGER FUNCTION square(x)
INTEGER, INTENT(IN) :: x

    square = x * x
    RETURN
END FUNCTION square
```
- `INTENT(IN)` indicates `x` is an input only.
- The function returns the value by assigning it to its name (`square`).


## 2. Subroutines

A subroutine performs tasks but does not return a value. It can modify variables passed as arguments.

Syntax:
```fortran
SUBROUTINE <name> ( <arguments> )
<declarations>
    <body>
    RETURN
END SUBROUTINE <name>
```

Example of subroutine to swap two numbers:
```fortran
PROGRAM test_subroutine
INTEGER :: a, b

    a = 10
    b = 20
    PRINT *, 'Before swap: a =', a, ', b =', b
    CALL swap(a, b)
    PRINT *, 'After swap: a =', a, ', b =', b
END PROGRAM test_subroutine

SUBROUTINE swap(x, y)
INTEGER, INTENT(INOUT) :: x, y
INTEGER :: temp

    temp = x
    x = y
    y = temp
    RETURN
END SUBROUTINE swap
```

- `INTENT(INOUT)` allows the subroutine to modify the variables.

## 3. Calling Subprograms

- Functions are used in expressions:
```fortran
z = square(4) + square(3)
```
- Subroutines are called using `CALL`:
```fortran
CALL swap(a, b)
```

## 4. Passing Arguments

Arguments can be passed in three ways:

| INTENT  | Meaning                          |
| ------- | -------------------------------- |
| `IN`    | Input only (cannot be modified)  |
| `OUT`   | Output only (initialized inside) |
| `INOUT` | Both input and output            |

Example: Function with multiple arguments
```fortran
PROGRAM test_sum
INTEGER :: result
    result = add(3, 7)
    PRINT *, 'Sum =', result
END PROGRAM test_sum

FUNCTION add(a, b)
INTEGER, INTENT(IN) :: a, b
INTEGER :: add

    add = a + b
END FUNCTION add
```


## 5. Scope and Local Variables

- Variables declared inside a function or subroutine are **local** by default.
- Local variables **do not affect** variables with the same name outside the subprogram.
- Use `SAVE` if you want a local variable to **retain its value** between calls.

Example: Persistent variable with `SAVE`
```fortran
PROGRAM test_counter
    CALL counter()
    CALL counter()
    CALL counter()
END PROGRAM test_counter


SUBROUTINE counter()
INTEGER :: count
SAVE count
    count = count + 1
    PRINT *, 'Call number:', count
END SUBROUTINE counter
```

- Calling `counter()` multiple times will increment `count` each call.
