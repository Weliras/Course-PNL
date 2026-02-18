# Lesson 1.2: Variables

## Overview

This example demonstrates how to declare, initialize, and print variables in **Fortran**.

- Variable types
- Declaration of variables
- Initialization of variables
- Printing of variables

## 1. Variable types

Fortran has several basic variable types:

| Type        | Example Values       | Description                       |
|------------|-------------------|-----------------------------------|
| `INTEGER`  | 0, 5, -10         | Whole numbers                     |
| `REAL`     | 3.14, -0.5        | Floating-point numbers            |
| `DOUBLE PRECISION` | 3.1415926535  | Higher-precision floating-point  |
| `COMPLEX`         | (1.0, 2.0), (3.0, -4.0) | Complex numbers with real and imaginary parts |
| `CHARACTER` | 'A', 'Hello'      | Single or multiple characters    |
| `LOGICAL`  | .TRUE., .FALSE.    | Boolean values                    |

Fortran enforces strong typing:
- setting `IMPLICIT NONE` -> all object's type must be declared and avoids legacy implicit typing
- Three numeric intrinsic types: `INTEGER`, `REAL`, `COMPLEX`
- Two  non-numeric intrinsic types: `CHARACTER`, `LOGICAL`
- Other types are non-intrinsic and are derived from the intrinsic types

## 2. Declaration of variables
Variables must be declared before use and before executable statements. The syntax is:

```fortran
INTEGER :: age
REAL :: height
COMPLEX :: z
CHARACTER(LEN=10) :: name
LOGICAL :: isStudent
```

- `::` separates the type from the variable name
- `LEN` specifies the length of character strings

Names are used for referencing many Fortran entities - e.g. variables, program units, constants, ...
Variable names constraints:
- between 1 and 63 alphanumeric (a – z, A – Z, 0 – 9, _) characters
- first character must be a letter
- no reserved words in Fortran → but do not use keywords (`PROGRAM`, `INTEGER`, `.AND.`, ...) anyway



## 3. Initialization of Variables

Variables can be assigned values after declaration:

```fortran
age = 25
height = 1.75
z = (3.0, 4.0)
name = 'Alice'
isStudent = .TRUE.
```

You can also combine declaration and initialization in modern Fortran:

```fortran
INTEGER :: age = 25
REAL :: height = 1.75
COMPLEX :: z = (3.0, 4.0)
CHARACTER(LEN=10) :: name = 'Alice'
LOGICAL :: isStudent = .TRUE.
```

## 4. Printing of variables

The `PRINT` statement displays variable values:

```fortran
PRINT *, 'Age:', age
PRINT *, 'Height:', height
PRINT *, 'The complex number z is:', z
PRINT *, 'Name:', name
PRINT *, 'Is Student:', isStudent
```


The complete program:

```fortran
PROGRAM main
! Declaration
INTEGER :: age
REAL :: height
COMPLEX :: z
CHARACTER(LEN=10) :: name
LOGICAL :: isStudent
    
    ! Initialization
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
```