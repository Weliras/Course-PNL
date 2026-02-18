# Lesson 1.5: Arrays

## Overview

This lesson introduces arrays in Fortran, which are collections of elements of the same type stored under a single name. Arrays are essential for handling multiple values efficiently.

- Declaring arrays
- Array indexing
- Array operations
- Multi-dimensional arrays
- Array intrinsic functions

## 1. Declaring arrays

Arrays must be declared with a type and size.

### a, Fixed-size array

```fortran
PROGRAM fixed_array
INTEGER :: numbers(5)   ! Array of 5 integers

    numbers = (/1, 2, 3, 4, 5/)
    PRINT *, 'Numbers:', numbers
END PROGRAM fixed_array
```

### b, Partially initialized array

```fortran
PROGRAM partial_array
INTEGER :: nums(5)

    nums(1:3) = (/10, 20, 30/)
    PRINT *, 'Nums:', nums
END PROGRAM partial_array
```

### c, Array with automatic size

```fortran
PROGRAM auto_array
INTEGER, DIMENSION(:), ALLOCATABLE :: arr

    ALLOCATE(arr(4))
    arr = (/5, 10, 15, 20/)
    PRINT *, 'Array:', arr
    DEALLOCATE(arr)
END PROGRAM auto_array

```


## 2. Array Indexing

Array elements are accessed by index, **starting from 1** by default.

```fortran
PROGRAM array_index
INTEGER :: a(3)

    a = (/100, 200, 300/)
    PRINT *, 'First element:', a(1)
    PRINT *, 'Last element:', a(3)
END PROGRAM array_index
```

- Slice notation: `a(1:2)` gives a subarray with the first two elements.
- Step size: `a(1:3:2)` selects elements 1 and 3.

## 3. Array operations

Fortran allows element-wise operations on arrays.

```fortran
PROGRAM array_operations
INTEGER :: x(3), y(3), z(3)

    x = (/1, 2, 3/)
    y = (/4, 5, 6/)
    z = x + y       ! Element-wise addition
    PRINT *, 'z =', z
END PROGRAM array_operations
```
- Other operations: -, *, / (element-wise multiplication/division).

## 4. Multi-Dimensional Arrays

Arrays can have two or more dimensions (matrices, tensors).

```fortran
PROGRAM matrix_example
INTEGER :: mat(2,3)

    mat = RESHAPE((/1,2,3,4,5,6/), (/2,3/))
    PRINT *, 'Matrix:'
    PRINT *, mat
END PROGRAM matrix_example
```

- Accessing elements: `mat(1,2)` → row 1, column 2
- Slicing rows or columns: `mat(:,2)` → entire 2nd column


## 5. Array Intrinsic Functions

Fortran provides built-in functions to work with arrays.

| Function       | Description                      |
| -------------- | -------------------------------- |
| `SIZE(arr)`    | Returns the number of elements   |
| `LBOUND(arr)`  | Returns the lower bound of array |
| `UBOUND(arr)`  | Returns the upper bound of array |
| `SUM(arr)`     | Sum of elements                  |
| `PRODUCT(arr)` | Product of elements              |
| `MAXVAL(arr)`  | Maximum value                    |
| `MINVAL(arr)`  | Minimum value                    |

```fortran
PROGRAM array_intrinsics
INTEGER :: arr(4) = (/3, 7, 1, 5/)

    PRINT *, 'Size:', SIZE(arr)
    PRINT *, 'Sum:', SUM(arr)
    PRINT *, 'Max:', MAXVAL(arr)
    PRINT *, 'Min:', MINVAL(arr)
END PROGRAM array_intrinsics
```

- Accessing elements: `mat(1,2)` → row 1, column 2
- Slicing rows or columns: `mat(:,2)` → entire 2nd column