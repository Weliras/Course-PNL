cmake_minimum_required(VERSION 3.22)
set(CMAKE_C_STANDARD 11)

project(lesson_1 LANGUAGES C Fortran)


# Put all executables into build/bin
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# C examples
add_executable(c_01_hello src/c_examples/01_hello.c)
add_executable(c_02_variables src/c_examples/02_variables.c)
add_executable(c_03_operations src/c_examples/03_operations.c)
add_executable(c_04_conditionals_and_loops src/c_examples/04_conditionals_and_loops.c)
add_executable(c_05_arrays src/c_examples/05_arrays.c)
add_executable(c_06_functions src/c_examples/06_functions.c)

# Fortran examples
add_executable(f_01_hello src/fortran_examples/01_hello.f90)
add_executable(f_02_variables src/fortran_examples/02_variables.f90)
add_executable(f_03_operations src/fortran_examples/03_operations.f90)
add_executable(f_04_conditionals_and_loops src/fortran_examples/04_conditionals_and_loops.f90)
add_executable(f_05_arrays src/fortran_examples/05_arrays.f90)
add_executable(f_06_functions src/fortran_examples/06_functions.f90)
