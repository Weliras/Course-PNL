# Lesson 1.0: Intel oneAPI HPC toolkit
The Intel oneAPI HPC Toolkit is a specialized software suite designed for high-performance computing (HPC) applications. It provides a set of tools, libraries, and compilers to help developers optimize applications for Intel CPUs, GPUs, and other accelerators, with a focus on scientific computing, simulations, and data-intensive workloads.

It contains:
1. **Compilers**
   - `icx` - C compiler
   - `icpx` - C++ compiler
   - `ifx` - modern **Fortran** compiler
2. **Libraries**
   - **Math Kernel Library (oneMKL)** - **BLAS** (Basic Linear Algebra Subprograms), **LAPACK** (Linear Algebra Package)
   - Data Analytics Library (oneDAL) - classification, regression, clustering algorithms
   - Threading Building Blocks (oneTBB) – parallel programming support for multicore CPUs
   - MPI and OpenMP support

   
## Step-by-step instalation for WSL Ubuntu

[Official Intel guide](https://www.intel.com/content/www/us/en/docs/oneapi/installation-guide-linux/2023-0/apt.html)

1. **Register Intel package repository**
```bash
wget -O- https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB | gpg --dearmor | sudo tee /usr/share/keyrings/oneapi-archive-keyring.gpg > /dev/null
echo "deb [signed-by=/usr/share/keyrings/oneapi-archive-keyring.gpg] https://apt.repos.intel.com/oneapi all main" | sudo tee /etc/apt/sources.list.d/oneAPI.list
```
2. **Install Intel oneAPI HPC toolkit**
```bash
sudo apt update
sudo apt install intel-hpckit
```
After installation, load environment variables:

```bash
source /opt/intel/oneapi/setvars.sh # needs to be done in every new session/terminal
```
⚠️ This must be done in every new terminal session unless automated.

3. [OPTIONAL] Load environment automatically

Open
```bash
sudo nano /etc/profile.d/intel_oneapi.sh
```
add:
```bash
source /opt/intel/oneapi/setvars.sh > /dev/null 2>&1
```
And restart your terminal.

4. **Check installation**
```bash
icx --version   # Intel C/C++ compiler
ifx --version   # Intel Fortran compiler
```

## Compiling C and Fortran with Intel Compilers

### C program
Example file: `hello.c`
```c
#include <stdio.h>

int main() {
    printf("Hello from Intel C!\n");
    return 0;
}
```

Compile:
```bash
icx hello.c -o hello_c
```

Run:
```bash
./hello_c
```

### Fortran program
Example file: `hello.f90`
```fortran
program hello
    implicit none
    print *, "Hello from Intel Fortran!"
end program hello
```

Compile:
```bash
ifx hello.f90 -o hello_f
```

Run:
```bash
./hello_f
```

### Useful `ifx` compiler flags

```bash
ifx --help
```

- Optimization levels: `-O0`, `-O1`, `-O2`, `-O3`, ... 
- Debugging:
  - debug information: `-g`
  - enable runtime checks (array bounds, uninitialized variables): `-check all`
  - print call stack on runtime errors: `-traceback`
- Parallelization / Vectorization:
  - detailed optimization report: `-qopt-report`
  - report vectorization info during compilation: `-vec-report`
  - enable OpenMP support: `-qopenmp`
  - enable automatic parallelization: `-parallel`
- Linking / Libraries
  - link Intel oneMKL libraries (BLAS/LAPACK): `-mkl`
  - add library search path: `-L<path>`
  - link with specific library: `-l<lib>`

### Using Intel Compilers with CMAKE
If your project uses CMake, you must specify Intel compilers during configuration.
From project root:
```bash
cmake -S . -B build \
  -DCMAKE_C_COMPILER=icx \
  -DCMAKE_Fortran_COMPILER=ifx
```

### Using Intel Compilers with CLion (WSL)
- Can be provided if someone wants to use CLion
- ?

