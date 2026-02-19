## Step-by-step tutorial

Tutorial made for IT4I Karolina cluster.

### Interactive job

1. **Allocate GPU node**
```bash
salloc -A <account> -p qgpu_exp
 ```
2. **Load system modules**

```bash
module load intel-compilers/2025.2.0
module load imkl
module load impi/2021.16.1-intel-compilers-2025.2.0
module load CUDA/12.4.0
```

3. **Compile GPU-aware PETSC**

```bash
cd /mnt/proj1/<account>/pta0054/PNK/ # switch to your working directory

git clone -b release https://gitlab.com/petsc/petsc.git # clone petsc git repo
cd petsc

./configure   --with-cc=/apps/all/impi/2021.16.1-intel-compilers-2025.2.0/mpi/2021.16/bin/mpicc   --with-cxx=/apps/all/impi/2021.16.1-intel-compilers-2025.2.0/mpi/2021.16/bin/mpicxx   --with-fc="/apps/all/impi/2021.16.1-intel-compilers-2025.2.0/mpi/2021.16/bin/mpiifort -f90=ifx"  --with-cuda=1 --with-mpi=1 --with-cuda-aware-mpi=1
make all -j16
make check

```

you should get output:

```bash
Running PETSc check examples to verify correct installation
Using PETSC_DIR=/mnt/proj1/open-36-34/pta0054/PNK/petsc and PETSC_ARCH=arch-linux-c-debug
C/C++ example src/snes/tutorials/ex19 run successfully with 1 MPI process
C/C++ example src/snes/tutorials/ex19 run successfully with 2 MPI processes
C/C++ example src/snes/tutorials/ex19 run successfully with CUDA
Fortran example src/snes/tutorials/ex5f run successfully with 1 MPI process
Completed PETSc check examples
```

4. **Compile and run your code**

Set your env. vars:
```bash
export PETSC_DIR=/mnt/proj1/<account>/pta0054/PNK/petsc
export PETSC_ARCH=arch-linux-c-debug
export LD_LIBRARY_PATH=${PETSC_DIR}/${PETSC_ARCH}/lib:$LD_LIBRARY_PATH
```

Prepare your code:
```c
#include <petscvec.h>
#include <stdio.h>

int main(int argc, char **argv) {
    PetscErrorCode ierr;
    Vec            x;
    PetscInt       n = 10;
    PetscScalar    norm;
    PetscMPIInt    rank;

    ierr = PetscInitialize(&argc, &argv, NULL, NULL); if (ierr) return ierr;
    MPI_Comm_rank(PETSC_COMM_WORLD, &rank);

    // Create vector
    ierr = VecCreate(PETSC_COMM_WORLD, &x);CHKERRQ(ierr);
    ierr = VecSetSizes(x, PETSC_DECIDE, n);CHKERRQ(ierr);
    ierr = VecSetFromOptions(x);CHKERRQ(ierr); // allows -vec_type cuda

    // Set all entries to 1.0
    ierr = VecSet(x, 1.0);CHKERRQ(ierr);

    // Compute 2-norm
    ierr = VecNorm(x, NORM_2, &norm);CHKERRQ(ierr);
    if (rank == 0) printf("Vector 2-norm: %g\n", (double)norm);

    ierr = VecDestroy(&x);CHKERRQ(ierr);
    ierr = PetscFinalize();CHKERRQ(ierr);

    return 0;
}
```

Compile your code:
```bash
cd ..
mpicc -o 01_main 01_main.c -I$PETSC_DIR/include -I$PETSC_DIR/$PETSC_ARCH/include -L$PETSC_DIR/$PETSC_ARCH/lib -lpetsc
```

Run your code (on karolina isn't gpu-aware intel mpi):
```bash
mpirun -np 2 ./01_main -vec_type cuda -use_gpu_aware_mpi 0
```