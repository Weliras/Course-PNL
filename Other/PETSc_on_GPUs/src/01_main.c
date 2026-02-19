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
