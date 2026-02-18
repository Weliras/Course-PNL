# Parallel Numerical Libraries
- Year: 2025/2026
- Edison link: [link](https://edison.sso.vsb.cz/cz.vsb.edison.edu.study.prepare.web/SubjectVersion.faces?version=9600-1011/01&subjectBlockAssignmentId=322948&studyFormId=1&studyPlanId=20606&locale=cs&back=true)
## Anotation
The course introduces students to effective parallel implementations of numerical methods and their use for parallel solving of very large tasks. Such tasks can no longer be calculated on standard personal computers and require the use of supercomputers. Students will encounter the most commonly used, predominantly open-source libraries for numerical calculations. The exercises will focus on implementation in C/C++ or FORTRAN.

## Course outline
1. Introduction to the FORTRAN programming language
2. BLAS Specification (Basic linear algebra subroutines)
   - Existing implementations (MKL, AOCL, cuBLAS)
3. Methods for solving dense systems of linear equations and eigenvalue problems 
   - Storage of dense matrices 
   - Existing implementations (LAPACK, ScaLAPACK, MKL, AOCL, cuSOLVER)
4. Methods for solving large sparse systems of linear equations and eigenvalue problems
   - Storage of sparse matrices (CSR, CSC, …)
   - Existing implementations (Intel MKL, AOCL, cuSOLVER, cuSPARSE)

**Semester project 1**

5. Advanced libraries for scientific computing
   - PETSc + toolkits/libraries
   - Trilinos
6. Tools for processing large data sets
   - Intel DAAL
7. Python and parallel numerical libraries 

**Semester project 2**

## Conditions for completing the course
- Two semestral projects: max 2x 20 points
- Exam: max 60 points (defense of final project)