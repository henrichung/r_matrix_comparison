#include <R.h>
#include <Rinternals.h>

extern void fortran_euclidean_distance_parallel_(double *matrix, int *rows, int *cols, int *num_threads, double *distances);

SEXP C_fortran_euclidean_distance_parallel(SEXP Rmatrix, SEXP Rrows, SEXP Rcols, SEXP RnumThreads, SEXP Rdistances) {
    // Ensure the R objects are of the correct type
    if(TYPEOF(Rmatrix) != REALSXP || TYPEOF(Rdistances) != REALSXP) {
        error("Invalid matrix type.");
    }
    if(TYPEOF(Rrows) != INTSXP || TYPEOF(Rcols) != INTSXP || TYPEOF(RnumThreads) != INTSXP) {
        error("Invalid integer argument.");
    }

    // Extract integers from SEXP
    int rows = INTEGER(Rrows)[0];
    int cols = INTEGER(Rcols)[0];
    int num_threads = INTEGER(RnumThreads)[0];

    // Ensure Rdistances is a matrix with appropriate dimensions
    if(!isMatrix(Rdistances) || nrows(Rdistances) != rows || ncols(Rdistances) != rows) {
        error("Rdistances must be a square matrix with dimensions equal to the number of rows in Rmatrix.");
    }
    
    // Call the Fortran subroutine directly with the arguments
    fortran_euclidean_distance_parallel_(REAL(Rmatrix), &rows, &cols, &num_threads, REAL(Rdistances));

    // Since the distances matrix is modified in-place, just return it directly
    return Rdistances;
}
