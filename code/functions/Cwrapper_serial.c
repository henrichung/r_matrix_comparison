#include <R.h>
#include <Rinternals.h>

extern void fortran_euclidean_distance_serial_(double *matrix, int *rows, int *cols, double *distances);

SEXP C_fortran_euclidean_distance_serial(SEXP Rmatrix, SEXP Rrows, SEXP Rcols, SEXP Rdistances) {
    // Ensure the R objects are of the correct type
    if(TYPEOF(Rmatrix) != REALSXP || TYPEOF(Rdistances) != REALSXP) {
        error("Invalid matrix type.");
    }
    if(TYPEOF(Rrows) != INTSXP || TYPEOF(Rcols) != INTSXP) {
        error("Invalid integer argument.");
    }

    // Extract integers from SEXP
    int rows = INTEGER(Rrows)[0];
    int cols = INTEGER(Rcols)[0];

    // Ensure Rdistances is a matrix with appropriate dimensions
    if(!isMatrix(Rdistances) || nrows(Rdistances) != rows || ncols(Rdistances) != rows) {
        error("Rdistances must be a square matrix with dimensions equal to the number of rows in Rmatrix.");
    }

    // Call the Fortran subroutine directly with the arguments
    fortran_euclidean_distance_serial_(REAL(Rmatrix), &rows, &cols, REAL(Rdistances));

    // Since the distances matrix is modified in-place, just return it directly
    return Rdistances;
}
