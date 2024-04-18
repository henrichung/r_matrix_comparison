#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix jaccard_distance(NumericMatrix mat) {
  int nrow = mat.nrow();
  int ncol = mat.ncol();
  NumericMatrix out(nrow, nrow); // Output is a symmetric matrix of nrow x nrow
  
  for (int i = 0; i < nrow; i++) {
    for (int j = i; j < nrow; j++) {
      if (i == j) {
        out(i, j) = 0.0; // Distance to itself is always 0
        continue;
      }
      
      double union_count = 0.0;
      double intersection_count = 0.0;
      
      for (int k = 0; k < ncol; k++) {
        bool a = mat(i, k) > 0;
        bool b = mat(j, k) > 0;
        
        if (a || b) {
          union_count += 1.0;
          if (a && b) {
            intersection_count += 1.0;
          }
        }
      }
      
      double jaccard_dist = (union_count - intersection_count) / union_count;
      out(i, j) = jaccard_dist;
      out(j, i) = jaccard_dist; // Fill both (i, j) and (j, i) since the matrix is symmetric
    }
  }
  return out;
}
