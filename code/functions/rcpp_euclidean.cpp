#include <Rcpp.h>
#ifdef _OPENMP
#include <omp.h>
#endif

// [[Rcpp::export]]
Rcpp::NumericMatrix rcpp_euclidean_distance(Rcpp::NumericMatrix mat, int num_threads) {
  int nrow = mat.nrow();
  int ncol = mat.ncol();
  Rcpp::NumericMatrix out(nrow, nrow);
  
#ifdef _OPENMP
  omp_set_num_threads(num_threads); // Set the number of threads to use
#endif

// #ifdef _OPENMP
// Rcpp::Rcout << "OpenMP is enabled. Max threads: " << omp_get_max_threads() << std::endl;
// #else
// Rcpp::Rcout << "OpenMP is not enabled.\n";
// #endif


  #pragma omp parallel for schedule(dynamic)
  for (int i = 0; i < nrow; i++) {
    // int thread_id = omp_get_thread_num();
    // Rcpp::Rcout << "Thread " << thread_id << " is working on iteration " << i << std::endl;
    for (int j = i; j < nrow; j++) {
      if (i == j) {
        out(i, j) = 0.0;
        continue;
      }
      
      double sum_sq_diff = 0.0;
      for (int k = 0; k < ncol; k++) {
        double diff = mat(i, k) - mat(j, k);
        sum_sq_diff += diff * diff;
      }
      
      double euclidean_dist = sqrt(sum_sq_diff);
      out(i, j) = euclidean_dist;
      out(j, i) = euclidean_dist;
    }
  }
  return out;
}
