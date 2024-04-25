library(GPUmatrix)
# clear workspace
rm(list = ls())
# set random seed
set.seed(123)

# use conda environment for tensorflow
tensorflow::use_condaenv("r_matrix_comparison")

# Rcpp uses OpenMP for parallelization
Sys.setenv("PKG_CXXFLAGS"="-fopenmp")
Sys.setenv("PKG_LIBS"="-fopenmp")
# Load Rcpp function
Rcpp::sourceCpp("code/functions/rcpp_euclidean.cpp")

# load fortran function
dyn.load("code/functions/fortran_euclidean_distance_parallel.so")
dyn.load("code/functions/fortran_euclidean_distance_serial.so")

# load C wrapped fortan function
dyn.load("code/functions/Cwrapper_parallel.so")
dyn.load("code/functions/Cwrapper_serial.so")

# load cwrapped fortran function
c_fortran_parallel <- function(matrix, rows, cols, numThreads, distances) {
  .Call("C_fortran_euclidean_distance_parallel", matrix, rows, cols, numThreads, distances)
}
c_fortran_serial <- function(matrix, rows, cols, distances) {
  .Call("C_fortran_euclidean_distance_serial", matrix, rows, cols, distances)
}

# Define a function to run the benchmark for a given number of vectors
benchmark_dist <- function(n_vectors, n_iterations) {
  # create random matrix
  vectors_mat <- matrix(as.numeric(sample(0:9, n_vectors * n_vectors, replace = TRUE)), nrow = n_vectors)
  
  # create empty matrix to store distances for fortran functions
  distances <- matrix(0, nrow = nrow(vectors_mat), ncol = nrow(vectors_mat))
  
  # create gpu matrix for torch
  gpu_matrix_torch <- GPUmatrix::gpu.matrix(vectors_mat, type = "torch")

  # create sparse matrix for proxyC
  sparse_matrix <- Matrix::Matrix(vectors_mat, sparse = TRUE)
  
  bench::mark(
    stats::dist(vectors_mat, method = "euclidean"),
    proxy::dist(vectors_mat, method = "Euclidean"),
    proxyC::dist(vectors_mat, method = "euclidean"),
    distances::distances(vectors_mat),
    as.matrix(distances::distances(vectors_mat)), 
    Rfast::Dist(vectors_mat, method = "euclidean"),
    rcpp_euclidean_distance(vectors_mat, 1),
    rcpp_euclidean_distance(vectors_mat, 8),
    .Fortran("fortran_euclidean_distance_serial", as.double(vectors_mat), as.integer(nrow(vectors_mat)), as.integer(ncol(vectors_mat)), distances),
    .Fortran("fortran_euclidean_distance_parallel", as.double(vectors_mat), as.integer(nrow(vectors_mat)), as.integer(ncol(vectors_mat)), 1L, distances),
    .Fortran("fortran_euclidean_distance_parallel", as.double(vectors_mat), as.integer(nrow(vectors_mat)), as.integer(ncol(vectors_mat)), 8L, distances),
    c_fortran_serial(vectors_mat, as.integer(nrow(vectors_mat)), as.integer(ncol(vectors_mat)), distances),
    c_fortran_parallel(vectors_mat, as.integer(nrow(vectors_mat)), as.integer(ncol(vectors_mat)), 8L, distances),
    #GPUmatrix::dist(gpu_matrix_tensorflow, method = "euclidean"),
    GPUmatrix::dist(gpu_matrix_torch, method = "euclidean"),
    parallelDist::parDist(vectors_mat, method = "euclidean", threads = 1),
    parallelDist::parDist(vectors_mat, method = "euclidean", threads = 8),
    check = FALSE,
    iterations = n_iterations
  )
}

# Use bench::press to run the benchmarks across different size matrices
s <- Sys.time()
small_results <- bench::press(n_vectors = c(2:10),{benchmark_dist(n_vectors, n_iterations = 1000)})
message("Time taken: ", Sys.time() - s)
saveRDS(small_results, "data/results2_10.rds")
rm(small_results); gc()

s <- Sys.time()
medium_results <- bench::press(n_vectors = seq(10, 100, 10),{benchmark_dist(n_vectors, n_iterations = 1000)})
message("Time taken: ", Sys.time() - s)
saveRDS(medium_results, "data/results10_100.rds")
rm(medium_results); gc()

s <- Sys.time()
large_results <- bench::press(n_vectors = seq(100, 1000, 100),{benchmark_dist(n_vectors, n_iterations = 1000)})
message("Time taken: ", Sys.time() - s)
saveRDS(large_results, "data/results100_1000.rds")
rm(large_results); gc()

# Write a benchmark function taking only the fastest methods for the extra large dataset
benchmark_dist_xl <- function(n_vectors, n_iterations) {
  vectors_mat <- matrix(as.numeric(sample(0:9, n_vectors * n_vectors, replace = TRUE)), nrow = n_vectors)
  distances <- matrix(0, nrow = nrow(vectors_mat), ncol = nrow(vectors_mat))
  gpu_matrix_torch <- GPUmatrix::gpu.matrix(vectors_mat, type = "torch")
  bench::mark(
    Rfast::Dist(vectors_mat, method = "euclidean"),
    rcpp_euclidean_distance(vectors_mat, 8),
    .Fortran("fortran_euclidean_distance_parallel", as.double(vectors_mat), as.integer(nrow(vectors_mat)), as.integer(ncol(vectors_mat)), 8L, distances),
    GPUmatrix::dist(gpu_matrix_torch, method = "euclidean"),
    parallelDist::parDist(vectors_mat, method = "euclidean", threads = 8),
    check = FALSE,
    iterations = n_iterations
  )
}

benchmark_dist_xl <- function(n_vectors, n_iterations) {
  vectors_mat <- matrix(as.numeric(sample(0:9, n_vectors * n_vectors, replace = TRUE)), nrow = n_vectors)
  # distances <- matrix(0, nrow = nrow(vectors_mat), ncol = nrow(vectors_mat))
  gpu_matrix_torch <- GPUmatrix::gpu.matrix(vectors_mat, type = "torch")
  bench::mark(
    # Rfast::Dist(vectors_mat, method = "euclidean"),
    # rcpp_euclidean_distance(vectors_mat, 8),
    # .Fortran("fortran_euclidean_distance_parallel", as.double(vectors_mat), as.integer(nrow(vectors_mat)), as.integer(ncol(vectors_mat)), 8L, distances),
    GPUmatrix::dist(gpu_matrix_torch, method = "euclidean"),
    # parallelDist::parDist(vectors_mat, method = "euclidean", threads = 8),
    check = FALSE,
    iterations = n_iterations
  )
}

# run benchmark for extra large dataset
s <- Sys.time()
extra_large_results <- bench::press(n_vectors = seq(1000, 10000, 1000), {benchmark_dist_xl(n_vectors, n_iterations = 10)})
message("Time taken: ", Sys.time() - s)
saveRDS(extra_large_results, "data/results1000_10000.rds")

# bench matrix multiplication
benchmark_matrix_multiplication <- function(n_vectors) {
  vectors_mat <- matrix(as.numeric(sample(0:9, n_vectors * n_vectors, replace = TRUE)), nrow = n_vectors)
  t_vectors_mat <- t(vectors_mat)
  gpu_matrix_torch <- GPUmatrix::gpu.matrix(vectors_mat, type = "torch")
  t_gpu_matrix_torch <- GPUmatrix::gpu.matrix(t_vectors_mat, type = "torch")
  bench::mark(
    vectors_mat %*% t_vectors_mat,
    crossprod(vectors_mat, t_vectors_mat),
    Rfast::Crossprod(vectors_mat, t_vectors_mat),
    Matrix::crossprod(vectors_mat, t_vectors_mat),
    GPUmatrix::crossprod(gpu_matrix_torch, t_gpu_matrix_torch),
    check = FALSE,
    iterations = 10
  )
}

# Use bench::press to run the benchmarks across different configurations
results_matrix_multiplication <- bench::press(
  n_vectors = c(10, 100, 1000), # Different matrix lengths to test
  {
    benchmark_matrix_multiplication(n_vectors)
  }
)
saveRDS(results_matrix_multiplication, "data/results_matrix_multiplication.rds")

#  big dist example
set.seed(123)
amat <- matrix(rnorm(1e5), ncol = 10)
td   <- tempdir()
dir.create(td)
temp <- bigdist::bigdist(mat = amat, method = "euclidean", file = file.path(td, "tempfile"))
