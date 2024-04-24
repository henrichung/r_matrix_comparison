library(GPUmatrix)
# clear workspace
rm(list = ls())
# set random seed
set.seed(123)

# use conda environment for tensorflow
tensorflow::use_condaenv("r_matrix_comparison")

# Define a function to run the benchmark for a given number of vectors
benchmark_dist <- function(n_vectors, n_iterations) {
  # create random matrix
  vectors_mat <- matrix(as.numeric(sample(0:9, n_vectors * n_vectors, replace = TRUE)), nrow = n_vectors) 
  # create gpu matrix for torch
  gpu_matrix_torch <- GPUmatrix::gpu.matrix(vectors_mat, type = "torch")
  bench::mark(
    GPUmatrix::dist(gpu_matrix_torch, method = "euclidean"),
    check = FALSE,
    iterations = n_iterations
  )
}

# Use bench::press to run the benchmarks across different size matrices
s <- Sys.time()
small_results <- bench::press(n_vectors = c(2:10),{benchmark_dist(n_vectors, n_iterations = 1000)})
message("Time taken: ", Sys.time() - s)

s <- Sys.time()
medium_results <- bench::press(n_vectors = seq(10, 100, 10),{benchmark_dist(n_vectors, n_iterations = 1000)})
message("Time taken: ", Sys.time() - s)


s <- Sys.time()
large_results <- bench::press(n_vectors = seq(100, 1000, 100),{benchmark_dist(n_vectors, n_iterations = 1000)})
message("Time taken: ", Sys.time() - s)


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

# run benchmark for extra large dataset
s <- Sys.time()
extra_large_results <- bench::press(n_vectors = seq(1000, 10000, 1000), {benchmark_dist_xl(n_vectors, n_iterations = 1)})
message("Time taken: ", Sys.time() - s)

