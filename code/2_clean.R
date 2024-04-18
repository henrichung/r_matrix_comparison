library(tidyverse)
rm(list = ls())

# read in results files
result_files <- list.files("data", pattern = "results[0-9].*rds", full.names = TRUE)

# map function expressions to easier to read names
mapping_vector <- setNames(
  # labels
  c("stats", "proxy", "proxyC",
    "distances", "distances_matrix", "Rfast",
    "rcpp_serial", "rcpp_parallel_8", "fortran_serial",
    "fortran_parallel_1", "fortran_parallel_8", "c_fortran_serial",
    "c_fortran_parallel_1", "c_fortran_parallel_8", "GPUmatrix",
    "parallelDist_1", "parallelDist_8"),
  # expressions
  c("^stats::dist", "^proxy::dist", "^proxyC::dist",
    "^distances",
    "^as.matrix\\(distances::distances",  "^Rfast::Dist",
    "^rcpp_euclidean_distance.*1",
    "^rcpp_euclidean_distance.*8",
    "^\\.Fortran\\(\"fortran_euclidean_distance_serial",
    "^\\.Fortran\\(\"fortran_euclidean_distance_parallel.*1L",
    "^\\.Fortran\\(\"fortran_euclidean_distance_parallel.*8L",
    "^c_fortran_serial", "^c_fortran_parallel.*1", "^c_fortran_parallel.*8",
    "^GPUmatrix::dist",
    "^parallelDist::parDist.*1",
    "^parallelDist::parDist.*8")
)

# function to apply mapping
apply_mapping <- function(input_string) {
  match <- sapply(names(mapping_vector), function(pattern) grepl(pattern, input_string))
  if (any(match)) {
    return(mapping_vector[names(match)[match]])
  } else {
    return("other")
  }
}

# define order levels for plotting
expression_levels <- c(
  "stats", "proxy", "proxyC",
  "distances", "distances_matrix",
  "Rfast", "rcpp_serial", "rcpp_parallel_8",
  "fortran_serial", "fortran_parallel_1",
  "fortran_parallel_8", "c_fortran_serial",
  "c_fortran_parallel_1","c_fortran_parallel_8",
  "parallelDist_1", "parallelDist_8", "GPUmatrix")

# reformat bench table results
reformat_bench_table <- function(bench_table) {
  bench_tibble <- bench_table %>%
    rowwise() %>%
    mutate(expression = paste0(deparse(expression), collapse = "")) %>%
    mutate(expression = apply_mapping(expression)) %>%
    filter(expression != "fortran_parallel_1") %>%
    mutate(expression = factor(expression, levels = expression_levels)) %>%
    mutate(n_vectors = as.numeric(n_vectors),
           min = as.numeric(min),
           median = as.numeric(median),
           `itr/sec` = as.numeric(`itr/sec`),
           mem_alloc = as.numeric(mem_alloc),
           `gc/sec` = as.numeric(`gc/sec`),
           total_time = as.numeric(total_time)) %>%
    select(-c(result)) 
  # does not work within mutate
  bench_tibble$sd <- unlist(purrr::map(bench_tibble$time, sd))
  return(bench_tibble)
}

# read in results files and apply reformatting
results_list <- result_files %>%
  lapply(readRDS) %>%
  lapply(reformat_bench_table)
names(results_list) <- gsub("data/results|\\.rds", "", result_files)

# convert to csv
results_tibble <- bind_rows(results_list, .id = "run") %>%
  select(-c("memory", "time", "gc"))
write_csv(results_tibble, "data/distance_timings.csv")

# Matrix multiplication results
results_mat <- readRDS("data/results_matrix_multiplication.rds")

# map function expressions to easier to read names
mapping_vector <- setNames(
  c("%*%", "crossprod", "Rfast", "Matrix", "GPUmatrix"),
  c("^vectors_mat", "^crossprod", "Rfast", "^Matrix","^GPUmatrix")
)

# reformat matrix multiplication results
results_mat_df <- results_mat %>%
  rowwise() %>%
  mutate(expression = paste0(deparse(expression), collapse = "")) %>%
  mutate(expression = apply_mapping(expression)) %>%
  mutate(median_label = case_when(
    n_vectors == "10" ~ paste0(signif(median * 1000000, 2), "ns"),
    n_vectors == "100" ~ paste0(signif(median * 1000000, 2), "ns"),
    n_vectors == "1000" ~ paste0(signif(median * 1000, 2), "ms"))) %>%
  group_by(n_vectors) %>%
  mutate(rank = row_number(median)) %>%
  select(-c("memory", "time", "gc"))
# write to file
write_csv(results_mat_df, "data/matmult_timings.csv")