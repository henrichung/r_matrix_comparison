library(tidyverse)
rm(list = ls())
# Define Jaccard distance function
jaccard_distance <- function(set1, set2) {
  intersect_size <- length(intersect(set1, set2))
  union_size <- length(union(set1, set2))
  return(1 - (intersect_size / union_size))
}
# Initialize variables to store time and memory
time_vec <- numeric()
mem_vec <- numeric()
# Loop to increase the number of vectors
n_lengths <- c(10, 100, 1000, 2000, 3000)
for (n_vectors in n_lengths) {
  message(Sys.time(), " ", n_vectors)
  # Generate a list of random binary vectors of length 1400
  vectors <- lapply(1:n_vectors, function(x) sample(0:1, 1400, replace = TRUE))
  # Measure memory before computation
  gc_before <- gc()
  mem_before <- gc_before[13]
  # Measure time before computation
  ptm <- proc.time()
  # Calculate Jaccard distance for all pairs
  dist_list <- list(); counter = 1
  for (i in 1:(n_vectors-1)) {
    for (j in (i+1):n_vectors) {
      dist_list[[counter]] <- jaccard_distance(vectors[[i]], vectors[[j]])
      counter <- counter + 1
    }
  }
  # Measure time after computation
  time_taken <- (proc.time() - ptm)[3]
  time_vec <- c(time_vec, time_taken)
  # Measure memory after computation
  mem_used <- pryr::object_size(dist_list)
  mem_vec <- c(mem_vec, mem_used)
}

# Print time and memory vectors
print(paste("Time taken for each iteration: ", toString(time_vec)))
print(paste("Memory used for each iteration: ", toString(mem_vec)))

# plot time and memory
plot_df <- data.frame(n_lengths, time_vec, mem_vec)
plot_df <- plot_df %>% gather(key = "variable", value = "value", -n_lengths) %>%
  mutate(variable_label = ifelse(variable == "time_vec", "Time (s)", "Memory (bytes)")) %>%
  mutate(method = "jaccard_loop") %>%
  mutate(variable_label = factor(variable_label, levels = c("Time (s)", "Memory (bytes)")))
write_csv(plot_df, "data/1_jaccard_results.csv")

p1 <- plot_df %>%
    ggplot(aes(x = n_lengths, y = value, color = variable)) + 
    geom_line() + 
    theme_bw() + 
    labs(title = "Jaccard loop") +
    xlab("Number of Profiles") + ylab("Seconds") +
    facet_wrap(~variable_label, ncol = 2, scales = "free") +
    guides(color = FALSE)

ggsave("outputs/p1.png", p1, width = 10, height = 5)

# time for one
microbenchmark::microbenchmark(jaccard_distance(vectors[[1]], vectors[[2]]))

#################
