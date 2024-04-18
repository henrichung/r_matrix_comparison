library(tidyverse)
rm(list = ls())
# Initialize variables to store time and memory
time_vec <- numeric()
mem_vec <- numeric()
# Loop to increase the number of vectors
n_lengths <- c(10, 100, 1000, 2000, 3000)
for (n_vectors in n_lengths) {
  message(Sys.time(), " ", n_vectors)
  # Generate a list of random binary vectors of length 1400
  vectors_mat <- matrix(sample(0:1, 1400 * n_vectors, replace = TRUE), nrow = n_vectors)
  # Measure time before computation
  ptm <- proc.time()
  # Calculate Jaccard distance for all pairs
  dist <- dist(vectors_mat, method = "binary", upper = TRUE)
  dist_mat <- as.matrix(dist)
  # Measure time after computation
  time_taken <- (proc.time() - ptm)[3]
  time_vec <- c(time_vec, time_taken)
  # Measure memory after computation
  mem_used <- pryr::object_size(dist_mat)
  mem_vec <- c(mem_vec, mem_used)
}

# Print time and memory vectors
print(paste("Time taken for each iteration: ", toString(time_vec)))
print(paste("Memory used for each iteration: ", toString(mem_vec)))

# plot time and memory
library(tidyverse)
jaccard_df <- read_csv("data/1_jaccard_results.csv")
plot_df <- data.frame(n_lengths, time_vec, mem_vec)
plot_df <- plot_df %>% gather(key = "variable", value = "value", -n_lengths) %>%
  mutate(variable_label = ifelse(variable == "time_vec", "Time (s)", "Memory (bytes)")) %>%
  mutate(method = "dist()") %>%
  bind_rows(jaccard_df) %>%
  mutate(variable_label = factor(variable_label, levels = c("Time (s)", "Memory (bytes)")))
write_csv(plot_df, "data/2_dist_results.csv")
p1 <- plot_df %>%
    ggplot(aes(x = n_lengths, y = value, linetype = method, color = variable)) + 
    geom_line() + 
    theme_bw() + 
    labs(title = "dist()") +
    xlab("Number of Profiles") + ylab("Seconds") +
    facet_wrap(~variable_label, ncol = 2, scales = "free") +
    guides(color = FALSE)

ggsave("outputs/p2.png", p1, width = 8, height = 4)


#################
