library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(ggpubr)
rm(list = ls())

# read in results file
results_mat <- readRDS("outputs/results_matrix_multiplication.rds")

# function to apply mapping
apply_mapping <- function(input_string) {
  match <- sapply(names(mapping_vector), function(pattern) grepl(pattern, input_string))
  if (any(match)) {
    return(mapping_vector[names(match)[match]])
  } else {
    return("other")
  }
}

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
  mutate(rank = row_number(median)) 

# define order levels for plotting
p1_order <- results_mat_df %>%
  group_by(expression) %>%
  summarize(mean = mean(rank), .groups = 'drop') %>%
  arrange(mean) %>%
  mutate(expression = as.character(expression)) %>%
  pull(expression)

# tile plot average
p1 <- results_mat_df %>%
  mutate(n_vectors = factor(n_vectors, levels = c("10", "100", "1000"))) %>%
  mutate(expression = factor(expression, levels = p1_order)) %>%
  ggplot(aes(y = expression, x = n_vectors, fill = rank)) +
  geom_tile(color = "black", size = 0.5) +
  geom_text(aes(label = median_label), size = 3, alpha = 1) +
  scale_fill_viridis_c(direction = -1, begin = 0.15, end = 1, guide = guide_colorbar(reverse = TRUE, ticks = TRUE)) +
  theme_bw() +
  labs(title = "Matrix Multiplication") +
  xlab("Dimensions") + ylab("") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = NA))
# save to file
ggsave("outputs/matrix_multiplication.png", p1, width = 20, height = 12, units = "cm")
