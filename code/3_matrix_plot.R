library(ggplot2)
library(RColorBrewer)
library(ggpubr)
library(tidyverse)
rm(list = ls())

# read in results file
results_mat_df <- readr::read_csv("data/matmult_timings.csv")

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
ggsave("outputs/figures/matrix_multiplication.png", p1, width = 20, height = 12, units = "cm")
