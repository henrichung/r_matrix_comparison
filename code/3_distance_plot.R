library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(ggpubr)
rm(list = ls())

distance_timings <- readr::read_csv("data/distance_timings.csv")

# Define a dataframe for function groups for plotting
variable_groups <- data.frame(
  expression = c(
    "stats", "proxy", "proxyC", "distances", "distances_matrix",
    "Rfast", "rcpp_serial", "rcpp_parallel_8", "fortran_serial",
    "fortran_parallel_8", "c_fortran_serial",
    "c_fortran_parallel_1", "c_fortran_parallel_8",
    "GPUmatrix", "parallelDist_1", "parallelDist_8"),
  family = c(
    "base", "proxy", "proxy", "distances", "distances",
    "Rfast", "Rcpp", "Rcpp", "Fortran",
    "Fortran", "c_Fortran", "c_Fortran", "c_Fortran",
    "GPUmatrix", "parallelDist", "parallelDist")
)

# generate a color palette from RColorBrewer Paired
num_families <- length(unique(variable_groups$family))
palette <- c(
  "#E41A1C", "#377EB8", "#4DAF4A",
  "#984EA3", "#FF7F00", "#FFD700",
  "#A65628", "#F781BF", "#999999")

# map colors to families 
family_colors <- setNames(palette, unique(variable_groups$family))
expression_colors <- family_colors[variable_groups$family]
expression_colors <- setNames(expression_colors, variable_groups$expression)

# map shapes to expression within a family
expressions <- unique(variable_groups$expression)
num_expressions <- length(expressions)
shapes <- c(16, 16, 17, 16, 17, 16, 16, 17, 16, 17, 16, 17, 18, 16, 16, 17)
shape_mapping <- setNames(shapes, expressions)

# plot distance benchmarks
p1_data <- filter(distance_timings, run == "2_10")

p1 <- p1_data %>%
  mutate(median_ns = median * 1000000, sd_ns = sd * 1000000) %>%
  ggplot(aes(x = n_vectors, y = median_ns, color = expression, shape = expression)) +
  geom_point(size = 2) +
  geom_line(aes(group = expression), size = 0.75) +
  scale_color_manual(values = expression_colors) +
  scale_shape_manual(values = shape_mapping) +
  scale_y_log10(labels = scales::label_number(suffix = " ns")) +
  scale_x_continuous(breaks = p1_data$n_vectors) +
  xlab("Dimensions") + ylab("Time") +
  labs(color = "Expression", shape = "Expression", title = "2-10") +
  theme_bw()

# save to file
ggsave("outputs/figures/distance_2-10.png", p1, width = 20, height = 12, units = "cm")

#
p2_data <- filter(distance_timings, run == "10_100")

p2 <- p2_data %>%
  mutate(median_ns = median * 1000000, sd_ns = sd * 1000000) %>%
  ggplot(aes(x = n_vectors, y = median_ns, color = expression, shape = expression)) +
  geom_point(size = 2) +
  geom_line(aes(group = expression), size = 0.75) +
  scale_color_manual(values = expression_colors) +
  scale_shape_manual(values = shape_mapping) +
  scale_y_log10(labels = scales::label_number(suffix = " ns")) +
  scale_x_continuous(breaks = p2_data$n_vectors) +
  xlab("Dimensions") + ylab("Time") +
  labs(color = "Expression", shape = "Expression", title = "10-100") +
  theme_bw()
# save to file
ggsave("outputs/figures/distance_10-100.png", p2, width = 20, height = 12, units = "cm")

#
p3_data <- filter(distance_timings, run == "100_1000")

p3 <- p3_data %>%
  mutate(median_ms = median * 1000, sd_ms = sd * 1000) %>%
  ggplot(aes(x = n_vectors, y = median_ms, color = expression, shape = expression)) +
  geom_point(size = 2) +
  geom_line(aes(group = expression), size = 0.75) +
  scale_color_manual(values = expression_colors) +
  scale_shape_manual(values = shape_mapping) +
  scale_y_log10(labels = scales::label_number(suffix = " ms")) +
  scale_x_continuous(breaks = p3_data$n_vectors) +
  xlab("Dimensions") + ylab("Time") +
  labs(color = "Expression", shape = "Expression", title = "100-1000") +
  theme_bw()
# save to file
ggsave("outputs/figures/distance_100-1000.png", p3, width = 20, height = 12, units = "cm")

p4_data <- filter(distance_timings, run == "1000_10000")
p4 <- p4_data %>%
  ggplot(aes(x = n_vectors, y = median, color = expression, shape = expression)) +
  geom_point(size = 2) +
  geom_line(aes(group = expression), size = 0.75) +
  scale_color_manual(values = expression_colors) +
  scale_shape_manual(values = shape_mapping) +
  scale_y_log10(labels = scales::label_number(suffix = " s")) +
  scale_x_continuous(breaks = p4_data$n_vectors) +
  xlab("Dimensions") + ylab("Time") +
  labs(color = "Expression", shape = "Expression", title = "1k-10k") +
  theme_bw()
# save to file
ggsave("outputs/figures/distance_1000-10000.png", p4, width = 20, height = 12, units = "cm")

#

# combine into a single plot
shared_legend <- ggpubr::get_legend(p1)

# remove legend from individual plots
p1a <- p1 + theme(legend.position = "none")
p2a <- p2 + theme(legend.position = "none")
p3a <- p3 + theme(legend.position = "none")
p4a <- p4 + theme(legend.position = "none")

# combine plots
g1 <- ggarrange(
  p1a, p2a, p3a, p4a, 
  ncol=2, nrow=2, 
  common.legend = TRUE, legend="right", 
  legend.grob = shared_legend)
ggsave("outputs/figures/distance_combined.png", g1, width = 30, height = 16, units = "cm")

# Order expressions by median time in a tile plot
p5_list <- list(p1_data, p2_data, p3_data, p4_data)
names(p5_list) <- c("2-10", "10-100", "100-1000", "1000-10000")
p5_data <- p5_list %>%
  lapply(function(.x){
    .x %>% 
      group_by(expression) %>%
      summarize(mean = mean(median)) 
  }) %>% bind_rows(.id = "range") %>%
  mutate(mean_label = case_when(
    range == "2-10" ~ paste0(signif(mean * 1000000, 2), "ns"),
    range == "10-100" ~ paste0(signif(mean * 1000000, 2), "ns"),
    range == "100-1000" ~ paste0(signif(mean * 1000, 2), "ms"),
    range == "1000-10000" ~ paste0(signif(mean, 2), "s"
  ))) %>%
  group_by(range) %>%
  mutate(Rank = row_number(mean))

# define average speed order
p5_order <- p5_data %>%
  group_by(expression) %>%
  summarize(mean = mean(Rank), .groups = 'drop') %>%
  arrange(mean) %>%
  mutate(expression = as.character(expression)) %>%
  pull(expression)

# tile plot average
p5 <- p5_data %>%
  mutate(range = factor(range, levels = c("2-10", "10-100", "100-1000", "1000-10000"))) %>%
  mutate(expression = factor(expression, levels = p5_order)) %>%
  ggplot(aes(y = expression, x = range, fill = Rank)) +
  geom_tile(color = "black", size = 0.5) +
  geom_text(aes(label = mean_label), size = 3, alpha = 1) +
  scale_fill_viridis_c(
    direction = -1, begin = 0.15, end = 1,
    guide = guide_colorbar(reverse = TRUE, ticks = TRUE)) +
  theme_bw() +
  labs(title = "Euclidean Distance") +
  xlab("Dimensions") + ylab("") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = NA))

# save to file
ggsave("outputs/figures/distance_tile.png", p5, width = 20, height = 12, units = "cm")

