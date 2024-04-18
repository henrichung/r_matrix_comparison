library(tidyverse)
library(microbenchmark)
library(Rcpp)
library(RcppArmadillo)
rm(list = ls())

# Loop to increase the number of vectors
sourceCpp("code/jaccard.cpp")
n_lengths <- c(10, 100, 500, 1000, 2000, 3000, 10000)
binary_time_list <- list()
size_list <- list()
for (i in 1:length(n_lengths)) {
  # Generate a list of random binary vectors of length 1400
  n_vectors <- n_lengths[[i]]
  vectors_mat <- matrix(sample(0:1, 1400 * n_vectors, replace = TRUE), nrow = n_vectors)
  rownames(vectors_mat) <- as.character(1:nrow(vectors_mat))
  # Measure time for binary
  message(Sys.time(), " binary time ", n_vectors)
  binary_temp <- microbenchmark(
      "ade4_dist" = {ade4::dist.binary(vectors_mat, method = "1")},
      "parDist_dist_36" = {parallelDist::parDist(vectors_mat, method = "binary", threads = 36)},
      "Rcpp_dist" = {Jaccard_cpp(vectors_mat, vectors_mat, rownames(vectors_mat), rownames(vectors_mat))}
    ,
    times = 10)
  binary_time_list[[i]] <- summary(binary_temp) %>% mutate(size = n_vectors)
}
saveRDS(binary_time_list, "data/3_best_dists.rds")
#
library(tidyverse)
library(microbenchmark)
rm(list = ls())
binary_time_list <- readRDS("data/3_best_dists.rds")
binary_df <- bind_rows(binary_time_list) %>% 
  mutate(expr = factor(expr, levels = c("ade4_dist", "parDist_dist_36", "Rcpp_dist")))
p1c <- binary_df %>%
  mutate(median = ifelse(size == 10, median / 1000, median)) %>%
  mutate(median = ifelse(size == 10000, median * 1000, median)) %>%
  ggplot(aes(x = size, y = median/1000, color = expr, shape = expr)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  xlab("Number of profiles") + ylab("Seconds")
png("outputs/p3c.png", width = 15, height = 10, units = "cm", res = 300)
p1c
dev.off()
# ggsave(p1b, file = "outputs/p3b.png", width = 15, height = 10, units = "cm", dpi = 300)

# calculate the size of returned dist objects for each function and number of vectors

names(size_list) <- n_lengths
size_df <- bind_rows(size_list, .id = "size") %>% 
  pivot_longer(-size, names_to = "method", values_to = "bytes") %>% 
  rowwise() %>%
  mutate(method = strsplit(method, split = " ")[[1]][1], kb = utils:::format.object_size(bytes, "auto")) %>%
  mutate(size = as.character(size))


size_join <- binary_df %>% 
  select(expr, median, size) %>% 
  mutate(size = as.character(size))%>% 
  filter(expr %in% c("stats_dist", "ade4_dist", "proxy_dist", "proxyC_dist_36", "parDist_dist_36")) %>%
  mutate(method = gsub("_36", "", expr)) %>% 
  select(-expr) %>%
  right_join(size_df, by = c("method", "size")) 
p2 <- size_join %>%
  mutate(size = factor(size, levels = n_lengths), method = factor(method, levels = c("stats_dist", "ade4_dist", "proxyC_dist", "proxy_dist", "parDist_dist"))) %>%
  ggplot(aes(x = size, y = bytes/(1.25e+8), fill = method)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  xlab("Number of profiles") + ylab("Gigabytes")
ggsave(p2, file = "outputs/p3c.png", width = 15, height = 10, units = "cm", dpi = 300)

binary_df %>% filter(expr == "parDist_dist_36")

par36 = 1 hr 40 minutes
memory = 356 Gb

