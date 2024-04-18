library(tidyverse)
library(microbenchmark)
rm(list = ls())

# Loop to increase the number of vectors
n_lengths <- c(10, 100, 500, 1000, 2000, 3000)
euclidean_time_list <- list()
binary_time_list <- list()
size_list <- list()
for (i in 1:length(n_lengths)) {
  # Generate a list of random binary vectors of length 1400
  n_vectors <- n_lengths[[i]]
  vectors_mat <- matrix(sample(0:1, 1400 * n_vectors, replace = TRUE), nrow = n_vectors)
  # Measure time for euclidean 
  message(Sys.time(), " euclidean time ", n_vectors)
  euclidean_temp <- microbenchmark(
      "stats_dist" = {stats::dist(vectors_mat, method = "euclidean")},
      "Rfast_dist" = {Rfast::Dist(vectors_mat, method = "euclidean")},
      #"ade4_dist" = {ade4::dist.binary(vectors_mat, method = "1")},
      "proxy_dist" = {proxy::dist(vectors_mat, method = "Euclidean")},
      "distances_dist" = {distances::distances(vectors_mat)},
      "proxyC_dist_1" = {RcppParallel::setThreadOptions(1); proxyC::dist(vectors_mat, method = "euclidean")},
      "parDist_dist_1" = {parallelDist::parDist(vectors_mat, method = "euclidean", threads = 1)},
      "proxyC_dist_16" = {RcppParallel::setThreadOptions(16); proxyC::dist(vectors_mat, method = "euclidean")},
      "parDist_dist_16" = {parallelDist::parDist(vectors_mat, method = "euclidean", threads = 16)},
      "proxyC_dist_36" = {RcppParallel::setThreadOptions(36); proxyC::dist(vectors_mat, method = "euclidean")},
      "parDist_dist_36" = {parallelDist::parDist(vectors_mat, method = "euclidean", threads = 36)}
    ,
    times = 10)
  euclidean_time_list[[i]] <- summary(euclidean_temp) %>% mutate(size = n_vectors)
  # Measure time for binary
  message(Sys.time(), " binary time ", n_vectors)
  binary_temp <- microbenchmark(
      "stats_dist" = {stats::dist(vectors_mat, method = "binary")},
      "ade4_dist" = {ade4::dist.binary(vectors_mat, method = "1")},
      "proxy_dist" = {proxy::dist(vectors_mat, method = "binary")},
      "proxyC_dist_1" = {RcppParallel::setThreadOptions(1); proxyC::simil(vectors_mat, method = "jaccard")},
      "parDist_dist_1" = {parallelDist::parDist(vectors_mat, method = "binary", threads = 1)},
      "proxyC_dist_16" = {RcppParallel::setThreadOptions(16); proxyC::simil(vectors_mat, method = "jaccard")},
      "parDist_dist_16" = {parallelDist::parDist(vectors_mat, method = "binary", threads = 16)},
      "proxyC_dist_36" = {RcppParallel::setThreadOptions(36); proxyC::simil(vectors_mat, method = "jaccard")},
      "parDist_dist_36" = {parallelDist::parDist(vectors_mat, method = "binary", threads = 36)}
    ,
    times = 10)
  binary_time_list[[i]] <- summary(binary_temp) %>% mutate(size = n_vectors)
  # measure size of objects produced
  size_temp <- tibble(
    stats_dist <- pryr::object_size(stats::dist(vectors_mat, method = "binary")),
    ade4_dist <- pryr::object_size(ade4::dist.binary(vectors_mat, method = "1")),
    proxy_dist <- pryr::object_size(proxy::dist(vectors_mat, method = "binary")),
    proxyC_dist <- pryr::object_size(proxyC::simil(vectors_mat, method = "jaccard")),
    parDist_dist <- pryr::object_size(parallelDist::parDist(vectors_mat, method = "binary", threads = 36))
  )
  size_list[[i]] <- size_temp
}
res <- list(euclidean_time_list, binary_time_list, size_list)
names(res) <- c("euclidean_time_list", "binary_time_list", "size_list")
saveRDS(res, "data/3_other_dists.rds")
#
library(tidyverse)
library(microbenchmark)
rm(list = ls())
res <- readRDS("data/3_other_dists.rds")
euclidean_time_list <- res$euclidean_time_list
binary_time_list <- res$binary_time_list
size_list <- res$size_list

euclidean_df <- bind_rows(euclidean_time_list) %>% mutate(expr = factor(expr, levels = c("stats_dist", "Rfast_dist", "ade4_dist", "proxy_dist", "distances_dist", "proxyC_dist_1", "proxyC_dist_16", "proxyC_dist_36", "parDist_dist_1", "parDist_dist_16",  "parDist_dist_36"))) 
binary_df <- bind_rows(binary_time_list) %>% mutate(expr = factor(expr, levels = c("stats_dist", "Rfast_dist", "ade4_dist", "proxy_dist", "distances_dist", "proxyC_dist_1", "proxyC_dist_16", "proxyC_dist_36", "parDist_dist_1", "parDist_dist_16",  "parDist_dist_36")))

p1a <- euclidean_df %>%
  mutate(median = ifelse(size == 10, median / 1000, median)) %>%
  ggplot(aes(x = size, y = median/1000, color = expr, shape = expr)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  xlab("Number of profiles") + ylab("Seconds") +
  scale_color_manual(values = c("stats_dist" = "red", "Rfast_dist" = "blue", "proxy_dist" = "green", "distances_dist" = "purple", "proxyC_dist_1" = "pink", "proxyC_dist_16" = "pink", "proxyC_dist_36" = "pink", "parDist_dist_1" = "brown", "parDist_dist_16" = "brown",  "parDist_dist_36" = "brown")) +
  scale_shape_manual(values = c("stats_dist" = 1, "Rfast_dist" = 1, "proxy_dist" = 1, "distances_dist" = 1, "proxyC_dist_1" = 2, "proxyC_dist_16" = 3, "proxyC_dist_36" = 4, "parDist_dist_1" = 2, "parDist_dist_16" = 3,  "parDist_dist_36" = 4)) 
png("outputs/p3a.png", width = 15, height = 10, units = "cm", res = 300)
p1a
dev.off()
# ggsave(p1a, file = "outputs/p3a.png", width = 15, height = 10, units = "cm", dpi = 300)


p1b <- binary_df %>%
  mutate(median = ifelse(size == 10, median / 1000, median)) %>%
  ggplot(aes(x = size, y = median/1000, color = expr, shape = expr)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  xlab("Number of profiles") + ylab("Seconds") +
  scale_color_manual(values = c("stats_dist" = "red", "Rfast_dist" = "blue", "ade4_dist" = "orange", "proxy_dist" = "green", "proxyC_dist_1" = "pink", "proxyC_dist_16" = "pink", "proxyC_dist_36" = "pink", "parDist_dist_1" = "brown", "parDist_dist_16" = "brown",  "parDist_dist_36" = "brown")) +
  scale_shape_manual(values = c("stats_dist" = 1, "Rfast_dist" = 1, "ade4_dist" = 1, "proxy_dist" = 1, "distances_dist" = 1, "proxyC_dist_1" = 2, "proxyC_dist_16" = 3, "proxyC_dist_36" = 4, "parDist_dist_1" = 2, "parDist_dist_16" = 3,  "parDist_dist_36" = 4)) 
png("outputs/p3b.png", width = 15, height = 10, units = "cm", res = 300)
p1b
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

