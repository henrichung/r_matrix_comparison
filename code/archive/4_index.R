library(tidyverse)
library(microbenchmark)
library(data.table)
rm(list = ls())

# time how long it takes to access a value by key
random_access_vector <- function(keys, values) {
  query <- sample(keys, 1)
  value <- values[keys == query]
  return(value)
}

random_access_list <- function(keys, values) {
  query <- sample(keys, 1)
  value <- values[[query]]
  return(value)
}

random_access_env <- function(keys, values) {
  query <- sample(keys, 1)
  value <- values[[query]]
  return(value)
}

random_access_df <- function(keys, values) {
  query <- sample(keys, 1)
  value <- values[values$key==query,]$key
  return(value)
}

random_access_tbl <- function(keys, values) {
  query <- sample(keys, 1)
  value <- dplyr::filter(values, key == query)$key
  return(value)
}

random_access_dt <- function(keys, values) {
  query <- sample(keys, 1)
  value <- values[key == query]$key
  return(value)
}

random_access_fastmap <- function(keys, values) {
  query <- sample(keys, 1)
  value <- values$get(query)
  return(value)
}

n_lengths <- c(10, 100, 1000, 10000, 100000, 1000000, 10000000)
results_list <- list()
for(i in 1:length(n_lengths)){
  message(Sys.time(), " ", i)
  n_length <- n_lengths[i]
  # generate 1,000,000 key-value pairs
  keys <- sample(1:n_length, n_length, replace = FALSE) %>% as.character()
  values <- sample(1:n_length, n_length, replace = FALSE) 
  # using a list
  values_list <- as.list(values)
  names(values_list) <- keys
  # using an environment
  values_env <- new.env(hash = TRUE)
  for(j in 1:length(keys)){values_env[[keys[[j]]]] <- values[[j]]}
  # using a data.frame 
  values_df <- data.frame(key = keys, value = values)
  # using a tibble with filter
  values_tbl <- tibble(key = keys, value = values)
  # using a data.table 
  values_dt <- as.data.table(values_df)
  # using a sorted data table
  values_dt2 <- copy(values_dt)
  setkey(values_dt2, key)
  # user fastmap
  values_fastmap <- fastmap::fastmap()
  values_fastmap$mset(.list = values_list)
  # use one function instead of many to benchmark
  result <- microbenchmark::microbenchmark(
    sample_key = sample(keys, 1),
    random_access_vector = random_access_vector(keys, values),
    random_access_list = random_access_list(keys, values_list),
    random_access_env = random_access_env(keys, values_env),
    random_access_df = random_access_df(keys, values_df),
    random_access_tbl = random_access_tbl(keys, values_tbl),
    random_access_dt = random_access_dt(keys, values_dt),
    random_access_dt2 = random_access_dt(keys, values_dt2),
    random_access_fastmap = random_access_fastmap(keys, values_fastmap),
    times = 1000
  )
  results_list[[i]] <- summary(result)
}

results_list2 <- Filter(function(x) !is.null(x), results_list)
names(results_list) <- n_lengths[1:length(results_list)]
saveRDS(results_list, "data/4_index.rds")
#
library(tidyverse)
library(microbenchmark)
library(data.table)
rm(list = ls())
results_list <- readRDS("data/4_index.rds")
results_df <- bind_rows(results_list, .id = "size")
p1 <- results_df %>%
  mutate(expr = case_when(
    grepl("sample_key", expr) ~ "Sample",
    grepl("random_access_vector", expr) ~ "Vector",
    grepl("random_access_list", expr) ~ "List",
    grepl("random_access_env", expr) ~ "Environment/Hash",
    grepl("random_access_df", expr) ~ "Dataframe",
    grepl("random_access_tbl", expr) ~ "Tibble",
    grepl("random_access_dt2", expr) ~ "Keyed DataTable",
    grepl("random_access_dt", expr) ~ "DataTable",
    grepl("random_access_fastmap", expr) ~ "Fastmap"
  )) %>%
  # convert expr into factor with levels in order of median
  mutate(expr = factor(expr, levels = rev(c("Sample", "Environment/Hash", "Fastmap", "Dataframe", "Keyed DataTable", "DataTable", "Vector","List", "Tibble")))) %>%
  ggplot(aes(x = size, y = median, color = expr, group = expr)) + 
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_y_log10() +
  ylab("Microseconds") +
  # use color pallette
  scale_color_brewer(palette = "Set3") 
png("outputs/4_index.png", width = 5, height = 5, units = "in", res = 300)
p1
dev.off()
#ggsave("outputs/4_index.png", p1, width = 5, height = 5, units = "in")

sample_time <- p1$data %>% filter(expr == "Sample") %>% pull(median) %>% rep(., each = 9)
p1b <- p1$data %>%
  mutate(median = median - sample_time) %>%
  filter(expr != "Sample") %>%
  ggplot(aes(x = size, y = median, color = expr, group = expr)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_y_log10() +
  ylab("Microseconds") +
  scale_color_brewer(palette = "Set3")
png("outputs/4_index2.png", width = 8, height = 5, units = "in", res = 300)
p1b
dev.off()
#ggsave("outputs/4_index2.png", p1b, width = 5, height = 5, units = "in")