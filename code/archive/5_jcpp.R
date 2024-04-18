library(Rcpp)
library(RcppArmadillo)
rm(list = ls())
# Load Rcpp function
sourceCpp("code/jaccard2.cpp")
# create a random matrix of size 10 x 10
mat1 <- matrix(sample(0:1, 1400 * 10, replace = TRUE), nrow = 10)
rownames(mat1) <- as.character(1:nrow(mat1))
# calculate jaccard distance with R
jac_dist <- dist(mat1, method = "binary")
# calculate jaccard distance with C++ function
cpp_dist <- jaccard_distance(mat1)
# compare
sum(cpp_dist == as.matrix(jac_dist))/100


library(microbenchmark)
sourceCpp("code/jaccard_slow.cpp")
sourceCpp("code/jaccard.cpp")
rcpp_arm_data <- microbenchmark(
  "Rcpp_dist" = {jaccard_distance(mat1)},
  "stats_dist" = {dist(mat1, method = "binary")},
  "Rcpp_armadillo" = {Jaccard_cpp(mat1, mat1, rownames(mat1), rownames(mat1))},
  times = 10)

summary(rcpp_arm_data)
p1 <- as.data.frame(summary(rcpp_arm_data)) %>%
    select(expr, median) %>%
    mutate(expr = factor(expr, levels = c("stats_dist","Rcpp_dist", "Rcpp_armadillo"))) %>%
    ggplot(aes(x = expr, y = median, fill = expr)) + geom_bar(stat = "identity") +
    xlab("Method") + ylab("Microseconds") + theme_bw() +
    # remove x axis labels
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
png("outputs/5_armadillo.png", height = 4, width = 4, units = "in", res = 300)
p1
dev.off()