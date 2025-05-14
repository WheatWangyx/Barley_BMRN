library(reshape)

maxorder <- function(x) x[order(x, decreasing = TRUE)]
minorder <- function(x) x[order(x, decreasing = FALSE)]
countCV <- function(x) (sd(x)/mean(x)) * 100

sample <- data_clean
data_clean$Average <- apply(sample, 1, mean)

max_values <- t(apply(sample, 1, function(x) head(maxorder(x), 10)))
colnames(max_values) <- paste0("Max_", 1:10)
data_clean <- cbind(data_clean, max_values)

min_values <- t(apply(sample, 1, function(x) head(minorder(x), 10)))
colnames(min_values) <- paste0("Min_", 1:10)
data_clean <- cbind(data_clean, min_values)

data_clean$Average_top10 <- apply(data_clean[, grep("Max_", names(data_clean))], 1, mean)
data_clean$Average_last10 <- apply(data_clean[, grep("Min_", names(data_clean))], 1, mean)
data_clean$"top10/last10" <- data_clean$Average_top10 / data_clean$Average_last10
data_clean$'CV%' <- apply(sample, 1, countCV)