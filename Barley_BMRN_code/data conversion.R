zs <- function(x) {
  rowmean <- apply(x, 1, mean)
  rowsd <- apply(x, 1, sd)
  rv <- sweep(x, 1, rowmean, "-")
  rv <- sweep(rv, 1, rowsd, "/")
  return(rv)
}

data_clean_zs <- zs(data_clean)
data_clean_minmax <- t(apply(data_clean, 1, function(x) (x - min(x))/(max(x) - min(x))))
data_clean_log2 <- log2(data_clean + 1)
data_clean_log2_zs <- zs(data_clean_log2)