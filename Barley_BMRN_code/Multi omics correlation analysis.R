library(Hmisc)

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor = cormat[ut],
    p = pmat[ut]
  )
}

mergedata <- rbind(data_clean1, data_clean2)
cor <- rcorr(t(as.matrix(mergedata)), type = "pearson")
cor_out <- flattenCorrMatrix(cor$r, cor$P)

write.table(cor$r, file = "cor.txt", sep = "\t", quote = FALSE, row.names = TRUE)
write.table(cor_out, file = "cor_pval.txt", sep = "\t", quote = FALSE, row.names = FALSE)