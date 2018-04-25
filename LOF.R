lof.ensemble.mean <- function(listk, data, name = "") {
  data <- read.csv(file = data, header = TRUE, sep = ",")
  data <- as.data.frame(lapply(data[, -c(1, ncol(data))], normalize))
  outlier.scores <- vector(mode = "numeric", length = nrow(data))
  for(i in listk) {
    score <- lofactor(data, k=i)
    outlier.scores <- cbind.data.frame(score, outlier.scores)
  }
  outlier.scores$outlier.scores <- NULL
  outlier.score <- rowMeans(outlier.scores)
  outlier.score <- cbind.data.frame(outlier.score, data)
  write.table(outlier.score[,1], file = paste("LOF-dataresult-", name), append = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
  return(outlier.score)
}

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
