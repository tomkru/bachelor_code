lof.ensemble.mean <- function(listk, data, name = "") {
  data <- read.csv(file = data, header = FALSE, sep = ",")
  outlier.scores <- vector(mode = "numeric", length = nrow(data))
  for(i in listk) {
    score <- lofactor(data, k=i)
    outlier.scores <- cbind.data.frame(score, outlier.scores)
  }
  outlier.scores$outlier.scores <- NULL
  outlier.score <- rowMeans(outlier.scores)
  outlier.score <- cbind.data.frame(outlier.score, data)
  write.table(outlier.score, file = paste("LOF-dataresult-", name, sep=""), append = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
  return(outlier.score)
}
