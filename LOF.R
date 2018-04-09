lof.ensemble.mean <- function(listk, data) {
  outlier.scores <- vector(mode = "numeric", length = nrow(data))
  for(i in listk) {
    score <- lofactor(data, k=i)
    outlier.scores <- cbind.data.frame(score, outlier.scores)
  }
  outlier.scores$outlier.scores <- NULL
  outlier.score <- rowMeans(outlier.scores)
  outlier.score <- cbind.data.frame(outlier.score, data)
  return(outlier.score)
}
