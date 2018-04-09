or.ensemble.method <- function() {
  zoo <- read.csv(file = "zoo.csv", header = TRUE, sep=",")
  zoo2 <- zoo[,2:17]
  outlier.scores <- vector(mode = "numeric", length = nrow(zoo))
  methods <- c("linear", "sigmoid", "sizeDiff")
  for(i in methods) {
  score <- outliers.ranking(zoo2, method = "sizeDiff", clus = list(dist="binary", alg="hclust", meth="average"))
  #meth possible values: {ward.D, ward.D2, single, complete, average, mcquitty, median, centroid}
  #alg: {hclust, diana}
  #dist: {euclidean, maximum, manhattan, canberra, binary, minkowski}
  #method: {linear, sigmoid, sizeDiff}
  outlier.scores <- cbind.data.frame(score$prob.outliers, outlier.scores)
  }
  outlier.scores$outlier.scores <- NULL
  outlier.score <- rowMeans(outlier.scores)
  outlier.score <- cbind.data.frame(outlier.score, names = zoo[,1])
  View(outlier.score)
}
