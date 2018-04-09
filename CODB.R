PCL <- function(j, K, data) {
  instance <- j[-ncol(j)]
  train.row <- nrow(data)
  dis <- c()
  for(i in 1:nrow(data)) {
    dis <- c(dis, dist(rbind(instance, data[i,-c(1,ncol(data))]))[1])
  }
  data <- cbind.data.frame(data, distance = dis)
  data <- data[order(data$distance),]
  classes <- data[1:K, ncol(data)-1]
  numOfSame <- length(which(classes == j[1,ncol(j)]))
  return (numOfSame / K)
}

Deviation <- function(j, data) {
  instance <- j[-ncol(j)]
  data <- data[which(data[,ncol(data)] == j[1,ncol(j)]),]
  dis <- 0
  for(i in 1:nrow(data)) {
    dis <- dis + dist(rbind(instance, data[i,-c(1,ncol(data))]))[1]
  }
  return(dis)
}

KDist <- function(j, K, data) {
  instance <- j[-ncol(j)]
  train.row <- nrow(data)
  dis <- c()
  for(i in 1:nrow(data)) {
    dis <- c(dis, dist(rbind(instance, data[i,-c(1,ncol(data))]))[1])
  }
  data <- cbind.data.frame(data, distance = dis)
  data <- data[order(data$distance),]
  distances <- data$distance[1:K]
  return(sum(distances))
}

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

Rank <- function(j, K, data, alpha, beta) {
  pcl <- 0
  deviation <- 0
  kdist <- 0
  
  pcl <- PCL(j, K, data)
  deviation <- Deviation(j, data)
  kdist <- KDist(j, K, data)
  
  cof <- K * pcl + (alpha/deviation) + (beta * kdist)
  print(paste("K: ", K, "  pcl: ", pcl, "  deviation: ", deviation, "  kdist: ", kdist))
  return(cof)
}

CODB <- function(K, data, alpha, beta) {
  dataNorm <- as.data.frame(lapply(data[, -c(1, ncol(data))], normalize))
  dataNorm <- cbind.data.frame(name = data[, 1], dataNorm)
  data <- cbind.data.frame(dataNorm, type = data[, ncol(data)])
  outliers <- data.frame()
  cof <- 0
  for (i in 1:nrow(data)) {
    print(data[i, "name"])
    cof <- Rank(data[i, -1], K, data, alpha, beta)
    outliers <- rbind.data.frame(outliers, cbind.data.frame(data[i,], cof))
  }
  return(outliers)
}

CODB.mean <- function(listK, data, alpha, beta) {
  outlier.scores <- vector(mode = "numeric", length = nrow(data))
  for(i in listK) {
    score <- CODB(i, data, alpha, beta)
    outlier.scores <- cbind.data.frame(score$cof, outlier.scores)
  }
  outlier.scores$outlier.scores <- NULL
  outlier.score <- rowMeans(outlier.scores)
  outlier.score <- cbind.data.frame(outlier.score, data)
  View(outlier.score)
}
