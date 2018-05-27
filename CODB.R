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

Rank <- function(j, K, data, minDev, maxDev, minKDist, maxKDist) {
  pcl <- 0
  deviation <- 0
  kdist <- 0
  
  pcl <- PCL(j, K, data)
  deviation <- Deviation(j, data)
  kdist <- KDist(j, K, data)
  
  cof <- K * pcl - ((deviation - minDev) / (maxDev - minDev)) + ((kdist - minKDist) / (maxKDist - minKDist))
  return(cof)
}

CODB <- function(K, data) {
  outliers <- data.frame()
  
  minDev <- Inf
  minKDist <- Inf
  maxDev <- -Inf
  maxKDist <- -Inf
  for (i in 1:nrow(data)) {
    tempDev <- Deviation(data[i, -1], data)
    tempKDist <- KDist(data[i, -1], K, data)
    if(tempDev > maxDev) {
      maxDev <- tempDev
    }
    if(tempDev < minDev) {
      minDev <- tempDev
    }
    if(tempKDist > maxKDist) {
      maxKDist <- tempKDist
    }
    if(tempKDist < minKDist) {
      minKDist <- tempKDist
    }
  }
  
  cof <- 0
  for (i in 1:nrow(data)) {
    cof <- Rank(data[i, -1], K, data, minDev, maxDev, minKDist, maxKDist)
    outliers <- rbind.data.frame(outliers, cbind.data.frame(data[i,], cof))
  }
  return(outliers)
}

CODB.mean <- function(listK, data, name = "") {
  data <- read.csv(file = data, header = TRUE, sep = ",")
  outlier.scores <- vector(mode = "numeric", length = nrow(data))
  for(i in listK) {
    score <- CODB(i, data)
    outlier.scores <- cbind.data.frame(score$cof, outlier.scores)
  }
  outlier.scores$outlier.scores <- NULL
  outlier.score <- rowMeans(outlier.scores)
  outlier.score <- cbind.data.frame(outlier.score, data)
  write.table(outlier.score, file = paste("CODB-dataresult-", name), append = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
  return(outlier.score)
}

