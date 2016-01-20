library(dbscan)

data(iris)
iris <- as.matrix(iris[,1:4])
res <- dbscan(iris, eps = .4, minPts = 4)
res
pairs(iris, col = res$cluster + 1L)
## compare with dbscan from package fpc (only if installed)
if (requireNamespace("fpc", quietly = TRUE)) {
  res2 <- fpc::dbscan(iris, eps = .4, MinPts = 4)
  pairs(iris, col = res2$cluster + 1L)
  ## make sure both version produce the same results
  all(res$cluster == res2$cluster)
}
## find suitable eps parameter (look at knee)
kNNdistplot(iris, k = 4)
## example data from fpc
set.seed(665544)
n <- 100
x <- cbind(
  x = runif(10, 0, 10) + rnorm(n, sd = 0.2),
  y = runif(10, 0, 10) + rnorm(n, sd = 0.2)
)
res <- dbscan::dbscan(x, eps = .2, minPts = 4)
res
plot(x, col=res$cluster + 1L)

## compare speed against fpc version (if microbenchmark is installed)
if (requireNamespace("microbenchmark", quietly = TRUE)) {
  t_dbscan <- microbenchmark::microbenchmark(
    dbscan::dbscan(x, .2, 4), times = 10, unit = "ms")
  t_dbscan_linear <- microbenchmark::microbenchmark(
    dbscan::dbscan(x, .2, 4, search = "linear"), times = 10, unit = "ms")
  t_fpc <- microbenchmark::microbenchmark(
    fpc::dbscan(x, .2, 4), times = 10, unit = "ms")
  rbind(t_fpc, t_dbscan_linear, t_dbscan)
  boxplot(rbind(t_fpc, t_dbscan_linear, t_dbscan),
          names = c("fpc (R)", "dbscan (linear)", "dbscan (kdtree)"),
          main = "Runtime comparison in ms")
  ## speedup of the kd-tree-based version compared to the fpc implementation
  median(t_fpc$time) / median(t_dbscan$time)
}  






#================================

data(iris)
iris <- as.matrix(iris[,1:4])
kNNdist(iris, k=4, search="kd")
kNNdistplot(iris, k=4)
## the knee is around a distance of .5
cl <- dbscan(iris, eps = .5, minPts = 4)
pairs(iris, col = cl$cluster+1L)
## Note: black are noise points