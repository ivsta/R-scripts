library(cluster)
library(fpc)
library(ggplot2)
library(data.table)

d <- dist(as.matrix(mtcars))

# Dendrogram
hc <- hclust(d)
plot(hc)

# kmeans
fit <- kmeans(d, 4)
plotcluster(mtcars, fit$cluster)



# http://stats.stackexchange.com/questions/31083/how-to-produce-a-pretty-plot-of-the-results-of-k-means-cluster-analysis
library(cluster)
library(HSAUR)
data(pottery)
km    <- kmeans(pottery,3)
dissE <- daisy(pottery) 
dE2   <- dissE^2
sk2   <- silhouette(km$cl, dE2)
plot(sk2)



# Classical MDS
# N rows (objects) x p columns (variables)
# each row identified by a unique row name


fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
fit # view results

fit_points <- data.table(fit$points)
setnames(fit_points, c('x', 'y'))

ggplot(data = fit_points, aes(x = x, y = y)) +
  geom_point() +
  geom_text(aes(label = row.names(mtcars)))