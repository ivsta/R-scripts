#---------------------------------------------------
# http://psych.colorado.edu/wiki/lib/exe/fetch.php?media=labs:learnr:emily_-_principal_components_analysis_in_r:pca_how_to.pdf
#---------------------------------------------------


# To begin, let’s assume we can control the contents of the black box so that we can see
# what our data will look like. Suppose we have four recorders, one at each corner of the
# box. We introduce coordinates onto the box so that we have A at location (0,0), B at
# (0,1), C at (1,1), and D at (1,0). For now, we’ll assume we have only two light sources,
# the first is at location (.3,.8) and its light intensity varies in a sine-wave pattern; the
# second is at location (.5,.2) and it varies in a cosine-wave pattern with a different period.

recorders = data.frame( "X" = c(0,0,1,1), "Y" = c(0,1,1,0),row.names=c("A", "B","C","D"))

locs = data.frame("X"=c(.3,.5),"Y"=c(.8,.2))

intensities = data.frame("sine"=sin(0:99*(pi/10))+1.2, "cosine"= .7*cos(0:99*(pi/15))+.9)



# We assume that the amount of light each recorder picks up decays exponentially with the
# distance from the light source to the recorder. We also assume that the lights combine
# linearly, that is, the sensors record a simple sum of the amount of light they receive from
# each source, though probably in a noisy way.


dists = matrix(nrow=dim(locs)[1], ncol=dim(recorders)[1], dimnames=list(NULL, row.names(recorders)))

for (i in 1:dim(dists)[2]){
  dists[,i]=sqrt((locs$X-recorders$X[i])^2
                 + (locs$Y-recorders$Y[i])^2)
  }

set.seed(500)
recorded.data = data.frame(jitter(as.matrix(intensities) %*% as.matrix(exp(-2*dists)),amount=0))

plot(recorded.data)
round(cor(recorded.data), 2)
plot.ts(recorded.data)




# Obtain data in a matrix
Xoriginal = t(as.matrix(recorded.data))




# Center the data so that the mean of each row is 0
rm = rowMeans(Xoriginal)


X = Xoriginal - matrix(rep(rm, dim(Xoriginal)[2]), nrow=dim(Xoriginal)[1])

# Calculate P
A=X %*% t(X)
E=eigen(A,TRUE)
P=t(E$vectors)

# Find the new data and standard deviations of the principal components
newdata = P %*% X
sdev = sqrt(diag((1/(dim(X)[2]-1)* P %*% A %*% t(P))))


pr=prcomp(recorded.data)
pr
plot(pr)
barplot(pr$sdev/pr$sdev[1])

pr2=prcomp(recorded.data, tol=.1)
plot.ts(pr2$x)

plot.ts(intensities)
plot.ts(recorded.data)
plot.ts(cbind(-1*pr2$x[,1],pr2$x[,2]))



od=pr$x %*% t(pr$rotation)
od2=pr2$x %*% t(pr2$rotation)

plot.ts(recorded.data)
plot.ts(od)
plot.ts(od2)
