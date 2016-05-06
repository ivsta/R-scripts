library(mRMRe)

data(cgps)
data.cgps <- data.frame(cgps.ic50, cgps.ge)

dd <- mRMR.data(data = data.cgps)
xx <- mRMR.classic(data = dd, target_indices = c(1), feature_count = 30)
print(apply(solutions(xx)[[1]], 2, function(x, y) { return(y[x]) }, y=featureNames(dd)))