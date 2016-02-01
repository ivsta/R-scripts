density_equalisation<-function(x_in,y_in,grid_size,sigma=1,sea=0){
  xq<-ecdf(x_in)
  yq<-ecdf(y_in)

  x_in<-xq(x_in)
  y_in<-yq(y_in)

  x_in<-x_in+rnorm(length(x_in),mean=0,sd=sigma)
  y_in<-y_in+rnorm(length(y_in),mean=0,sd=sigma)

  dat<-data.table(v1=round(grid_size*x_in)-min(round(grid_size*x_in))+1,
                  v2=round(grid_size*y_in)-min(round(grid_size*y_in))+1)
  print(dat)
  dat2<-dcast.data.table(dat[,as.numeric(.N),keyby="v1,v2"],v1~v2,fill=0)[,v1:=NULL]
  image(as.matrix(dat2),useRaster=TRUE,zlim=c(0,150), col=rainbow(150))

  cart<-cartogram(t(as.matrix(dat2)),blur=0,sea=sea)

  new<-predict.Cartogram(cart,x=dat$v1,y=dat$v2)

  res<-data.table(x=new[,1],y=new[,2])
  # print(ggplot(data.table(x=new[,1],y=new[,2])[sample(1:.N,5000)])+geom_point(aes(x=x,y=y),alpha=0.015))
  res
}

# 
# r1<-density_equalisation(pca_res2$x[,"PC1"],pca_res2$x[,"PC3"],grid_size=100,sigma=0.005,sea=0.00)
# r1[is.na(x),':='(x=0,y=0)]
# 
# r2<-density_equalisation(r1$x,r1$y,grid_size=100,sigma=0.005,sea=0.0)
# #r3<-density_equalisation(r2$x+runif(nrow(r2))*4,r2$y+runif(nrow(r2))*4,1)