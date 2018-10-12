library(RCurl)
library(tidyverse)
library(forecast)


train_daily <- getURL('https://raw.githubusercontent.com/M4Competition/M4-methods/master/Dataset/Train/Daily-train.csv') %>%
	read_delim(delim = ',')

test_daily <- getURL('https://raw.githubusercontent.com/M4Competition/M4-methods/master/Dataset/Test/Daily-test.csv') %>%
	read_delim(delim = ',')


train_daily %>% str()
test_daily %>% str()

train_daily_v2 <- tibble(V1 = train_daily$V1, V2 = train_daily$V2)
test_daily_v2 <- tibble(V1 = test_daily$V1, V2 = test_daily$V2)

train_daily_v2 %>%
	ggplot(aes(x = V1, y = V2, group = 1)) +
	geom_point() +
	geom_line()


train_daily_v2$V2



for (i in 1:length(data_train)){

  fh <- 6
  insample <- train_daily$V2
  outsample <- test_daily$V2
  forecasts <- Benchmarks(input=insample, fh=fh)

  #sMAPE
  for (j in 1:length(Names_benchmarks)){
    Total_smape[j,,i] <- smape_cal(outsample, forecasts[[j]]) #j the # of the benchmark
  }
  #MASE
  for (j in 1:length(Names_benchmarks)){
    Total_mase[j,,i] <- mase_cal(insample, outsample, forecasts[[j]]) #j the # of the benchmark
  }

}




smape_cal <- function(outsample, forecasts){
  #Used to estimate sMAPE
  outsample <- as.numeric(outsample) ; forecasts<-as.numeric(forecasts)
  smape <- (abs(outsample-forecasts)*200)/(abs(outsample)+abs(forecasts))
  return(smape)
}

mase_cal <- function(insample, outsample, forecasts){
  #Used to estimate MASE
  frq <- frequency(insample)
  forecastsNaiveSD <- rep(NA,frq)
  for (j in (frq+1):length(insample)){
    forecastsNaiveSD <- c(forecastsNaiveSD, insample[j-frq])
  }
  masep<-mean(abs(insample-forecastsNaiveSD),na.rm = TRUE)

  outsample <- as.numeric(outsample) ; forecasts <- as.numeric(forecasts)
  mase <- (abs(outsample-forecasts))/masep
  return(mase)
}

naive_seasonal <- function(input, fh){
  #Used to estimate Seasonal Naive
  frcy <- frequency(input)
  frcst <- naive(input, h=fh)$mean
  if (frcy>1){
    frcst <- head(rep(as.numeric(tail(input,frcy)), fh), fh) + frcst - frcst
  }
  return(frcst)
}

Theta.classic <- function(input, fh){
  #Used to estimate Theta classic

  #Set parameters
  wses <- wlrl<-0.5 ; theta <- 2
  #Estimate theta line (0)
  observations <- length(input)
  xt <- c(1:observations)
  xf <- c((observations+1):(observations+fh))
  train <- data.frame(input=input, xt=xt)
  test <- data.frame(xt = xf)

  estimate <- lm(input ~ poly(xt, 1, raw=TRUE))
  thetaline0In <- as.numeric(predict(estimate))
  thetaline0Out <- as.numeric(predict(estimate,test))

  #Estimate theta line (2)
  thetalineT <- theta*input+(1-theta)*thetaline0In
  sesmodel <- ses(thetalineT, h=fh)
  thetaline2In <- sesmodel$fitted
  thetaline2Out <- sesmodel$mean

  #Theta forecasts
  forecastsIn <- (thetaline2In*wses)+(thetaline0In*wlrl)
  forecastsOut <- (thetaline2Out*wses)+(thetaline0Out*wlrl)

  #Zero forecasts become positive
  for (i in 1:length(forecastsOut)){
    if (forecastsOut[i]<0){ forecastsOut[i]<-0 }
  }

  output=list(fitted = forecastsIn, mean = forecastsOut,
              fitted0 = thetaline0In, mean0 = thetaline0Out,
              fitted2 = thetaline2In, mean2 = thetaline2Out)

  return(output)
}

SeasonalityTest <- function(input, ppy){
  #Used to determine whether a time series is seasonal
  tcrit <- 1.645
  if (length(input)<3*ppy){
    test_seasonal <- FALSE
  }else{
    xacf <- acf(input, plot = FALSE)$acf[-1, 1, 1]
    clim <- tcrit/sqrt(length(input)) * sqrt(cumsum(c(1, 2 * xacf^2)))
    test_seasonal <- ( abs(xacf[ppy]) > clim[ppy] )

    if (is.na(test_seasonal)==TRUE){ test_seasonal <- FALSE }
  }

  return(test_seasonal)
}

Benchmarks <- function(input, fh){
  #Used to estimate the statistical benchmarks of the M4 competition

  #Estimate seasonaly adjusted time series
  ppy <- frequency(input) ; ST <- F
  if (ppy>1){ ST <- SeasonalityTest(input,ppy) }
  if (ST==T){
    Dec <- decompose(input,type="multiplicative")
    des_input <- input/Dec$seasonal
    SIout <- head(rep(Dec$seasonal[(length(Dec$seasonal)-ppy+1):length(Dec$seasonal)], fh), fh)
  }else{
    des_input <- input ; SIout <- rep(1, fh)
  }

  f1 <- naive(input, h=fh)$mean #Naive
  f2 <- naive_seasonal(input, fh=fh) #Seasonal Naive
  f3 <- naive(des_input, h=fh)$mean*SIout #Naive2
  f4 <- ses(des_input, h=fh)$mean*SIout #Ses
  f5 <- holt(des_input, h=fh, damped=F)$mean*SIout #Holt
  f6 <- holt(des_input, h=fh, damped=T)$mean*SIout #Damped
  f7 <- Theta.classic(input=des_input, fh=fh)$mean*SIout #Theta
  f8 <- (f4+f5+f6)/3 #Comb

  return(list(f1,f2,f3,f4,f5,f6,f7,f8))
}
