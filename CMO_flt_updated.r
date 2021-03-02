library(astsa)
library(car)
library(fGarch)
library(forecast)
library(leaps)
library(lmtest)
library(robustbase)
library(stats)
library(tseries)
library(usdm)

setwd("//us/home/P/pedro.martinez/R/R-3.2.2/data/annual_cycle/Investment_Spreads_part_II")

## Download of Data
data <- read.csv(file = "CMO_flt_updated.csv",header=T) # uploading response variable's time series
data <- apply(data[,-1],MARGIN=2,FUN=as.numeric)

  # from Basis Points to Percentage Points
  pct <- function(x){
    x/100 }
  
data <- ts(data,start=c(2000,01),end=c(2018,03),frequency=12)

data.log <- apply(data,MARGIN=2,FUN=log)
data.diff <- apply(data.log,MARGIN=2,FUN=diff)
data.diff <- ts(data.diff,start=c(2000,02),end=c(2018,03),frequency=12)

  par(bg='grey'); plot(data.diff,plot.type="s",col=rainbow(6:ncol(data.diff)),lwd=2); abline(h=0,lty=3)

  #### Average Assumption Test ####
  
  c6.0_c6.5 <- data.diff[,"X6.Discount.Margin"]   - data.diff[,"X6.5.Discount.Margin"]; 
  c6.5_c7.0 <- data.diff[,"X6.5.Discount.Margin"] - data.diff[,"X7.Discount.Margin"]  ; 
  c7.0_c7.5 <- data.diff[,"X7.Discount.Margin"]   - data.diff[,"X7.5.Discount.Margin"]; 
  c7.5_c8.0 <- data.diff[,"X7.5.Discount.Margin"] - data.diff[,"X8.Discount.Margin"]  ; 
  c8.0_c8.5 <- data.diff[,"X8.Discount.Margin"]   - data.diff[,"X8.5.Discount.Margin"]; 
  c8.5_c9.0 <- data.diff[,"X8.5.Discount.Margin"] - data.diff[,"X9.Discount.Margin"]  ; 
    
    plot(c6.0_c6.5,type="l")
    plot(c6.5_c7.0,type="l")
    plot(c7.0_c7.5,type="l")
    plot(c7.5_c8.0,type="l")
    plot(c8.0_c8.5,type="l")
    plot(c8.5_c9.0,type="l")
    
  avg.diff <- matrix(c(c6.0_c6.5,c6.5_c7.0,c7.0_c7.5,c7.5_c8.0,c8.0_c8.5,c8.5_c9.0),
                     ncol=ncol(data.diff)-1,nrow=nrow(data.diff),byrow=F)  
    colnames(avg.diff) <- c("c6.0_c6.5","c6.5_c7.0","c7.0_c7.5","c7.5_c8.0","c8.0_c8.5","c8.5_c9.0")
  
    avg.diff <- apply(avg.diff,MARGIN=2,pct)
    t <- nrow(avg.diff)
    
  for (i in 1:ncol(avg.diff)){
    t <- ts(1:length( na.omit( avg.diff[,i] ) ) )
    print(summary(lm(na.omit(avg.diff[,i])~0+t)) )
    }

  ###

### Avg of changes as Dependent Variable ###          
cmo <- rowMeans(as.matrix(data.diff),na.rm=T)
cmo <- ts(cmo,start=c(2000,02),end=c(2018,03),frequency=12)

  lines(cmo,lty=2,col="white")

adf.test(na.omit(cmo)); # Stationarity Test (before modeling)
acf2(na.omit(cmo)) # autocorrelation assessment (before modeling)


#### Predictor Variables ####
predictor <- read.csv(file = "PredictorVariables_updated.csv",header=T)
predictor <- apply(predictor[,-1],2,as.numeric)

  # transformation of smaller units
  predictor[,"FARB..Assets...Total.assets...Mil..USD..NSA."] <- predictor[,"FARB..Assets...Total.assets...Mil..USD..NSA."]/1000
  

predictor.diff <- apply(predictor,MARGIN=2,FUN=diff) # transformation into monthly difference

  #####  Relevant  Variables (from direct feedback of MBS trader)   ####
  predictor.diff <- predictor.diff[,c("X5.Years.Swap.Rate",
                                      "X5.Years.Swap.Rate.lag.1",
                                      "X5.Years.Swap.Rate.lag.2",
                                      "X10.Years.Swap.Rate",
                                      "X10.Years.Swap.Rate.lag.1",
                                      "X10.Years.Swap.Rate.lag.2",
                                      "X7.year.note.Current.Mid.Yield",
                                      "X7.year.note.Current.Mid.Yield.lag.1",
                                      "X7.year.note.Current.Mid.Yield.lag.2",
                                      "UST.YC.Slope..2y.v.10y.",
                                      "UST.YC.Slope..2y.v.10y..lag.1",
                                      "UST.YC.Slope..2y.v.10y..lag.2",
                                      "FHLMC.Fixed.30.Rate",
                                      "FHLMC.Fixed.30.Rate.lag.1",
                                      "FHLMC.Fixed.30.Rate.lag.2",
                                      "PMMS..30.year.FRM...Commitment.rate.....p.a...NSA.",
                                      "Moody.s.Intermediate.Term.Bond.Yield.Average..Corporate...Rated.A.....p.a...NSA.",
                                      "Aggregate.Reserves..Borrowings.from.F.R....Total.borrowings.of.depository.institutions...Mil..USD..NSA.",
                                      "TED.spread",
                                      "TED.spread.lag.1",
                                      "TED.spread.lag.2",
                                      "TED.Spread.JPM",
                                      "TED.Spread.JPM.lag.1",
                                      "TED.Spread.JPM.lag.2",
                                      "FARB..Assets...Total.assets...Mil..USD..NSA.",
                                      "VIX",
                                      "VIX.lag.1",
                                      "VIX.lag.2")]


predictor.diff.all <- as.data.frame(predictor.diff)


#### All Data ####
datall <- as.data.frame(cbind(as.numeric(cmo),as.data.frame(predictor.diff.all)))
  colnames(datall)[1] <- "cmo.diff"


#### Train / Test Split ####
  N.A. <- summary(datall[,1])["NA's"] # captures amount of not-available data since Jan/2000
  n.train <- 165 # ceiling(length(na.omit(datall[,1]))*.88) ### 88%/12% split between In-Sample and Out-of-Sample
train <- datall[(N.A.+1):(N.A.+n.train),] # splitting the Train data  from the Test
test  <- datall[(N.A.+n.train+1):nrow(datall),]


  #### Correlation Analysis ####
  m <- ncol(train)
  COR <- as.data.frame(matrix(rep(NA,m-1),nrow = m-1,ncol=1,byrow = T))
  
  for (i in 2:m){
    COR[i-1,1] <- cor(train[,1],train[,i],use="pairwise.complete.obs")
    row.names(COR)[i-1] <- colnames(train)[i]
  } ; 
  corr <- COR[order(abs(COR$V1),decreasing = T),,drop=F]; 
  
  print(corr)
  
  # Plot Analysis
  par(mfrow=c(2,2))
  for (i in 2:ncol(train)){
    plot(as.numeric(train[,1]),train[,i],xlab=colnames(train[i]),ylab=colnames(train[1]),
         main=toString(round(COR[i-1,1],digits=3) )
    )
  }
  par(mfrow=c(1,1))
      
  write.table(corr,"CMO_corr_CCR.txt",sep="\t")


  ### Statistical Significance Test on  Correlations ####
  COR.test <- as.data.frame(matrix(rep(NA,m-1),nrow = m-1,ncol=1,byrow = T))
  
  for (i in 2:m){
    COR.test[i-1,1] <- cor.test(train[,1],train[,i])$p.value
    row.names(COR.test)[i-1] <- colnames(train)[i]
  } ; 
  corr.test <- COR.test[order(abs(COR.test$V1),ascending = T),,drop=F]; 
  print(corr.test)
  rownames(corr.test[corr.test<.1,,drop=F])
  relevantVariables <- rownames(corr.test[corr.test<.1,,drop=F])

  
#### Stepwise Regression "Supervised" ####
  colnames(train[,-1])
leaps <- regsubsets(x=train[,-1],y=train[,"cmo.diff"],intercept=F,
                    force.in=c(19,11,22),
                    force.out=c(10,12,13:18,23:27),
                    #force.in=c(4,5),
                    #force.out=c(5),
                    nvmax=6,method="exhaustive",really.big=T) 
summary.regsubsets <- summary(leaps)

  names(coef(leaps,which.min(summary.regsubsets$bic)))
  coef(leaps,which.min(summary.regsubsets$bic))
  plot(leaps,scale="bic"); summary.regsubsets$bic # top models with set of predictor.variables ranked by AIC information criterion 
  
  names(coef(leaps,which.max(summary.regsubsets$adjr2)))
  coef(leaps,which.max(summary.regsubsets$adjr2))
  plot(leaps,scale="adjr2"); summary.regsubsets$adjr2 # top models with set of predictor.variables ranked by adjusted.R.square 
  
  

#### Regression  ####   
lm <- lm(cmo.diff~0+
           X10.Years.Swap.Rate+
           UST.YC.Slope..2y.v.10y..lag.1+
           PMMS..30.year.FRM...Commitment.rate.....p.a...NSA.+
           TED.spread.lag.1,    
           #Moody.s.Intermediate.Term.Bond.Yield.Average..Corporate...Rated.A.....p.a...NSA.+
           #FARB..Assets...Total.assets...Mil..USD..NSA.+
           #TED.spread,
           #Aggregate.Reserves..Borrowings.from.F.R....Total.borrowings.of.depository.institutions...Mil..USD..NSA.+
           data=train)
summary(lm)
BIC(lm)

## checking Multicollinearity
vif(lm$model[,-1])

## Unit Roots Check on all variables 
for (i in 1:ncol(lm$model)){
  print(adf.test(lm$model[,i],k=5) )
}
  ## check for autocorrelation
  acf2(lm$residuals)
  
  ## check for heteroscedasticity
  acf2(lm$residuals^2)
  plot(lm$residuals^2,type="l")
  
  # First Difference (Delta) Plot
  plot(train[,"cmo.diff"],type="l")
  lines(lm$fitted.values,col="blue")
  
  # Level Plot
  plot(,type="l")
  lines(data[1:(n.train+1)]+lm$fitted.values)


  # if no Heteroscedasticity:
  lm.wls <- lm

  
### Log-Correction-Factor (aka "lcf")  ####
## WLS
lcf <- lm(exp(train[,"cmo.diff"])~0+exp(lm.wls$fitted.values))$coefficients
print(lcf)


      
##### Test ####
fcst <- forecast(lm.wls,newdata=test,level=10)
  

### Plots  
plot(test[,"cmo.diff"],type="l") # actual data
lines(fcst$mean, col="blue") # garch Regression

  ## plot of Confidence Intervals
  # Upper
  lines(as.numeric(fcst$upper),lty=3,col="blue")
  lines(fcst.lm$upper,col="orange");lines(fcst.lm$mean,col=" dark orange",type="o")
  
  # Lower
  lines(as.numeric(fcst$lower),lty=3,col="blue")
  lines(fcst.lm$lower,col="orange")  


## RMSE Test set ##
sqrt(mean((test[,1] - fcst$mean)^2))


## Accumulated Errors: plot of OLS vs WLS out-of-sample 
plot(cumsum(abs(test[,"cmo.diff"]-fcst.lm$mean)),type="l",col="orange",ylab="acum errors")
lines(cumsum(abs(test[,"cmo.diff"]-fcst$pred)),col="blue")
lines(cumsum(abs(test[,"cmo.diff"]-fcst.pred.is)),col="red")


### Plot on Levels ####
## CMO 6.5% Cap
plot(data[(N.A.+n.train+1+1):nrow(data),"X6.5.Discount.Margin"],type="l",ylab="bps",main="6.5% Cap CMO",ylim=c(0,50))
lines(data[N.A.+n.train+1,"X6.5.Discount.Margin"]*cumprod(lcf*exp(fcst$mean)),col="blue")
fcst.test.6.5.level <- data[N.A.+n.train+1,"X6.5.Discount.Margin"]*cumprod(lcf*exp(fcst$mean))
  # confident intervals
  lines(data[N.A.+n.train+1,"X6.5.Discount.Margin"]*cumprod(lcf*exp(fcst$upper)),col="blue",lty=3)
  lines(data[N.A.+n.train+1,"X6.5.Discount.Margin"]*cumprod(lcf*exp(fcst$lower)),col="blue",lty=3)
  # Plot of Error
  plot(data[(N.A.+n.train+1+1):nrow(data),"X6.5.Discount.Margin"] - fcst.test.6.5.level,type="l",ylab="Test error")


## CMO 7.0% Cap
plot(data[(N.A.+n.train+1+1):nrow(data),"X7.Discount.Margin"],type="l",ylab="bps",ylim=c(0,50),main="7.0% Cap CMO")
lines(data[N.A.+n.train+1,"X7.Discount.Margin"]*cumprod(lcf*exp(fcst$mean)),col="blue")
fcst.test.7.0.level <- data[N.A.+n.train+1,"X7.Discount.Margin"]*cumprod(lcf*exp(fcst$mean))
  # confident intervals
  lines(data[N.A.+n.train+1,"X7.Discount.Margin"]*cumprod(lcf*exp(fcst$upper)),col="red",lty=3)
  lines(data[N.A.+n.train+1,"X7.Discount.Margin"]*cumprod(lcf*exp(fcst$lower)),col="red",lty=3)
  # Plot of Error
  plot(data[(N.A.+n.train+1+1):nrow(data),"X7.Discount.Margin"] - fcst.test.7.0.level,type="l",ylab="Test error")

  
#### fANCY Plot ####
  {
  par(mfrow=c(1,2)); par(bg='aliceblue')
plot(ts(train[,1],start=c(2002,05),end=c(2015,10),frequency=12),type="l",lwd=2.5,col="brown1",main="Train",ylab="change of spread",xlab="Date",ylim=c(-.5,.5))
  grid(col="white",lty=1,nx=4)
  lines(ts(train[,1],start=c(2002,05),end=c(2015,10),frequency=12),lwd=3,col="brown1")
lines(ts(lm$fitted.values,start=c(2002,05),end=c(2015,10),frequency=12),lwd=3.5,col="cyan3")

plot(ts(test[,1],start=c(2015,11),end=c(2017,04),frequency=12),col="brown1",main="Test",ylab="change of spread",xlab="Date",ylim=c(-.5,.5))
  grid(col="white",lty=1,nx=4)
  lines(ts(test[,1],start=c(2015,11),end=c(2017,04),frequency=12),lwd=3,col="brown1")
lines(ts(fcst$mean,start=c(2015,11),end=c(2017,04),frequency=12),lwd=3,col="cyan3")    

  }

  
#### Exporting Data ####
## Train ##
write.table(fitted(lm.wls),"CMO_train_ccr.txt",sep="\t")  

## Test ##
TEST <- matrix(NA,nrow=nrow(test),ncol=3,byrow=F)
colnames(TEST) <- c("fcst", "upper", "lower")

TEST[,"fcst"] <- fcst$mean
TEST[,"upper"] <- fcst$upper
TEST[,"lower"] <- fcst$lower

write.table(TEST,"CMO_test_ccr.txt",sep="\t")


test_level <- matrix(c(fcst.test.6.5.level,fcst.test.7.0.level),nrow=nrow(test),ncol=2,byrow=F)
write.table(test_level,"CMO_test_level.txt",sep="\t")  


  
#################################*
##### CST - 2018 Annual-Cycle ####
#################################*

setwd("//us/home/P/pedro.martinez/R/R-3.2.2/data/annual_cycle/Investment_Spreads_part_II/CST/2018_Annual-Cycle")



#### SupBaseline ####
sup.baseline <- read.csv(file = "cst_SupBaseline.csv",header=T) # uploading response variable's time series
actuals <- read.csv(file = "actuals.csv",header=T) # data with missing months between "last.test" and "first.cst"
names(lm.wls$coefficients)

## Data Transformation
sup.baseline[,"UST.YC.Slope..2y.v.10y."] <- (sup.baseline[,"TREASURY_10Y"] - sup.baseline[,"TREASURY_2Y"])*100
sup.baseline[,"UST.YC.Slope..2y.v.10y..lag.1"] <- c(56.161, sup.baseline[-nrow(sup.baseline),"UST.YC.Slope..2y.v.10y."] ) # hard-coded number is Dec/17 value = 1st_lag_variable value
sup.baseline[,"X10.Years.Swap.Rate"] <- sup.baseline[,"SWAP_10Y"]
sup.baseline[,"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."] <- sup.baseline[,"Commitment_Rate_30Y"]
sup.baseline[,"TED.spread"] <- (sup.baseline[,"LIBOR_3M"] - sup.baseline[,"TREASURY_3M"])*100
sup.baseline[,"TED.spread.lag.1"] <- c(26.091,sup.baseline[-nrow(sup.baseline),"TED.spread"]) # hard-coded number is Dec/17 value = 1st_lag_variable value

sup.baseline <- sup.baseline[, names(lm.wls$coefficients) ]
sup.baseline <- apply(sup.baseline,MARGIN=2,FUN=as.numeric)
sup.baseline.diff <- apply(sup.baseline,MARGIN=2,FUN=diff)
sup.baseline.diff <- as.data.frame(sup.baseline.diff)

## Forecast
fcst.sup.baseline <- forecast(lm.wls,newdata=sup.baseline.diff,level=10)
plot(fcst.sup.baseline$mean,type="l"); abline(h=0,lty=3)

# Level    
fcst.6.5.sup.baseline <- actuals[,"X6.5.Discount.Margin"]*cumprod(lcf*exp(fcst.sup.baseline$mean) ) # cst 1st data point is Jul/2017; last data point is Nov/2017
fcst.7.0.sup.baseline <- actuals[,  "X7.Discount.Margin"]*cumprod(lcf*exp(fcst.sup.baseline$mean) ) # cst 1st data point is Jul/2017; last data point is Nov/2017

# Plots in Level
plot(fcst.6.5.sup.baseline , type="l",main="SupBaseline - CMO 6.5% Cap",ylab="%")
plot(fcst.7.0.sup.baseline,  type="l",main="SupBaseline - CMO 7.0% Cap",ylab="%")
# Plot of Yield 
x1m_LIBOR.sup.baseline <- read.csv(file = "cst_SupBaseline.csv",header=T)
x1m_LIBOR.sup.baseline <- x1m_LIBOR.sup.baseline[,"LIBOR_1M"]
plot(fcst.6.5.sup.baseline/100 + x1m_LIBOR.sup.baseline[-1] , type="l",main="Yield CMO 6.5% Cap",ylab="%",ylim=c(0,7)); abline(h=6.5,lty=3); lines(x1m_LIBOR.sup.baseline[-1],col="dark grey", lty=3)
plot(fcst.7.0.sup.baseline/100 + x1m_LIBOR.sup.baseline[-1] , type="l",main="Yield CMO 7.0% Cap",ylab="%",ylim=c(0,7)); abline(h=7,lty=3); lines(x1m_LIBOR.sup.baseline[-1],col="dark grey", lty=3)


### Cap ###
## 6.5
for (i in 1:length(fcst.6.5.sup.baseline) ){
  if ( (fcst.6.5.sup.baseline[i]/100 + x1m_LIBOR.sup.baseline[-1][i]) > 6.5 ) {
    fcst.6.5.sup.baseline[i] <- (6.5 - x1m_LIBOR.sup.baseline[-1][i])*100 
  }
}

plot(fcst.6.5.sup.baseline/100 + x1m_LIBOR.sup.baseline[-1] , type="l",main="Yield CMO 6.5% Cap",ylab="%",ylim=c(0,7)); abline(h=6.5,lty=3)


## 7.0
for (i in 1:length(fcst.7.0.sup.baseline) ){
  if ( (fcst.7.0.sup.baseline[i]/100 + x1m_LIBOR.sup.baseline[-1][i]) > 7.0 ) {
    fcst.6.5.sup.baseline[i] <- (7.0 - x1m_LIBOR.sup.baseline[-1][i])*100 
  }
}
plot(fcst.7.0.sup.baseline/100 + x1m_LIBOR.sup.baseline[-1] , type="l",main="Yield CMO 7.0% Cap",ylab="%",ylim=c(0,7)); abline(h=7,lty=3)


### Floor ###
## 6.5
for (i in 1:length(fcst.6.5.sup.baseline) ){
  if ( (fcst.6.5.sup.baseline[i]/100 + x1m_LIBOR.sup.baseline[-1][i]) < x1m_LIBOR.sup.baseline[-1][i]) {
    fcst.6.5.sup.baseline[i] <- 0 
  }
}
plot(fcst.6.5.sup.baseline/100 + x1m_LIBOR.sup.baseline[-1] , type="l",main="Yield CMO 6.5% Cap",ylab="%",ylim=c(0,7)); abline(h=6.5,lty=3)
lines(x1m_LIBOR.sup.baseline[-1],lty=3,col="grey")

## 7.0
for (i in 1:length(fcst.7.0.sup.baseline) ){
  if ( (fcst.7.0.sup.baseline[i]/100 + x1m_LIBOR.sup.baseline[-1][i] ) < x1m_LIBOR.sup.baseline[-1][i]) {
    fcst.7.0.sup.baseline[i] <- 0 
  }
}
plot(fcst.7.0.sup.baseline/100 + x1m_LIBOR.sup.baseline[-1] , type="l",main="Yield CMO 7.0% Cap",ylab="%",ylim=c(0,7)); abline(h=7.0,lty=3)
lines(x1m_LIBOR.sup.baseline[-1],lty=3,col="grey")


#### Exporting CST - SupBaseline ####
baseline.cst <- matrix(NA,nrow=nrow(sup.baseline.diff),ncol=5,byrow=F) # 3 bond classes; 1 scenario + CI
colnames(baseline.cst) <- c("SupBaseline.ccr","SupBaseline.ccr.upper","SupBaseline.ccr.lower",
                       "fcst.6.5.sup.baseline","fcst.7.0.sup.baseline")

baseline.cst[,"SupBaseline.ccr"] <- fcst.sup.baseline$mean
baseline.cst[,"SupBaseline.ccr.upper"] <- fcst.sup.baseline$upper
baseline.cst[,"SupBaseline.ccr.lower"] <- fcst.sup.baseline$lower

baseline.cst[,"fcst.6.5.sup.baseline"] <- fcst.6.5.sup.baseline
baseline.cst[,"fcst.7.0.sup.baseline"] <- fcst.6.5.sup.baseline

write.table(baseline.cst,"CMO_CST_SupBaseline.txt",sep="\t")  



#### SupAdverse ####
sup.adverse <- read.csv(file = "cst_SupAdverse.csv",header=T) # uploading response variable's time series
actuals <- read.csv(file = "actuals.csv",header=T) # data with missing months between "last.test" and "first.cst"
names(lm.wls$coefficients)

## Data Transformation
sup.adverse[,"UST.YC.Slope..2y.v.10y."] <- (sup.adverse[,"TREASURY_10Y"] - sup.adverse[,"TREASURY_2Y"])*100
sup.adverse[,"UST.YC.Slope..2y.v.10y..lag.1"] <- c(56.161, sup.adverse[-nrow(sup.adverse),"UST.YC.Slope..2y.v.10y."] ) # hard-coded number is Dec/17 value = 1st_lag_variable value
sup.adverse[,"X10.Years.Swap.Rate"] <- sup.adverse[,"SWAP_10Y"]
sup.adverse[,"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."] <- sup.adverse[,"Commitment_Rate_30Y"]
sup.adverse[,"TED.spread"] <- (sup.adverse[,"LIBOR_3M"] - sup.adverse[,"TREASURY_3M"])*100
sup.adverse[,"TED.spread.lag.1"] <- c(26.091,sup.adverse[-nrow(sup.adverse),"TED.spread"]) # hard-coded number is Dec/17 value = 1st_lag_variable value

sup.adverse <- sup.adverse[, names(lm.wls$coefficients) ]
sup.adverse <- apply(sup.adverse,MARGIN=2,FUN=as.numeric)
sup.adverse.diff <- apply(sup.adverse,MARGIN=2,FUN=diff)
sup.adverse.diff <- as.data.frame(sup.adverse.diff)

## Forecast
fcst.sup.adverse <- forecast(lm.wls,newdata=sup.adverse.diff,level=10)
plot(fcst.sup.adverse$mean,type="l"); abline(h=0,lty=3)

# Level    
fcst.6.5.sup.adverse <- actuals[,"X6.5.Discount.Margin"]*cumprod(lcf*exp(fcst.sup.adverse$mean) ) # cst 1st data point is Jul/2017; last data point is Nov/2017
fcst.7.0.sup.adverse <- actuals[,  "X7.Discount.Margin"]*cumprod(lcf*exp(fcst.sup.adverse$mean) ) # cst 1st data point is Jul/2017; last data point is Nov/2017

# Plots in Level
plot(fcst.6.5.sup.adverse , type="l",main="SupAdverse - CMO 6.5% Cap",ylab="%")
plot(fcst.7.0.sup.adverse,  type="l",main="SupAdverse - CMO 7.0% Cap",ylab="%")
# Plot of Yield 
x1m_LIBOR.sup.adverse <- read.csv(file = "cst_SupAdverse.csv",header=T)
x1m_LIBOR.sup.adverse <- x1m_LIBOR.sup.adverse[,"LIBOR_1M"]
plot(fcst.6.5.sup.adverse/100 + x1m_LIBOR.sup.adverse[-1] , type="l",main="Yield CMO 6.5% Cap",ylab="%",ylim=c(0,7)); abline(h=6.5,lty=3); lines(x1m_LIBOR.sup.adverse[-1],col="dark grey", lty=3)
plot(fcst.7.0.sup.adverse/100 + x1m_LIBOR.sup.adverse[-1] , type="l",main="Yield CMO 7.0% Cap",ylab="%",ylim=c(0,7)); abline(h=7,lty=3); lines(x1m_LIBOR.sup.adverse[-1],col="dark grey", lty=3)


### Cap ###
## 6.5
for (i in 1:length(fcst.6.5.sup.adverse) ){
  if ( (fcst.6.5.sup.adverse[i]/100 + x1m_LIBOR.sup.adverse[-1][i]) > 6.5 ) {
    fcst.6.5.sup.adverse[i] <- (6.5 - x1m_LIBOR.sup.adverse[-1][i])*100 
  }
}

plot(fcst.6.5.sup.adverse/100 + x1m_LIBOR.sup.adverse[-1] , type="l",main="Yield CMO 6.5% Cap",ylab="%",ylim=c(0,7)); abline(h=6.5,lty=3)


## 7.0
for (i in 1:length(fcst.7.0.sup.adverse) ){
  if ( (fcst.7.0.sup.adverse[i]/100 + x1m_LIBOR.sup.adverse[-1][i]) > 7.0 ) {
    fcst.6.5.sup.adverse[i] <- (7.0 - x1m_LIBOR.sup.adverse[-1][i])*100 
  }
}
plot(fcst.7.0.sup.adverse/100 + x1m_LIBOR.sup.adverse[-1] , type="l",main="Yield CMO 7.0% Cap",ylab="%",ylim=c(0,7)); abline(h=7,lty=3)


### Floor ###
## 6.5
for (i in 1:length(fcst.6.5.sup.adverse) ){
  if ( (fcst.6.5.sup.adverse[i]/100 + x1m_LIBOR.sup.adverse[-1][i]) < x1m_LIBOR.sup.adverse[-1][i]) {
    fcst.6.5.sup.adverse[i] <- 0 
  }
}
plot(fcst.6.5.sup.adverse/100 + x1m_LIBOR.sup.adverse[-1] , type="l",main="Yield CMO 6.5% Cap",ylab="%",ylim=c(0,7)); abline(h=6.5,lty=3)
lines(x1m_LIBOR.sup.adverse[-1],lty=3,col="grey")

## 7.0
for (i in 1:length(fcst.7.0.sup.adverse) ){
  if ( (fcst.7.0.sup.adverse[i]/100 + x1m_LIBOR.sup.adverse[-1][i] ) < x1m_LIBOR.sup.adverse[-1][i]) {
    fcst.7.0.sup.adverse[i] <- 0 
  }
}
plot(fcst.7.0.sup.adverse/100 + x1m_LIBOR.sup.adverse[-1] , type="l",main="Yield CMO 7.0% Cap",ylab="%",ylim=c(0,7)); abline(h=7.0,lty=3)
lines(x1m_LIBOR.sup.adverse[-1],lty=3,col="grey")


#### Exporting CST - SupAdverse ####
adverse.cst <- matrix(NA,nrow=nrow(sup.adverse.diff),ncol=5,byrow=F) # 3 bond classes; 1 scenario + CI
colnames(adverse.cst) <- c("SupAdverse.ccr","SupAdverse.ccr.upper","SupAdverse.ccr.lower",
                            "fcst.6.5.sup.adverse","fcst.7.0.sup.adverse")

adverse.cst[,"SupAdverse.ccr"] <- fcst.sup.adverse$mean
adverse.cst[,"SupAdverse.ccr.upper"] <- fcst.sup.adverse$upper
adverse.cst[,"SupAdverse.ccr.lower"] <- fcst.sup.adverse$lower

adverse.cst[,"fcst.6.5.sup.adverse"] <- fcst.6.5.sup.adverse
adverse.cst[,"fcst.7.0.sup.adverse"] <- fcst.6.5.sup.adverse

write.table(adverse.cst,"CMO_CST_SupAdverse.txt",sep="\t")  



#### SupSevAdverse ####
sup.sev.adverse <- read.csv(file = "cst_SupSevAdverse.csv",header=T) # uploading response variable's time series
actuals <- read.csv(file = "actuals.csv",header=T) # data with missing months between "last.test" and "first.cst"
names(lm.wls$coefficients)

## Data Transformation
sup.sev.adverse[,"UST.YC.Slope..2y.v.10y."] <- (sup.sev.adverse[,"TREASURY_10Y"] - sup.sev.adverse[,"TREASURY_2Y"])*100
sup.sev.adverse[,"UST.YC.Slope..2y.v.10y..lag.1"] <- c(56.161, sup.sev.adverse[-nrow(sup.sev.adverse),"UST.YC.Slope..2y.v.10y."] ) # hard-coded number is Dec/17 value = 1st_lag_variable value
sup.sev.adverse[,"X10.Years.Swap.Rate"] <- sup.sev.adverse[,"SWAP_10Y"]
sup.sev.adverse[,"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."] <- sup.sev.adverse[,"Commitment_Rate_30Y"]
sup.sev.adverse[,"TED.spread"] <- (sup.sev.adverse[,"LIBOR_3M"] - sup.sev.adverse[,"TREASURY_3M"])*100
sup.sev.adverse[,"TED.spread.lag.1"] <- c(26.091,sup.sev.adverse[-nrow(sup.sev.adverse),"TED.spread"]) # hard-coded number is Dec/17 value = 1st_lag_variable value

sup.sev.adverse <- sup.sev.adverse[, names(lm.wls$coefficients) ]
sup.sev.adverse <- apply(sup.sev.adverse,MARGIN=2,FUN=as.numeric)
sup.sev.adverse.diff <- apply(sup.sev.adverse,MARGIN=2,FUN=diff)
sup.sev.adverse.diff <- as.data.frame(sup.sev.adverse.diff)

## Forecast
fcst.sup.sev.adverse <- forecast(lm.wls,newdata=sup.sev.adverse.diff,level=10)
plot(fcst.sup.sev.adverse$mean,type="l"); abline(h=0,lty=3)

# Level    
fcst.6.5.sup.sev.adverse <- actuals[,"X6.5.Discount.Margin"]*cumprod(lcf*exp(fcst.sup.sev.adverse$mean) ) # cst 1st data point is Jul/2017; last data point is Nov/2017
fcst.7.0.sup.sev.adverse <- actuals[,  "X7.Discount.Margin"]*cumprod(lcf*exp(fcst.sup.sev.adverse$mean) ) # cst 1st data point is Jul/2017; last data point is Nov/2017

# Plots in Level
plot(fcst.6.5.sup.sev.adverse , type="l",main="SupSevAdverse - CMO 6.5% Cap",ylab="%")
plot(fcst.7.0.sup.sev.adverse,  type="l",main="SupSevAdverse - CMO 7.0% Cap",ylab="%")
# Plot of Yield 
x1m_LIBOR.sup.sev.adverse <- read.csv(file = "cst_SupSevAdverse.csv",header=T)
x1m_LIBOR.sup.sev.adverse <- x1m_LIBOR.sup.sev.adverse[,"LIBOR_1M"]
plot(fcst.6.5.sup.sev.adverse/100 + x1m_LIBOR.sup.sev.adverse[-1] , type="l",main="Yield CMO 6.5% Cap",ylab="%",ylim=c(0,7)); abline(h=6.5,lty=3); lines(x1m_LIBOR.sup.sev.adverse[-1],col="dark grey", lty=3)
plot(fcst.7.0.sup.sev.adverse/100 + x1m_LIBOR.sup.sev.adverse[-1] , type="l",main="Yield CMO 7.0% Cap",ylab="%",ylim=c(0,7)); abline(h=7,lty=3); lines(x1m_LIBOR.sup.sev.adverse[-1],col="dark grey", lty=3)


### Cap ###
## 6.5
for (i in 1:length(fcst.6.5.sup.sev.adverse) ){
  if ( (fcst.6.5.sup.sev.adverse[i]/100 + x1m_LIBOR.sup.sev.adverse[-1][i]) > 6.5 ) {
    fcst.6.5.sup.sev.adverse[i] <- (6.5 - x1m_LIBOR.sup.sev.adverse[-1][i])*100 
  }
}

plot(fcst.6.5.sup.sev.adverse/100 + x1m_LIBOR.sup.sev.adverse[-1] , type="l",main="Yield CMO 6.5% Cap",ylab="%",ylim=c(0,7)); abline(h=6.5,lty=3)


## 7.0
for (i in 1:length(fcst.7.0.sup.sev.adverse) ){
  if ( (fcst.7.0.sup.sev.adverse[i]/100 + x1m_LIBOR.sup.sev.adverse[-1][i]) > 7.0 ) {
    fcst.6.5.sup.sev.adverse[i] <- (7.0 - x1m_LIBOR.sup.sev.adverse[-1][i])*100 
  }
}
plot(fcst.7.0.sup.sev.adverse/100 + x1m_LIBOR.sup.sev.adverse[-1] , type="l",main="Yield CMO 7.0% Cap",ylab="%",ylim=c(0,7)); abline(h=7,lty=3)


### Floor ###
## 6.5
for (i in 1:length(fcst.6.5.sup.sev.adverse) ){
  if ( (fcst.6.5.sup.sev.adverse[i]/100 + x1m_LIBOR.sup.sev.adverse[-1][i]) < x1m_LIBOR.sup.sev.adverse[-1][i]) {
    fcst.6.5.sup.sev.adverse[i] <- 0 
  }
}
plot(fcst.6.5.sup.sev.adverse/100 + x1m_LIBOR.sup.sev.adverse[-1] , type="l",main="Yield CMO 6.5% Cap",ylab="%",ylim=c(0,7)); abline(h=6.5,lty=3)
lines(x1m_LIBOR.sup.sev.adverse[-1],lty=3,col="grey")

## 7.0
for (i in 1:length(fcst.7.0.sup.sev.adverse) ){
  if ( (fcst.7.0.sup.sev.adverse[i]/100 + x1m_LIBOR.sup.sev.adverse[-1][i] ) < x1m_LIBOR.sup.sev.adverse[-1][i]) {
    fcst.7.0.sup.sev.adverse[i] <- 0 
  }
}
plot(fcst.7.0.sup.sev.adverse/100 + x1m_LIBOR.sup.sev.adverse[-1] , type="l",main="Yield CMO 7.0% Cap",ylab="%",ylim=c(0,7)); abline(h=7.0,lty=3)
lines(x1m_LIBOR.sup.sev.adverse[-1],lty=3,col="grey")


#### Exporting CST - SupSevAdverse ####
sev.adverse.cst <- matrix(NA,nrow=nrow(sup.sev.adverse.diff),ncol=5,byrow=F) # 3 bond classes; 1 scenario + CI
colnames(sev.adverse.cst) <- c("SupSevAdverse.ccr","SupSevAdverse.ccr.upper","SupSevAdverse.ccr.lower",
                           "fcst.6.5.sup.sev.adverse","fcst.7.0.sup.sev.adverse")

sev.adverse.cst[,"SupSevAdverse.ccr"] <- fcst.sup.sev.adverse$mean
sev.adverse.cst[,"SupSevAdverse.ccr.upper"] <- fcst.sup.sev.adverse$upper
sev.adverse.cst[,"SupSevAdverse.ccr.lower"] <- fcst.sup.sev.adverse$lower

sev.adverse.cst[,"fcst.6.5.sup.sev.adverse"] <- fcst.6.5.sup.sev.adverse
sev.adverse.cst[,"fcst.7.0.sup.sev.adverse"] <- fcst.6.5.sup.sev.adverse

write.table(sev.adverse.cst,"CMO_CST_SupSevAdverse.txt",sep="\t") 




#### SchStress ####
sch.stress <- read.csv(file = "cst_SchStress.csv",header=T) # uploading response variable's time series
actuals <- read.csv(file = "actuals.csv",header=T) # data with missing months between "last.test" and "first.cst"
names(lm.wls$coefficients)


## Data Transformation
sch.stress[,"UST.YC.Slope..2y.v.10y."] <- (sch.stress[,"TREASURY_10Y"] - sch.stress[,"TREASURY_2Y"])*100
sch.stress[,"UST.YC.Slope..2y.v.10y..lag.1"] <- c(56.161, sch.stress[-nrow(sch.stress),"UST.YC.Slope..2y.v.10y."] ) # hard-coded number is Dec/17 value = 1st_lag_variable value
sch.stress[,"X10.Years.Swap.Rate"] <- sch.stress[,"SWAP_10Y"]
sch.stress[,"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."] <- sch.stress[,"Commitment_Rate_30Y"]
sch.stress[,"TED.spread"] <- (sch.stress[,"LIBOR_3M"] - sch.stress[,"TREASURY_3M"])*100
sch.stress[,"TED.spread.lag.1"] <- c(26.091,sch.stress[-nrow(sch.stress),"TED.spread"]) # hard-coded number is Dec/17 value = 1st_lag_variable value

sch.stress <- sch.stress[, names(lm.wls$coefficients) ]
sch.stress <- apply(sch.stress,MARGIN=2,FUN=as.numeric)
sch.stress.diff <- apply(sch.stress,MARGIN=2,FUN=diff)
sch.stress.diff <- as.data.frame(sch.stress.diff)

## Forecast
fcst.sch.stress <- forecast(lm.wls,newdata=sch.stress.diff,level=10)
plot(fcst.sch.stress$mean,type="l"); abline(h=0,lty=3)

# Level    
fcst.6.5.sch.stress <- actuals[,"X6.5.Discount.Margin"]*cumprod(lcf*exp(fcst.sch.stress$mean) ) # cst 1st data point is Jul/2017; last data point is Nov/2017
fcst.7.0.sch.stress <- actuals[,  "X7.Discount.Margin"]*cumprod(lcf*exp(fcst.sch.stress$mean) ) # cst 1st data point is Jul/2017; last data point is Nov/2017

# Plots in Level
plot(fcst.6.5.sch.stress , type="l",main="SchStress - CMO 6.5% Cap",ylab="%")
plot(fcst.7.0.sch.stress,  type="l",main="SchStress - CMO 7.0% Cap",ylab="%")
# Plot of Yield 
x1m_LIBOR.sch.stress <- read.csv(file = "cst_SchStress.csv",header=T)
x1m_LIBOR.sch.stress <- x1m_LIBOR.sch.stress[,"LIBOR_1M"]
plot(fcst.6.5.sch.stress/100 + x1m_LIBOR.sch.stress[-1] , type="l",main="Yield CMO 6.5% Cap",ylab="%",ylim=c(0,7)); abline(h=6.5,lty=3); lines(x1m_LIBOR.sch.stress[-1],col="dark grey", lty=3)
plot(fcst.7.0.sch.stress/100 + x1m_LIBOR.sch.stress[-1] , type="l",main="Yield CMO 7.0% Cap",ylab="%",ylim=c(0,7)); abline(h=7,lty=3); lines(x1m_LIBOR.sch.stress[-1],col="dark grey", lty=3)


### Cap ###
## 6.5
for (i in 1:length(fcst.6.5.sch.stress) ){
  if ( (fcst.6.5.sch.stress[i]/100 + x1m_LIBOR.sch.stress[-1][i]) > 6.5 ) {
    fcst.6.5.sch.stress[i] <- (6.5 - x1m_LIBOR.sch.stress[-1][i])*100 
  }
}

plot(fcst.6.5.sch.stress/100 + x1m_LIBOR.sch.stress[-1] , type="l",main="Yield CMO 6.5% Cap",ylab="%",ylim=c(0,7)); abline(h=6.5,lty=3)


## 7.0
for (i in 1:length(fcst.7.0.sch.stress) ){
  if ( (fcst.7.0.sch.stress[i]/100 + x1m_LIBOR.sch.stress[-1][i]) > 7.0 ) {
    fcst.6.5.sch.stress[i] <- (7.0 - x1m_LIBOR.sch.stress[-1][i])*100 
  }
}
plot(fcst.7.0.sch.stress/100 + x1m_LIBOR.sch.stress[-1] , type="l",main="Yield CMO 7.0% Cap",ylab="%",ylim=c(0,7)); abline(h=7,lty=3)


### Floor ###
## 6.5
for (i in 1:length(fcst.6.5.sch.stress) ){
  if ( (fcst.6.5.sch.stress[i]/100 + x1m_LIBOR.sch.stress[-1][i]) < x1m_LIBOR.sch.stress[-1][i]) {
    fcst.6.5.sch.stress[i] <- 0 
  }
}
plot(fcst.6.5.sch.stress/100 + x1m_LIBOR.sch.stress[-1] , type="l",main="Yield CMO 6.5% Cap",ylab="%",ylim=c(0,7)); abline(h=6.5,lty=3)
lines(x1m_LIBOR.sch.stress[-1],lty=3,col="grey")

## 7.0
for (i in 1:length(fcst.7.0.sch.stress) ){
  if ( (fcst.7.0.sch.stress[i]/100 + x1m_LIBOR.sch.stress[-1][i] ) < x1m_LIBOR.sch.stress[-1][i]) {
    fcst.7.0.sch.stress[i] <- 0 
  }
}
plot(fcst.7.0.sch.stress/100 + x1m_LIBOR.sch.stress[-1] , type="l",main="Yield CMO 7.0% Cap",ylab="%",ylim=c(0,7)); abline(h=7.0,lty=3)
lines(x1m_LIBOR.sch.stress[-1],lty=3,col="grey")


    #### Exporting CST - SchStress ####
    sch.cst <- matrix(NA,nrow=nrow(sch.stress.diff),ncol=5,byrow=F) # 3 bond classes; 1 scenario + CI
    colnames(sch.cst) <- c("SchStress.ccr","SchStress.ccr.upper","SchStress.ccr.lower",
                           "fcst.6.5.sch.stress","fcst.7.0.sch.stress")
    
    sch.cst[,"SchStress.ccr"] <- fcst.sch.stress$mean
    sch.cst[,"SchStress.ccr.upper"] <- fcst.sch.stress$upper
    sch.cst[,"SchStress.ccr.lower"] <- fcst.sch.stress$lower
    
    sch.cst[,"fcst.6.5.sch.stress"] <- fcst.6.5.sch.stress
    sch.cst[,"fcst.7.0.sch.stress"] <- fcst.6.5.sch.stress
    
    write.table(sch.cst,"CMO_CST_SchStress.txt",sep="\t")  

    

##########################*
### 2017 Mid-Cycle CST ####
##########################*
  

# last day of test set
  last.test <- as.Date("2017-11-30") 
# first day of Mid-Cycle CST: Jul/2017
  first.cst  <- as.Date("2017-06-30")      
# time difference
  td <- length(seq(from=first.cst, to=last.test, by='month'))


#### Baseline ####
baseline <- read.csv(file = "cst_baseline.csv",header=T) # uploading response variable's time series
names(lm.wls$coef)

  ## Data Transformation
  baseline[,"UST.YC.Slope..2y.v.10y."] <- (baseline[,"TREASURY_10Y"] - baseline[,"TREASURY_2Y"])*100
  baseline[,"UST.YC.Slope..2y.v.10y..lag.1"] <- c(predictor[nrow(data)-td-1,"UST.YC.Slope..2y.v.10y."], baseline[-nrow(baseline),"UST.YC.Slope..2y.v.10y."] )

  baseline[,"X10.Years.Swap.Rate"] <- baseline[,"SWAP_10Y"]
  
  baseline[,"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."] <- baseline[,"Commitment_Rate_30Y"]
  
  baseline[,"TED.spread"] <- (baseline[,"LIBOR_3M"] - baseline[,"TREASURY_3M"])*100
  baseline[,"TED.spread.lag.1"] <- c(predictor[nrow(data)-td-1,"TED.spread"],baseline[-nrow(baseline),"TED.spread"]) # last value of Test set is Nov/2017; first data point of cst is Jul/2017; thus: nrow(test)-5
  
  ##

baseline <- baseline[, names(lm.wls$coefficients) ]
baseline <- apply(baseline,MARGIN=2,FUN=as.numeric)
baseline.diff <- apply(baseline,MARGIN=2,FUN=diff)
baseline.diff <- as.data.frame(baseline.diff)

fcst.baseline <- forecast(lm.wls,newdata=baseline.diff,level=10)
plot(fcst.baseline$mean[-1],type="l"); abline(h=0,lty=3)

# Level    
fcst.6.5.baseline <- data[nrow(data)-td,"X6.5.Discount.Margin"]*cumprod(lcf*exp(fcst.baseline$mean) ) # cst 1st data point is Jul/2017; last data point is Nov/2017
fcst.7.0.baseline <- data[nrow(data)-td,"X7.Discount.Margin"]*cumprod(lcf*exp(fcst.baseline$mean) ) # cst 1st data point is Jul/2017; last data point is Nov/2017

# Plots in Level
plot(fcst.6.5.baseline , type="l",main="Baseline - CMO 6.5% Cap",ylab="%")
plot(fcst.7.0.baseline,  type="l",main="Baseline - CMO 7.0% Cap",ylab="%")
 # Plot of Yield 
  x1m_LIBOR.baseline <- read.csv(file = "cst_baseline.csv",header=T)
  x1m_LIBOR.baseline <- x1m_LIBOR.baseline[,"LIBOR_1M"]
 plot(fcst.6.5.baseline/100 + x1m_LIBOR.baseline[-1] , type="l",main="Yield CMO 6.5% Cap",ylab="%",ylim=c(0,7)); abline(h=6.5,lty=3); lines(x1m_LIBOR.baseline[-1],col="dark grey", lty=3)
 plot(fcst.7.0.baseline/100 + x1m_LIBOR.baseline[-1] , type="l",main="Yield CMO 7.0% Cap",ylab="%",ylim=c(0,7)); abline(h=7,lty=3); lines(x1m_LIBOR.baseline[-1],col="dark grey", lty=3)
 

 ### Cap ###
 ## 6.5
 for (i in 1:length(fcst.6.5.baseline) ){
   if ( (fcst.6.5.baseline[i]/100 + x1m_LIBOR.baseline[-1][i]) > 6.5 ) {
     fcst.6.5.baseline[i] <- (6.5 - x1m_LIBOR.baseline[-1][i])*100 
   }
 }
 
 plot(fcst.6.5.baseline/100 + x1m_LIBOR.baseline[-1] , type="l",main="Yield CMO 6.5% Cap",ylab="%",ylim=c(0,7)); abline(h=6.5,lty=3)
 
 
 ## 7.0
 for (i in 1:length(fcst.7.0.baseline) ){
   if ( (fcst.7.0.baseline[i]/100 + x1m_LIBOR.baseline[-1][i]) > 7.0 ) {
     fcst.6.5.baseline[i] <- (7.0 - x1m_LIBOR.baseline[-1][i])*100 
   }
 }
 plot(fcst.7.0.baseline/100 + x1m_LIBOR.baseline[-1] , type="l",main="Yield CMO 7.0% Cap",ylab="%",ylim=c(0,7)); abline(h=7,lty=3)
 
 
 ### Floor ###
 ## 6.5
 for (i in 1:length(fcst.6.5.baseline) ){
   if ( (fcst.6.5.baseline[i]/100 + x1m_LIBOR.baseline[-1][i]) < x1m_LIBOR.baseline[-1][i]) {
     fcst.6.5.baseline[i] <- 0 
   }
 }
 plot(fcst.6.5.baseline/100 + x1m_LIBOR.baseline[-1] , type="l",main="Yield CMO 6.5% Cap",ylab="%",ylim=c(0,7)); abline(h=6.5,lty=3)
 lines(x1m_LIBOR.baseline[-1],lty=3,col="grey")
 
 ## 7.0
 for (i in 1:length(fcst.7.0.baseline) ){
   if ( (fcst.7.0.baseline[i]/100 + x1m_LIBOR.baseline[-1][i] ) < x1m_LIBOR.baseline[-1][i]) {
     fcst.7.0.baseline[i] <- 0 
   }
 }
 plot(fcst.7.0.baseline/100 + x1m_LIBOR.baseline[-1] , type="l",main="Yield CMO 7.0% Cap",ylab="%",ylim=c(0,7)); abline(h=7.0,lty=3)
 lines(x1m_LIBOR.baseline[-1],lty=3,col="grey")
 
 
 
#### Adverse ####
adverse <- read.csv(file = "cst_adverse.csv",header=T) # uploading response variable's time series
 names(lm.wls$coef)
 
 ## Data Transformation
 adverse[,"UST.YC.Slope..2y.v.10y."] <- (adverse[,"TREASURY_10Y"] - adverse[,"TREASURY_2Y"])*100
 adverse[,"UST.YC.Slope..2y.v.10y..lag.1"] <- c(predictor[nrow(data)-td-1,"UST.YC.Slope..2y.v.10y."], adverse[-nrow(adverse),"UST.YC.Slope..2y.v.10y."] )
 
 adverse[,"X10.Years.Swap.Rate"] <- adverse[,"SWAP_10Y"]
 
 adverse[,"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."] <- adverse[,"Commitment_Rate_30Y"]
 
 adverse[,"TED.spread"] <- (adverse[,"LIBOR_3M"] - adverse[,"TREASURY_3M"])*100
 adverse[,"TED.spread.lag.1"] <- c(predictor[nrow(data)-td-1,"TED.spread"],adverse[-nrow(adverse),"TED.spread"]) # last value of Test set is Nov/2017; first data point of cst is Jul/2017; thus: nrow(test)-5
 
 ##
 
 adverse <- adverse[, names(lm.wls$coefficients) ]
 adverse <- apply(adverse,MARGIN=2,FUN=as.numeric)
 adverse.diff <- apply(adverse,MARGIN=2,FUN=diff)
 adverse.diff <- as.data.frame(adverse.diff)
 
 fcst.adverse <- forecast(lm.wls,newdata=adverse.diff,level=10)
 plot(fcst.adverse$mean[-1],type="l"); abline(h=0,lty=3)
 
 # Level    
 fcst.6.5.adverse <- data[nrow(data)-td,"X6.5.Discount.Margin"]*cumprod(lcf*exp(fcst.adverse$mean) ) # cst 1st data point is Jul/2017; last data point is Nov/2017
 fcst.7.0.adverse <- data[nrow(data)-td,"X7.Discount.Margin"]*cumprod(lcf*exp(fcst.adverse$mean) ) # cst 1st data point is Jul/2017; last data point is Nov/2017
 
 # Plots in Level
 plot(fcst.6.5.adverse , type="l",main="Adverse - CMO 6.5% Cap",ylab="%")
 plot(fcst.7.0.adverse,  type="l",main="Adverse - CMO 7.0% Cap",ylab="%")
 
 # Plot of Yield 
 x1m_LIBOR.adverse <- read.csv(file = "cst_adverse.csv",header=T)
 x1m_LIBOR.adverse <- x1m_LIBOR.adverse[,"LIBOR_1M"]
 plot(fcst.6.5.adverse/100 + x1m_LIBOR.adverse[-1] , type="l",main="Yield CMO 6.5% Cap",ylab="%",ylim=c(0,7)); abline(h=6.5,lty=3); lines(x1m_LIBOR.adverse[-1],col="dark grey", lty=3)
 plot(fcst.7.0.adverse/100 + x1m_LIBOR.adverse[-1] , type="l",main="Yield CMO 7.0% Cap",ylab="%",ylim=c(0,7)); abline(h=7,lty=3); lines(x1m_LIBOR.adverse[-1],col="dark grey", lty=3)
 
  ### Cap ###
  ## 6.5
    for (i in 1:length(fcst.6.5.adverse) ){
      if ( (fcst.6.5.adverse[i]/100 + x1m_LIBOR.adverse[-1][i]) > 6.5 ) {
          fcst.6.5.adverse[i] <- (6.5 - x1m_LIBOR.adverse[-1][i])*100 
        }
    }
 
 plot(fcst.6.5.adverse/100 + x1m_LIBOR.adverse[-1] , type="l",main="Yield CMO 6.5% Cap",ylab="%",ylim=c(0,7)); abline(h=6.5,lty=3)
 
 
 ## 7.0
  for (i in 1:length(fcst.7.0.adverse) ){
    if ( (fcst.7.0.adverse[i]/100 + x1m_LIBOR.adverse[-1][i]) > 7.0 ) {
      fcst.6.5.adverse[i] <- (7.0 - x1m_LIBOR.adverse[-1][i])*100 
    }
  }
 plot(fcst.7.0.adverse/100 + x1m_LIBOR.adverse[-1] , type="l",main="Yield CMO 7.0% Cap",ylab="%",ylim=c(0,7)); abline(h=7,lty=3)
 
 
 ### Floor ###
 ## 6.5
 for (i in 1:length(fcst.6.5.adverse) ){
   if ( (fcst.6.5.adverse[i]/100 + x1m_LIBOR.adverse[-1][i]) < x1m_LIBOR.adverse[-1][i]) {
     fcst.6.5.adverse[i] <- 0 
   }
 }
 plot(fcst.6.5.adverse/100 + x1m_LIBOR.adverse[-1] , type="l",main="Yield CMO 6.5% Cap",ylab="%",ylim=c(0,7)); abline(h=6.5,lty=3)
 lines(x1m_LIBOR.adverse[-1],lty=3,col="grey")
 
 ## 7.0
 for (i in 1:length(fcst.7.0.adverse) ){
   if ( (fcst.7.0.adverse[i]/100 + x1m_LIBOR.adverse[-1][i]) < x1m_LIBOR.adverse[-1][i]) {
     fcst.7.0.adverse[i] <- 0 
   }
 }
 plot(fcst.7.0.adverse/100 + x1m_LIBOR.adverse[-1] , type="l",main="Yield CMO 7.0% Cap",ylab="%",ylim=c(0,7)); abline(h=7.0,lty=3)
 lines(x1m_LIBOR.adverse[-1],lty=3,col="dark grey")
 
 

#### Sev.Adverse ####
 sev.adverse <- read.csv(file = "cst_sev.adverse.csv",header=T) # uploading response variable's time series
 names(lm.wls$coef)
 
 ## Data Transformation
 sev.adverse[,"UST.YC.Slope..2y.v.10y."] <- (sev.adverse[,"TREASURY_10Y"] - sev.adverse[,"TREASURY_2Y"])*100
 sev.adverse[,"UST.YC.Slope..2y.v.10y..lag.1"] <- c(predictor[nrow(data)-td-1,"UST.YC.Slope..2y.v.10y."], sev.adverse[-nrow(sev.adverse),"UST.YC.Slope..2y.v.10y."] )
 
 sev.adverse[,"X10.Years.Swap.Rate"] <- sev.adverse[,"SWAP_10Y"]
 
 sev.adverse[,"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."] <- sev.adverse[,"Commitment_Rate_30Y"]
 
 sev.adverse[,"TED.spread"] <- ( sev.adverse[,"LIBOR_3M"] - sev.adverse[,"TREASURY_3M"] )*100
 sev.adverse[,"TED.spread.lag.1"] <- c(predictor[nrow(data)-td-1,"TED.spread"],sev.adverse[-nrow(sev.adverse),"TED.spread"]) # last value of Test set is Nov/2017; first data point of cst is Jul/2017; thus: nrow(test)-5
 
 ##
 
 sev.adverse <- sev.adverse[, names(lm.wls$coefficients) ]
 sev.adverse <- apply(sev.adverse,MARGIN=2,FUN=as.numeric)
 sev.adverse.diff <- apply(sev.adverse,MARGIN=2,FUN=diff)
 sev.adverse.diff <- as.data.frame(sev.adverse.diff)
 
 fcst.sev.adverse <- forecast(lm.wls,newdata=sev.adverse.diff,level=10)
 plot(fcst.sev.adverse$mean[-1],type="l"); abline(h=0,lty=3)
 
 # Level    
 fcst.6.5.sev.adverse <- data[nrow(data)-td,"X6.5.Discount.Margin"]*cumprod(lcf*exp(fcst.sev.adverse$mean) )
 fcst.7.0.sev.adverse <- data[nrow(data)-td,"X7.Discount.Margin"]*cumprod(lcf*exp(fcst.sev.adverse$mean) )
 
 # Plots in Level
 plot(fcst.6.5.sev.adverse , type="l",main="Sev.Adverse - CMO 6.5% Cap",ylab="%")
 plot(fcst.7.0.sev.adverse,  type="l",main="Sev.Adverse - CMO 7.0% Cap",ylab="%")
 # Plot of Yield 
 x1m_LIBOR.sev.adverse <- read.csv(file = "cst_sev.adverse.csv",header=T)
 x1m_LIBOR.sev.adverse <- x1m_LIBOR.sev.adverse[,"LIBOR_1M"]
 plot(fcst.6.5.sev.adverse/100 + x1m_LIBOR.sev.adverse[-1] , type="l",main="Yield CMO 6.5% Cap",ylab="%",ylim=c(0,7)); abline(h=6.5,lty=3); lines(x1m_LIBOR.sev.adverse[-1],col="dark grey",lty=3)
 plot(fcst.7.0.sev.adverse/100 + x1m_LIBOR.sev.adverse[-1] , type="l",main="Yield CMO 7.0% Cap",ylab="%",ylim=c(0,7)); abline(h=7,lty=3); lines(x1m_LIBOR.sev.adverse[-1],col="dark grey",lty=3)
 
 ### Cap ###
 ## 6.5
 for (i in 1:length(fcst.6.5.sev.adverse) ){
   if ( (fcst.6.5.sev.adverse[i]/100 + x1m_LIBOR.sev.adverse[-1][i]) > 6.5 ) {
     fcst.6.5.sev.adverse[i] <- (6.5 - x1m_LIBOR.sev.adverse[-1][i])*100 
   }
 }
 plot(fcst.6.5.sev.adverse/100 + x1m_LIBOR.sev.adverse[-1] , type="l",main="Yield CMO 6.5% Cap",ylab="%",ylim=c(0,7)); abline(h=6.5,lty=3)
 
 
 ## 7.0
 for (i in 1:length(fcst.7.0.sev.adverse) ){
   if ( (fcst.7.0.sev.adverse[i]/100 + x1m_LIBOR.sev.adverse[-1][i]) > 7.0 ) {
     fcst.6.5.sev.adverse[i] <- (7.0 - x1m_LIBOR.sev.adverse[-1][i])*100 
   }
 }
 plot(fcst.7.0.sev.adverse/100 + x1m_LIBOR.sev.adverse[-1] , type="l",main="Yield CMO 7.0% Cap",ylab="%",ylim=c(0,7)); abline(h=7,lty=3)
 
 
 ### Floor ###
 ## 6.5
 for (i in 1:length(fcst.6.5.sev.adverse) ){
   if ( (fcst.6.5.sev.adverse[i]/100 + x1m_LIBOR.sev.adverse[-1][i])< x1m_LIBOR.sev.adverse[-1][i]) {
     fcst.6.5.sev.adverse[i] <- 0 
   }
 }
 plot(fcst.6.5.sev.adverse/100 + x1m_LIBOR.sev.adverse[-1] , type="l",main="Yield CMO 6.5% Cap",ylab="%",ylim=c(0,7)); abline(h=6.5,lty=3)
 lines(x1m_LIBOR.sev.adverse[-1],lty=3,col="grey")
 
 ## 7.0
 for (i in 1:length(fcst.7.0.sev.adverse) ){
   if ( (fcst.7.0.sev.adverse[i]/100 + x1m_LIBOR.sev.adverse[-1][i]) < x1m_LIBOR.sev.adverse[-1][i]) {
     fcst.7.0.sev.adverse[i] <- 0 
   }
 }
 plot(fcst.7.0.sev.adverse/100 + x1m_LIBOR.sev.adverse[-1] , type="l",main="Yield CMO 7.0% Cap",ylab="%",ylim=c(0,7)); abline(h=7.0,lty=3)
 lines(x1m_LIBOR.sev.adverse[-1],lty=3,col="grey")
 
 
  

#### Exporting Data ####  
# CST - CCR
cst <- matrix(NA,nrow=nrow(baseline.diff),ncol=3*3,byrow=F) # 3 bond classes; 3 scenarios
colnames(cst) <- c("baseline","baseline.upper","baseline.lower",
                   "adverse", "adverse.upper","adverse.lower",
                   "sev.adverse","sev.adverse.upper","sev.adverse.lower")

cst[,"baseline"] <- fcst.baseline$mean
cst[,"baseline.upper"] <- fcst.baseline$upper
cst[,"baseline.lower"] <- fcst.baseline$lower

cst[,"adverse"] <- fcst.adverse$mean
cst[,"adverse.upper"] <- fcst.adverse$upper
cst[,"adverse.lower"] <- fcst.adverse$lower

cst[,"sev.adverse"] <- fcst.sev.adverse$mean
cst[,"sev.adverse.upper"] <- fcst.sev.adverse$upper
cst[,"sev.adverse.lower"] <- fcst.sev.adverse$lower

write.table(cst,"CMO_cst_ccr.txt",sep="\t")

# CST - Level
cst.level <- matrix(cbind(fcst.6.5.baseline,fcst.7.0.baseline,
                  fcst.6.5.adverse,fcst.7.0.adverse,
                  fcst.6.5.sev.adverse,fcst.7.0.sev.adverse),
                nrow=length(fcst.6.5.baseline),ncol=3*2,byrow=F)
write.table(cst.level,"CMO_cst_level_cap.floor.txt",sep="\t")

