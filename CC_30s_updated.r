library(astsa)
library(car)
library(FinTS)
library(fGarch)
library(forecast)
library(leaps)
library(lmtest)
library(MTS)
library(robustbase)
library(tseries)
library(usdm)

setwd("//us/home/P/pedro.martinez/R/R-3.2.2/data/annual_cycle/Investment_Spreads_part_II")

## Download of Data
data <- read.csv(file = "CC_updated.csv",header=T) # uploading response variable's time series
data <- apply(data[,-1],MARGIN=2,FUN=as.numeric)
data <- ts(data,start=c(2000,01),end=c(2018,03),frequency=12)

data.diff <- apply(data,MARGIN=2,FUN=diff)
data.diff <- ts(data.diff,start=c(2000,02),end=c(2018,03),frequency=12)
  # Plot
  par(bg='grey'); plot(data.diff,plot.type="s",col=rainbow(6:ncol(data.diff)),lwd=2); abline(h=0,lty=3)

## Means:
  
  # Mean of 30's (FNs + GNs)
  cc.30.diff.mean <- rowMeans(as.matrix(data.diff[,c(2,4,6)]),na.rm=T)
  cc.30.diff.mean <- ts(cc.30.diff.mean,start=c(2000,02),end=c(2018,03),frequency=12)

cc.diff.mean <- cc.30.diff.mean

plot(cc.diff.mean,type="l")

adf.test(na.omit(cc.diff.mean)); ## Unit Root Analysis
acf2(na.omit(cc.diff.mean)) ## Autocorrelation


      ### statistical difference between FNs vs GNs ####
      gap.30s <- data.diff[,"FNMA.CC.15yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]-data.diff[,"GNMA.CC.15yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]
      plot(gap.30s,type="l"); abline(h=0,lty=3)
      
        #outliers.30s <- (40:43)
      t <- ts(1:length(as.numeric(gap.30s)))
        #lm.30s <- lm(gap.30s[-outliers.30s]~0+t[-outliers.30s]); summary(lm.30s)
      lm.30s <- lm(gap.30s~0+t); summary(lm.30s)
      plot(as.numeric(t.30s),as.numeric(gap.30s))
      
      gap.30sII <- data.diff[,"FNMA.CC.15yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]-data.diff[,"GNMAII.CC.15yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]
      plot(gap.30sII,type="l")
      t2 <- ts(1:length(as.numeric(gap.30sII)))
      lm.30sII <- lm(gap.30sII~0+t2); summary(lm.30sII)

      
### Predictor Variables ####
predictor <- read.csv(file = "PredictorVariables_updated.csv",header=T)
predictor <- apply(predictor[,-1],2,as.numeric)

predictor.diff <- apply(predictor,MARGIN=2,FUN=diff)

  ##### Related Variables #### (from a feedback of CC trader)
  predictor.diff <- predictor.diff[,c(
    "X7.Years.Swap.Rate",
    "X10.Years.Swap.Rate",
    "X5.Years.Swap.Rate",
    "X10.year.note.Current.Mid.Yield",
    #"PMMS..15.year.FRM...Commitment.rate.....p.a...NSA.",
    "PMMS..30.year.FRM...Commitment.rate.....p.a...NSA.",
    "UST.YC.Slope..2y.v.10y.",
    "S.P.CoreLogic.Case.Shiller.Home.Price.Indices..Single.family...Aggregate.index...20.metro.composite...Index.Jan2000.100.3.mo..MA..SA.",
    "Moody.s.Intermediate.Term.Bond.Yield.Average..Corporate...Rated.Aa.....p.a...NSA."
  )]
  
predictor.diff.all <- as.data.frame(predictor.diff)
  

#### Primary-Secondary Mortgage Spread (PSS)- plot ####
pss.FN <- predictor[,"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."]-
  data[,"FNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]
#pss.FN <- ts(pss.FN,start=c(2000,01),end=c(2017,11),frequency=12)
plot(pss.FN,type="l")

pss.GNI <- predictor[,"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."]-
  data[,"GNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]
#pss.GNI <- ts(pss.GNI,start=c(2000,01),end=c(2017,11),frequency=12)
plot(pss.GNI,type="l")

pss.GNII <- predictor[,"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."]-
  data[,"GNMAII.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]
#pss.GNII <- ts(pss.GNII,start=c(2000,01),end=c(2017,11),frequency=12)
plot(pss.GNII,type="l")


#### All Data ####
datall <- as.data.frame(cbind(as.numeric(cc.diff.mean),as.data.frame(predictor.diff.all)))
colnames(datall)[1] <- "cc.diff.mean"

##### Train / Test Split ####
N.A. <- 0 #summary(datall[,1])["NA's"] # captures amount of not-available data since Jan/2000
n.train <- 189 # before: ceiling(length(na.omit(datall[,1]))*.88) 
train <- datall[(N.A.+1):(N.A.+n.train),] # splitting the Train data  from the Test
test  <- datall[(N.A.+n.train+1):nrow(datall),]


# Correlation (see Correlation_Matrices file) ####
m <- ncol(train)
COR <- as.data.frame(matrix(rep(NA,m-1),nrow = m-1,ncol=1,byrow = T))

for (i in 2:m){
  COR[i-1,1] <- cor(train[,1],train[,i],use="pairwise.complete.obs")
  row.names(COR)[i-1] <- colnames(train)[i]
} ; 
corr <- COR[order(abs(COR$V1),decreasing = T),,drop=F]; 
print(corr)
  write.table(corr,"CC_30y_diff.txt",sep="\t")


# Plot Analysis
par(mfrow=c(2,2))
for (i in 2:ncol(train)){
  plot(as.numeric(train[,1]),train[,i],xlab=colnames(train[i]),ylab=colnames(train[1]),
       main=toString(round(COR[i-1,1],digits=3) )
  )
}
par(mfrow=c(1,1))


#### Stepwise Regression "Supervised" ####
  colnames(train[,-1])
leaps <- regsubsets(x=train[,-1],y=train[,"cc.diff.mean"],intercept=F,
                    force.in=c(5),
                    nvmax=3,method="exhaustive",really.big=T) 
summary.regsubsets <- summary(leaps)
  
  plot(leaps,scale="adjr2"); summary.regsubsets$adjr2 # top models with set of predictor.variables ranked by adjusted.R.square 
  plot(leaps,scale="bic"); summary.regsubsets$bic # top models with set of predictor.variables ranked by AIC information criterion 
  
  names(coef(leaps,which.min(summary.regsubsets$bic)))
  coef(leaps,which.min(summary.regsubsets$bic))
  
  
#### Regression  ####   
lm <- lm(cc.diff.mean~0+PMMS..30.year.FRM...Commitment.rate.....p.a...NSA.+
           X7.Years.Swap.Rate+
           UST.YC.Slope..2y.v.10y.,
         data=train)
summary(lm)
BIC(lm)
  
## checking Multicollinearity
vif(lm$model[,-1])
  
## check Unit Roots
for (i in 1:ncol(lm$model)){
print(adf.test(lm$model[,i],k=5) )
}
## check for autocorrelation
acf2(lm$residuals)

## check for heteroscedasticity
acf2(lm$residuals^2)


# First Difference (Delta) Plot
plot(train[,"cc.diff.mean"],type="l")
lines(lm$fitted.values,col="blue")



#### GARCH on residuals ####
garch <- garchFit(~garch(1,1),include.mean=F,data=residuals(lm),cond.dist="sstd",include.shapre=T,trace=F)
  summary(garch)
  
plot(lm$residuals^2,type="l")
lines(garch@sigma.t^2,col="blue")

    #### GARCH formulae
    omega <- garch@fit$par["omega"]
    alpha <- garch@fit$par["alpha1"]
    beta <- garch@fit$par["beta1"]
    
      #### Standardized Residuals ####
    
    stand.resid <- garch@residuals/garch@sigma.t
    plot(stand.resid,type="l") # Plot of Standardized.Residuals
    
    hist(stand.resid,col="light grey") # Histogram of Standardized Residuals
    
    acf2(stand.resid) # autocorrelation of Standardized.Residuals
    acf2(stand.resid^2);  # autocorrelation of Variance
    
    plot(stand.resid^2,type="l") # Variance Plot
    
    # Box test. Null Hypothesis->TS is independent (i.e. NOT autocorrelated)
    Box.test(stand.resid,lag = 1, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)
    Box.test(stand.resid,lag = 6, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)
    Box.test(stand.resid,lag = 12, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)
    
    # Autocorrelation Test. Null Hypothesis-> No Autocorr {FinTS}
    AutocorTest(stand.resid,lag = 1)
    AutocorTest(stand.resid,lag = 6)
    AutocorTest(stand.resid,lag = 12)
    
    # ARCH Test (Lagrange Multiplier): Null->There are NO ARCH effects {MTS}
    archTest(stand.resid, lag=1)
    archTest(stand.resid, lag=5)
    archTest(stand.resid, lag=12)
    
    # ADF Test
    adf.test(stand.resid,k=5)
    
    # Best ARIMA for residuals {forecast}
    summary( auto.arima(stand.resid,max.p=3,max.q=3, ic="bic") )
    
    # Question: Standarized Residuals are normally distributed ?
    qqnorm(stand.resid); qqline(stand.resid) # QQ-Plot
    jarque.bera.test(stand.resid) # Null: Residuals ~ Normal
    
    ## Question: Standarized Residuals are standard student-t distributed    
    # Welch's t-test -> Null: ~ Student_t-dist
    t.test(stand.resid)
    
    # Cramér-von Mises test -> Null: White-Noise
    whitenoise.test(stand.resid)
    
    ## Plot of Density of Standarized Residuals (Median +/- SD)
    # Frequency
    h <- hist(stand.resid, breaks = 10, plot=FALSE)
    h$counts=h$counts/sum(h$counts)
    plot(h,main="Histogram of Standarized Residuals",col="dodgerblue")
    lines(density(stand.resid), col="blue", lwd=1);
    
    # All together
    hist(stand.resid,col="light grey", prob=T,ylim=c(0,.6),
         main="Histogram of Standarized Residuals",
         xlab="blue: actual.density (line->Median)| red: NormalDist density (assymptotic)")
    # True density (from data)
    lines(density(stand.resid), col="blue", lwd=2);
    # Median
    abline(v=median(stand.resid),col="navy blue",lwd=2)
    
    # Assymptotic Normal Distribution
    x<-rnorm(n=length(lm$residuals), mean=mean(stand.resid), sd=sd(stand.resid) )
    curve( dnorm(x, mean=mean(stand.resid),sd=sd(stand.resid) ),
           add=T, col="red",lwd=1)
    
    # Assymptotic student-t Distribution
    std <-stdFit(stand.resid)
    x <- rt(n=length(lm$residuals), df=std$par["nu"])
    curve(dstd(x, mean=mean(stand.resid),sd=sd(stand.resid), nu=std$par["nu"] ),
          add=T, col="dark green",lwd=2)
    
    # Assymptotic Skewed t-Distribution
    sstd <- sstdFit(stand.resid)
    x <- rsstd(n=length(lm$residuals),nu=sstd$estimate["nu"],xi=sstd$estimate["xi"])
    curve( dsstd(x, mean=sstd$estimate["mean"],sd=sstd$estimate["sd"],
                 nu=sstd$estimate["nu"],xi=sstd$estimate["xi"] ), 
           add=T, col="green",lwd=2)
    }
    


#### Weighted Least-Squares ####
lm.wls <-lm(cc.diff.mean~0+
              PMMS..30.year.FRM...Commitment.rate.....p.a...NSA.+
              X7.Years.Swap.Rate+
              UST.YC.Slope..2y.v.10y.,
            weights = 1/garch@sigma.t^2,
           data=train)
summary(lm.wls)
  summary(lm)
      
  
  
##### Test ####
fcst <- forecast(lm.wls,newdata=test,level=3.75)

# comparing with original OLS  
fcst.lm <- forecast(lm,newdata=test,level=1)

### Plots  
plot(test[,"cc.diff.mean"],type="l",ylim=c(-.3,.4)) # actual data
lines(fcst$mean,col="blue") # wls w/ out-of-sample forecast
lines(fcst.lm$mean,col="orange") # ols model (no garch, no weights)

  ## plot of Confidence Intervals
  # Upper
  lines(fcst$upper,type="l",col="blue",lty=2)
  lines(fcst.lm$upper,col="light blue",lty=2)

  # Lower
  lines(fcst$lower,type="l",col="blue",lty=2)
  lines(fcst.lm$lower,col="orange",lty=2)  
  
 ## RMSE Test set ##
sqrt(mean((test[,1] - fcst$mean)^2))
  

## plot of OLS vs WLS out-of-sample 'errors' 
plot(cumsum(abs(test[,"cc.diff.mean"]-fcst.lm$mean)),type="l",col="orange",ylab="acum errors")
lines(cumsum(abs(test[,"cc.diff.mean"]-fcst$mean)),col="blue")


### Plot on Levels ###
## FNMA 30y
plot(data[(n.train+1+1):nrow(data),"FNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."],type="l",ylab="%",ylim=c(0,4))
lines(data[n.train+1,"FNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst$mean),col="blue")
  fcst.test.FN.level <- data[n.train+1,"FNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst$mean)
    # confident intervals
    lines(data[n.train+1,"FNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst$lower),col="blue",lty=2)
    lines(data[n.train+1,"FNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst$upper),col="blue",lty=2)
    # confident intervals using original OLS
    lines(data[n.train+1,"FNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst.lm$lower),col="light blue",lty=2)
    lines(data[n.train+1,"FNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst.lm$upper),col="light blue",lty=2)
    
  
## GNMA I 30y
plot(data[(n.train+1+1):nrow(data),"GNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."],type="l",ylab="%",ylim=c(0,4))
lines(data[n.train+1,"GNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst$mean),col="blue")
  fcst.test.GNI.level <- data[n.train+1,"GNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst$mean)
    # confident intervals
    lines(data[n.train+1,"GNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst$lower),col="blue",lty=2)
    lines(data[n.train+1,"GNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst$upper),col="blue",lty=2)
    # confident intervals using original OLS
    lines(data[n.train+1,"GNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst.lm$lower),col="light blue",lty=2)
    lines(data[n.train+1,"GNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst.lm$upper),col="light blue",lty=2)
    
    
  
## GNMA II 30y
plot(data[(n.train+1+1):nrow(data),"GNMAII.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."],type="l",ylab="%",ylim=c(0,4))
lines(data[n.train+1,"GNMAII.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst$mean),col="blue")
  fcst.test.GNII.level <- data[n.train+1,"GNMAII.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst$mean)
    # confident intervals
    lines(data[n.train+1,"GNMAII.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst$lower),col="blue",lty=2)
    lines(data[n.train+1,"GNMAII.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst$upper),col="blue",lty=2)
    # confident intervals using original OLS
    lines(data[n.train+1,"GNMAII.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst.lm$lower),col="light blue",lty=2)
    lines(data[n.train+1,"GNMAII.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst.lm$upper),col="light blue",lty=2)
  
  # plot of forecast error on levels
  plot((data[n.train+1,"GNMAII.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst$mean))-
         data[(n.train+1+1):nrow(data),"GNMAII.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."],
      type="l",ylab="%",main="forecast error")
  


### Exporting ###

## Train ##  
write.table(fitted(lm.wls),"CC_30s_diff_Train.txt",sep="\t")

## Test ##
TEST <- matrix(NA,nrow=nrow(test),ncol=3,byrow=F)
colnames(TEST) <- c("fcst", "upper", "lower")

TEST[,"fcst"] <- fcst$mean
TEST[,"upper"] <- fcst$upper
TEST[,"lower"] <- fcst$lower

write.table(TEST,"CC_30s_diff_Test.txt", sep="\t")



#########################*
##### CST - Mid-Cycle ####
#########################*
{
# last day of test set
n.test <- as.Date("2017-11-30") 
# first day of Mid-Cycle CST: Jul/2017
first.cst  <- as.Date("2017-06-30")      
# time difference
td <- length(seq(from=first.cst, to=n.test, by='month'))


#### Baseline ####
baseline <- read.csv(file = "cst_baseline.csv",header=T) # uploading response variable's time series
names(lm.wls$coefficients)

## Data Transformation
baseline[,"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."] <- baseline[,"Commitment_Rate_30Y"]
baseline[,"X7.Years.Swap.Rate"] <- baseline[,"SWAP_7Y"]
baseline[,"UST.YC.Slope..2y.v.10y."] <- (baseline[,"TREASURY_10Y"] - baseline[,"TREASURY_2Y"])*100

baseline <- baseline[,names(lm.wls$coefficients)]

baseline <- apply(baseline,MARGIN=2,FUN=as.numeric)

baseline.diff <- apply(baseline,MARGIN=2,FUN=diff)
baseline.diff <- as.data.frame(baseline.diff)

## Forecast
fcst.baseline <- forecast(lm.wls,newdata=baseline.diff,level=10)
plot(fcst.baseline$mean,type="l")

# Level    
fcst.baseline.FN   <- data[nrow(data)-td,"FNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst.baseline$mean) # CST forecasts starts from Aug/17; 1st value (Jul/17) is brought from Test
fcst.baseline.GNI  <- data[nrow(data)-td,"GNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst.baseline$mean) # CST forecasts starts from Aug/17; 1st value (Jul/17) is brought from Test
fcst.baseline.GNII <- data[nrow(data)-td,"GNMAII.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst.baseline$mean) # CST forecasts starts from Aug/17; 1st (Jul/17) is brought from Test

# Plots in Level
plot(fcst.baseline.FN , type="l",main="Baseline - FN 30y",ylab="%")
plot(fcst.baseline.GNI, type="l",main="Baseline - GN I 30y",ylab="%")
plot(fcst.baseline.GNII,type="l",main="Baseline - GN II 30y",ylab="%")


## PSS - baseline
pss.baseline.FN   <- baseline[,"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."]-c(fcst.test.FN.level[nrow(data)-td],fcst.baseline.FN) # CST forecasts starts from Aug/17; 1st value (Jul/17) is brought from Test
pss.baseline.GNI  <- baseline[,"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."]-c(fcst.test.GNI.level[nrow(data)-td],fcst.baseline.GNI) # CST forecasts starts from Aug/17; 1st value (Jul/17) is brought from Test
pss.baseline.GNII <- baseline[,"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."]-c(fcst.test.GNII.level[nrow(data)-td],fcst.baseline.GNII) # CST forecasts starts from Aug/17; 1st value (Jul/17) is brought from Test

  # Plots  
  plot(c(as.numeric(pss.FN),pss.baseline.FN),type="l",ylab="PSS FN",ylim=c(0,4));
  abline(v=nrow(data),lty=2)
  
  plot(c(as.numeric(pss.GNI),pss.baseline.GNI),type="l",ylab="PSS GNI",ylim=c(0,4));
  abline(v=nrow(data),lty=2)
  
  plot(c(as.numeric(pss.GNII),pss.baseline.GNI),type="l",ylab="PSS GNII",ylim=c(0,4));
  abline(v=nrow(data),lty=2)


#### Adverse ####
  adverse <- read.csv(file = "cst_adverse.csv",header=T) # uploading response variable's time series
  names(lm.wls$coefficients)
  
  ## Data Transformation
  adverse[,"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."] <- adverse[,"Commitment_Rate_30Y"]
  adverse[,"X7.Years.Swap.Rate"] <- adverse[,"SWAP_7Y"]
  adverse[,"UST.YC.Slope..2y.v.10y."] <- (adverse[,"TREASURY_10Y"] - adverse[,"TREASURY_2Y"])*100
  
  adverse <- adverse[,names(lm.wls$coefficients)]
  
  adverse <- apply(adverse,MARGIN=2,FUN=as.numeric)
  
  adverse.diff <- apply(adverse,MARGIN=2,FUN=diff)
  adverse.diff <- as.data.frame(adverse.diff)
  
  ## Forecast
  fcst.adverse <- forecast(lm.wls,newdata=adverse.diff,level=10)
  plot(fcst.adverse$mean,type="l")
  
  # Level    
  fcst.adverse.FN   <- data[nrow(data)-td,"FNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst.adverse$mean) # CST forecasts starts from Aug/17; 1st value (Jul/17) is brought from Test
  fcst.adverse.GNI  <- data[nrow(data)-td,"GNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst.adverse$mean) # CST forecasts starts from Aug/17; 1st value (Jul/17) is brought from Test
  fcst.adverse.GNII <- data[nrow(data)-td,"GNMAII.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst.adverse$mean) # CST forecasts starts from Aug/17; 1st (Jul/17) is brought from Test
  
  # Plots in Level
  plot(fcst.adverse.FN , type="l",main="Adverse - FN 30y",ylab="%")
  plot(fcst.adverse.GNI, type="l",main="Adverse - GN I 30y",ylab="%")
  plot(fcst.adverse.GNII,type="l",main="Adverse - GN II 30y",ylab="%")
  
  
  ## PSS - adverse
  pss.adverse.FN   <- adverse[,"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."]-c(fcst.test.FN.level[nrow(data)-td],fcst.adverse.FN) # CST forecasts starts from Aug/17; 1st value (Jul/17) is brought from Test
  pss.adverse.GNI  <- adverse[,"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."]-c(fcst.test.GNI.level[nrow(data)-td],fcst.adverse.GNI) # CST forecasts starts from Aug/17; 1st value (Jul/17) is brought from Test
  pss.adverse.GNII <- adverse[,"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."]-c(fcst.test.GNII.level[nrow(data)-td],fcst.adverse.GNII) # CST forecasts starts from Aug/17; 1st value (Jul/17) is brought from Test
  
  # Plots  
  plot(c(as.numeric(pss.FN),pss.adverse.FN),type="l",ylab="PSS FN",ylim=c(0,4));
  abline(v=nrow(data),lty=2)
  
  plot(c(as.numeric(pss.GNI),pss.adverse.GNI),type="l",ylab="PSS GNI",ylim=c(0,4));
  abline(v=nrow(data),lty=2)
  
  plot(c(as.numeric(pss.GNII),pss.adverse.GNI),type="l",ylab="PSS GNII",ylim=c(0,4));
  abline(v=nrow(data),lty=2)


#### SevAdverse ####
  sev.adverse <- read.csv(file = "cst_sev.adverse.csv",header=T) # uploading response variable's time series
  names(lm.wls$coefficients)
  
  ## Data Transformation
  sev.adverse[,"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."] <- sev.adverse[,"Commitment_Rate_30Y"]
  sev.adverse[,"X7.Years.Swap.Rate"] <- sev.adverse[,"SWAP_7Y"]
  sev.adverse[,"UST.YC.Slope..2y.v.10y."] <- (sev.adverse[,"TREASURY_10Y"] - sev.adverse[,"TREASURY_2Y"])*100
  
  sev.adverse <- sev.adverse[,names(lm.wls$coefficients)]
  
  sev.adverse <- apply(sev.adverse,MARGIN=2,FUN=as.numeric)
  
  sev.adverse.diff <- apply(sev.adverse,MARGIN=2,FUN=diff)
  sev.adverse.diff <- as.data.frame(sev.adverse.diff)
  
  ## Forecast
  fcst.sev.adverse <- forecast(lm.wls,newdata=sev.adverse.diff,level=10)
  plot(fcst.sev.adverse$mean,type="l")
  
  # Level    
  fcst.sev.adverse.FN   <- data[nrow(data)-td,"FNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst.sev.adverse$mean) # CST forecasts starts from Aug/17; 1st value (Jul/17) is brought from Test
  fcst.sev.adverse.GNI  <- data[nrow(data)-td,"GNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst.sev.adverse$mean) # CST forecasts starts from Aug/17; 1st value (Jul/17) is brought from Test
  fcst.sev.adverse.GNII <- data[nrow(data)-td,"GNMAII.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst.sev.adverse$mean) # CST forecasts starts from Aug/17; 1st (Jul/17) is brought from Test
  
  # Plots in Level
  plot(fcst.sev.adverse.FN , type="l",main="Sev.Adverse - FN 30y",ylab="%")
  plot(fcst.sev.adverse.GNI, type="l",main="Sev.Adverse - GN I 30y",ylab="%")
  plot(fcst.sev.adverse.GNII,type="l",main="Sev.Adverse - GN II 30y",ylab="%")
  
  
  ## PSS - sev.adverse
  pss.sev.adverse.FN   <- sev.adverse[,"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."]-c(fcst.test.FN.level[nrow(data)-td],fcst.sev.adverse.FN) # CST forecasts starts from Aug/17; 1st value (Jul/17) is brought from Test
  pss.sev.adverse.GNI  <- sev.adverse[,"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."]-c(fcst.test.GNI.level[nrow(data)-td],fcst.sev.adverse.GNI) # CST forecasts starts from Aug/17; 1st value (Jul/17) is brought from Test
  pss.sev.adverse.GNII <- sev.adverse[,"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."]-c(fcst.test.GNII.level[nrow(data)-td],fcst.sev.adverse.GNII) # CST forecasts starts from Aug/17; 1st value (Jul/17) is brought from Test
  
  # Plots  
  plot(c(as.numeric(pss.FN),pss.sev.adverse.FN),type="l",ylab="PSS FN",ylim=c(0,5));
  abline(v=nrow(data),lty=2)
  
  plot(c(as.numeric(pss.GNI),pss.sev.adverse.GNI),type="l",ylab="PSS GNI",ylim=c(0,5));
  abline(v=nrow(data),lty=2)
  
  plot(c(as.numeric(pss.GNII),pss.sev.adverse.GNI),type="l",ylab="PSS GNII",ylim=c(0,5));
  abline(v=nrow(data),lty=2)
  

##  Exporting CST ##
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

write.table(cst,"CC_30s_diff_CST.txt",sep="\t")  

}

#################################*
##### CST - 2018 Annual-Cycle ####
#################################*

setwd("//us/home/P/pedro.martinez/R/R-3.2.2/data/annual_cycle/Investment_Spreads_part_II/CST/2018_Annual-Cycle")

# last day of train set
last.train <- as.Date("2015-10-31") 
# first day of Mid-Cycle CST: Jul/2017
first.cst  <- as.Date("2018-01-31")      
# time difference
td <- length(seq(from=last.train, to=first.cst, by='month')) - 1


#### SupBaseline ####
sup.baseline <- read.csv(file = "cst_SupBaseline_forCCwGARCH.csv",header=T) # uploading response variable's time series
actuals <- read.csv(file = "actuals.csv",header=T) # Dec/17 data
names(lm.wls$coefficients)


## Data Transformation
sup.baseline[,"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."] <- sup.baseline[,"Commitment_Rate_30Y"]
sup.baseline[,"X7.Years.Swap.Rate"] <- sup.baseline[,"SWAP_7Y"]
sup.baseline[,"UST.YC.Slope..2y.v.10y."] <- (sup.baseline[,"TREASURY_10Y"] - sup.baseline[,"TREASURY_2Y"])*100

sup.baseline <- sup.baseline[,names(lm.wls$coefficients)]
sup.baseline <- apply(sup.baseline,MARGIN=2,FUN=as.numeric)
sup.baseline.diff <- apply(sup.baseline,MARGIN=2,FUN=diff)
sup.baseline.diff <- as.data.frame(sup.baseline.diff)

## Forecast
fcst.sup.baseline <- forecast(lm.wls,newdata=sup.baseline.diff,level=3.75)
plot(fcst.sup.baseline$mean[-(1:td)],type="l")

# Level    
fcst.sup.baseline.FN   <- actuals[,"FNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst.sup.baseline$mean[-(1:td)])
fcst.sup.baseline.GNI  <- actuals[,"GNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst.sup.baseline$mean[-(1:td)])
fcst.sup.baseline.GNII <- actuals[,"GNMAII.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst.sup.baseline$mean[-(1:td)])

# Plots in Level
plot(fcst.sup.baseline.FN , type="l",main="SupBaseline - FN 30y",ylab="%")
plot(fcst.sup.baseline.GNI, type="l",main="SupBaseline - GN I 30y",ylab="%")
plot(fcst.sup.baseline.GNII,type="l",main="SupBaseline - GN II 30y",ylab="%")

# Plots in Level (all time series)
plot(c(data[,"FNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."],actuals[,"FNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."],fcst.sup.baseline.FN ), type="l",main="SupBaseline - FN 30y",ylab="%"); abline(v=(nrow(data)+nrow(actuals)),lty=2)
plot(c(data[,"GNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."],actuals[,"GNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."],fcst.sup.baseline.GNI ), type="l",main="SupBaseline - GN I 30y",ylab="%"); abline(v=(nrow(data)+nrow(actuals)),lty=2)
plot(c(data[,"GNMAII.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."],actuals[,"GNMAII.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."],fcst.sup.baseline.GNII ), type="l",main="SupBaseline - GN II 30y",ylab="%"); abline(v=(nrow(data)+nrow(actuals)),lty=2)


## PSS - sup.baseline
pss.sup.baseline.FN   <- sup.baseline[-(1:(td+1)),"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."]-fcst.sup.baseline.FN #,
pss.sup.baseline.GNI  <- sup.baseline[-(1:(td+1)),"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."]-fcst.sup.baseline.GNI #,
pss.sup.baseline.GNII <- sup.baseline[-(1:(td+1)),"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."]-fcst.sup.baseline.GNII #,

# Plots  
plot(c(as.numeric(pss.FN),pss.sup.baseline.FN),type="l",ylab="PSS FN",ylim=c(0,4));
abline(v=nrow(data),lty=2)

plot(c(as.numeric(pss.GNI),pss.sup.baseline.GNI),type="l",ylab="PSS GNI",ylim=c(0,4));
abline(v=nrow(data),lty=2)

plot(c(as.numeric(pss.GNII),pss.sup.baseline.GNI),type="l",ylab="PSS GNII",ylim=c(0,4));
abline(v=nrow(data),lty=2)


## Exporting CST - SupBaseline ##
baseline.cst <- matrix(NA,nrow=nrow(sup.baseline.diff),ncol=3,byrow=F) # 3 bond classes; 1 scenario + CI
colnames(baseline.cst) <- c("SupBaseline.diff","SupBaseline.diff.upper","SupBaseline.diff.lower")
#                     ,"fcst.sup.baseline.FN","fcst.sup.baseline.GNI","fcst.sup.baseline.GNII",
#                     "pss.sup.baseline.FN","pss.sup.baseline.GNI","pss.sup.baseline.GNII")

baseline.cst[,"SupBaseline.diff"] <- fcst.sup.baseline$mean
baseline.cst[,"SupBaseline.diff.upper"] <- fcst.sup.baseline$upper
baseline.cst[,"SupBaseline.diff.lower"] <- fcst.sup.baseline$lower

#baseline.cst[,"fcst.sup.baseline.FN"] <- fcst.sup.baseline.FN
#baseline.cst[,"fcst.sup.baseline.GNI"] <- fcst.sup.baseline.GNI
#baseline.cst[,"fcst.sup.baseline.GNII"] <- fcst.sup.baseline.GNII

#baseline.cst[,"pss.sup.baseline.FN"] <- pss.sup.baseline.FN
#baseline.cst[,"pss.sup.baseline.GNI"] <- pss.sup.baseline.GNI
#baseline.cst[,"pss.sup.baseline.GNII"] <- pss.sup.baseline.GNII

write.table(baseline.cst,"CC_30s_CST_SupBaseline.txt",sep="\t")



#### SupAdverse ####
sup.adverse <- read.csv(file = "cst_SupAdverse_forCCwGARCH.csv",header=T) # uploading response variable's time series
actuals <- read.csv(file = "actuals.csv",header=T) # Dec/17 data
names(lm.wls$coefficients)


## Data Transformation
sup.adverse[,"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."] <- sup.adverse[,"Commitment_Rate_30Y"]
sup.adverse[,"X7.Years.Swap.Rate"] <- sup.adverse[,"SWAP_7Y"]
sup.adverse[,"UST.YC.Slope..2y.v.10y."] <- (sup.adverse[,"TREASURY_10Y"] - sup.adverse[,"TREASURY_2Y"])*100

sup.adverse <- sup.adverse[,names(lm.wls$coefficients)]
sup.adverse <- apply(sup.adverse,MARGIN=2,FUN=as.numeric)
sup.adverse.diff <- apply(sup.adverse,MARGIN=2,FUN=diff)
sup.adverse.diff <- as.data.frame(sup.adverse.diff)

## Forecast
fcst.sup.adverse <- forecast(lm.wls,newdata=sup.adverse.diff,level=3.75)
plot(fcst.sup.adverse$mean[-(1:td)],type="l")

# Level    
fcst.sup.adverse.FN   <- actuals[,"FNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst.sup.adverse$mean[-(1:td)])
fcst.sup.adverse.GNI  <- actuals[,"GNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst.sup.adverse$mean[-(1:td)])
fcst.sup.adverse.GNII <- actuals[,"GNMAII.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst.sup.adverse$mean[-(1:td)])

# Plots in Level
plot(fcst.sup.adverse.FN , type="l",main="SupAdverse - FN 30y",ylab="%")
plot(fcst.sup.adverse.GNI, type="l",main="SupAdverse - GN I 30y",ylab="%")
plot(fcst.sup.adverse.GNII,type="l",main="SupAdverse - GN II 30y",ylab="%")

# Plots in Level (all time series)
plot(c(data[,"FNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."],actuals[,"FNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."],fcst.sup.adverse.FN ), type="l",main="SupAdverse - FN 30y",ylab="%"); abline(v=(nrow(data)+nrow(actuals)),lty=2)
plot(c(data[,"GNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."],actuals[,"GNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."],fcst.sup.adverse.GNI ), type="l",main="SupAdverse - GN I 30y",ylab="%"); abline(v=(nrow(data)+nrow(actuals)),lty=2)
plot(c(data[,"GNMAII.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."],actuals[,"GNMAII.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."],fcst.sup.adverse.GNII ), type="l",main="SupAdverse - GN II 30y",ylab="%"); abline(v=(nrow(data)+nrow(actuals)),lty=2)


## PSS - sup.adverse
pss.sup.adverse.FN   <- sup.adverse[-(1:(td+1)),"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."]-fcst.sup.adverse.FN #,
pss.sup.adverse.GNI  <- sup.adverse[-(1:(td+1)),"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."]-fcst.sup.adverse.GNI #,
pss.sup.adverse.GNII <- sup.adverse[-(1:(td+1)),"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."]-fcst.sup.adverse.GNII #,

# Plots  
plot(c(as.numeric(pss.FN),pss.sup.adverse.FN),type="l",ylab="PSS FN",ylim=c(0,4));
abline(v=nrow(data),lty=2); abline(h=0,lty=2)

plot(c(as.numeric(pss.GNI),pss.sup.adverse.GNI),type="l",ylab="PSS GNI",ylim=c(0,4));
abline(v=nrow(data),lty=2); abline(h=0,lty=2)

plot(c(as.numeric(pss.GNII),pss.sup.adverse.GNI),type="l",ylab="PSS GNII",ylim=c(0,4));
abline(v=nrow(data),lty=2); abline(h=0,lty=2)


## Exporting CST - SupAdverse ##
adverse.cst <- matrix(NA,nrow=nrow(sup.adverse.diff),ncol=3,byrow=F) # 3 bond classes; 1 scenario + CI
colnames(adverse.cst) <- c("SupAdverse.diff","SupAdverse.diff.upper","SupAdverse.diff.lower")
#                     ,"fcst.sup.adverse.FN","fcst.sup.adverse.GNI","fcst.sup.adverse.GNII",
#                     "pss.sup.adverse.FN","pss.sup.adverse.GNI","pss.sup.adverse.GNII")

adverse.cst[,"SupAdverse.diff"] <- fcst.sup.adverse$mean
adverse.cst[,"SupAdverse.diff.upper"] <- fcst.sup.adverse$upper
adverse.cst[,"SupAdverse.diff.lower"] <- fcst.sup.adverse$lower

#adverse.cst[,"fcst.sup.adverse.FN"] <- fcst.sup.adverse.FN
#adverse.cst[,"fcst.sup.adverse.GNI"] <- fcst.sup.adverse.GNI
#adverse.cst[,"fcst.sup.adverse.GNII"] <- fcst.sup.adverse.GNII

#adverse.cst[,"pss.sup.adverse.FN"] <- pss.sup.adverse.FN
#adverse.cst[,"pss.sup.adverse.GNI"] <- pss.sup.adverse.GNI
#adverse.cst[,"pss.sup.adverse.GNII"] <- pss.sup.adverse.GNII

write.table(adverse.cst,"CC_30s_CST_SupAdverse.txt",sep="\t")




#### SupSevAdverse ####
sup.sev.adverse <- read.csv(file = "cst_SupSevAdverse_forCCwGARCH.csv",header=T) # uploading response variable's time series
actuals <- read.csv(file = "actuals.csv",header=T) # Dec/17 data
names(lm.wls$coefficients)


## Data Transformation
sup.sev.adverse[,"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."] <- sup.sev.adverse[,"Commitment_Rate_30Y"]
sup.sev.adverse[,"X7.Years.Swap.Rate"] <- sup.sev.adverse[,"SWAP_7Y"]
sup.sev.adverse[,"UST.YC.Slope..2y.v.10y."] <- (sup.sev.adverse[,"TREASURY_10Y"] - sup.sev.adverse[,"TREASURY_2Y"])*100

sup.sev.adverse <- sup.sev.adverse[,names(lm.wls$coefficients)]
sup.sev.adverse <- apply(sup.sev.adverse,MARGIN=2,FUN=as.numeric)
sup.sev.adverse.diff <- apply(sup.sev.adverse,MARGIN=2,FUN=diff)
sup.sev.adverse.diff <- as.data.frame(sup.sev.adverse.diff)

## Forecast
fcst.sup.sev.adverse <- forecast(lm.wls,newdata=sup.sev.adverse.diff,level=3.75)
plot(fcst.sup.sev.adverse$mean[-(1:td)],type="l")

# Level    
fcst.sup.sev.adverse.FN   <- actuals[,"FNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst.sup.sev.adverse$mean[-(1:td)])
fcst.sup.sev.adverse.GNI  <- actuals[,"GNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst.sup.sev.adverse$mean[-(1:td)])
fcst.sup.sev.adverse.GNII <- actuals[,"GNMAII.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst.sup.sev.adverse$mean[-(1:td)])

# Plots in Level
plot(fcst.sup.sev.adverse.FN , type="l",main="SupSevAdverse - FN 30y",ylab="%")
plot(fcst.sup.sev.adverse.GNI, type="l",main="SupSevAdverse - GN I 30y",ylab="%")
plot(fcst.sup.sev.adverse.GNII,type="l",main="SupSevAdverse - GN II 30y",ylab="%")

# Plots in Level (all time series)
plot(c(data[,"FNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."],actuals[,"FNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."],fcst.sup.sev.adverse.FN ), type="l",main="SupSevAdverse - FN 30y",ylab="%"); abline(v=(nrow(data)+nrow(actuals)),lty=2)
plot(c(data[,"GNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."],actuals[,"GNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."],fcst.sup.sev.adverse.GNI ), type="l",main="SupSevAdverse - GN I 30y",ylab="%"); abline(v=(nrow(data)+nrow(actuals)),lty=2)
plot(c(data[,"GNMAII.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."],actuals[,"GNMAII.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."],fcst.sup.sev.adverse.GNII ), type="l",main="SupSevAdverse - GN II 30y",ylab="%"); abline(v=(nrow(data)+nrow(actuals)),lty=2)


## PSS - sup.sev.adverse
pss.sup.sev.adverse.FN   <- sup.sev.adverse[-(1:(td+1)),"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."]-fcst.sup.sev.adverse.FN #,
pss.sup.sev.adverse.GNI  <- sup.sev.adverse[-(1:(td+1)),"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."]-fcst.sup.sev.adverse.GNI #,
pss.sup.sev.adverse.GNII <- sup.sev.adverse[-(1:(td+1)),"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."]-fcst.sup.sev.adverse.GNII #,

# Plots  
plot(c(as.numeric(pss.FN),pss.sup.sev.adverse.FN),type="l",ylab="PSS FN",ylim=c(0,4));
abline(v=nrow(data),lty=2); abline(h=0,lty=2)

plot(c(as.numeric(pss.GNI),pss.sup.sev.adverse.GNI),type="l",ylab="PSS GNI",ylim=c(0,4));
abline(v=nrow(data),lty=2); abline(h=0,lty=2)

plot(c(as.numeric(pss.GNII),pss.sup.sev.adverse.GNI),type="l",ylab="PSS GNII",ylim=c(0,4));
abline(v=nrow(data),lty=2); abline(h=0,lty=2)


## Exporting CST - SupSevAdverse ##
sev.adverse.cst <- matrix(NA,nrow=nrow(sup.sev.adverse.diff),ncol=3,byrow=F) # 3 bond classes; 1 scenario + CI
colnames(sev.adverse.cst) <- c("SupSevAdverse.diff","SupSevAdverse.diff.upper","SupSevAdverse.diff.lower")
#                     ,"fcst.sup.sev.adverse.FN","fcst.sup.sev.adverse.GNI","fcst.sup.sev.adverse.GNII",
#                     "pss.sup.sev.adverse.FN","pss.sup.sev.adverse.GNI","pss.sup.sev.adverse.GNII")

sev.adverse.cst[,"SupSevAdverse.diff"] <- fcst.sup.sev.adverse$mean
sev.adverse.cst[,"SupSevAdverse.diff.upper"] <- fcst.sup.sev.adverse$upper
sev.adverse.cst[,"SupSevAdverse.diff.lower"] <- fcst.sup.sev.adverse$lower

#sev.adverse.cst[,"fcst.sup.sev.adverse.FN"] <- fcst.sup.sev.adverse.FN
#sev.adverse.cst[,"fcst.sup.sev.adverse.GNI"] <- fcst.sup.sev.adverse.GNI
#sev.adverse.cst[,"fcst.sup.sev.adverse.GNII"] <- fcst.sup.sev.adverse.GNII

#sev.adverse.cst[,"pss.sup.sev.adverse.FN"] <- pss.sup.sev.adverse.FN
#sev.adverse.cst[,"pss.sup.sev.adverse.GNI"] <- pss.sup.sev.adverse.GNI
#sev.adverse.cst[,"pss.sup.sev.adverse.GNII"] <- pss.sup.sev.adverse.GNII

write.table(sev.adverse.cst,"CC_30s_CST_SupSevAdverse.txt",sep="\t")




#### SchStress ####
sch.stress <- read.csv(file = "cst_SchStress_forCCwGARCH.csv",header=T) # uploading response variable's time series
actuals <- read.csv(file = "actuals.csv",header=T) # Dec/17 data
names(lm.wls$coefficients)


## Data Transformation
sch.stress[,"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."] <- sch.stress[,"Commitment_Rate_30Y"]
sch.stress[,"X7.Years.Swap.Rate"] <- sch.stress[,"SWAP_7Y"]
sch.stress[,"UST.YC.Slope..2y.v.10y."] <- (sch.stress[,"TREASURY_10Y"] - sch.stress[,"TREASURY_2Y"])*100

sch.stress <- sch.stress[,names(lm.wls$coefficients)]
sch.stress <- apply(sch.stress,MARGIN=2,FUN=as.numeric)
sch.stress.diff <- apply(sch.stress,MARGIN=2,FUN=diff)
sch.stress.diff <- as.data.frame(sch.stress.diff)

## Forecast
fcst.sch.stress <- forecast(lm.wls,newdata=sch.stress.diff,level=3.75)
plot(fcst.sch.stress$mean[-(1:td)],type="l")

# Level    
fcst.sch.stress.FN   <- actuals[,"FNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst.sch.stress$mean[-(1:td)])
fcst.sch.stress.GNI  <- actuals[,"GNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst.sch.stress$mean[-(1:td)])
fcst.sch.stress.GNII <- actuals[,"GNMAII.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."]+cumsum(fcst.sch.stress$mean[-(1:td)])

# Plots in Level
plot(fcst.sch.stress.FN , type="l",main="SchStress - FN 30y",ylab="%")
plot(fcst.sch.stress.GNI, type="l",main="SchStress - GN I 30y",ylab="%")
plot(fcst.sch.stress.GNII,type="l",main="SchStress - GN II 30y",ylab="%")

# Plots in Level (all time series)
plot(c(data[,"FNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."],actuals[,"FNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."],fcst.sch.stress.FN ), type="l",main="SchStress - FN 30y",ylab="%"); abline(v=(nrow(data)+nrow(actuals)),lty=2)
plot(c(data[,"GNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."],actuals[,"GNMA.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."],fcst.sch.stress.GNI ), type="l",main="SchStress - GN I 30y",ylab="%"); abline(v=(nrow(data)+nrow(actuals)),lty=2)
plot(c(data[,"GNMAII.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."],actuals[,"GNMAII.CC.30yr.Parity.Coupon..BEY..Default.Model.2014.SRM3."],fcst.sch.stress.GNII ), type="l",main="SchStress - GN II 30y",ylab="%"); abline(v=(nrow(data)+nrow(actuals)),lty=2)


## PSS - sch.stress
pss.sch.stress.FN   <- sch.stress[-(1:(td+1)),"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."]-fcst.sch.stress.FN #,
pss.sch.stress.GNI  <- sch.stress[-(1:(td+1)),"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."]-fcst.sch.stress.GNI #,
pss.sch.stress.GNII <- sch.stress[-(1:(td+1)),"PMMS..30.year.FRM...Commitment.rate.....p.a...NSA."]-fcst.sch.stress.GNII #,

# Plots  
plot(c(as.numeric(pss.FN),pss.sch.stress.FN),type="l",ylab="PSS FN",ylim=c(0,4));
abline(v=nrow(data),lty=2); abline(h=0,lty=2)

plot(c(as.numeric(pss.GNI),pss.sch.stress.GNI),type="l",ylab="PSS GNI",ylim=c(0,4));
abline(v=nrow(data),lty=2); abline(h=0,lty=2)

plot(c(as.numeric(pss.GNII),pss.sch.stress.GNI),type="l",ylab="PSS GNII",ylim=c(0,4));
abline(v=nrow(data),lty=2); abline(h=0,lty=2)


## Exporting CST - SchStress ##
stress.cst <- matrix(NA,nrow=nrow(sch.stress.diff),ncol=3,byrow=F) # 3 bond classes; 1 scenario + CI
colnames(stress.cst) <- c("SchStress.diff","SchStress.diff.upper","SchStress.diff.lower")
#                     ,"fcst.sch.stress.FN","fcst.sch.stress.GNI","fcst.sch.stress.GNII",
#                     "pss.sch.stress.FN","pss.sch.stress.GNI","pss.sch.stress.GNII")

stress.cst[,"SchStress.diff"] <- fcst.sch.stress$mean
stress.cst[,"SchStress.diff.upper"] <- fcst.sch.stress$upper
stress.cst[,"SchStress.diff.lower"] <- fcst.sch.stress$lower

#stress.cst[,"fcst.sch.stress.FN"] <- fcst.sch.stress.FN
#stress.cst[,"fcst.sch.stress.GNI"] <- fcst.sch.stress.GNI
#stress.cst[,"fcst.sch.stress.GNII"] <- fcst.sch.stress.GNII

#stress.cst[,"pss.sch.stress.FN"] <- pss.sch.stress.FN
#stress.cst[,"pss.sch.stress.GNI"] <- pss.sch.stress.GNI
#stress.cst[,"pss.sch.stress.GNII"] <- pss.sch.stress.GNII

write.table(stress.cst,"CC_30s_CST_SchStress.txt",sep="\t")