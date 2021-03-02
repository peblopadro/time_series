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

#### Download of Data ####
data <- read.csv(file = "CMBS_updated.csv",header=T) # uploading response variable's time series
data <- apply(data[,-1],MARGIN=2,FUN=as.numeric)
data <- ts(data,start=c(2000,01),end=c(2018,03),frequency=12)

data.log <- apply(data,MARGIN=2,FUN=log)
data.diff <- apply(data.log,MARGIN=2,FUN=diff)
data.diff <- ts(data.diff,start=c(2000,02),end=c(2018,03),frequency=12)

par(bg='grey'); plot(data.diff,plot.type="s",col=rainbow(6:ncol(data.diff)),lwd=2); abline(h=0,lty=3)

# Mean of All (7y & 10y)  
cmbs.diff.mean <- rowMeans(as.matrix(data.diff),na.rm=T)
cmbs.diff.mean <- ts(cmbs.diff.mean,start=c(2000,02),end=c(2018,03),frequency=12)

  # Unit Root Check  
  adf.test(na.omit(cmbs.diff.mean),k=4)
  
lines(cmbs.diff.mean,lty=2,col="blue")

#### Average Assumption Test ####
  s7y_s10y <- data.diff[,"Fannie.DUS.10.9.5.DUS.Spread"] - data.diff[,"Fannie.DUS.7.6.5.DUS.Spread"]; 
  
  plot(s7y_s10y,type="l")
 
  t <- ts(1:length( as.numeric(na.omit( s7y_s10y ) ) ) )
    plot(as.numeric(t),as.numeric(na.omit( s7y_s10y ) ),ylab="diff.10y vs diff 7y",xlab="t"); abline(h=0,lty=3)
  summary(lm(as.numeric(na.omit(s7y_s10y))~0+t) )
  


##### Predictor Variables ####
predictor <- read.csv(file = "PredictorVariables_updated.csv",header=T)
predictor <- apply(predictor[,-1],2,as.numeric)

predictor.diff <- apply(predictor,MARGIN=2,FUN=diff)


  ## Relevant Variables (from CMBS trader) ##
  predictor.diff <- predictor.diff[,c("TED.spread",
                                      "TED.spread.lag.1",
                                      "TED.spread.lag.2",
                                      "TED.Spread.JPM",
                                      "TED.Spread.JPM.lag.1",
                                      "TED.Spread.JPM.lag.2",
                                      "VIX",
                                      "VIX.lag.1",
                                      "X7.Years.Swap.Rate",
                                      "X10.Years.Swap.Rate",
                                      "DJIA",
                                      "Corporate.Profits.with.IVA.and.CCAdj...Bil..USD..SAAR.",
                                      "Retail.Sales..Retail.sales...Total.excl..motor.vehicle.and.parts.dealers...Mil..USD..CDASA."
                                      )]
                                      

predictor.diff.all <- as.data.frame(predictor.diff)




### All Data
# Mean of All
datall <- as.data.frame(cbind(as.numeric(cmbs.diff.mean),as.data.frame(predictor.diff.all)))
  colnames(datall)[1] <- "cmbs.diff.mean"
  

#### Train / Test Split ####
  N.A. <- summary(datall[,1])["NA's"] # captures amount of not-available data since Jan/2000
  n.train <- 150 # ceiling(length(na.omit(datall[,1]))*.92) # 92%/8% split of In-Sample vs Out-of-Sample data   # 141  (if no info on leaving zero-bound int rate environment)  # vector of months of Train data (reaches to Oct/2015, end of zero-interest rate bound <expectation>)
train <- datall[(N.A.+1):(N.A.+n.train),] # splitting the Train data  from the Test
test  <- datall[(N.A.+n.train+1):nrow(datall),]

    ### Correlation Analysis ####
    m <- ncol(train)
    COR <- as.data.frame(matrix(rep(NA,m-1),nrow = m-1,ncol=1,byrow = T))
    
    for (i in 2:m){
      COR[i-1,1] <- cor(train[,1],train[,i],use="pairwise.complete.obs")
      row.names(COR)[i-1] <- colnames(train)[i]
    } ; 
    corr <- COR[order(abs(COR$V1),decreasing = T),,drop=F]; 
    #print(corr)
    write.table(corr,"COR_CMBS_ccr.txt",sep="\t")
    
    # Plot Analysis
    par(mfrow=c(2,2))
    for (i in 2:ncol(train)){
      plot(as.numeric(train[,1]),train[,i],xlab=colnames(train[i]),ylab=colnames(train[1]),
           main=toString(round(COR[i-1,1],digits=3) )
      )
    }
    par(mfrow=c(1,1))
    
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
## using all "approved" variables from traders:
  {
  colnames(train[,-1])
leaps <- regsubsets(x=train[,-1],y=train[,"cmbs.diff.mean"],intercept=F,
                    force.in=c(7,10),
                    force.out=c(8,9),
                    nvmax=6,method="exhaustive",really.big=T) 
summary.regsubsets <- summary(leaps)
  }

## using statistically-significant variables from the "approved" by traders:
  {
  relevantVariables
leaps <- regsubsets(x=train[,relevantVariables],y=train[,"cmbs.diff.mean"],intercept=F,
                    force.in=c(17,10), # from traders' feedback
                    force.out=c(19,18,16,11,13,15), # for Multicolinearity w 10y Swap and VIX
                    nvmax=6,method="exhaustive",really.big=T) 
summary.regsubsets <- summary(leaps)
  }

  {  
  names(coef(leaps,which.min(summary.regsubsets$bic)))
  coef(leaps,which.min(summary.regsubsets$bic))
  plot(leaps,scale="bic"); summary.regsubsets$bic # top models with set of predictor.variables ranked by AIC information criterion 
  
  names(coef(leaps,which.max(summary.regsubsets$adjr2)))
  coef(leaps,which.max(summary.regsubsets$adjr2))
  plot(leaps,scale="adjr2"); summary.regsubsets$adjr2 # top models with set of predictor.variables ranked by adjusted.R.square 
  }


#### Linear Regression ####
lm <- lm(cmbs.diff.mean~0+
           TED.spread.lag.1+
           X10.Years.Swap.Rate+
           VIX
         ,data=train); 
summary(lm)
BIC(lm)

vif(lm$model[,-1])

acf2(lm$residuals);
acf2(lm$residuals^2)

# check Unit Roots
for (i in 1:ncol(lm$model)){
  print(adf.test(lm$model[,i],k=5) )
}

# Plot: Fit
plot(na.omit(train[,"cmbs.diff.mean"]),type="l")
lines(lm$fitted.values,col="blue")

# Plot: residuals
plot(lm$residuals,type="l")



#### Dynamic Regression ####
lm.dyn <- arima(train[,"cmbs.diff.mean"],order=c(2,0,0),seasonal=list(order=c(1,0,0),period=6),include.mean=F,
                xreg=train[,names(lm$coefficients) ] )
coeftest(lm.dyn)
BIC(lm.dyn)
summary(lm.dyn)

p <- length(lm.dyn$coef) - length(lm$coefficients) # number of autoregressive parameters

acf2(residuals(lm.dyn))
acf2(residuals(lm.dyn)^2)
  archTest(residuals(lm.dyn)^2,lag=6)


### Log-Correction-Factor ####
## SARMAX (Seasonal-ARMA-Xogeneous model)
lcf.sarmax <- lm(exp(train[,"cmbs.diff.mean"])~0+exp(fitted(lm.dyn)))$coefficients
print(lcf.sarmax)



##### Test ####
fcst.lm <- forecast(lm,newdata=test)
fcst.lm.dyn <- predict(lm.dyn,newxreg=test[, names(lm.dyn$coef)[-(1:p)] ])
  ## Confidence Intervals
  CI <- 0.10 # +/- 10%
  q <- qt(CI+(1-CI)/2,df=n.train-length(lm.dyn$coef))
  fcst.lm.dyn$upper <- fcst.lm.dyn$pred + as.numeric(q*fcst.lm.dyn$se)
  fcst.lm.dyn$lower <- fcst.lm.dyn$pred - as.numeric(q*fcst.lm.dyn$se)
  

## Plot
plot(test[,"cmbs.diff.mean"], type="l",main="Test",ylab="bps")
lines(fcst.lm$mean,col="light blue")
lines(as.numeric(fcst.lm.dyn$pred),col="red")
  # confidence intervals
  lines(as.numeric(fcst.lm.dyn$upper),col="red",lty=3)
  lines(as.numeric(fcst.lm.dyn$lower),col="red",lty=3)

  ### RMSE Test set ###
  sqrt(mean((test[,1] - fcst$mean)^2))

# Forecast Errors
plot(test[,"cmbs.diff.mean"]-fcst$mean,type="l",ylab="ordinary error")



### Plot on Levels ###
## CMBS 7y
  N.A.7y <- summary(data[,"Fannie.DUS.7.6.5.DUS.Spread"])["NA's"]
plot(data[(N.A.7y+(N.A.+n.train-N.A.7y)+1+1):nrow(data),"Fannie.DUS.7.6.5.DUS.Spread"],type="l",ylab="%",ylim=c(25,75))
lines(data[(N.A.7y+(N.A.+n.train-N.A.7y)+1),"Fannie.DUS.7.6.5.DUS.Spread"]*cumprod(lcf.sarmax*exp(fcst.lm.dyn$pred)),col="blue")
      lines(data[(N.A.7y+(N.A.+n.train-N.A.7y)+1),"Fannie.DUS.7.6.5.DUS.Spread"]*cumprod(lcf.sarmax*exp(fcst.lm.dyn$lower)),col="light blue",lty=3)
      lines(data[(N.A.7y+(N.A.+n.train-N.A.7y)+1),"Fannie.DUS.7.6.5.DUS.Spread"]*cumprod(lcf.sarmax*exp(fcst.lm.dyn$upper)),col="light blue",lty=3)
  fcst.test.CMBS_7y.level <- data[(N.A.7y+(N.A.+n.train-N.A.7y)+1),"Fannie.DUS.7.6.5.DUS.Spread"]*cumprod(lcf.sarmax*exp(fcst.lm.dyn$pred))

## CMBS 10y
plot(data[(N.A.+n.train+1+1):nrow(data),"Fannie.DUS.10.9.5.DUS.Spread"],type="l",ylab="%",ylim=c(45,95))
lines(data[(N.A.+n.train+1),"Fannie.DUS.10.9.5.DUS.Spread"]*cumprod(lcf.sarmax*exp(fcst.lm.dyn$pred)),col="blue")
      lines(data[(N.A.+n.train+1),"Fannie.DUS.10.9.5.DUS.Spread"]*cumprod(lcf.sarmax*exp(fcst.lm.dyn$upper)),col="light blue",lty=3)
      lines(data[(N.A.+n.train+1),"Fannie.DUS.10.9.5.DUS.Spread"]*cumprod(lcf.sarmax*exp(fcst.lm.dyn$lower)),col="light blue",lty=3)
  fcst.test.CMBS_10y.level <- data[(N.A.+n.train+1),"Fannie.DUS.10.9.5.DUS.Spread"]*cumprod(lcf.sarmax*exp(fcst.lm.dyn$pred))


#### fANCY Plot ####
{
par(mfrow=c(1,2)); par(bg='aliceblue')
plot(ts(na.omit(train[,1]),start=c(2002,05),end=c(2015,10),frequency=12),type="l",lwd=2.5,col="brown1",main="Train",ylab="change of spread",xlab="Date",ylim=c(-50,70))
grid(col="white",lty=1,nx=4)
lines(ts(na.omit(train[,1]),start=c(2002,05),end=c(2015,10),frequency=12),lwd=3,col="brown1")
lines(ts(lm$fitted.values,start=c(2002,05),end=c(2015,10),frequency=12),lwd=3.5,col="cyan3")

plot(ts(test[,1],start=c(2015,11),end=c(2018,03),frequency=12),col="brown1",main="Test",ylab="change of spread",xlab="Date",ylim=c(-50,70))
grid(col="white",lty=1,nx=4)
lines(ts(test[,1],start=c(2015,11),end=c(2018,03),frequency=12),lwd=3,col="brown1")
lines(ts(fcst$mean,start=c(2015,11),end=c(2018,03),frequency=12),lwd=3,col="cyan3")    
lines(ts(fcst2$pred,start=c(2015,11),end=c(2018,03),frequency=12),lwd=3,col="blueviolet")    
}


#### Exporting Data ####
# Train
write.table(fitted(lm.dyn),"CMBS_ccr_train_ccr.txt",sep="\t")  

# Test
TEST <- matrix(NA,nrow=nrow(test),ncol=3,byrow=F)
colnames(TEST) <- c("fcst", "upper", "lower")

TEST[,"fcst"] <- fcst.lm.dyn$pred
TEST[,"upper"] <- fcst.lm.dyn$upper
TEST[,"lower"] <- fcst.lm.dyn$lower

write.table(TEST,"CMBS_ccr_test_ccr.txt",sep="\t")

test_level <- matrix(c(fcst.test.CMBS_7y.level,fcst.test.CMBS_10y.level),nrow=nrow(test),ncol=2,byrow=F)
write.table(test_level,"CMBS_ccr_test_level.txt",sep="\t")  




#################################*
##### CST - 2018 Annual-Cycle ####
#################################*

setwd("//us/home/P/pedro.martinez/R/R-3.2.2/data/annual_cycle/Investment_Spreads_part_II/CST/2018_Annual-Cycle")

# last day of train set
last.train <- as.Date("2016-11-30") 
# first day of Mid-Cycle CST: Jul/2017
first.cst  <- as.Date("2018-01-31")      
# time difference
td <- length(seq(from=last.train, to=first.cst, by='month')) - 1


#### SupBaseline ####
baseline <- read.csv(file = "cst_SupBaseline_forCMBSwARX.csv",header=T) # uploading response variable's time series
actuals <- read.csv(file = "actuals.csv",header=T) # data with missing months between "last.test" and "first.cst"
names(lm.dyn$coef)

## Data Transformation
 baseline[,"TED.spread"] <- (baseline[,"LIBOR_3M"] - baseline[,"TREASURY_3M"])*100
 baseline[,"TED.spread.lag.1"] <- c(26.091, baseline[-nrow(baseline),"TED.spread"] ) # hard-coded number is Dec/17 value = 1st_lag_variable value
 baseline[,"X10.Years.Swap.Rate"] <- baseline[,"SWAP_10Y"]
 baseline[,"VIX"] <- baseline[,"CBOE_Average_VIX"]

baseline <- baseline[, names(lm.dyn$coef)[-(1:p)] ]

baseline <- apply(baseline,MARGIN=2,FUN=as.numeric)
baseline.diff <- apply(baseline,MARGIN=2,FUN=diff)
baseline.diff <- as.data.frame(baseline.diff)

# Forecast
fcst.baseline <- predict(lm.dyn,newxreg=baseline.diff)
## Confidence Intervals
CI <- 0.10 # +/- 10%
q <- qt(CI+(1-CI)/2,df=nrow(baseline)-length(lm.dyn$coef))
fcst.baseline$upper <- fcst.baseline$pred + as.numeric(q*fcst.baseline$se)
fcst.baseline$lower <- fcst.baseline$pred - as.numeric(q*fcst.baseline$se)

plot(fcst.baseline$pred[-(1:td)],type="l",col="purple"); lines(fcst.baseline$mean,col="blue")

# Level    
fcst.baseline.CMBS_7y  <- actuals[,"Fannie.DUS.7.6.5.DUS.Spread"] *cumprod(lcf.sarmax*exp(fcst.baseline$pred))[-(1:td)]
fcst.baseline.CMBS_10y <- actuals[,"Fannie.DUS.10.9.5.DUS.Spread"]*cumprod(lcf.sarmax*exp(fcst.baseline$pred))[-(1:td)]

# Plots in Level
plot(fcst.baseline.CMBS_7y , type="l",main="Baseline - CMBS 7y",ylab="%",col="purple")

plot(fcst.baseline.CMBS_10y , type="l",main="Baseline - CMBS 10y",ylab="%",col="purple")

## Exporting CST - SupBaseline ##
baseline.cst <- matrix(NA,nrow=nrow(baseline.diff),ncol=3,byrow=F) # 3 bond classes; 1 scenario + CI
colnames(baseline.cst) <- c("SupBaseline.ccr","SupBaseline.ccr.upper","SupBaseline.ccr.lower")
                          #,"fcst.7y.sup.baseline","fcst.10y.sup.baseline")

baseline.cst[,"SupBaseline.ccr"] <- fcst.baseline$pred
baseline.cst[,"SupBaseline.ccr.upper"] <- fcst.baseline$upper
baseline.cst[,"SupBaseline.ccr.lower"] <- fcst.baseline$lower

#baseline.cst[,"fcst.7y.sup.baseline"] <- fcst.baseline.CMBS_7y
#baseline.cst[,"fcst.10y.sup.baseline"] <- fcst.baseline.CMBS_10y

write.table(baseline.cst,"CMBS_CST_SupBaseline.txt",sep="\t")  




#### SupAdverse ####
adverse <- read.csv(file = "cst_SupAdverse_forCMBSwARX.csv",header=T) # uploading response variable's time series
actuals <- read.csv(file = "actuals.csv",header=T) # data with missing months between "last.test" and "first.cst"
names(lm.dyn$coef)

## Data Transformation
adverse[,"TED.spread"] <- (adverse[,"LIBOR_3M"] - adverse[,"TREASURY_3M"])*100
adverse[,"TED.spread.lag.1"] <- c(26.091, adverse[-nrow(adverse),"TED.spread"] ) # hard-coded number is Dec/17 value = 1st_lag_variable value
adverse[,"X10.Years.Swap.Rate"] <- adverse[,"SWAP_10Y"]
adverse[,"VIX"] <- adverse[,"CBOE_Average_VIX"]

adverse <- adverse[, names(lm.dyn$coef)[-(1:p)] ]

adverse <- apply(adverse,MARGIN=2,FUN=as.numeric)
adverse.diff <- apply(adverse,MARGIN=2,FUN=diff)
adverse.diff <- as.data.frame(adverse.diff)

# Forecast
fcst.adverse <- predict(lm.dyn,newxreg=adverse.diff)
## Confidence Intervals
CI <- 0.10 # +/- 10%
q <- qt(CI+(1-CI)/2,df=nrow(adverse)-length(lm.dyn$coef))
fcst.adverse$upper <- fcst.adverse$pred + as.numeric(q*fcst.adverse$se)
fcst.adverse$lower <- fcst.adverse$pred - as.numeric(q*fcst.adverse$se)

plot(fcst.adverse$pred[-(1:td)],type="l",col="purple"); lines(fcst.adverse$mean,col="blue")

# Level    
fcst.adverse.CMBS_7y  <- actuals[,"Fannie.DUS.7.6.5.DUS.Spread"] *cumprod(lcf.sarmax*exp(fcst.adverse$pred))[-(1:td)]
fcst.adverse.CMBS_10y <- actuals[,"Fannie.DUS.10.9.5.DUS.Spread"]*cumprod(lcf.sarmax*exp(fcst.adverse$pred))[-(1:td)]

# Plots in Level
plot(fcst.adverse.CMBS_7y , type="l",main="Adverse - CMBS 7y",ylab="%",col="purple")

plot(fcst.adverse.CMBS_10y , type="l",main="Adverse - CMBS 10y",ylab="%",col="purple")

## Exporting CST - SupAdverse ##
adverse.cst <- matrix(NA,nrow=nrow(adverse.diff),ncol=3,byrow=F) # 3 bond classes; 1 scenario + CI
colnames(adverse.cst) <- c("SupAdverse.ccr","SupAdverse.ccr.upper","SupAdverse.ccr.lower")
                          #,"fcst.7y.sup.adverse","fcst.10y.sup.adverse")

adverse.cst[,"SupAdverse.ccr"] <- fcst.adverse$pred
adverse.cst[,"SupAdverse.ccr.upper"] <- fcst.adverse$upper
adverse.cst[,"SupAdverse.ccr.lower"] <- fcst.adverse$lower

#adverse.cst[,"fcst.7y.sup.adverse"] <- fcst.adverse.CMBS_7y
#adverse.cst[,"fcst.10y.sup.adverse"] <- fcst.adverse.CMBS_10y

write.table(adverse.cst,"CMBS_CST_SupAdverse.txt",sep="\t") 




#### SupSevAdverse ####
sev.adverse <- read.csv(file = "cst_SupSevAdverse_forCMBSwARX.csv",header=T) # uploading response variable's time series
actuals <- read.csv(file = "actuals.csv",header=T) # data with missing months between "last.test" and "first.cst"
names(lm.dyn$coef)

## Data Transformation
sev.adverse[,"TED.spread"] <- (sev.adverse[,"LIBOR_3M"] - sev.adverse[,"TREASURY_3M"])*100
sev.adverse[,"TED.spread.lag.1"] <- c(26.091, sev.adverse[-nrow(sev.adverse),"TED.spread"] ) # hard-coded number is Dec/17 value = 1st_lag_variable value
sev.adverse[,"X10.Years.Swap.Rate"] <- sev.adverse[,"SWAP_10Y"]
sev.adverse[,"VIX"] <- sev.adverse[,"CBOE_Average_VIX"]

sev.adverse <- sev.adverse[, names(lm.dyn$coef)[-(1:p)] ]

sev.adverse <- apply(sev.adverse,MARGIN=2,FUN=as.numeric)
sev.adverse.diff <- apply(sev.adverse,MARGIN=2,FUN=diff)
sev.adverse.diff <- as.data.frame(sev.adverse.diff)

# Forecast
fcst.sev.adverse <- predict(lm.dyn,newxreg=sev.adverse.diff)
## Confidence Intervals
CI <- 0.10 # +/- 10%
q <- qt(CI+(1-CI)/2,df=nrow(sev.adverse)-length(lm.dyn$coef))
fcst.sev.adverse$upper <- fcst.sev.adverse$pred + as.numeric(q*fcst.sev.adverse$se)
fcst.sev.adverse$lower <- fcst.sev.adverse$pred - as.numeric(q*fcst.sev.adverse$se)

plot(fcst.sev.adverse$pred[-(1:td)],type="l",col="purple"); lines(fcst.sev.adverse$mean,col="blue")

# Level    
fcst.sev.adverse.CMBS_7y  <- actuals[,"Fannie.DUS.7.6.5.DUS.Spread"] *cumprod(lcf.sarmax*exp(fcst.sev.adverse$pred))[-(1:td)]
fcst.sev.adverse.CMBS_10y <- actuals[,"Fannie.DUS.10.9.5.DUS.Spread"]*cumprod(lcf.sarmax*exp(fcst.sev.adverse$pred))[-(1:td)]

# Plots in Level
plot(fcst.sev.adverse.CMBS_7y , type="l",main="SevAdverse - CMBS 7y",ylab="%",col="purple")

plot(fcst.sev.adverse.CMBS_10y , type="l",main="SevAdverse - CMBS 10y",ylab="%",col="purple")

## Exporting CST - SupSevAdverse ##
sev.adverse.cst <- matrix(NA,nrow=nrow(sev.adverse.diff),ncol=3,byrow=F) # 3 bond classes; 1 scenario + CI
colnames(sev.adverse.cst) <- c("SupSevAdverse.ccr","SupSevAdverse.ccr.upper","SupSevAdverse.ccr.lower")
#,"fcst.7y.sup.sev.adverse","fcst.10y.sup.sev.adverse")

sev.adverse.cst[,"SupSevAdverse.ccr"] <- fcst.sev.adverse$pred
sev.adverse.cst[,"SupSevAdverse.ccr.upper"] <- fcst.sev.adverse$upper
sev.adverse.cst[,"SupSevAdverse.ccr.lower"] <- fcst.sev.adverse$lower

#sev.adverse.cst[,"fcst.7y.sup.sev.adverse"] <- fcst.sev.adverse.CMBS_7y
#sev.adverse.cst[,"fcst.10y.sup.sev.adverse"] <- fcst.sev.adverse.CMBS_10y

write.table(sev.adverse.cst,"CMBS_CST_SupSevAdverse.txt",sep="\t") 




#### SchStress ####
stress <- read.csv(file = "cst_SchStress_forCMBSwARX.csv",header=T) # uploading response variable's time series
actuals <- read.csv(file = "actuals.csv",header=T) # data with missing months between "last.test" and "first.cst"
names(lm.dyn$coef)

## Data Transformation
stress[,"TED.spread"] <- (stress[,"LIBOR_3M"] - stress[,"TREASURY_3M"])*100
stress[,"TED.spread.lag.1"] <- c(26.091, stress[-nrow(stress),"TED.spread"] ) # hard-coded number is Dec/17 value = 1st_lag_variable value
stress[,"X10.Years.Swap.Rate"] <- stress[,"SWAP_10Y"]
stress[,"VIX"] <- stress[,"CBOE_Average_VIX"]

stress <- stress[, names(lm.dyn$coef)[-(1:p)] ]

stress <- apply(stress,MARGIN=2,FUN=as.numeric)
stress.diff <- apply(stress,MARGIN=2,FUN=diff)
stress.diff <- as.data.frame(stress.diff)

# Forecast
fcst.stress <- predict(lm.dyn,newxreg=stress.diff)
## Confidence Intervals
CI <- 0.10 # +/- 10%
q <- qt(CI+(1-CI)/2,df=nrow(stress)-length(lm.dyn$coef))
fcst.stress$upper <- fcst.stress$pred + as.numeric(q*fcst.stress$se)
fcst.stress$lower <- fcst.stress$pred - as.numeric(q*fcst.stress$se)

plot(fcst.stress$pred[-(1:td)],type="l",col="purple"); lines(fcst.stress$mean,col="blue")

# Level    
fcst.stress.CMBS_7y  <- actuals[,"Fannie.DUS.7.6.5.DUS.Spread"] *cumprod(lcf.sarmax*exp(fcst.stress$pred))[-(1:td)]
fcst.stress.CMBS_10y <- actuals[,"Fannie.DUS.10.9.5.DUS.Spread"]*cumprod(lcf.sarmax*exp(fcst.stress$pred))[-(1:td)]

# Plots in Level
plot(fcst.stress.CMBS_7y , type="l",main="SevAdverse - CMBS 7y",ylab="%",col="purple")

plot(fcst.stress.CMBS_10y , type="l",main="SevAdverse - CMBS 10y",ylab="%",col="purple")

## Exporting CST - SchStress ##
stress.cst <- matrix(NA,nrow=nrow(stress.diff),ncol=3,byrow=F) # 3 bond classes; 1 scenario + CI
colnames(stress.cst) <- c("SchStress.ccr","SchStress.ccr.upper","SchStress.ccr.lower")
#,"fcst.7y.sup.stress","fcst.10y.sup.stress")

stress.cst[,"SchStress.ccr"] <- fcst.stress$pred
stress.cst[,"SchStress.ccr.upper"] <- fcst.stress$upper
stress.cst[,"SchStress.ccr.lower"] <- fcst.stress$lower

#stress.cst[,"fcst.7y.sup.stress"] <- fcst.stress.CMBS_7y
#stress.cst[,"fcst.10y.sup.stress"] <- fcst.stress.CMBS_10y

write.table(stress.cst,"CMBS_CST_SchStress.txt",sep="\t") 



#########################*
## 2017 Mid-Cycle CST ####
#########################*
  

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
  {
    baseline[,"TED.spread"] <- (baseline[,"LIBOR_3M"] - baseline[,"TREASURY_3M"])*100
    baseline[,"TED.spread.lag.1"] <- c(predictor[nrow(data)-td-1,"TED.spread"], baseline[-nrow(baseline),"TED.spread"] ) # last test month is Nov; first cst forecast is Jul
    
    baseline[,"X10.Years.Swap.Rate"] <- baseline[,"SWAP_10Y"]
    
    baseline[,"VIX"] <- baseline[,"CBOE_Average_VIX"]
   }

baseline <- baseline[, names(lm.dyn$coef)[-(1:p)] ]


  baseline <- apply(baseline,MARGIN=2,FUN=as.numeric)
  baseline.diff <- apply(baseline,MARGIN=2,FUN=diff)
  baseline.diff <- as.data.frame(baseline.diff)

# Forecast
fcst.baseline <- predict(lm.dyn,newxreg=baseline.diff)
  ## Confidence Intervals
  CI <- 0.10 # +/- 10%
  q <- qt(CI+(1-CI)/2,df=nrow(baseline)-length(lm.dyn$coef))
  fcst.baseline$upper <- fcst.baseline$pred + as.numeric(q*fcst.baseline$se)
  fcst.baseline$lower <- fcst.baseline$pred - as.numeric(q*fcst.baseline$se)
  
plot(fcst.baseline$pred,type="l",col="purple"); lines(fcst.baseline$mean,col="blue")

# Level    
fcst.baseline.CMBS_7y  <- data[nrow(data)-td,"Fannie.DUS.7.6.5.DUS.Spread"]*cumprod(lcf.sarmax*exp(fcst.baseline$pred))
fcst.baseline.CMBS_10y <- data[nrow(data)-td,"Fannie.DUS.10.9.5.DUS.Spread"]*cumprod(lcf.sarmax*exp(fcst.baseline$pred))

# Plots in Level
plot(fcst.baseline.CMBS_7y , type="l",main="Baseline - CMBS 7y",ylab="%",col="purple")

plot(fcst.baseline.CMBS_10y , type="l",main="Baseline - CMBS 10y",ylab="%",col="purple")



#### Adverse ####

adverse <- read.csv(file = "cst_adverse.csv",header=T) # uploading response variable's time series
names(lm.wls$coefficients)

## Data Transformation
{
  adverse[,"TED.spread"] <- (adverse[,"LIBOR_3M"] - adverse[,"TREASURY_3M"])*100
  adverse[,"TED.spread.lag.1"] <- c(predictor[nrow(data)-td-1,"TED.spread"], adverse[-nrow(adverse),"TED.spread"] ) # last test month is Nov; first cst forecast is Jul
  
  adverse[,"X10.Years.Swap.Rate"] <- adverse[,"SWAP_10Y"]
  
  adverse[,"VIX"] <- adverse[,"CBOE_Average_VIX"]
}

adverse <- adverse[, names(lm.dyn$coef)[-(1:p)] ]


adverse <- apply(adverse,MARGIN=2,FUN=as.numeric)
adverse.diff <- apply(adverse,MARGIN=2,FUN=diff)
adverse.diff <- as.data.frame(adverse.diff)

# Forecast
fcst.adverse <- predict(lm.dyn,newxreg=adverse.diff)
  ## Confidence Intervals
  CI <- 0.10 # +/- 10%
  q <- qt(CI+(1-CI)/2,df=nrow(adverse)-length(lm.dyn$coef))
  fcst.adverse$upper <- fcst.adverse$pred + as.numeric(q*fcst.adverse$se)
  fcst.adverse$lower <- fcst.adverse$pred - as.numeric(q*fcst.adverse$se)
  
plot(fcst.adverse$pred,type="l",col="purple"); lines(fcst.adverse$mean,col="blue")

# Level    
fcst.adverse.CMBS_7y  <- data[nrow(data)-td,"Fannie.DUS.7.6.5.DUS.Spread"]*cumprod(lcf.sarmax*exp(fcst.adverse$pred))
fcst.adverse.CMBS_10y <- data[nrow(data)-td,"Fannie.DUS.10.9.5.DUS.Spread"]*cumprod(lcf.sarmax*exp(fcst.adverse$pred))

# Plots in Level
plot(fcst.adverse.CMBS_7y , type="l",main="Adverse - CMBS 7y",ylab="%",col="purple")

plot(fcst.adverse.CMBS_10y , type="l",main="Adverse - CMBS 10y",ylab="%",col="purple")



#### SevAdverse ####
sev.adverse <- read.csv(file = "cst_sev.adverse.csv",header=T) # uploading response variable's time series
names(lm.wls$coefficients)

## Data Transformation
{
  sev.adverse[,"TED.spread"] <- (sev.adverse[,"LIBOR_3M"] - sev.adverse[,"TREASURY_3M"])*100
  sev.adverse[,"TED.spread.lag.1"] <- c(predictor[nrow(data)-td-1,"TED.spread"], sev.adverse[-nrow(sev.adverse),"TED.spread"] ) # last test month is Nov; first cst forecast is Jul
  
  sev.adverse[,"X10.Years.Swap.Rate"] <- sev.adverse[,"SWAP_10Y"]
  
  sev.adverse[,"VIX"] <- sev.adverse[,"CBOE_Average_VIX"]
}

sev.adverse <- sev.adverse[, names(lm.dyn$coef)[-(1:p)] ]

sev.adverse <- apply(sev.adverse,MARGIN=2,FUN=as.numeric)
sev.adverse.diff <- apply(sev.adverse,MARGIN=2,FUN=diff)
sev.adverse.diff <- as.data.frame(sev.adverse.diff)

# Forecast
fcst.sev.adverse <- predict(lm.dyn,newxreg=sev.adverse.diff)
  ## Confidence Intervals
  CI <- 0.10 # +/- 10%
  q <- qt(CI+(1-CI)/2,df=nrow(sev.adverse)-length(lm.dyn$coef))
  fcst.sev.adverse$upper <- fcst.sev.adverse$pred + as.numeric(q*fcst.sev.adverse$se)
  fcst.sev.adverse$lower <- fcst.sev.adverse$pred - as.numeric(q*fcst.sev.adverse$se)
  
plot(fcst.sev.adverse$pred,type="l",col="purple"); lines(fcst.sev.adverse$mean,col="blue")

# Level    
fcst.sev.adverse.CMBS_7y  <- data[nrow(data)-td,"Fannie.DUS.7.6.5.DUS.Spread"]*cumprod(lcf.sarmax*exp(fcst.sev.adverse$pred))
fcst.sev.adverse.CMBS_10y <- data[nrow(data)-td,"Fannie.DUS.10.9.5.DUS.Spread"]*cumprod(lcf.sarmax*exp(fcst.sev.adverse$pred))

# Plots in Level
plot(fcst.sev.adverse.CMBS_7y , type="l",main="Sev.adverse - CMBS 7y",ylab="%",col="purple")
plot(fcst.sev.adverse.CMBS_10y , type="l",main="Sev.adverse - CMBS 10y",ylab="%",col="purple")


#### Exporting Data ####
# CST - CCR
cst <- matrix(NA,nrow=nrow(baseline.diff),ncol=3*3,byrow=F) # 3 bond classes; 3 scenarios
colnames(cst) <- c("baseline","baseline.upper","baseline.lower",
                   "adverse", "adverse.upper","adverse.lower",
                   "sev.adverse","sev.adverse.upper","sev.adverse.lower")

cst[,"baseline"] <- fcst.baseline$pred
cst[,"baseline.upper"] <- fcst.baseline$upper
cst[,"baseline.lower"] <- fcst.baseline$lower

cst[,"adverse"] <- fcst.adverse$pred
cst[,"adverse.upper"] <- fcst.adverse$upper
cst[,"adverse.lower"] <- fcst.adverse$lower

cst[,"sev.adverse"] <- fcst.sev.adverse$pred
cst[,"sev.adverse.upper"] <- fcst.sev.adverse$upper
cst[,"sev.adverse.lower"] <- fcst.sev.adverse$lower

write.table(cst,"CMBS_cst_ccr.txt",sep="\t")


# CST - Level
cst <- matrix(c(fcst.baseline.CMBS_7y,fcst.baseline.CMBS_10y,
                    fcst.adverse.CMBS_7y,fcst.adverse.CMBS_10y,
                    fcst.sev.adverse.CMBS_7y,fcst.sev.adverse.CMBS_10y),
              nrow=length(fcst.baseline.CMBS_7y),ncol=3*2,byrow=F) 
write.table(cst,"CMBS_cst_level.txt",sep="\t")


