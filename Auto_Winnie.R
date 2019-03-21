
library(ggplot2)
setwd("/Users/yizihuang/Documents/CSC425 Time Series/Final Project")
library(reshape)
library(lubridate)
#library(forecast)
#library(TSA)
library(RColorBrewer)
library(ggrepel)
library(cowplot)
library(fUnitRoots)
library(lmtest)  # for coefficient test
source("backtest.R")

ds = read.csv("train.csv")

ds1 = aggregate(ds$sales, by = list(ds$item, ds$date), FUN = "sum")
ds1 = as.data.frame(ds1)
names(ds1) = c("item", "date", "total_sales")


############################################ Weekly basis ############################################
# TOTAL DAILY SALES OF ITEM 1 ACROSS ALL STORES
item1 = ds1[ds1$item==1,]
item2 = ds1[ds1$item==2,]
item3 = ds1[ds1$item==3,]
item4 = ds1[ds1$item==4,]
item5 = ds1[ds1$item==5,]
item6 = ds1[ds1$item==6,]
item7 = ds1[ds1$item==7,]
item8 = ds1[ds1$item==8,]
item9 = ds1[ds1$item==9,]
item10 = ds1[ds1$item==10,]


library(xts)
xts_item1 <- xts(item1$total_sales, as.Date(item1$date, "%Y-%m-%d"))
xts_item2 <- xts(item2$total_sales, as.Date(item1$date, "%Y-%m-%d"))
xts_item3 <- xts(item3$total_sales, as.Date(item1$date, "%Y-%m-%d"))
xts_item4 <- xts(item4$total_sales, as.Date(item1$date, "%Y-%m-%d"))
xts_item5 <- xts(item5$total_sales, as.Date(item1$date, "%Y-%m-%d"))
xts_item6 <- xts(item6$total_sales, as.Date(item1$date, "%Y-%m-%d"))
xts_item7 <- xts(item7$total_sales, as.Date(item1$date, "%Y-%m-%d"))
xts_item8 <- xts(item8$total_sales, as.Date(item1$date, "%Y-%m-%d"))
xts_item9 <- xts(item9$total_sales, as.Date(item1$date, "%Y-%m-%d"))
xts_item10 <- xts(item10$total_sales, as.Date(item1$date, "%Y-%m-%d"))

####### Weekly data ####### 
xts_w_item1 <- apply.weekly(xts_item1, FUN = sum)
xts_w_item2 <- apply.weekly(xts_item2, FUN = sum)
xts_w_item3 <- apply.weekly(xts_item3, FUN = sum)
xts_w_item4 <- apply.weekly(xts_item4, FUN = sum)
xts_w_item5 <- apply.weekly(xts_item5, FUN = sum)
xts_w_item6 <- apply.weekly(xts_item6, FUN = sum)
xts_w_item7 <- apply.weekly(xts_item7, FUN = sum)
xts_w_item8 <- apply.weekly(xts_item8, FUN = sum)
xts_w_item9 <- apply.weekly(xts_item9, FUN = sum)
xts_w_item10 <- apply.weekly(xts_item10, FUN = sum)

week_item1= as.vector(xts_w_item1)
week_item2= as.vector(xts_w_item2)
week_item3= as.vector(xts_w_item3)
week_item4= as.vector(xts_w_item4)
week_item5= as.vector(xts_w_item5)
week_item6= as.vector(xts_w_item6)
week_item7= as.vector(xts_w_item7)
week_item8= as.vector(xts_w_item8)
week_item9= as.vector(xts_w_item9)
week_item10= as.vector(xts_w_item10)

######## CCF for weekly sales of item 1 vs item 2...10
ccf(week_item1,week_item2) 
ccf(week_item1,week_item3) 
ccf(week_item1,week_item4) 
ccf(week_item1,week_item5) 
ccf(week_item1,week_item6)
ccf(week_item1,week_item7) 
ccf(week_item1,week_item8) 
ccf(week_item1,week_item9) 
ccf(week_item1,week_item10) 


#########Prewhiten####### 
pw_test1 = prewhiten(week_item1,week_item2) #0.8
pw_test2= prewhiten(week_item1,week_item3) #0.8
pw_test3= prewhiten(week_item1,week_item4) #0.6
pw_test4= prewhiten(week_item1,week_item5) #0.6
pw_test5= prewhiten(week_item1,week_item6)  #0.8
pw_test6= prewhiten(week_item1,week_item7) #0.8
pw_test7= prewhiten(week_item1,week_item8) #0.8
pw_test8= prewhiten(week_item1,week_item9)  #0.7
pw_test9= prewhiten(week_item1,week_item10)  #0.8


######   ARIMA model
m1 = arima(ts(week_item1), order = c(0,1,1), seasonal = list(order=c(0,1,1), period=52))
coeftest(m1) 
plot(m1$residuals)
acf(m1$residual)

### Residual analysis 
plot(m1$residuals)
acf(m1$residual)
Box.test(m1$residuals, type="Ljung-Box")  # Could not reject ; may be white noise
jarque.bera.test((m1$residuals))  #  Not Normally distributed

qqnorm(m1$residuals)
qqline(m1$residuals, col = 2) 


## Regression model with ARMA errors
m2 = arima(ts(week_item1), order = c(0,1,1), seasonal = list(order=c(0,1,1), period=52),xreg=data.frame(week_item2,week_item3,week_item6,week_item7,week_item8,week_item10)) ## select value >=0.8
coeftest(m2) 

## Final Regression model with ARMA errors
m3 = arima(ts(week_item1), order = c(0,1,1), seasonal = list(order=c(0,1,1), period=52),xreg=data.frame(week_item3,week_item6,week_item8),fixed = c(NA,0,NA,NA,NA))     ## remove insignificant parameters 
coeftest(m3) 
acf(m3$residual)
plot(m3$residuals)
Box.test(m3$residuals, type="Ljung-Box")  # Could not reject, may be white noise
jarque.bera.test((m3$residuals))  # Not Normally distributed

qqnorm(m3$residuals)
qqline(m3$residuals, col = 2) 


## Plot residuals together 
ts.plot(cbind(m1$residuals, 
              m3$residuals), col=4:3,
        xlab="Time", 
        ylab="Residuals",
        main=
          "Residuals for Two Moving Average Models")
legend("bottomleft",c("Without Regression Coefficient", "With Regression Coefficient"), fill=4:3, cex=.5, pt.cex =50)




#Back test  Almost the same performance
ntest=round(0.85*length(week_item1),digits = 0) # validation set is 85% of entire series
pm1 = backtest(m1, week_item1, ntest, h=1)
pm2 = backtest(m3, week_item1, ntest, h=1)

pm3 = backtest(m1, week_item6, ntest, h=1) ##  select one of  week_item6,week_item7,week_item8 to test model
pm4 = backtest(m3, week_item6, ntest, h=1) ##  select one of  week_item6,week_item7,week_item8 to test model


### Forecast using Final Regression model with ARMA errors m3 model
plot(forecast(m3, h=100, xreg=data.frame(week_item3,week_item6,week_item8)))
lines(week_item1, type="l")
lines(fitted(m3),col="orange")



############################################  Daily data using the first 365 data ######################

item1 = ds1[ds1$item==1,]
item2 = ds1[ds1$item==2,]
item3 = ds1[ds1$item==3,]
item4 = ds1[ds1$item==4,]
item5 = ds1[ds1$item==5,]
item6 = ds1[ds1$item==6,]
item7 = ds1[ds1$item==7,]
item8 = ds1[ds1$item==8,]
item9 = ds1[ds1$item==9,]
item10 = ds1[ds1$item==10,]



daily_item1  <- ts(item1$total_sales[1:365], frequency = 7)
daily_item2  <- ts(item2$total_sales[1:365], frequency = 7)
daily_item3  <- ts(item3$total_sales[1:365], frequency = 7)
daily_item4  <- ts(item4$total_sales[1:365], frequency = 7)
daily_item5  <- ts(item5$total_sales[1:365], frequency = 7)
daily_item6  <- ts(item6$total_sales[1:365], frequency = 7)
daily_item7  <- ts(item7$total_sales[1:365], frequency = 7)
daily_item8  <- ts(item8$total_sales[1:365], frequency = 7)
daily_item9  <- ts(item9$total_sales[1:365], frequency = 7)
daily_item10 <- ts(item10$total_sales[1:365], frequency = 7)



daily_item1= as.vector(daily_item1)
daily_item2= as.vector(daily_item2)
daily_item3= as.vector(daily_item3)
daily_item4= as.vector(daily_item4)
daily_item5= as.vector(daily_item5)
daily_item6= as.vector(daily_item6)
daily_item7= as.vector(daily_item7)
daily_item8= as.vector(daily_item8)
daily_item9= as.vector(daily_item9)
daily_item10= as.vector(daily_item10)


######## CCF for daily sales of item 1 vs item 2...10
ccf(daily_item1,daily_item2) 
ccf(daily_item1,daily_item3) 
ccf(daily_item1,daily_item4) 
ccf(daily_item1,daily_item5) 
ccf(daily_item1,daily_item6)
ccf(daily_item1,daily_item7) 
ccf(daily_item1,daily_item8) 
ccf(daily_item1,daily_item9) 
ccf(daily_item1,daily_item10) 


### prewhitten
d_pw_test1 = prewhiten(daily_item1,daily_item2) #0.3
d_pw_test2= prewhiten(daily_item1,daily_item3) #0.25
d_pw_test3= prewhiten(daily_item1,daily_item4) #0.25
d_pw_test4= prewhiten(daily_item1,daily_item5) #0.15
d_pw_test5= prewhiten(daily_item1,daily_item6)  #0.2
d_pw_test6= prewhiten(daily_item1,daily_item7) #0.35
d_pw_test7= prewhiten(daily_item1,daily_item8) #0.3
d_pw_test8= prewhiten(daily_item1,daily_item9)  #0.25
d_pw_test9= prewhiten(daily_item1,daily_item10)  #0.25 

### ARMAR model on daily basis
m4 = arima(daily_item1, order = c(1,0,1), seasonal = list(order=c(2,0,0), period=7),include.mean = T) 
coeftest(m4) 
plot(m4$residuals)
Acf(m4$residual)
Box.test(m4$residuals, type="Ljung-Box")  
jarque.bera.test((m4$residuals))  

qqnorm(m4$residuals)
qqline(m4$residuals, col = 2) 


### Regression model with ARMA erros 
m5 = arima(daily_item1, order = c(1,0,1), seasonal = list(order=c(2,0,0), period=7),xreg=data.frame(daily_item2,daily_item7,daily_item8)) ## select item 2, 7, 8 based on value >0.3
coeftest(m5) ## ar1,ma1,sar2,intercept are not significant, used the fixed to get rid of them # aic = 2941.95

### Final Regression model with ARMA erros 
m6 = arima(daily_item1, order = c(1,0,1), seasonal = list(order=c(2,0,0), period=7),xreg=data.frame(daily_item2,daily_item7,daily_item8),fixed=c(0,0,NA,0,0,NA,NA,NA))
coeftest(m6)  ## All significant ## aic = 2935.57 get better result


### Residual analysis 
plot(m6$residuals)
Acf(m6$residual)
Box.test(m6$residuals, type="Ljung-Box")  # Could not reject ; may be white noise
jarque.bera.test((m6$residuals))  #  Normally distributed

qqnorm(m6$residuals)
qqline(m6$residuals, col = 2) 


## Plot residuals together 
ts.plot(cbind(m4$residuals, 
              m6$residuals), col=4:3,
        xlab="Time", 
        ylab="Residuals",
        main=
          "Residuals for Two Moving Average Models")
legend("bottomleft",c("Without Regression Coefficient", "With Regression Coefficient"), fill=4:3, cex=.5, pt.cex =50)

#Back test  
ntest=round(0.85*length(daily_item1),digits = 0) # validation set is 85% of entire series
pm5 = backtest(m4, daily_item1, ntest, h=1)
pm6 = backtest(m6, daily_item1, ntest, h=1)


pm7 = backtest(m4, daily_item2, ntest, h=1) ##  select one of  week_item2,week_item7,week_item8 to test model
pm8 = backtest(m6, daily_item2, ntest, h=1) ##  select one of  week_item2,week_item7,week_item8 to test model


## Forecast
plot(forecast(m6, h=100, xreg=data.frame(daily_item2,daily_item7,daily_item8)))
lines(daily_item1, type="l")
lines(fitted(m6),col="orange")



