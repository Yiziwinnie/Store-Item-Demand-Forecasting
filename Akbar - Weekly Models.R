dirname(getSourceEditorContext()$path)
setwd(dirname(getSourceEditorContext()$path))
source(paste0(getwd(),'/backtesting/backtesting/backtest.R'))
source(paste0(getwd(), '/Week 9 GARCH backtest CCF/GarchBacktestExample/GarchBacktestExample/backtestGarch.R'))
source('DSC425-Util.R')

source('backtest.R')
library(lmtest)
library(TSA)
library(fBasics)
library(forecast)
library(fUnitRoots)
library(ggplot2)
ds = read.csv("train.csv")
ds[1:30,]
dim(ds)

# Aggregating TOTAL DAILY SALES PER ITEM ACROSS ALL STORES
ds1 = aggregate(ds$sales, by = list(ds$item, ds$date), FUN = "sum")
# dim(ds1)
# head(ds1)

ds1 = as.data.frame(ds1)
#head(ds1)
names(ds1) = c("item", "date", "total_sales")

# TOTAL DAILY SALES OF ITEM 1 ACROSS ALL STORES
item1 = ds1[ds1$item==1,]

# dim(item1)
head(item1)
# basicStats(item1$total_sales)
##########################################################################

# Aggregating total DAILY sales of item 1 into weekly sales
library(xts)
xts <- xts(item1$total_sales, as.Date(item1$date, "%Y-%m-%d"))
xts_w <- apply.weekly(xts, FUN = sum)
ts_w <- ts(xts_w, start = 2013, frequency = 52)
plot(ts_w, main = 'Total Weekly Sales of Item 1', ylab = 'Total Sales')
plot(xts_w, main = 'Total Weekly Sales of Item 1', ylab = 'Total Sales')
dec = decompose(ts_w, 'm')
plot(dec)

stl_w <- stl(ts_w[,1], s.window = 52)
trend_stlw <- stl_w$time.series[,2]
seas_stlw <- stl_w$time.series[,1]
remainder <- stl_w$time.series[,3]
plot(trend_stlw)
plot(seas_stlw)
plot(remainder)

ts_w[,1]
ts_w

par(mfcol=c(1,2))
Acf(ts_w, lag.max = 160, main = 'ACF of Total Weekly Sales of ITEM 1', 
    ylab = 'ACF')
Pacf(ts_w, lag.max = 160, main = 'PACF of Total Weekly Sales of ITEM 1', 
     ylab = 'PACF')
eacf(ts_w)
adfTest(ts_w, type = 'nc')

# Differencing at lag-1 to obtain stationarity.
rln_w = diff(log(ts_w))
Acf(rln_w, lag.max = 260)
Pacf(rln_w,lag.max = 260)
eacf(rln_w)
#auto.arima(rln_w)
#coeftest(auto.arima(rln_w))
test_model1 <- Arima(y = rln_w, order = c(0,0,1), seasonal = list(order=c(0,0,1),period=52), include.mean = F)
test_model1
coeftest(test_model1)
Acf(test_model1$residuals, lag.max = 400)
Box.test(test_model1$residuals, type = 'Ljung')
Box.test(test_model1$residuals, lag = 26,type = 'Ljung')
adfTest(test_model1$residuals,type='nc') # Needs seasonal differencing.

# Seasonal Differencing at lag-52 to obtain stationarity.
srln_w = diff(rln_w, 52)
Acf(srln_w, lag.max = 48, main = 'Deseasoned Log-Returns', ylab = 'ACF')
Pacf(srln_w,lag.max = 48, main = 'Deseasoned Log-Returns', ylab = 'PACF')
Box.test(srln_w, lag = 1, type='Ljung')
eacf(srln_w) # eacf suggest (0,0,1)

test_model2 <- Arima(y = srln_w, order = c(0,0,1), seasonal = list(order=c(0,0,1),period=52), include.mean = F)
test_model2
coeftest(test_model2)
Acf(test_model2$residuals, lag.max = 208)
Pacf(test_model2$residuals, lag.max = 208)
Box.test(test_model2$residuals, type = 'Ljung')
Box.test(test_model2$residuals, lag = 5,type = 'Ljung')
Box.test(test_model2$residuals, lag = 10,type = 'Ljung')
adfTest(test_model2$residuals, type='nc')

autom <- auto.arima(srln_w, stationary = T, seasonal = T,seasonal.test = 'ocsb',test = 'adf', ic='bic', stepwise = F, parallel = T, num.cores = 7)
autom
coeftest(autom) # auto.arima agrees with the results.

# Seasonal model 1 looks as following:
seas_model1 <- Arima(y=ts_w, order = c(0,1,2), seasonal = list(order=c(0,1,1), period=52), include.mean =F)
seas_model1
coeftest(seas_model1)
res1 <- seas_model1$residuals
Acf(res1, lag.max = 260, main = 'ACF Plot of Residuals\nARIMA(0,1,2)x(0,1,1)[52]')
Box.test(res1, type = 'Ljung')
Box.test(res1, lag = 3,type = 'Ljung')
Box.test(res1, lag =12, type = 'Ljung')
normalTest(res1, method = c('jb')) # Reject that the data is coming from normally distributed data.
plot(res1, type='l', main='Residuals\nARIMA(0,1,2)(0,1,1)[52]', ylab='')
adfTest(res1, type='nc') # The residuals are uncorrelated, BUT they are not Gaussian White Noise.

fs = forecast(seas_model1, h=208)
plot(fs, include=260, ylab='Total Sales', main='Forecasts of ARIMA(0,1,2)(0,1,1)[52]')
orig = ceiling(length(ts_w)*0.8)
seas_model1_eval <- backtest(seas_model1,rt=ts_w,orig = orig,h = 1)

# Seasonal model 2 looks as following:
seas_model2 <- Arima(y=ts_w, order = c(0,1,1), seasonal = list(order=c(0,1,1), period=52), include.mean =F)
seas_model2
coeftest(seas_model2)
res2 <- seas_model2$residuals
Acf(res2, lag.max = 208, main = 'ACF Plot of Residuals: ARIMA(0,1,1)x(0,1,1)[52]')
Pacf(res2, lag.max = 208, main = 'PACF Plot of Residuals: ARIMA(0,1,1)x(0,1,1)[52]')
Box.test(res2, type = 'Ljung')
Box.test(res2, lag = 3,type = 'Ljung')
Box.test(res2, lag =12, type = 'Ljung')
Box.test(res2, lag =32, type = 'Ljung')
normalTest(res2, method = c('jb')) # Reject that the data is coming from normally distributed data.
plot(res2, type='l', main='Residuals\nARIMA(0,1,1)(0,1,1)[52]', ylab='')
plot(stl_w)
plot(remainder)
adfTest(res2, type='nc') # The residuals are uncorrelated, BUT they are not Gaussian White Noise.

fs = forecast(seas_model2, h=52)
plot(fs, include=260, ylab='Total Sales', main='Forecasts of ARIMA(0,1,1)(0,1,1)[52]')
orig = ceiling(length(ts_w)*0.8)
seas_model2_eval <- backtest(seas_model2,rt=ts_w,orig = orig,h = 1)
seas_model1_eval
seas_model2_eval # This model is better.

# Experimenting with ARCH

#install.packages("rugarch")
#install.packages("fGarch")
library(rugarch)
library(fGarch)
Acf(srln_w, lag.max = 208) # Use residuals for more accurate determining order of GARCH

Acf(abs(srln_w),lag.max = 52, main='ACF of Absolute Log-Returns')
Pacf(abs(srln_w), lag.max = 52, main='PACF of Absolute Log-Returns')
Box.test(abs(srln_w), lag=1, type='Ljung')
Box.test(abs(srln_w), lag=4, type='Ljung')
Box.test(abs(srln_w), lag=8, type='Ljung')

Acf(seas_model2$residuals**2, lag.max = 52, main='Weekly Model: ACF of Squared Residuals')
Pacf(seas_model2$residuals**2, lag.max = 52, main='Weekly Model: PACF of Squared Residuals')
Box.test(seas_model2$residuals**2, lag = 1,type = 'Ljung')
Box.test(srln_w**2, lag = 4,type = 'Ljung')
Box.test(srln_w**2, lag = 8,type = 'Ljung')
Box.test(srln_w**2, lag = 12,type = 'Ljung')

garch0111 <- garchFit(~arma(0,1) + garch(1,1), data = srln_w, trace = F, include.mean = F)
garch0111
summary(garch0111)
plot(garch0111@residuals, type="l")
Acf(garch0111@residuals^2, lag = 208)
Box.test(garch0111@residuals^2, type='Ljung', lag=1)
Box.test(garch0111@residuals^2, type='Ljung', lag=100)
Box.test(garch0111@residuals^2, type='Ljung', lag=150)
jarque.bera.test((garch0111@residuals^2))

Box.test(garch0111@residuals, type='Ljung', lag=1)
Box.test(garch0111@residuals, type='Ljung', lag=100)
Box.test(garch0111@residuals, type='Ljung', lag=150)
jarque.bera.test((garch0111@residuals))
plot(srln_w) # progress!!

plot(garch0111@residuals^2, type="l")
lines(garch0111@h.t, col="red")

plot(garch0111@residuals^2, type="l", ylim=c(0, .015), lwd=1, 
     main ='Conditional SD vs Log-Returns', ylab = 'Volatility',
     xlab='')
lines(garch0111@h.t, col="red", lwd=2)
plot(garch0111@h.t, type="l")

plot((garch0111@residuals / sqrt(garch0111@h.t))^2, type="l")
plot(garch0111@residuals / garch0111@sigma.t, type='l')

# Prediction
par(mfcol=c(1,1))
predict(garch0111, n.ahead=10, plot=T, nx=58)
ceiling(208*0.8)
# Backtesting
source("/GarchBacktestExample/backtestGarch.R")
backtestGarch(m1 = garch0111, rt = srln_w,orig = ceiling(208*0.8), h = 1)


# Same model using rugarch.
garch11.spec = ugarchspec(variance.model = list(garchOrder=c(1,1)), mean.model = list(armaOrder=c(0,1), include.mean=F))
# setfixed(garch11.spec) <- list('mu'=0,'alpha1'=0, 'omega'=0)
garch11.fit = ugarchfit(spec = garch11.spec, data=srln_w)
garch11.fit
persistence(garch11.fit)
halflife(garch11.fit)
plot(garch11.fit)
par(mfcol=c(1,2))
plot(garch11.fit, which=1)
plot(ugarchforecast(fitORspec = garch11.fit, n.ahead = 20), which=3)
par(mfcol=c(1,1))
plot(garch11.fit, which='all')
plot(ugarchforecast(fitORspec = garch11.fit, n.ahead = 20), which=1)

# IGNORE THE CODE BELOW
# 
# X_t = Phi_0 + Phi_1*B + a_t # AR
# a_t = theta_0 - theta_1*a_t-1 - e_t# MA
# e_t comes from N(0,sigma^2)
# sigma^2 = omega + alpha_1*a_t-1 + beta_1*sigma^2 # GARCH
#
# res <- seas_model$residuals
# plot(res, type='l', main='Residuals\nARIMA(0,1,1)(0,1,1)[52]', ylab='')
# sqres <- res**2
# plot(sqres, main='Squared Residuals\nARIMA(0,1,1)(0,1,1)[52]',ylab='')
# Acf(abs(res),lag.max = 30, main='ACF Plot of Absolute Residuals\nARIMA(0,1,1)(0,1,1)[52]')
# Acf(sqres, lag.max = 30, main='ACF Plot of Squared Residuals\nARIMA(0,1,1)(0,1,1)[52]')
# 
# McLL_sqres <- McLeod.Li.test(object=seas_model,gof.lag = 25, type='b')
# sig_lags <- which(McLL_sqres$p.values<0.05)
# sig_lags # Non-constant variance is detected.
# 
# 
# archFit <- garch(srln_w, order = c(1,1), control = )
# archFit
# coeftest(archFit)
# arch_res <- archFit$residuals[-(1:5)]
# plot(arch_res, type='l')
# 
# plot(srln_w, type='l', main='Time Plot of Log-Returns of\nTotal Weekly Sales: Item 1', ylab='Log-Returns')
# 
# sqrln_w <- srln_w**2
# Acf(abs(srln_w))
# Acf(sqrln_w, lag.max = 260)
# McLeod.Li.test(object = seas_model,y=ts_w,col='red', gof.lag = 25, type='b')
# McLL_sqrln <- McLeod.Li.test(object = seas_model,y=ts_w,col='red', gof.lag = 25, type='b', plot = F)
# sig_lags <- which(McLL_sqrln$p.values<0.05)
# sig_lags
# plot(sqrln_w)
