#install.packages("rstudioapi")
library(rstudioapi)
dirname(getSourceEditorContext()$path)
setwd(dirname(getSourceEditorContext()$path))
#source(paste0(getwd(),'/backtesting/backtesting/backtest.R'))
source(paste0(getwd(), '/Week 9 GARCH backtest CCF/GarchBacktestExample/GarchBacktestExample/backtestGarch.R'))
source('DSC425-Util.R')

source('backtest.R')
library(tseries)
library(lmtest)
library(TSA)
library(fBasics)
library(forecast)
library(fUnitRoots)
library(ggplot2)
#install.packages("rugarch")
library(rugarch)
library(fGarch)
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
options(max.print = 10000)
head(item1, 365)

ts_d <- ts(item1$total_sales[1:365], frequency = 7)
dec_tsd <- decompose((ts_d))
plot(ts_d, main = 'Total Daily Sales of Item 1 for 2013', ylab='Quantity')
plot(dec_tsd)
plot(dec_tsd$random)

par(mfcol=c(1,2))
Acf(ts_d, lag.max = 365, main = 'ACF of Daily Sales of ITEM 1', 
    ylab = 'ACF')
Pacf(ts_d, lag.max = 365, main = 'PACF of Daily Sales of ITEM 1', 
     ylab = 'PACF')
eacf(ts_d)
adfTest(ts_d, type = 'nc')

# Differencing at lag-1 to obtain stationarity.
rln_d = diff(log(ts_d))
Acf(rln_d, lag.max = 260)
Pacf(rln_d,lag.max = 260)
eacf(rln_d)
#auto.arima(rln_d)
#coeftest(auto.arima(rln_d))
test_model1 <- Arima(y = rln_d, order = c(0,0,1), seasonal = list(order=c(0,0,1),period=7), include.mean = F)
test_model1
coeftest(test_model1)
Acf(test_model1$residuals, lag.max = 40)
Pacf(test_model1$residuals, lag.max = 40)
Box.test(test_model1$residuals, type = 'Ljung')
Box.test(test_model1$residuals, lag = 26,type = 'Ljung')
adfTest(test_model1$residuals,type='nc') # Needs seasonal differencing.

# Seasonal Differencing at lag-7 to obtain stationarity.
srln_d = diff(rln_d, 7)
Acf(srln_d, lag.max = 48, main = 'Deseasoned Log-Returns', ylab = 'ACF')
Pacf(srln_d,lag.max = 48, main = 'Deseasoned Log-Returns', ylab = 'PACF')
Box.test(srln_d, lag = 1, type='Ljung')
eacf(srln_d) # eacf suggest (1,0,2)

test_model2 <- Arima(y = ts_d, order = c(1,1,2), seasonal = list(order=c(1,1,1),period=7), include.mean = F)
test_model2
coeftest(test_model2)
Acf(test_model2$residuals, lag.max = 208, main = 'ACF of ARIMA(1,1,2)(1,1,1)[7]\nResiduals')
Pacf(test_model2$residuals, lag.max = 208, main = 'PACF of ARIMA(1,1,2)(1,1,1)[7]\nResiduals')
Box.test(test_model2$residuals, type = 'Ljung')
Box.test(test_model2$residuals, lag = 5,type = 'Ljung')
Box.test(test_model2$residuals, lag = 10,type = 'Ljung')
adfTest(test_model2$residuals, type='nc')
normalTest(test_model2$residuals, method = c('jb')) # Not normal

autom <- auto.arima(ts_d, stationary = T, seasonal = T,seasonal.test = 'ocsb',test = 'adf', ic='bic', stepwise = F, parallel = T, num.cores = 7)
autom
coeftest(autom) # auto.arima suggests ARIMA(1,0,1)(2,0,0)[7]
resid_autom <- autom$residuals
par(mfcol=c(1,1))
plot(resid_autom, type='l', main = 'ARIMA(1,0,1)(2,0,0)[7] Residuals', ylab='Residuals')
par(mfcol=c(1,2))
Acf(resid_autom, lag = 365, main = 'ACF of ARIMA(1,0,1)(2,0,0)[7]\nResiduals')
Pacf(resid_autom, lag = 365, main = 'PACF of ARIMA(1,0,1)(2,0,0)[7]\nResiduals')
Box.test(resid_autom, type='Ljung') # The residuals are uncorrelated.
Box.test(resid_autom, type='Ljung', lag = 10)
Box.test(resid_autom, type='Ljung', lag = 27)
tsdiag.Arima(autom,gof.lag = 150)
adfTest(resid_autom, type='nc')
normalTest(resid_autom, method = c('jb')) # Not normal

# Forecasting and Backtesting.
fs = forecast(test_model2, h=14)
par(mfcol=c(1,1))
plot(dec_tsd$trend)
plot(ts_d)
plot(fs, include=63, ylab='Total Sales', main='Forecasts of ARIMA(1,1,2)(1,1,1)[7]')
orig = ceiling(length(ts_d)*0.8)
test_model2_eval <- backtest(test_model2,rt=ts_d,orig = orig,h = 1)

fs_auto = forecast(autom, h=14)
plot(fs_auto, include=63, ylab='Total Sales', main='Forecasts of ARIMA(1,0,1)(2,0,0)[7]')
autom_eval <- backtest(autom, rt=ts_d, orig=orig,h=1)

par(mfcol=c(1,2))
plot(fs, include=63, ylab='Total Sales', main='Forecasts of ARIMA(1,1,2)(1,1,1)[7]')
plot(fs_auto, include=63, ylab='Total Sales', main='Forecasts of ARIMA(0,0,1)(2,0,0)[7]')
par(mfcol=c(1,1))

# AUTO-ARIMA's model is more efficient, because it achieves almost the same performance results as our model but with two less terms. Fitting the model below:

sarima_daily <- Arima(ts_d, order = c(1,0,1), seasonal = list(order =c(2,0,0),period = 7), include.mean = T)
sarima_daily
coeftest(sarima_daily) # Exactly like auto.arima results
backtest(m1 = sarima_daily,rt = ts_d,orig = orig,h = 1)

#############################################################################################
# GARCH

resid_sarimad <- sarima_daily$residuals
plot(resid_sarimad, type='l', main = 'Best SARIMA Daily Model (ARIMA(1,0,1)(2,0,0)[7])\nPlot of Residuals', ylab='Residuals')
par(mfcol=c(1,2))
Acf(resid_sarimad, lag = 365, main = 'ACF of SARIMA Daily Residuals')
Pacf(resid_sarimad, lag = 365, main = 'PACF of SARIMA Daily Residuals')
par(mfcol=c(1,1))
Box.test(resid_sarimad, type='Ljung') # The residuals are uncorrelated at lag-1 and lag-16
Box.test(resid_sarimad, type='Ljung', lag = 16)
Box.test(resid_sarimad, type='Ljung', lag = 21) # The residuals are autocorrelated at lag-21
normalTest(resid_sarimad, method = c('jb')) # Not normal
adfTest(resid_sarimad, type = 'nc')

Acf(resid_sarimad**2, lag = 100, main = 'Daily Model: ACF of Squared Residuals')
Pacf(resid_sarimad**2, lag = 100, main = 'Daily Model: PACF of Squared Residuals')
eacf(resid_sarimad**2)
Box.test(resid_sarimad**2, type='Ljung') # The residuals are uncorrelated at lag-1
Box.test(resid_sarimad**2, type='Ljung', lag = 16)
Box.test(resid_sarimad**2, type='Ljung', lag = 21)
normalTest(resid_sarimad**2, method = c('jb')) # Not normal

Acf(abs(resid_sarimad), lag = 365, main = 'ACF of Absolute SARIMA Residuals')

Pacf(abs(resid_sarimad), lag = 365, main = 'PACF of Absolute SARIMA Residuals')
Box.test(abs(resid_sarimad), type='Ljung') # The residuals are uncorrelated.
Box.test(abs(resid_sarimad), type='Ljung', lag = 16)
Box.test(abs(resid_sarimad), type='Ljung', lag = 21)
normalTest(abs(resid_sarimad), method = c('jb')) # Not normal

GARCH11_11 <- garchFit(~arma(1,1) + garch(1,1), data=srln_d, trace = F, include.mean = T)
GARCH11_11
garchFit()
summary(GARCH11_11)
plot(GARCH11_11@residuals, type="l")
plot(resid_sarimad,type='l')

Acf(GARCH11_11@residuals^2)
plot(GARCH11_11@residuals^2, type="l")
lines(GARCH11_11@h.t, col="red")

jarque.bera.test((GARCH11_11@residuals))
Box.test(GARCH11_11@residuals^2, type = 'Ljung')
jarque.bera.test((GARCH11_11@residuals^2))
par(mfcol=c(1,1))

#Prediction
par(mfcol=c(1,1))
predict(GARCH11_11, n.ahead=30, plot=T, nx=357)

#Backtesting
backtestGarch(m1 = GARCH11_11, rt = srln_d,orig = ceiling(length(srln_d)*0.8), h = 1)


spec <- ugarchspec(variance.model = list(garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1), include.mean=T))
UGARCH11_11 <- ugarchfit(spec = spec, data = srln_d)
UGARCH11_11

plot(UGARCH11_11, which =1)
plot(ugarchforecast(fitORspec = UGARCH11_11, n.ahead = 20), which=3)

plot(UGARCH11_11, which='all')
plot(ugarchforecast(fitORspec = UGARCH11_11, n.ahead = 20), which=1)