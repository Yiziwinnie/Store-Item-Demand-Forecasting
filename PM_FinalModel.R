library(ggplot2)
library(Hmisc)
library(tseries)
library(fBasics)
library(zoo)
library(TSA)
library(lmtest)
library(forecast)
library(fUnitRoots)
library(xts)
library(lubridate)

ds = read.csv("~/Downloads/train.csv")
dim(ds)
head(ds)

#=============================================================================
# Create clean table -> CORPORATE SALES ALL ITEMS (sales across 10 stores)
#=============================================================================

corp_sales = aggregate(ds$sales, by = list(ds$item, ds$date), FUN = "sum") 
dim(corp_sales)

corp_sales = as.data.frame(corp_sales)
names(corp_sales) = c("item", "date", "total_sales")
item1 = corp_sales[corp_sales$item==1,] #isolate item 1 (or any item)
rownames(item1) = 1:nrow(item1)
head(item1, 100)
dim(item1)

plot(item1$total_sales, type='l')

i1_salesdiff = as.data.frame(diff(item1$total_sales)) #difference data to normalize/try to get stationarity
i1_salesdiff <- rbind(c(0), i1_salesdiff)
i1_salesdiff[1,] = NA

#Compute difference and add to clean table
i1_salesdiff$row = 1:nrow(i1_salesdiff) # add index column to merge with item1
head(i1_salesdiff)

item1$row = 1:nrow(item1) # add index column to merge with i1_salesdiff
item1 = merge.data.frame(item1, i1_salesdiff, by="row") #merge
item1$row <- NULL

names(item1) = c("item", "date", "total_sales", "sales_difference")
head(item1)

#================================================================
#SUMMARY STATISTICS
#================================================================
summary(item1$sales_difference)
skewness(item1$sales_difference[-1])
kurtosis(item1$sales_difference[-1])

#================================================================
#EXPLORATORY VISUALIZATIONS
#================================================================

#Q-Q plots - sales differnce and total sales
qqnorm(item1$total_sales)
qqline(item1$total_sales, col=2)

qqnorm(item1$sales_difference)
qqline(item1$sales_difference, col=2)

ts_item1 = ts(item1$total_sales, start=2013, frequency = 365) #create timeseries object
plot(decompose(ts_item1))

ggplot(item1) + 
  geom_line(aes(x=as.Date(date), y=total_sales, group=1), color="black") +
  geom_line(aes(x=as.Date(date), y=sales_difference, group=1), color="blue") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b-%y") + 
  ggtitle("Item 1 - Total corporate sales and differenced sales") + 
  xlab("Date") + ylab("Sales") + theme_minimal() + 
  theme(plot.title=element_text(face="bold", hjust = 0.5))

ggplot() + geom_histogram(aes(item1$sales_difference), color="black", fill="lightgray", binwidth = 10) + 
  scale_x_continuous(breaks=c(seq(-200, 200, 20))) + 
  ggtitle("Item 1 - Distribution of Sales Difference (Previous Day)") + 
  theme(plot.title=element_text(face="bold", hjust = 0.5)) + 
  xlab('Change') + ylab("Count")

ggplot() + geom_histogram(aes(item1$total_sales), color="black", fill="lightgray", binwidth = 10)  + 
  ggtitle("Item 1 - Total Sales") + 
  theme(plot.title=element_text(face="bold", hjust = 0.5)) + 
  xlab('# Sales') + ylab("Count")

#================================================================
#Determining seasonality
#================================================================

acf(item1$total_sales, lag=50, main="ACF item 1 total sales")

acf(item1$sales_difference[-1], lag=400, main="ACF item 1 differenced sales (1)")

d7 = diff(item1$sales_difference[-1], 7)
acf(d7, lag=40, main="ACF item 1 differenced sales (1, 7)")
eacf(d7)

d365 = diff(item1$sales_difference[-1], 365)
acf(d365, lag=400)

#================================================================
#Building SARIMA model based on seasonality
#================================================================

#Weekly lag
model1 = arima(item1$total_sales,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=7), method="ML") 
coeftest(model1)
model1

#Model based on differenced sales
model2 = arima(item1$sales_difference[-1],order=c(0,1,1),seasonal=list(order=c(0,1,1),period=7), method="ML") 
model2a = auto.arima(item1$sales_difference[-1])
coeftest(model2a)
model2

#Try to model yearly seasonality
model3 = auto.arima(item1$total_sales)
coeftest(model3)
model3


#Model checking / residual analysis
tsdiag(model1)
Box.test(model1$residuals, 6, "Ljung-Box") 
Box.test(model1$residuals, 12, "Ljung-Box") 

acf(model1$residuals)
qqnorm(model1$residuals)
qqline(model1$residuals, col=2)

#================================================================
#Backtest
#================================================================

source("~/Desktop/dsc425/lecture5/HoneyNet/backtest.R")
len1 = 0.8*length(item1$total_sales)
bt1 = backtest(model1, item1$total_sales, orig=len1, h=1)

mean(item1$total_sales) # 219.8
#We can say this is a pretty accurate model, RMSE for this model
#per the backtesting procedure is 18.3, and considering the mean
#of total sales is around 220, we aren't that far off for a given
#sales number prediction (for the scale of this data). 

#================================================================
#Compute predictions / forecast plot for original variable
#================================================================

forecast1 = predict(model1, 50)
#item1WithForecasts = c(item1$total_sales, forecast1$pred)
#mylcl = c(item1$total_sales, forecast1$pred-2*forecast1$se)
#myucl = c(item1$total_sales, forecast1$pred+2*forecast1$se)
plot(item1$total_sales, type="l", col="red", xlim=c(1780,1880), 
     main="Model 1 forecasts", xlab="Day", ylab="Item 1 Total Sales")
points(item1$total_sales)
lines(forecast1$pred, type="l", col="blue", xlim=c(1780,1880))
points(forecast1$pred)


#This shows that our model only works on a weekly scale, not yearly
forecast2 = predict(model1, 600)
plot(item1$total_sales, type="l", col="red", xlim=c(1000,2000), 
     main="Model 1 forecasts", xlab="Day", ylab="Item 1 Total Sales")
lines(forecast2$pred, type="l", col="blue", xlim=c(1000,2000))


#Visualizations of model 2, which is based on differenced sales
forecast3 = predict(model2, 50)
plot(item1$sales_difference[-1], type="l", col="red", xlim=c(1780,1880), 
     main="Model 2 forecasts", xlab="Day", ylab="Item Differenced Sales")
points(item1$sales_difference[-1])
lines(forecast3$pred, type="l", col="blue", xlim=c(1780,1880))
points(forecast3$pred)

#Probably works yearly?
forecast4 = predict(model2, 600)
plot(item1$sales_difference[-1], type="l", col="red", xlim=c(1000,2000), 
     main="Model 2 forecasts", xlab="Day", ylab="Item 1 Differenced Sales")
lines(forecast4$pred, type="l", col="blue", xlim=c(1000,2000))

forecast5 = predict(model3, 200)
plot(item1$total_sales, type="l", col="red", xlim=c(1500,2000), 
     main="Model 1 forecasts", xlab="Day", ylab="Item 1 Total Sales")
points(item1$total_sales)
lines(forecast5$pred, type="l", col="blue", xlim=c(1500,2000))
points(forecast5$pred)







