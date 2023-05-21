

library(forecast)
library(zoo)
#library("xlsx")

setwd("~/Desktop/time_series_analytics/new")

# Create data frame.
sales.data <- read.csv("Sales_vitacost.csv")

head(sales.data)
tail(sales.data)

sales.ts <- ts(sales.data$Sales, 
                     start = c(2010, 1), end = c(2022, 12), freq = 12)
sales.ts

## Using plot() to plot time series data  
plot(sales.ts, 
     xlab = "Time", ylab = "Sales (in million-Dollars)", 
     ylim = c(30000, 85000), xaxt = 'n',
     main = "Vitacost Sales in the U.S.")

# Establish x-axis scale interval for time in months.
axis(1, at = seq(2010, 2022, 1), labels = format(seq(2010, 2022, 1)))

salesdata.stl <- stl(sales.ts, s.window = "periodic")
autoplot(salesdata.stl, main = "Vitacost Sales in the U.S Time Series Components")

# Using Acf() function to identify autocorrelation and plot autocorrelation for different lags.
autocor <- Acf(sales.ts, lag.max = 12, 
               main = "Autocorrelation for Vitacost Sales in the U.S")

# Display autocorrelation coefficients for various lags.
Lag <- round(autocor$lag, 0)
ACF <- round(autocor$acf, 3)
data.frame(Lag, ACF)

# Use Arima() function to fit AR(1) model.
# The ARIMA model of order = c(1,0,0) gives an AR(1) model.
sales.ar1<- Arima(sales.ts, order = c(1,0,0))
summary(sales.ar1)

ar1 <- 0.9549
s.e. <- 0.0258
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
  "Reject null hypothesis"
} else {
  "Accept null hypothesis"
}

diff.sales <- diff(sales.ts, lag = 1)

# Use Acf() function to identify autocorrelation for the model difference. 
# and plot autocorrelation for different lags (up to maximum of 8).
Acf(diff.sales, lag.max = 12, 
    main = "Autocorrelation for First Differencing of Sales")

#Smoothing Methods: two level forecast with regression and trailing MA residuals
# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
nValid <- 48
nTrain <- length(sales.ts) - nValid
train.ts <- window(sales.ts, start = c(2010, 1), end = c(2010, nTrain))
valid.ts <- window(sales.ts, start = c(2010, nTrain + 1), 
                   end = c(2010, nTrain + nValid))
train.ts
valid.ts


#### results shows the data set is predictable and not random

##############################################################################################################################

###### Model 1 -------------------HW Model

# Use ets() function with model = "ZZZ", i.e., automated selection of error, trend, and seasonality options.
# Use optimal alpha, beta, & gamma to fit HW over the training period.
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ 

# Use forecast() function to make predictions using this HW model with validation period (nValid). 
# Show predictions in tabular format.
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred

# Plot HW predictions for original data, automatic selection of the 
# model and optimal smoothing parameters.
plot(hw.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Sales (in million $)", ylim = c(25000, 90000), 
     bty = "l", xlim = c(2010, 2023.25), xaxt = "n",
     main = "Holt-Winter's Model with Automatic Selection of Model Options", 
     lty = 5, col = "blue", lwd = 2) 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(hw.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(sales.ts)
legend(2010,80000, 
       legend = c("Sales", 
                  "Holt-Winter's Model for Training Partition",
                  "Holt-Winter's Model for Validation Partition"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019, 2019), c(0, 90000))
lines(c(2023, 2023), c(0, 90000))
text(2014, 90000, "Training")
text(2020.5, 90000, "Validation")
text(2023.5, 90000, "Future")
arrows(2010, 84000, 2018.9, 84000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 84000, 2022.9, 84000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023.1, 84000, 2024, 84000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

## Regression Based Models
############### Model 2----------------- LINEAR TREND AND SEASONALITY MODEL.
# Use tslm() function to create linear trend and seasonal model.
train.lin.trend.season <- tslm(train.ts ~ trend  + season)

# See summary of linear trend and seasonality model and associated parameters.
summary(train.lin.trend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.lin.trend.season.pred <- 
  forecast(train.lin.trend.season, h = nValid, level = 0)
train.lin.trend.season.pred

plot(train.lin.trend.season.pred$mean, 
     xlab = "Time", ylab = "Sales (in million $)", ylim = c(25000, 90000), 
     bty = "l", xlim = c(2010, 2023.25), xaxt = "n",
     main = "Regression Model with Linear Trend and Seasonality", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)) )
lines(train.lin.trend.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
legend(2010,80000, legend = c("Sales Time Series", "Linear Trend and Seasonality Model for Training Data",
                              "Linear Trend and Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019, 2019), c(0, 90000))
lines(c(2023, 2023), c(0, 90000))
text(2014, 90000, "Training")
text(2020.5, 90000, "Validation")
text(2023.5, 90000, "Future")
arrows(2010, 84000, 2018.9, 84000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 84000, 2022.9, 84000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023.1, 84000, 2024, 84000, code = 3, length = 0.1,
       lwd = 1, angle = 30)



###############Model 3-Regression with Linear Trend and Seasonality with AR(4) Model

# Use Acf() function to identify autocorrelation for the model residuals 
# (training and validation sets), and plot autocorrelation for different 
# lags (up to maximum of 12).
Acf(train.lin.trend.season.pred$residuals, lag.max = 12, 
    main = "Autocorrelation of Regression with linear trend and seasonality model for Training Residuals")
Acf(valid.ts - train.lin.trend.season.pred$mean, lag.max = 12, 
    main = "Autocorrelation of Regression with linear trend and seasonality model for Validation Residuals")


## USE Arima() FUNCTION TO CREATE AR(1) MODEL FOR TRAINING RESIDUALS.
## CREATE TWO-LEVEL MODEL WITH LINEAR TREND AND SEASONALITY MODEL 
## AND AR(1) RESIDUALS.
## PLOT DATA AND IDENTIFY ACCURACY MEASURES.

# Use Arima() function to fit AR(1) model for training residuals. The Arima model of 
# order = c(1,0,0) gives an AR(1) model.
# Use summary() to identify parameters of AR(1) model. 
res.ar1 <- Arima(train.lin.trend.season$residuals, order = c(4,0,0))
summary(res.ar1)

res.ar1$fitted

# Use forecast() function to make prediction of residuals in validation set.
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)
res.ar1.pred

# Develop a data frame to demonstrate the training AR model results 
# vs. original training series, training regression model, 
# and its residuals.  
train.df <- round(data.frame(train.ts, train.lin.trend.season$fitted, 
                             train.lin.trend.season$residuals, res.ar1$fitted, res.ar1$residuals), 3)
names(train.df) <- c("Sales", "Regression", "Residuals",
                     "AR.Model", "AR.Model.Residuals")
train.df


# Use Acf() function to identify autocorrelation for the training 
# residual of residuals and plot autocorrelation for different lags 
# (up to maximum of 12).
Acf(res.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for Vitacost Training Residuals of Residuals")

# Create two-level model's forecast with linear trend and seasonality 
# regression + AR(1) for residuals for validation period.

# Create data table with validation data, regression forecast
# for validation period, AR(1) residuals for validation, and 
# two level model results. 
valid.two.level.pred <- train.lin.trend.season.pred$mean + res.ar1.pred$mean

valid.df <- round(data.frame(valid.ts, train.lin.trend.season.pred$mean, 
                             res.ar1.pred$mean, valid.two.level.pred),3)
names(valid.df) <- c("Sales", "Reg.Forecast", 
                     "AR(4)Forecast", "Combined.Forecast")
valid.df

# plot ts data, linear trend and seasonality data, and predictions 
# for validation period.
plot(valid.two.level.pred, 
     xlab = "Time", ylab = "Sales (in million $)", ylim = c(25000, 90000), 
     bty = "l", xlim = c(2010, 2023.25), xaxt = "n",
     main = "Two-Level Forecast: Regression with Trend
             and Seasonlity + AR(4) for Residuals", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)) )
lines(train.lin.trend.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
legend(2010,80000, legend = c("Sales Time Series", "Regression for Training Data",
                              "Two Level Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019, 2019), c(0, 90000))
lines(c(2023, 2023), c(0, 90000))
text(2014, 90000, "Training")
text(2020.5, 90000, "Validation")
text(2023.5, 90000, "Future")
arrows(2010, 84000, 2018.9, 84000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 84000, 2022.9, 84000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023.1, 84000, 2024, 84000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use accuracy() function to identify common accuracy measures for validation period forecast:
# (1) two-level model (linear trend and seasonal model + AR(4) model for residuals),
# (2) linear trend and seasonality model only.
round(accuracy(valid.two.level.pred, valid.ts), 3)
round(accuracy(train.lin.trend.season.pred$mean, valid.ts), 3)



############# Model 4------------------- QUADRATIC TREND AND SEASONALITY MODEL.

# Use tslm() function to create quadratic trend and seasonal model.
train.quad.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)

# See summary of quadratic trend and seasonality model and associated parameters.
summary(train.quad.trend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.quad.trend.season.pred <- 
  forecast(train.quad.trend.season, h = nValid, level = 0)
train.quad.trend.season.pred

#plot
plot(train.quad.trend.season.pred$mean, 
     xlab = "Time", ylab = "Sales (in million $)", ylim = c(25000, 90000), 
     bty = "l", xlim = c(2010, 2023.25), xaxt = "n",
     main = "Regression Model with Quadratic Trend and Seasonality", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)) )
lines(train.quad.trend.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
legend(2010,80000, legend = c("Sales Time Series", "Quadratic Trend and Seasonality Model for Training Data",
                              "Quadratic Trend and Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019, 2019), c(0, 90000))
lines(c(2023, 2023), c(0, 90000))
text(2014, 90000, "Training")
text(2020.5, 90000, "Validation")
text(2023.5, 90000, "Future")
arrows(2010, 84000, 2018.9, 84000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 84000, 2022.9, 84000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023.1, 84000, 2024, 84000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


#######################
###############Model 5- Regression with Quadratic Trend and Seasonality with AR(4) Model

# Use Acf() function to identify autocorrelation for the model residuals 
# (training and validation sets), and plot autocorrelation for different 
# lags (up to maximum of 12).
Acf(train.quad.trend.season.pred$residuals, lag.max = 12, 
    main = "Autocorrelation of Quadratic Trend and Seasonality Model for Training Residuals")
Acf(valid.ts - train.quad.trend.season.pred$mean, lag.max = 12, 
    main = "Autocorrelation of Quadratic Trend and Seasonality Model for Validation Residuals")


## USE Arima() FUNCTION TO CREATE AR(1) MODEL FOR TRAINING RESIDUALS.
## CREATE TWO-LEVEL MODEL WITH LINEAR TREND AND SEASONALITY MODEL 
## AND AR(1) RESIDUALS.
## PLOT DATA AND IDENTIFY ACCURACY MEASURES.

# Use Arima() function to fit AR(1) model for training residuals. The Arima model of 
# order = c(1,0,0) gives an AR(1) model.
# Use summary() to identify parameters of AR(1) model. 
res.ar1.quad <- Arima(train.quad.trend.season$residuals, order = c(4,0,0))
summary(res.ar1.quad)
res.ar1.quad$fitted

# Use forecast() function to make prediction of residuals in validation set.
res.ar1.quad.pred <- forecast(res.ar1.quad, h = nValid, level = 0)
res.ar1.quad.pred

# Develop a data frame to demonstrate the training AR model results 
# vs. original training series, training regression model, 
# and its residuals.  
train2.df <- round(data.frame(train.ts, train.quad.trend.season$fitted, 
                             train.quad.trend.season$residuals, res.ar1.quad$fitted, res.ar1.quad$residuals), 3)
names(train2.df) <- c("Ridership", "Regression", "Residuals",
                     "AR.Model", "AR.Model.Residuals")
train2.df


# Use Acf() function to identify autocorrelation for the training 
# residual of residuals and plot autocorrelation for different lags 
# (up to maximum of 12).
Acf(res.ar1.quad$residuals, lag.max = 12, 
    main = "Autocorrelation for Vitacost Training Residuals of Residuals")

# Create two-level model's forecast with linear trend and seasonality 
# regression + AR(4) for residuals for validation period.

# Create data table with validation data, regression forecast
# for validation period, AR(4) residuals for validation, and 
# two level model results. 
valid2.two.level.pred <- train.quad.trend.season.pred$mean + res.ar1.quad.pred$mean

valid2.df <- round(data.frame(valid.ts, train.quad.trend.season.pred$mean, 
                             res.ar1.quad.pred$mean, valid2.two.level.pred),3)
names(valid2.df) <- c("Sales", "Reg.Forecast", 
                     "AR(4)Forecast", "Combined.Forecast")
valid2.df

# plot ts data, linear trend and seasonality data, and predictions 
# for validation period.

plot(valid2.two.level.pred, 
     xlab = "Time", ylab = "Sales (in million $)", ylim = c(25000, 90000), 
     bty = "l", xlim = c(2010, 2023.25), xaxt = "n",
     main = "Two-Level Forecast: Regression with Quadratic Trend
             and Seasonality + AR(4) for Residuals", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)) )
lines(train.quad.trend.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
legend(2010,80000, legend = c("Sales Time Series", "Regression for Training Data",
                              "Two Level Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019, 2019), c(0, 90000))
lines(c(2023, 2023), c(0, 90000))
text(2014, 90000, "Training")
text(2020.5, 90000, "Validation")
text(2023.5, 90000, "Future")
arrows(2010, 84000, 2018.9, 84000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 84000, 2022.9, 84000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023.1, 84000, 2024, 84000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


# Use accuracy() function to identify common accuracy measures for validation period forecast:
# (1) two-level model (linear trend and seasonal model + AR(4) model for residuals),
# (2) linear trend and seasonality model only.
round(accuracy(valid2.two.level.pred, valid.ts), 3)
round(accuracy(train.quad.trend.season.pred$mean, valid.ts), 3)



############# Model 6------------------- Auto ARIMA MODEL

# Utilize auto.arima() function to automatically identify 
# the ARIMA model structure and parameters. 
# Develop the ARIMA forecast for the validation period. 
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)

train.auto.arima.pred <- forecast(train.auto.arima, 
                                  h = nValid, level = 0)
train.auto.arima.pred

#plot
plot(train.auto.arima.pred, 
     xlab = "Time", ylab = "Sales (in million $)", ylim = c(25000, 90000), 
     bty = "l", xlim = c(2010, 2023.25), xaxt = "n",
     main = "Auto ARIMA Model", 
     lwd = 2, flty = 5)
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)) )
lines(train.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(2010,80000, legend = c("Sales Time Series", "Auto ARIMA Forecast for Training Data",
                              "Auto ARIMA Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")



# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019, 2019), c(0, 90000))
lines(c(2023, 2023), c(0, 90000))
text(2014, 90000, "Training")
text(2020.5, 90000, "Validation")
text(2023.5, 90000, "Future")
arrows(2010, 84000, 2018.9, 84000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019.1, 84000, 2022.9, 84000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023.1, 84000, 2024, 84000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use accuracy() function to identify common accuracy measures
# for the developed forecasts in the validation period.
round(accuracy(hw.ZZZ.pred$mean, valid.ts), 3)
round(accuracy(train.lin.trend.season.pred$mean, valid.ts),3)
round(accuracy(valid.two.level.pred, valid.ts), 3)
round(accuracy(train.quad.trend.season.pred$mean, valid.ts),3)
round(accuracy(valid2.two.level.pred, valid.ts), 3)
round(accuracy(train.auto.arima.pred$mean, valid.ts), 3)


###########################################################################################

#### Fit the models with entire data set


############### Model 1 ------------- Automated HW model (Entire data set)

# Use ets() function with model = "ZZZ", to identify the best hw option
# and optimal alpha, beta, & gamma to fit HW for the entire data period.
HW.ZZZ <- ets(sales.ts, model = "ZZZ")
HW.ZZZ 

# Use forecast() function to make predictions using this HW model for future
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 12 , level = 0)
HW.ZZZ.pred

# Plot HW predictions for original data, automatic selection of the 
# model and optimal smoothing parameters.
plot(HW.ZZZ.pred$mean, 
     xlab = "Time", ylab = "Sales (in million $)", ylim = c(25000, 90000), 
     bty = "l", xlim = c(2010, 2023.25), xaxt = "n",
     main = "Holt-Winter's Automated Model for Entire Data Set and Forecast for Future 12 Periods", 
     lty = 5, col = "blue", lwd = 2) 
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)))
lines(HW.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(sales.ts)
legend(2010,80000, 
       legend = c("Sales", 
                  "Holt-Winter'sModel for Entire Data Set",
                  "Holt-Winter's Model Forecast, Future 12 Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2023, 2023), c(0, 90000))
#lines(c(2023, 2023), c(0, 90000))
text(2016, 90000, "Data set")
#text(2020.5, 90000, "Data set")
text(2023.5, 90000, "Future")
arrows(2010, 84000, 2022.9, 84000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
# arrows(2019.1, 84000, 2022.9, 84000, code = 3, length = 0.1,
#        lwd = 1, angle = 30)
arrows(2023.1, 84000, 2024, 84000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use Acf() function to create autocorrelation chart of 
# residuals of HW model's with automated options and parameters. 
Acf(HW.ZZZ.pred$residuals, lag.max = 12, 
    main = "Autocorrelations of Residuals of Automated Holt-Winter's Model")


############## Model 2 ------------ LINEAR TREND AND SEASONALITY MODEL( Entire data set)
# Use tslm() function to create linear trend and seasonality model.
lin.trend.season <- tslm(sales.ts ~ trend + season)
# See summary of linear trend equation and associated parameters.
summary(lin.trend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in the future 12 months.  
lin.trend.season.pred <- forecast(lin.trend.season, h = 12, level = 0)
lin.trend.season.pred

# Plot ts data, linear trend and forecast for validation period.
plot(lin.trend.season.pred$mean, 
     xlab = "Time", ylab = "Sales (in million $)", ylim = c(25000, 90000), 
     bty = "l", xlim = c(2010, 2023.25), xaxt = "n",
     main = "Regression Model with Linear Trend and seasonality for Future Periods", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)) )
lines(lin.trend.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
legend(2010,80000, legend = c("Sales Time Series", "Linear Trend and Seasonality Model for Entire Data",
                              "Linear and Seasonality Forecast for Future 12 Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2023, 2023), c(0, 90000))
#lines(c(2023, 2023), c(0, 90000))
text(2016, 90000, "Data set")
#text(2020.5, 90000, "Data set")
text(2023.5, 90000, "Future")
arrows(2010, 84000, 2022.9, 84000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
# arrows(2019.1, 84000, 2022.9, 84000, code = 3, length = 0.1,
#        lwd = 1, angle = 30)
arrows(2023.1, 84000, 2024, 84000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use Acf() function to create autocorrelation chart of 
# residuals of linear trend and seasonality model with automated options and parameters. 
Acf(lin.trend.season.pred$residuals, lag.max = 12, 
    main = "Autocorrelations of Residuals of Linear Trend and Seasonality Model with Entire Data Set")

############### Model 3 ------------------- Regression with linear Trend and Seasonality with with AR(4) Model
## FIT REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY 
## FOR ENTIRE DATASET. FORECAST AND PLOT DATA, AND MEASURE ACCURACY.


# Use Acf() function to identify autocorrelation for the model residuals 
# for entire data set, and plot autocorrelation for different 
# lags (up to maximum of 12).
Acf(lin.trend.season.pred$residuals, lag.max = 12, 
    main = "Autocorrelation of Regression Residuals for Entire Data Set")


# Use Arima() function to fit AR(1) model for regression residuals.
# The ARIMA model order of order = c(1,0,0) gives an AR(1) model.
# Use forecast() function to make prediction of residuals into the future 12 months.
residual.lin.ar1 <- Arima(lin.trend.season$residuals, order = c(4,0,0))
residual.lin.ar1.pred <- forecast(residual.lin.ar1, h = 12, level = 0)

# Use summary() to identify parameters of AR(1) model.
summary(residual.lin.ar1)

# Use Acf() function to identify autocorrelation for the residuals of residuals 
# and plot autocorrelation for different lags (up to maximum of 12).
Acf(residual.lin.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for Residuals of Residuals for Entire Data Set")


# Identify forecast for the future 12 periods as sum of linear trend and 
# seasonal model and AR(1) model for residuals.
lin.season.ar1.pred <- lin.trend.season.pred$mean + residual.lin.ar1.pred$mean
lin.season.ar1.pred


# Create a data table with linear trend and seasonal forecast 
# for 12 future periods,
# AR(4) model for residuals for 12 future periods, and combined 
# two-level forecast for 12 future periods. 
table1.df <- round(data.frame(lin.trend.season.pred$mean, 
                              residual.lin.ar1.pred$mean, lin.season.ar1.pred),3)
names(table1.df) <- c("Reg.Forecast", "AR(4)Forecast","Combined.Forecast")
table1.df


# Plot historical data, predictions for historical data, and forecast 
# for 12 future periods.
plot(sales.ts, 
     xlab = "Time", ylab = "Sales (in million $)", ylim = c(25000, 90000), 
     bty = "l", xlim = c(2010, 2023.25), xaxt = "n",
     main = "Two-Level Forecast: Regression with Linear Trend and Seasonlity + AR(4)
     for Residuals", lwd = 2, col = "black")
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)) )
lines(lin.trend.season$fitted + residual.lin.ar1$fitted, col = "blue", lwd = 2)
lines(lin.season.ar1.pred, col = "blue", lty = 5, lwd = 2)

legend(2010,80000, legend = c("Sales Time Series", "Two-Level Forecast for Training and Validation Periods",
                              "Two-Level Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2023, 2023), c(0, 90000))
#lines(c(2023, 2023), c(0, 90000))
text(2016, 90000, "Data set")
#text(2020.5, 90000, "Data set")
text(2023.5, 90000, "Future")
arrows(2010, 84000, 2022.9, 84000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
# arrows(2019.1, 84000, 2022.9, 84000, code = 3, length = 0.1,
#        lwd = 1, angle = 30)
arrows(2023.1, 84000, 2024, 84000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


# Use accuracy() function to identify common accuracy measures for:
# (1) two-level model (linear trend and seasonality model 
#     + AR(4) model for residuals),
# (2) linear trend and seasonality model only, and
round(accuracy(lin.trend.season.pred$fitted + residual.lin.ar1$fitted, sales.ts), 3)
round(accuracy(lin.trend.season.pred$fitted, sales.ts), 3)


################# Model 4 --------------------- QUADRATIC TREND AND SEASONALITY MODEL (Entire data set)

# Use tslm() function to create quadratic trend and seasonality model.
quad.trend.season <- tslm(sales.ts ~ trend + I(trend^2)+ season)

# See summary of linear trend equation and associated parameters.
summary(quad.trend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in the future 12 months.  
quad.trend.season.pred <- forecast(quad.trend.season, h = 12, level = 0)
quad.trend.season.pred

# Plot ts data, linear trend and forecast for validation period.
plot(quad.trend.season.pred$mean, 
     xlab = "Time", ylab = "Sales (in million $)", ylim = c(25000, 90000), 
     bty = "l", xlim = c(2010, 2023.25), xaxt = "n",
     main = "Regression Model with Quadratic Trend and seasonality for Future Periods", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)) )
lines(quad.trend.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
legend(2010,80000, legend = c("Sales Time Series", "Quadratic Trend and Seasonality Model for Entire Data",
                              "Quadratic trend and Seasonality Forecast for Future 12 Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2023, 2023), c(0, 90000))
#lines(c(2023, 2023), c(0, 90000))
text(2016, 90000, "Data set")
#text(2020.5, 90000, "Data set")
text(2023.5, 90000, "Future")
arrows(2010, 84000, 2022.9, 84000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
# arrows(2019.1, 84000, 2022.9, 84000, code = 3, length = 0.1,
#        lwd = 1, angle = 30)
arrows(2023.1, 84000, 2024, 84000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use Acf() function to create autocorrelation chart of 
# residuals of quadratic trend and seasonality model with automated options and parameters. 
Acf(quad.trend.season.pred$residuals, lag.max = 12, 
    main = "Autocorrelations of Residuals of quadratic trend and seasonality model for entire dataset")


#######################Model 5- Regression with Quadratic Trend and Seasonality with with AR(4) Model


# Use Acf() function to identify autocorrelation for the model residuals 
# for entire data set, and plot autocorrelation for different 
# lags (up to maximum of 12).
Acf(quad.trend.season.pred$residuals, lag.max = 12, 
    main = "Autocorrelation of Regression Residuals for Entire Data Set")


# Use Arima() function to fit AR(1) model for regression residuals.
# The ARIMA model order of order = c(1,0,0) gives an AR(1) model.
# Use forecast() function to make prediction of residuals into the future 12 months.
residual.quad.ar1 <- Arima(quad.trend.season$residuals, order = c(4,0,0))
residual.quad.ar1.pred <- forecast(residual.quad.ar1, h = 12, level = 0)

# Use summary() to identify parameters of AR(1) model.
summary(residual.quad.ar1)

# Use Acf() function to identify autocorrelation for the residuals of residuals 
# and plot autocorrelation for different lags (up to maximum of 12).
Acf(residual.quad.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for Residuals of Residuals for Entire Data Set")


# Identify forecast for the future 12 periods as sum of quadratic trend and 
# seasonal model and AR(1) model for residuals.
quad.season.ar1.pred <- quad.trend.season$mean + residual.quad.ar1.pred$mean
quad.season.ar1.pred


# Create a data table with quadratic trend and seasonal forecast 
# for 12 future periods,
# AR(4) model for residuals for 12 future periods, and combined 
# two-level forecast for 12 future periods. 
table2.df <- round(data.frame(quad.trend.season.pred$mean, 
                              residual.quad.ar1.pred$mean, quad.season.ar1.pred),3)
names(table2.df) <- c("Reg.Forecast", "AR(4)Forecast","Combined.Forecast")
table2.df


# Plot historical data, predictions for historical data, and forecast 
# for 12 future periods.
plot(sales.ts, 
     xlab = "Time", ylab = "Sales (in million $)", ylim = c(25000, 90000), 
     bty = "l", xlim = c(2010, 2023.25), xaxt = "n",
     main = "Two-Level Forecast: Regression with Quadratic Trend and Seasonality + AR(4)
     for Residuals", lwd = 2, col = "black")
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)) )
lines(quad.trend.season$fitted + residual.quad.ar1$fitted, col = "blue", lwd = 2)
lines(quad.season.ar1.pred, col = "blue", lty = 5, lwd = 2)

legend(2010,80000, legend = c("Sales Time Series", "Two-Level Forecast for Training and Validation Periods",
                              "Two-Level Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2023, 2023), c(0, 90000))
#lines(c(2023, 2023), c(0, 90000))
text(2016, 90000, "Data set")
#text(2020.5, 90000, "Data set")
text(2023.5, 90000, "Future")
arrows(2010, 84000, 2022.9, 84000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
# arrows(2019.1, 84000, 2022.9, 84000, code = 3, length = 0.1,
#        lwd = 1, angle = 30)
arrows(2023.1, 84000, 2024, 84000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


# Use accuracy() function to identify common accuracy measures for:
# (1) two-level model (quadratic trend and seasonality model 
#     + AR(4) model for residuals),
# (2) quadratic trend and seasonality model only, and
round(accuracy(quad.trend.season.pred$fitted + residual.quad.ar1$fitted, sales.ts), 3)
round(accuracy(quad.trend.season.pred$fitted, sales.ts), 3)


################# Model 6 --------------------- AURO ARIMA MODEL (Entire data set)
# Use auto.arima() function for the entire data set..
# Use summary() to show ARIMA model and its parameters.
# Apply forecast for the 8 periods in the future. 
auto.arima <- auto.arima(sales.ts)
summary(auto.arima)
auto.arima.pred <- forecast(auto.arima, h = 12, level = 0)
auto.arima.pred

#plot
plot(sales.ts, 
     xlab = "Time", ylab = "Sales (in million $)", ylim = c(25000, 90000), 
     bty = "l", xlim = c(2010, 2023.25), xaxt = "n",
     main = "Auto ARIMA Model for Entire Dataset")
axis(1, at = seq(2010, 2024, 1), labels = format(seq(2010, 2024, 1)) )
lines(auto.arima$fitted, col = "blue", lwd = 2)
lines(auto.arima.pred$mean, col = "blue", lty = 5, lwd = 2)
legend(2010,80000, legend = c("Sales Time Series", "Auto ARIMA Forecast",
                              "Auto ARIMA Forecast for 12 Future Period"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2023, 2023), c(0, 90000))
#lines(c(2023, 2023), c(0, 90000))
text(2016, 90000, "Data set")
#text(2020.5, 90000, "Data set")
text(2023.5, 90000, "Future")
arrows(2010, 84000, 2022.9, 84000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
# arrows(2019.1, 84000, 2022.9, 84000, code = 3, length = 0.1,
#        lwd = 1, angle = 30)
arrows(2023.1, 84000, 2024, 84000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use Acf() function to create autocorrelation chart of 
# residuals of Auto ARIMA model with automated options and parameters. 
Acf(auto.arima.pred$residuals, lag.max = 12, 
    main = "Autocorrelations of Residuals of Auto ARIMA Model with Entire dataset")

########################################################################################################################

# Compare accuracy measures of regression forecast with 
# HW model
# linear trend and seasonality 
# linear trend 
# quadratic trend and seasonality
# Auto ARIMA model
# seasonal forecast
# naive forecast for entire data set

round(accuracy(HW.ZZZ.pred$fitted, sales.ts), 3)
round(accuracy(lin.trend.season.pred$fitted, sales.ts),3)
round(accuracy(lin.trend.season.pred$fitted + residual.lin.ar1$fitted, sales.ts), 3)
round(accuracy(quad.trend.season.pred$fitted, sales.ts),3)
round(accuracy(quad.trend.season.pred$fitted + residual.quad.ar1$fitted, sales.ts), 3)
round(accuracy(auto.arima.pred$fitted, sales.ts), 3)
round(accuracy(snaive(sales.ts)$fitted, sales.ts), 3)
round(accuracy((naive(sales.ts))$fitted,sales.ts), 3)




