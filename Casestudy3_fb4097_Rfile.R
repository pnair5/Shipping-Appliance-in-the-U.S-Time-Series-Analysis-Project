## USE FORECAST LIBRARY.

library(forecast)

## CREATE DATA FRAME. 

# Set working directory for locating files.
setwd("/Users/pallavinair/Desktop/CSUEB BA/BAN 673/Casestudy 2")

# Create data frame.
shipment.data <- read.csv("673_case2.csv")

# See the first and last 6 records of the file for data.
head(shipment.data)
tail(shipment.data)

# Function ts() takes three arguments: start, end, and freq.
# With quarterly data, frequency in a season (year) is equal to 4.
# Arguments start and end are pairs: (season number, period number).
# Now, create the time series object
shipments.ts <- ts(shipment.data$Shipments, start = c(2006, 1), frequency = 4)

#Plot the time series data
plot(shipments.ts, xlab = "Year", ylab = "Shipments (Thousands of Units)", 
     main = "Quarterly Shipments of Appliances in the U.S. (2006-2023)", 
     type = "o", col = "blue")

#Develop data partition with the validation partition of 20 periods and the rest for the 
#training partition.
# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
nValid <- 20 
nTrain <- length(shipments.ts) - nValid
# Define the training and validation sets using window()
train.ts <- window(shipments.ts, end = c(2018, 4)) # Adjusted to ensure correct partitioning
valid.ts <- window(shipments.ts, start = c(2019, 1)) # Starting from 2019 Q1 for validation

#1 (a)
# Fit AR(1) model to the training data
shipments_ar1_model <- Arima(shipments.ts, order = c(1,0,0))

# Print the model summary to get the intercept and coefficient
summary(shipments_ar1_model)


# Apply z-test to test the null hypothesis that beta 
# coefficient of AR(1) is equal to 1.
ar1 <- 0.7062
s.e. <- 0.0825
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

# Create first difference of ClosePrice data using diff() function.
diff.shipments <- diff(shipments.ts, lag = 1)
diff.shipments

#1 (b)
# Use Acf() function to identify autocorrealtion for first differenced
# and plot autocorrelation for different lags 
# (up to maximum of 8).
Acf(diff.shipments, lag.max = 8, 
    main = "Autocorrelation for Diff Shipments")

#2(a)
## FIT REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY 
## FORECAST AND MEASURE ACCURACY.

# Use tslm() function to create linear trend and seasonal model.
train.lin.season <- tslm(train.ts ~ trend + season)

# See summary of linear trend and seasonality model and associated parameters.
summary(train.lin.season)

# Apply forecast() function to make predictions for ts with 
# linear trend and seasonality data in validation set.  
train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)

#2 (b)
# Use Acf() function to identify autocorrelation for the model residuals 
# (training sets), and plot autocorrelation for different 
# lags (up to maximum of 8).
train.lin.season.pred$residuals
Acf(train.lin.season.pred$residuals, lag.max = 8, 
    main = "Autocorrelation for Shipments Training Residuals")

## USE Arima() FUNCTION TO CREATE AR(1) MODEL FOR TRAINING RESIDUALS.
## CREATE TWO-LEVEL MODEL WITH LINEAR TREND AND SEASONALITY MODEL 
## AND AR(1) RESIDUALS.
## PLOT DATA AND IDENTIFY ACCURACY MEASURES.

#2 (c)
# Use Arima() function to fit AR(1) model for training residuals. The Arima model of 
# order = c(1,0,0) gives an AR(1) model.
# Use summary() to identify parameters of AR(1) model. 
shipments_res_ar1 <- Arima(train.lin.season$residuals, order = c(1,0,0))
summary(shipments_res_ar1)
shipments_res_ar1$fitted

# Use Acf() function to identify autocorrelation for the training 
# residual of residuals and plot autocorrelation for different lags 
# (up to maximum of 8).
Acf(shipments_res_ar1$residuals, lag.max = 8, 
    main = "Autocorrelation for Shipments Training Residuals of Residuals")


#2 (d)

## USE Arima() FUNCTION TO CREATE AR(1) MODEL FOR TRAINING RESIDUALS.
## CREATE TWO-LEVEL MODEL WITH LINEAR TREND AND SEASONALITY MODEL 
## AND AR(1) RESIDUALS.
## IDENTIFY ACCURACY MEASURES.

# Use Arima() function to fit AR(1) model for training residuals. The Arima model of 
# order = c(1,0,0) gives an AR(1) model.
# Use summary() to identify parameters of AR(1) model. 
shipments_res_ar1 <- Arima(train.lin.season$residuals, order = c(1,0,0))
summary(shipments_res_ar1)
shipments_res_ar1$fitted

# Use forecast() function to make prediction of residuals in validation set.
shipments_res_ar1_pred <- forecast(shipments_res_ar1, h = nValid, level = 0)
shipments_res_ar1_pred

# Create two-level model's forecast with linear trend and seasonality 
# regression + AR(1) for residuals for validation period.

# Create data table with validation data, regression forecast
# for validation period, AR(1) residuals for validation, and 
# two level model results. 
valid.two.level.pred <- train.lin.season.pred$mean + shipments_res_ar1_pred$mean

valid.df <- round(data.frame(valid.ts, train.lin.season.pred$mean, 
                             shipments_res_ar1_pred$mean, valid.two.level.pred),3)
names(valid.df) <- c("Shipments", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid.df

# Use accuracy() function to identify common accuracy measures for validation period forecast:
# (1) two-level model (linear trend and seasonal model + AR(1) model for residuals),
# (2) linear trend and seasonality model only.
round(accuracy(valid.two.level.pred, valid.ts), 3)
round(accuracy(train.lin.season.pred$mean, valid.ts), 3)

#2 (e)
## FIT REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY 
## FOR ENTIRE DATASET. FORECAST AND PLOT DATA, AND MEASURE ACCURACY.
# Use tslm() function to create linear trend and seasonality model.
lin.season <- tslm(shipments.ts ~ trend + season)

# See summary of linear trend equation and associated parameters.
summary(lin.season)

# Apply forecast() function to make predictions with linear trend and seasonal 
# model into the future 12 months.  
lin.season.pred <- forecast(lin.season, h = 8, level = 0)
lin.season.pred

# Use Acf() function to identify autocorrelation for the model residuals 
# for entire data set, and plot autocorrelation for different 
# lags (up to maximum of 12).
Acf(lin.season.pred$residuals, lag.max = 8, 
    main = "Autocorrelation of Regression Residuals for Entire Data Set")

# Use Arima() function to fit AR(1) model for regression residuals.
# The ARIMA model order of order = c(1,0,0) gives an AR(1) model.
# Use forecast() function to make prediction of residuals into the future 8 Quarters.
residual.ar1 <- Arima(lin.season$residuals, order = c(1,0,0))
residual.ar1.pred <- forecast(residual.ar1, h = 8, level = 0)

# Identify forecast for the future 8 periods as sum of linear trend and 
# seasonal model and AR(1) model for residuals.
lin.season.ar1.pred <- lin.season.pred$mean + residual.ar1.pred$mean
lin.season.ar1.pred

# Create a data table with linear trend and seasonal forecast 
# for 8 future periods,
# AR(1) model for residuals for 8 future periods, and combined 
# two-level forecast for 8 future periods. 
table.df <- round(data.frame(lin.season.pred$mean, 
                             residual.ar1.pred$mean, lin.season.ar1.pred),3)
names(table.df) <- c("Reg.Forecast", "AR(1)Forecast","Combined.Forecast")
table.df

#3 (a)
## FIT ARIMA(1,1,1)(1,1,1)MODEL.

# Use Arima() function to fit ARIMA(1,1,1)(1,1,1)model for 
# trend and seasonality.
# Use summary() to show ARIMA model and its parameters.
train.arima.seas <- Arima(train.ts, order = c(1,1,1), 
                          seasonal = c(1,1,1)) 
summary(train.arima.seas)

# Apply forecast() function to make predictions for ts with 
# ARIMA model in validation set.    
train.arima.seas.pred <- forecast(train.arima.seas, h = nValid, level = 0)
train.arima.seas.pred

#3 (b)
## FIT AUTO ARIMA MODEL.
# Use auto.arima() function to fit ARIMA model.
# Use summary() to show auto ARIMA model and its parameters.
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model in validation set.  
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred

#3 (c)
round(accuracy(train.arima.seas.pred$mean, valid.ts), 3)
round(accuracy(train.auto.arima.pred$mean, valid.ts), 3)

#3 (d)
## FIT SEASONAL ARIMA AND AUTO ARIMA MODELS FOR ENTIRE DATA SET. 
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use arima() function to fit seasonal ARIMA(1,1,1)(1,1,1) model 
# for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
arima.seas <- Arima(shipments.ts, order = c(1,1,1), 
                    seasonal = c(1,1,1)) 
summary(arima.seas)

# Apply forecast() function to make predictions for ts with 
# seasonal ARIMA model for the future 8 periods. 
arima.seas.pred <- forecast(arima.seas, h = 8, level = 0)
arima.seas.pred

# Use auto.arima() function to fit ARIMA model for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
auto.arima <- auto.arima(shipments.ts)
summary(auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model for the future 8 periods. 
auto.arima.pred <- forecast(auto.arima, h = 8, level = 0)
auto.arima.pred

#3 (e) # MEASURE FORECAST ACCURACY FOR ENTIRE DATA SET.

# Use accuracy() function to identify common accuracy measures for:
#(1) regression model with linear trend and seasonality; 
#(2) two-level model (with AR(1) model for residuals); 
#(3) ARIMA(1,1,1)(1,1,1) model; 
#(4) auto ARIMA model; 
#(5) seasonal naïve forecast for the entire data set
round(accuracy(lin.season.pred$fitted, shipments.ts), 3)
round(accuracy(lin.season$fitted + residual.ar1.pred$fitted, shipments.ts), 3)
round(accuracy(arima.seas.pred$fitted, shipments.ts), 3)
round(accuracy(auto.arima.pred$fitted, shipments.ts), 3)
round(accuracy((snaive(shipments.ts))$fitted, shipments.ts), 3)
round(accuracy((naive(shipments.ts))$fitted, shipments.ts), 3)
