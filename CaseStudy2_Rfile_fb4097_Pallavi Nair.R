## USE FORECAST LIBRARY.

library(forecast)

## CREATE DATA FRAME. 

# Set working directory for locating files.
setwd("/Users/pallavinair/Desktop/CSUEB BA/BAN 673/Casestudy 2")

# Create data frame.
shipment.data <- read.csv("673_case2.csv")

# See the first 6 records of the file.
head(shipment.data)

#1 (a)Time series data set
## USE ts() FUNCTION TO CREATE TIME SERIES DATA SET

# Function ts() takes three arguments: start, end, and freq.
# With quarterly data, frequency in a season (year) is equal to 4.
# Arguments start and end are pairs: (season number, period number).
# Now, create the time series object
shipments.ts <- ts(shipment.data$Shipments, start = c(2006, 1), frequency = 4)
# 1 (b) Plot the time series data
plot(shipments.ts, xlab = "Year", ylab = "Shipments (Thousands of Units)", 
     main = "Quarterly Shipments of Appliances in the U.S. (2006-2023)", 
     type = "o", col = "blue")

# 2(a) Develop data partition with the validation partition of 20 periods and the rest for the 
#training partition.
# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
nValid <- 20 
nTrain <- length(shipments.ts) - nValid
# Define the training and validation sets using window()
train.ts <- window(shipments.ts, end = c(2018, 4)) # Adjusted to ensure correct partitioning
valid.ts <- window(shipments.ts, start = c(2019, 1)) # Starting from 2019 Q1 for validation

#2 (b)
## FIT REGRESSION MODEL WITH LINEAR TREND: MODEL 1. 
## FORECAST AND MEASURE ACURACY.
# Use tslm() function (time series linear model) to create regression model with 
# linear trend.
train.lin <- tslm(train.ts ~ trend)

# See summary of linear trend model and associated parameters.
summary(train.lin)

# Apply forecast() function to make forecast for validation period.
train.lin.pred <- forecast(train.lin, h = nValid, level = 0)

# Plot the original time series with forecast overlaid
plot(train.lin.pred, main = "Forecast vs Actual Shipments", xlab = "Year", ylab = "Shipments")
lines(train.ts, col = "blue") # Plotting the training data
lines(valid.ts, col = "red") # Optionally, add the validation data to compare visually

# Use accuracy() function to identify common accuracy measures with rounded
# values to 3 decimals.
round(accuracy(train.lin.pred$mean, valid.ts), 3)


## FIT REGRESSION MODEL WITH QUADRATIC (POLYNOMIAL) TREND: MODEL 2. 
## FORECAST AND MEASURE ACCURACY.

# Use tslm() function to create quadratic (polynomial) trend model.
train.quad <- tslm(train.ts ~ trend + I(trend^2))

# See summary of quadratic trend model and associated parameters.
summary(train.quad)

# Apply forecast() function to make predictions for ts data in
# validation set.  
train.quad.pred <- forecast(train.quad, h = nValid, level = 0)


# Use accuracy() function to identify common accuracy measures
# for regression models with linear trend and quadratic (polynomial) trend.
round(accuracy(train.lin.pred$mean, valid.ts), 3)
round(accuracy(train.quad.pred$mean, valid.ts), 3)


## FIT REGRESSION MODEL WITH SEASONALITY: MODEL 3. 
## FORECAST AND MEASURE ACCURACY.

# Use tslm() function to create seasonal model.
train.season <- tslm(train.ts ~ season)

# See summary of seasonal model and associated parameters.
summary(train.season)

# If necessary, run the following code to identify seasons.
train.season$data 

# Apply forecast() function to make predictions for ts with 
# seasonality data in validation set.  
train.season.pred <- forecast(train.season, h = nValid, level = 0)

# Use accuracy() function to identify common accuracy measures
# for regression models with (1) linear trend, (2) quadratic (polynomial) trend,
# and (3) seasonality.
round(accuracy(train.lin.pred$mean, valid.ts), 3)
round(accuracy(train.quad.pred$mean, valid.ts), 3)
round(accuracy(train.season.pred$mean, valid.ts), 3)


## FIT REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY: MODEL 4. 
## FORECAST AND MEASURE ACCURACY.

# Use tslm() function to create linear trend and seasonal model.
train.lin.season <- tslm(train.ts ~ trend + season)

# See summary of linear trend and seasonality model and associated parameters.
summary(train.lin.season)

# Apply forecast() function to make predictions for ts with 
# linear trend and seasonality data in validation set.  
train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)

# Use accuracy() function to identify common accuracy measures
# for various regression models: (1)linear trend, (2) quadratic  
# (polynomial) trend, (3) seasonality, and (4) linear trend and seasonality.
round(accuracy(train.lin.pred$mean, valid.ts),3)
round(accuracy(train.quad.pred$mean, valid.ts),3)
round(accuracy(train.season.pred$mean, valid.ts),3)
round(accuracy(train.lin.season.pred$mean, valid.ts),3)

## FIT REGRESSION MODEL WITH QUADRATIC TREND AND SEASONALITY: MODEL 5. 
## FORECAST AND MEASURE ACCURACY.

# Use tslm() function to create quadratic trend and seasonal model.
train.quad.season <- tslm(train.ts ~ trend + I(trend^2) + season)

# See summary of quadratic trend and seasonality model and associated parameters.
summary(train.quad.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.quad.season.pred <- forecast(train.quad.season, h = nValid, level = 0)

#2 (c)
# Use accuracy() function to identify common accuracy measures
# for various regression models: (1) linear trend, (2) quadratic (polynomial) trend, (3) seasonality, 
# (4) linear trend and seasonality, and (5) quadratic trend 
# and seasonality. 
round(accuracy(train.lin.pred$mean, valid.ts),3)
round(accuracy(train.quad.pred$mean, valid.ts),3)
round(accuracy(train.season.pred$mean, valid.ts),3)
round(accuracy(train.lin.season.pred$mean, valid.ts),3)
round(accuracy(train.quad.season.pred$mean, valid.ts),3)


#3(a)
## FIT REGRESSION MODELS WITH LINEAR TREND, WITH LINEAR TREND 
## AND SEASONALITY, WITH QUADRATIC TREND AND SEASONALITY FOR ENTIRE DATASET. 
#FORECAST AND MEASURE ACCURACY.

# Use tslm() function to create linear trend model.
lin.trend <- tslm(shipments.ts ~ trend)

# See summary of linear trend equation and associated parameters.
summary(lin.trend)

# Apply forecast() function to make predictions for ts with 
# linear trend  data in 8 future quarters
lin.trend.pred <- forecast(lin.trend, h = 8, level = 0)
lin.trend.pred 
# Use tslm() function to create regression model with linear trend 
# and seasonality.
lin.season <- tslm(shipments.ts ~ trend + season)

# See summary of linear trend and seasonality equation and associated parameters.
summary(lin.season)

# Apply forecast() function to make predictions for ts with 
# linear trend and seasonality data in 8 future quarters
lin.season.pred <- forecast(lin.season, h = 8, level = 0)
lin.season.pred 
# Use tslm() function to create regression model with quadratic trend 
# and seasonality.
quad.season <- tslm(shipments.ts ~ trend + I(trend^2) + season)

# See summary of quadratic trend and seasonality equation and associated parameters.
summary(quad.season)

# Apply forecast() function to make predictions for ts with 
# quadratic trend and seasonality data in 8 future quarters
quad.season.pred <- forecast(quad.season, h = 8, level = 0)
quad.season.pred

## COMPARE ACCURACY MEASURES OF REGRESSION FORECAST WITH REGRESSION MODELS 
# WITH LINEAR TREND, WITH LINEAR TREND AND SEASONALITY,
# WITH QUADRATIC TREND AND SEASONALITY 
## ACCURACY MEASURES OF NAIVE FORECAST AND SEASONAL NAIVE FORECAST 
## FOR ENTIRE DATA SET.

# Use accuracy() function to identify common accuracy measures
# for naive model, seasonal naive, and regression model with quadratic trend and seasonality.
round(accuracy(lin.trend.pred$fitted, shipments.ts),3)
round(accuracy(lin.season.pred$fitted, shipments.ts),3)
round(accuracy(quad.season.pred$fitted, shipments.ts),3)
round(accuracy((naive(shipments.ts))$fitted, shipments.ts), 3)
round(accuracy((snaive(shipments.ts))$fitted, shipments.ts), 3)




