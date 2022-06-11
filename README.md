# Multivariate-time-series-analysis
require(tidyverse)
require(data.table)
require(ggfortify) #for plotting timeseries
require(forecast) #for forecast function
require(tseries)
require(chron)
require(lubridate)
require(zoo)
require(lmtest)
require(TTR) #for smoothing the time series
require(MTS)
require(vars)
require(fUnitRoots)
require(lattice)
require(grid)

### Multivariate Time Series Datasets

mymts2 = ts(adeola,
           frequency = 1,
           start = c(1985, 1), end = c(2018,1))
autoplot(mymts2) +
  ggtitle("Time Series Plot") +
  theme(plot.title = element_text(hjust = 0.5))



## Testing for stationarity

apply(mymts2, 2, adf.test)


# Differencing the whole series
stnry2 = diffM(mymts2) #difference operation on a vector of time series. Default order of differencing is 1.

# Retest the stationary
apply(stnry2, 2, adf.test)

# Plotting the stationary
autoplot(ts(stnry2,
            start = c(1985),
            frequency = 1)) +
  ggtitle("Time Series Plot of the stationary Time-Series")


# Lag order identification
#We will use two different functions, from two different packages to identify the lag order for the VAR model. Both functions are quite similar to each other but differ in the output they produce. vars::VAR is a more powerful and convinient function to identify the correct lag order.
VARselect(stnry2,
          type = "none", #type of deterministic regressors to include. We use none becasue the time series was made stationary using differencing above.
          lag.max = 2) #highest lag order


# Creating a VAR model with vars
var.a <- vars::VAR(stnry2,
                   lag.max = 2, #highest lag order for lag length selection according to the choosen ic
                   ic = "AIC", #information criterion
                   type = "none") #type of deterministic regressors to include
summary(var.a)



# Residual diagnostics
#serial.test function takes the VAR model as the input.
serial.test(var.a)

#selecting the variables
# Granger test for causality
#for causality function to give reliable results we need all the variables of the multivariate time series to be stationary.
causality(var.a, #VAR model
          cause = c("Total_RealGDP"))

## Forecasting VAR models
fcast = predict(var.a, n.ahead = 25) # we forecast over a short horizon because beyond short horizon prediction becomes unreliable or uniform
plot(fcast)

# Forecasting the DAX index
fcast$fcst[1];  # type list

fcast$fcst[2];

fcast$fcst[3];

fcast$fcst[4];

fcast$fcst[5];

fcast$fcst[6];
