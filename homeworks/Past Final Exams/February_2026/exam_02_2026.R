# Set working directory

# Set working directory, every path will now be relative to this one
# Change this to point to the directory where the csv file is located
setwd("homeworks/Past Final Exams/February_2026/")

# Load packages
library(pacman)
p_load(tidyverse, fpp3, forecast, tseries)

# if you don't have pacman installed then you can just load the libraries like
# this

# library(tidyverse)
# library(fpp3)
# library(forecast)
# library(tseries)

# Load data in memory
unemp <- read.csv("unemp.csv", sep=";") # sep could be ","
head(unemp) # inspect first lines of dataset

# Run this to understand the types of the columns
# from it we understand that date is not encoded as a proper
# date variable
summary(unemp)

unemp <- unemp %>%
  mutate(date=as.Date(date))

# Running the summary again we see that date is properly treated as a date
summary(unemp)

dates <- seq(yearmonth("1948 Jan"), yearmonth("1981 Dec"), by = 1)

unemp_ts <- as_tsibble(
  tibble(Month = dates, unemp = unemp$unemp),
  index = Month)


# When looking at a lineplot always make sure to extract two characteristics:
# 1. trend;
# 2. seasonality.
# This time series exhibits both.
autoplot(unemp_ts, unemp)

# can also use this to check for seasonality
# if the lines follow a similar pattern across years then there is seasonality
gg_season(unemp_ts, unemp)

#(b)
# box cox when variance isn't constant - so answer is yes it is appropriate
# box cox can also be used when the data does not closely mirror a normal
# distribution in order to make it look more normal.

# Estimate lambda value to use inside of the box cox transformation
guer <- features(unemp_ts, unemp, features = guerrero)
# Extract it
lambda <- guer$lambda_guerrero 

# have a look at the distribution prior to the transformation
hist(unemp_ts$unemp)

# plot the transformed time series
# notice the reduced range in the y-axis
autoplot(unemp_ts, box_cox(unemp, lambda)) +
  ggtitle("Original unemp")

# now have a look at the distribution after the transformation, it looks more
# like a normal distribution!
# When forecasting, it is usually a good thing to have the target variable
# follow a normal distribution as closely as possible
hist(box_cox(unemp_ts$unemp, lambda), main = "Transformed unemp")

# (c) one step ahead forecasts
#(i) 

unemp_transformed <- mutate(unemp_ts, unemp_bc=box_cox(unemp, lambda))
model1 <- model(unemp_transformed,
                TSLM_ts  = TSLM(unemp_bc ~ trend() + season()),
                SNaive   = SNAIVE(unemp_bc ~ lag("year")))

fore <- forecast(model1, h=1)
fore
# (d)
report(select(model1, "TSLM_ts"))
#comment: among the components of the multiple linear regression model, some are not statistically significant
# trend is significant and hints at a positive trend (look at estimate if estimate is positive then trend is positive)
# seasonality can be interpreted as the difference between a given period in a year and all others
# for instance in this model, since we have monthly data, we are considering different periods, where
# each period is exactly one month. Then seasonality is represented through a dummy variable that checks
# the difference between the first period (january which is missing) and all other periods (all other months).
# Therefore, the estimates represent the average difference between a month and the reference period (january).
# For instance if we were to comment the season()year6 estimate we would say that on average unemployment
# tends to decrease by 1.52 (remember this is in thousands!) compared to january.

# wrong as we are not forecasting anything here
# augment(model1) |>
#   ggplot(aes(x = Month)) +
#   geom_line(aes(y = unemp_bc), colour = "black", alpha = 0.4) +
#   geom_line(aes(y = .fitted, colour = .model))

# to produce forecasts use the `forecast` function after fitting a model as we
# did in (c)

# h controls how many periods in the future you would like to forecast
# since we are producing one-step-ahead forecast the horizon (h) is set to 1

autoplot(fore, unemp_transformed, level = NULL)

# (e) cross validation 

# create the new tsibble structure for CV
# this basically creates a series of training sets that will be used to
# retrain the models multiple times.
# Then accuracy metrics are computed on the different steps and the training
# set grows by 1 observation each time
unemp_cv <- stretch_tsibble(unemp_transformed, .init = 60, .step=1)

# fit the models on the CV dataset
fit_cv_u <- model(unemp_cv,
                  TSLM_ts = TSLM(unemp_bc ~ trend() + season()),
                  SNaive  = SNAIVE(unemp_bc ~ lag("year")))

# use the models fit on the CV dataset to produce the one-step-ahead forecasts
fc_cv_u <- forecast(fit_cv_u, h = 1)
acc_u <- accuracy(fc_cv_u, unemp_transformed)
select(acc_u, .model, RMSE, MAE, MAPE) #model with smaller value is the best

#(f)
# compute the accuracy of the models on the entire dataset (the one you used
# as input for the stretch_tsibble function originally)

#here snaive is better because less complicated, even though results are almost similar

############ AGRICULTURE ##################

# (a)
agriculture <- read.csv("agriculture.csv", sep=";") # sep could be ","
# analyzing the structure of the dataset we notice that agriculture is not
# a proper date but a an integer (int) number
str(agriculture)

# convert to proper date
# agriculture <- agriculture %>% 
#   mutate(year=year(as.Date(year)))
# 
# str(agriculture)

# (b)
# convert the dataset to a tsibble before moving on otherwise we will not have
# access to the different time-series functions we are used to.
agriculture_ts <- tsibble(agriculture, index=year)
# compute the AutoCorrelation Function (ACF)
acf_ag <- ACF(agriculture_ts, agriculture)
# plot the auto correlations
autoplot(acf_ag)
# or
# agriculture_ts %>% 
#   ACF(agriculture) %>% 
#   autoplot()

# The plot shows significant degrees of autocorrelation especially at lags of one
# period. This means that the time series at time T tends to be positively correlated
# to the value strictly preceding it (T-1).

# (c)
# before we move on to using the Ljung-Box test to evaluate autocorrelation using
# a sound statistical methodology we need to identify the maximum lag parameter
# to use when computing the test statistic.

# The book suggests following this decision-making process:
# 1. if the time series is seasonal, then use a lag parameter (l) of 2*m where m is the period of seasonality
# (if monthly data it is 12, quarterly data it is 4, daily data it is 7, hourly data it is 24)
# 2. if the time series is not seasonal, then use l = 10

# After picking the appropriate l then check if it is larger than T/5 where T is the number
# of observations. If it is larger, then use l = T/5

# However we know that yearly data does not exhibit seasonality by definition!
# seasonality refers to patterns that occur at a known, fixed, and predictable frequency within a larger time frame.
# since we are already operating at the yearly level there are no subdivisions

# Check if 10 is smaller than T/5
10 < nrow(agriculture_ts) / 5
# Since the check above stated that 10 is smaller than T/5 we can use that
# Now we therefore apply the Ljung-Box test using l = 10
features(agriculture_ts, agriculture, ljung_box, lag = 10)

# As expected given the very small p-value (definitely smaller than any commonly used significance level) we can determine that there is a significant
# degree of autocorrelation.

# Reference code is in chapter 5

####### 3 ###########

# Forecasting with decomposition is like predicting a company's total sales by looking at its steady underlying
# growth separately from its predictable holiday spikes.

# To do this, you first split the time series into its core building blocks: the repeating seasonal pattern
# the overall trend, and the random background noise. Once separated, you temporarily remove the seasonal 
# pattern from the data. This leaves you with the "seasonally adjusted" data, which is much smoother and easier to predict
# using basic forecasting methods. After you predict the future path of this smooth, underlying trend, you simply grab
# the historical seasonal pattern—like a typical December spike or a summer slump—and add it back on top of your new
# forecast to get the final result.

# This approach is highly appropriate for time series that exhibit strong, consistent, and well-defined seasonal patterns, 
# such as monthly retail sales, daily energy consumption, or quarterly passenger numbers. It is especially useful when the
# seasonal fluctuations are so large that they obscure the underlying trend, making it difficult for standard models to see
# the bigger picture without first stripping the seasonality away. 
# Naturally, this method is not appropriate for strictly annual data or any dataset that
# lacks a repeating cycle, because there is no seasonal component to extract in the first place.

# Reference code is in chapter 5.

##### 4 ######

# The naive method is the simplest forecasting approach possible. It assumes that whatever happened yesterday will happen exactly the 
# same way tomorrow. It completely ignores all historical data except for the single most recent observation.

# SES generates a forecast by taking a weighted average of all past observations. 
# However, instead of treating all historical data equally, it applies exponentially decreasing 
# weights as the observations get older. Recent data is treated as highly relevant, 
# while data from years ago is given very little weight.
# The parameter alpha [0,1] regulates how strong the smoothing should be with a high alpha meaning the model
# reacts quickly to recent changes and a small alpha meaning that the model smooths out recent volatility
# and bases its prediction on past patterns.