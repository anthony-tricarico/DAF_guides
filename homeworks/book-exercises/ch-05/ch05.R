# load library used for all exercises
library(fpp3)

####### EX 1
# Produce forecasts for the following series using whichever of NAIVE(y), 
# SNAIVE(y) or RW(y ~ drift()) is more appropriate in each case:

# (a) Australian Population (global_economy)
au_pop <- global_economy %>% 
  filter(Country == "Australia") %>% 
  select(Population)

autoplot(au_pop)

# Given the strong trend and almost no seasonality, we use the drift method

dr <- au_pop %>% 
  model(
    drift = RW(Population ~ drift())
  )

# optionally we can also plot the forecasts (NOT REQUIRED)
fore <- forecast(dr, h=10)

autoplot(fore, au_pop, level = NULL)

# gg_tsresiduals(dr %>% select(drift))
  
## (b) Bricks (aus_production)

bricks <- aus_production %>% 
  select(Bricks) %>% 
  drop_na()

autoplot(bricks)

# unclear trend which is first positive, then reverts to negative but in the
# last part of the plot is actually stabilized

gg_season(bricks)
# strong seasonality

# due to the presence of very strong seasonality, seasonal naive is a good
# candidate method

sn <- bricks %>% 
  model(
    seasonal_naive = SNAIVE(Bricks ~ lag("year"))
  )

fore <- forecast(sn, h=8)
autoplot(fore, bricks, level = NULL)

## (c) NSW Lambs (aus_livestock)

aus_livestock %>% 
  distinct(Animal)

aus_livestock %>% 
  distinct(State)

lambs_nsw <- aus_livestock %>% 
  filter(Animal == "Lambs", State == "New South Wales")

autoplot(lambs_nsw)

# some trend is visible from the plot, first negative then slightly positive after 1998
# let's investigate seasonality some more

gg_season(lambs_nsw)
# still not completely clear from this plot as there appear to be some years that
# do not behave consistently with other ones

gg_subseries(lambs_nsw)
# all months exhibit similar behavior and their mean values are not very different

autoplot(ACF(lambs_nsw, Count))
# strong autocorrelation from the time series implies that there is a strong
# dependence between lagged values. Basically, most of the observations exhibit
# very high correlation with their 1-lag version. This means we can actually extract
# a lot of information just by looking at that 1-lag. Therefore, we use the NAIVE method
# in this case.

na <- lambs_nsw %>% 
  model(
    naive = NAIVE(Count)
  )

fore <- forecast(na, h = 12)

autoplot(fore, lambs_nsw, level = NULL)

# (d) Household wealth (hh_budget).

wealth <- hh_budget %>% 
  select(Wealth)

autoplot(wealth)
# the different countries present some differences in their trend over time
# we cannot rely on a seasonal pattern here since we are looking at yearly data
# considering that each series exhibits a specific trend we can use a drift method

dr <- wealth %>% 
  model(
    drift = RW(Wealth ~ drift())
  )

fore <- forecast(dr, h=5)

autoplot(fore, wealth, level = NULL)

# given that the cycles by definition do not occur in regular patterns we cannot
# use those to produce a better forecasting method

# (e) Australian takeaway food turnover (aus_retail).

aus_retail %>% 
  distinct(Industry)

ta_aus <- aus_retail %>% 
  filter(Industry == "Takeaway food services") %>% 
  select(Turnover)

autoplot(ta_aus)
# steadily increasing trend overall

gg_season(ta_aus)
# there is seasonality in the data and so we can take advantage of a SNAIVE model
# this model would be useful to forecast in the short-term, however, since it does
# not incorporate any information about the trend, its forecasts will likely fail
# in longer forecasting horizons.

sn <- ta_aus %>% 
  model(
    snaive = SNAIVE(Turnover ~ lag("year"))
  )

fore <- forecast(sn, h = 12)

autoplot(fore, ta_aus, level = NULL)

####### EX 2

## Use the Facebook stock price (data set gafa_stock) to do the following:

## (a) Produce a time plot of the series.
gafa_stock %>% 
  distinct(Symbol)

fb_ts <- gafa_stock %>% 
  filter(Symbol == "FB") %>% 
  select(Close) %>% 
  mutate(day = row_number()) %>% # day will be the number of the row in the dataset, this will avoid using the trading days in the original dataset which makes an irregular index
  update_tsibble(index = day, regular = T) # remember to make the index of the tsibble regular

autoplot(fb_ts)

## (b) Produce forecasts using the drift method and plot them.

dr <- fb_ts %>% 
  model(
    drift = RW(Close ~ drift())
  )

fore <- forecast(dr, h=30)

autoplot(fore, fb_ts, level = NULL)

## (c) Show that the forecasts are identical to extending the line drawn between the first and last observations.

# to do this we need to remember the equation of a line
# y = q + m*x
# where q is the intercept (intersection with y-axis)
# m is the slope of the line
# x is the date itself
# q will be computed after the slope is computed
# m is computed as ratio of the difference between the change in y and the change in x
# m = (y_T - y_1) / (x_T - x_1)

y_t <- tail(fb_ts, 1) %>% 
  pull(Close)

y_1 <- head(fb_ts, 1) %>% 
  pull(Close) # extract the first value

x_t <- tail(fb_ts, 1) %>% 
  pull(day)

x_1 <- head(fb_ts, 1) %>% 
  pull(day) # extract the first value

m <- (y_t - y_1) / (x_t - x_1)

q <- y_1 - m * x_1

# putting together the equation now and forecasting one period after the last one

fore_1 <- q + m * (x_t + 1)

# comparing with the drift method forecasts

tidy(dr) # first notice that the estimate for the slope (b) is the same as our slope (m) (after approximation)

fore_1_dr <- forecast(dr, h = 1)

# then we check the equality and we see that indeed they are the same
pull(fore_1_dr, .mean) == fore_1

# (d) Try using some of the other benchmark functions to forecast the same data set. Which do you think is best? Why?

fitted_mods <- fb_ts %>% 
  model(
    na = NAIVE(Close),
    sna = SNAIVE(Close ~ lag(7)),
    dr = RW(Close ~ drift())
  )

fore <- forecast(fitted_mods, h = 30)

autoplot(fore, fb_ts, level = NULL)

# probably the best is NAIVE due to the highly randomness connected to the series
# and no specific seasonality or trend arising

####### EX 3

## Apply a seasonal naïve method to the quarterly Australian beer production data from 1992. 
# Check if the residuals look like white noise, and plot the forecasts.

# Extract data of interest
recent_production <- aus_production |>
  filter(year(Quarter) >= 1992)
# Define and estimate a model
fit <- recent_production |> model(SNAIVE(Beer))
# Look at the residuals
fit |> gg_tsresiduals()
# some lags are beyond the significance threshold, indicating the possible presence of autocorrelation.
# the residuals do not look normally distributed either and their mean is not 0.
# Look a some forecasts
fit |> forecast() |> autoplot(recent_production)

####### EX 4

# Repeat the previous exercise using the Australian Exports series from global_economy 
# and the Bricks series from aus_production. Use whichever of NAIVE() or SNAIVE() 
# is more appropriate in each case.

au_ex <- global_economy %>% 
  filter(Country == "Australia") %>% 
  select(Exports)

autoplot(au_ex)  
# strong trend but no seasonal pattern (yearly data)
# we use NAIVE in this case

fit <- au_ex |> model(NAIVE(Exports))
# Look at the residuals
fit |> gg_tsresiduals()

# residuals look good with no hints of autocorrelation. Since no significant pattern
# is detected in them we can consider these residuals to be a white noise series (only one lag is significant).
# They also closely mirror a normal distribution with a mean of 0.
fit |> forecast() |> autoplot(au_ex)

## BRICKS

# using the same dataset provided during EX 1
autoplot(bricks)

gg_season(bricks)
# seasonal pattern is present therefore it is better to model using seasonal naive

fit <- bricks |> model(SNAIVE(Bricks))
# Look at the residuals
fit |> gg_tsresiduals()

# residuals are not white noise as evidenced by the presence of strong autocorrelation
# the distribution is not centered around 0 and cannot be assumed to be normal.
fit |> forecast() |> autoplot(bricks)

##### EX 5
# Produce forecasts for the 7 Victorian series in aus_livestock using SNAIVE(). 
# Plot the resulting forecasts including the historical data.
# Is this a reasonable benchmark for these series?

aus_livestock %>% 
  filter(State == "Victoria") %>% 
  model(SNAIVE(Count ~ lag("year"))) %>% 
  forecast(h = "2 years") %>% 
  autoplot(aus_livestock)
  
# this might be an appropriate benchmark for some of the series in the dataset
# for instance the case of calves which follows a highly regular pattern.
# The idea is that you would use a SNAIVE when you have a regular seasonal pattern
# with a trend that changes very slowly.

###### EX 6

# Are the following statements true or false? Explain your answer.

# Good forecast methods should have normally distributed residuals.
# False. Generally it is preferred to have normally distributed residuals because
# this will lead to have normally distributed prediction intervals. This is a desired
# property because such intervals can be computed more easily if their underlying
# distribution is assumed to be normal.
# However, this is not necessary to have this property to have a "good" forecasting method.

# A model with small residuals will give good forecasts.
# False. Remember that residuals are computed on the entire dataset using fitted
# values that are estimated using observations that come also from future periods (in some methodologies like mean and drift methods).
# Forecasts are different in that they can only rely on previous periods to 
# estimate future values (the actual forecasts!). Therefore, a model with small
# residuals is not guaranteed to perform well on forecasts because it might well be that the model overfit the training data.

# The best measure of forecast accuracy is MAPE.
# False. The choice of forecast accuracy metric is dependent on the task under consideration.
# While being a scale-independent measure of accuracy, MAPE also suffers from other drawbacks.
# For instance, this metric can provide meaningless results in the case when the variable 
# of interest takes the value of 0 (remember we cannot divide by zero!)

# If your model doesn’t forecast well, you should make it more complicated.
# False. If your model does not forecast well you should try to diagnose the issues
# it presents by looking at its residuals, for example. Then you should work on
# fixing these issues. Making the model more complicated by adding more predictors
# or other estimated terms will make it more likely to overfit the training data, 
# thus producing poor results when producing forecasts.

# Always choose the model with the best forecast accuracy as measured on the test set.
# False. While this is a fair indication of actual forecast performance of your model, it might still be
# that by coincidence your model performed well just because of the actual data that is selected in the test set.
# One way to improve this is to use multiple test sets and see how the model behaves across them.
# This is exactly what cross-validation does. You can use the model that produces the best forecast accuracy as measured
# by a cross-validation (CV) procedure.

####### EX 7

# (a) Create a training dataset consisting of observations before 2011 using

set.seed(12345678)
myseries <- aus_retail |>
  filter(`Series ID` == sample(aus_retail$`Series ID`,1))

myseries_train <- myseries |>
  filter(year(Month) < 2011)

# (b)
autoplot(myseries, Turnover) +
  autolayer(myseries_train, Turnover, colour = "red")

# (c)

fit <- myseries_train |>
  model(SNAIVE(Turnover))

# (d)

fit |> gg_tsresiduals()
# residuals are neither white noise (too many spikes in the correlogram) nor normally distributed (long tails in histogram)

# (e)

fc <- fit |>
  forecast(new_data = anti_join(myseries, myseries_train))
fc |> autoplot(myseries)

# (f)

fit |> accuracy()
fc |> accuracy(myseries)

# (g) How sensitive are the accuracy measures to the amount of training data used?

# The accuracy metrics are very sensitive. Since almost all of those are based on some
# sort of average (MAE, RMSE) the number of observations plays a crucial role in
# determining the actual accuracy score that we get. Usually, with more observations
# you get higher accuracies (i.e., lower scores for the error metrics); the opposite
# is also true.
# In this case we see that since less observations are in the test set the error metrics are higher.

###### EX 8

# Consider the number of pigs slaughtered in New South Wales (data set aus_livestock).

# (a) Produce some plots of the data in order to become familiar with it.
nws_pigs <- aus_livestock %>% 
  filter(Animal == "Pigs", State == "New South Wales")

autoplot(nws_pigs)
# trend is reverted multiple times in the data

gg_season(nws_pigs)
# strong seasonal pattern across most of the years. However, it is not consistent for all years

gg_subseries(nws_pigs)
# subseries plot shows that the data presents some differences across months (see means; blue bars)
# however the difference is not substantial.
# Jan, Feb, and Apr show the lowest means across all months.

# (b) Create a training set of 486 observations, withholding a test set of 72 observations (6 years).
train_idx <- 1:486

train <- nws_pigs %>% 
  slice(train_idx)
 
test <- nws_pigs %>% 
  slice(-train_idx)


# (c) Try using various benchmark methods to forecast the training set and compare the results on the test set. Which method did best?

fitted_mods <- train %>% 
  model(
    naive = NAIVE(Count),
    seasonal_naive = SNAIVE(Count ~ lag("year")),
    mean = MEAN(Count),
    drift = RW(Count ~ drift())
  )

fore <- forecast(fitted_mods, h = 12)

# check test accuracy (both methods work the same way producing the same result)
# fore %>% accuracy(nws_pigs) 
acc <- fore %>% 
  accuracy(test) %>% # compute accuracy on test set
  arrange(RMSE) # re-arrange rows based on values of RMSE (from smallest to largest)

acc
# from the table above the model that performed better is the drift model since
# it is the method that presents the lowest RMSE and MAE compared to the
# other benchmark methods tested.

# this is to get the accuracy on the training set (not asked in question)
# accuracy(fitted_mods)

# (d) Check the residuals of your preferred method. Do they resemble white noise?

fitted_mods %>% 
  select(drift) %>% 
  gg_tsresiduals()

# residuals are highly autocorrelated and their distribution is not normal.
# This hints at the fact that these models can be improved.

####### EX 9
# (a) Create a training set for household wealth (hh_budget) by withholding the last four years as a test set.
autoplot(wealth)
# using dataset already extracted in a previous exercise

train <- wealth %>% 
  filter_index(. ~ 2012)

test <- wealth %>% 
  filter_index(2013 ~ .)

# (b) Fit all the appropriate benchmark methods to the training set and forecast the periods covered by the test set.

fitted_mods <- train %>% 
  model(
    naive = NAIVE(Wealth),
    drift = RW(Wealth ~ drift()),
    mean = MEAN(Wealth)
  )

fore <- forecast(fitted_mods, h = 4)

# (c) Compute the accuracy of your forecasts. Which method does best?

acc <- accuracy(fore, test) %>% 
  arrange(RMSE)
acc
# the method that seems to be performing best across all time series is the
# drift method. Indeed, it is the method that reports the lowest RMSE for each country.

# (d) Do the residuals from the best method resemble white noise?
fitted_mods %>%
  filter(Country == "Australia") %>% 
  select(drift) %>% 
  gg_tsresiduals()

# residuals are white noise for the drift method fitted to Australian data

fitted_mods %>%
  filter(Country == "Japan") %>% 
  select(drift) %>% 
  gg_tsresiduals()

# residuals are white noise for the drift method fitted to Japan data

fitted_mods %>%
  filter(Country == "USA") %>% 
  select(drift) %>% 
  gg_tsresiduals()
# residuals are white noise for the drift method fitted to USA data

fitted_mods %>%
  filter(Country == "Canada") %>% 
  select(drift) %>% 
  gg_tsresiduals()

# residuals are white noise for the drift method fitted to Canadian data