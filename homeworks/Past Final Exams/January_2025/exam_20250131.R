library(pacman)

p_load(fpp3)


# exercise 1 --------------------------------------------------------------

View(aus_tobacco)

nsw_df <- aus_tobacco %>% 
  filter(State == 'NSW')

# Both seasonality and trend
autoplot(nsw_df)

model_stl <- nsw_df %>% 
  model(
  'STL' = STL(Expenditure ~ trend() + season(), robust=FALSE)
)

# We need to use a large window special to avoid overfitting since the series has a varying slope in the trend and a large window allows to smooth out the trend without losing too much information
# The option robust=TRUE makes the algorithm more robust to the presence of outliers, we can check if outliers are present by plotting the histogram of the time series

ggplot(nsw_df, aes(x = Expenditure)) +
  geom_histogram()

# alternative 

hist(nsw_df$Expenditure)

# We can see that there are no observations that are far away and also their value is not significantly larger than any other value

boxplot(nsw_df$Expenditure)

# this is confirmed by the boxplot which does not show any point (i.e., outlier) beyond the whiskers
# We conclude that it is not necessary to use the robust=TRUE option

autoplot(components(model_stl))

# From the decomposition we can see that the smoothed trend is downward sloping indicating that over the years more and more people quit smoking
# The seasonality retains the shape but decreases in magnitude reflecting the observation made above (less people consuming tobacco). Also notice that the seasonality is on a much smaller scale.
# The remainder component, despite being on a smaller scale as we can notice from the larger box on the left, seems to gather a lot of variation that is not strictly explained by the trend and seasonality comnponents. Yet, we can show that this component overall sums to 0.

sum(components(model_stl)$remainder)

# Exercise 2 --------------------------------------------------------------

men_10000 <- olympic_running %>% 
  filter(Sex == 'men', Length == 10000)

men_10000[which(!is.na(men_10000$Time)), ] # alternative way to remove missing values (not asked by the exercise)

autoplot(men_10000)

fit <- men_10000 %>% 
  model(
    'linear' = TSLM(Time ~ trend()),
    'log-linear' = TSLM(log(Time) ~ trend()),
    'piecewise' = TSLM(Time ~ trend(knots = 1978))
  )

augment(fit) %>% 
  autoplot(.vars = .fitted)

augment(fit) %>% 
  ggplot(aes(x = Year, y = .innov, color = .model)) +
  geom_point()

filter(augment(fit), .model == 'log-linear')%>% 
  ggplot(aes(x = Year, y = .innov, color = .model)) +
  geom_point()

filter(augment(fit), .model == 'linear')%>% 
  ggplot(aes(x = Year, y = .innov, color = .model)) +
  geom_point()

filter(augment(fit), .model == 'piecewise')%>% 
  ggplot(aes(x = Year, y = .innov, color = .model)) +
  geom_point()

# Alternatively, in an easier way
select(fit, 'log-linear')%>% 
  gg_tsresiduals()

select(fit, 'linear')%>% 
  gg_tsresiduals()

select(fit, 'piecewise')%>%
  gg_tsresiduals()

# by looking both at the time plot of the residuals and the ACF function plotted below we can say that the residuals do not appear to be autocorrelated and therefore appear to come from a white noise process

# compute forecasts (b)

fore <- forecast(fit, h = 2)

# test set (c)

test <- tsibble(
  'Year' = c(2020, 2024),
  'Time' = c(1663, 1603),
  index = 'Year'
)

accuracy(fore, test)
# according to the RMSE the best model is the piecewise model since it has the lowest RMSE

autoplot(fore, men_10000)

