library(fpp3)
###### EX 1
df_global <- global_economy
View(df_global)

df_global <- df_global %>% 
  mutate(gdppc = GDP / Population)

distinct(df_global, Country)
# autoplot(df_global, gdppc)

# check which country has highest GDP

# get the max gdppc for each country

top_20 <- df_global %>% 
  as_tibble() %>% 
  group_by(Country) %>% 
  summarize(max_gdppc = max(gdppc, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(desc(max_gdppc)) %>% 
  slice(1:20) %>% 
  inner_join(df_global, by = "Country") %>% 
  as_tsibble(index = Year, key = Country)

autoplot(top_20, gdppc)

# The country with the highest gdppc is the north america region exhbiting
# a very high positive trend.
# Some cycles are visible and mostly due to macroeconomic shocks and
# disruptions.

####### EX 2

# (a) US GDP
us_gdp <- global_economy %>% 
  filter(Country == "United States")

autoplot(us_gdp)

# There is just an upward trend in the time series.

# ggplot(us_gdp, aes(x = GDP)) +
#   geom_histogram()

# Also the range of the values is very large.
# This makes this time series a good candidate for a box-cox transformation.

lambda <- features(us_gdp, GDP, "guerrero") %>% pull(lambda_guerrero)

autoplot(us_gdp, box_cox(GDP, lambda))

# (b) Victorian “Bulls, bullocks and steers” in aus_livestock
aus_livestock %>% 
  distinct(State)

vic_bulls <- aus_livestock %>% 
  filter(State == "Victoria", Animal == "Bulls, bullocks and steers")

autoplot(vic_bulls)

gg_season(vic_bulls)

# very complex seasonal pattern that would not greatly benefit from
# a transformation due to its high irregularity.
# This is not a time series that exhibit a seasonal pattern whose intensity
# changes with the level of the time series.

ggplot(vic_bulls, aes(x = Count)) +
  geom_histogram()

# However, looking at the distribution, it could be made more regular by
# applying a Box-Cox transformation to make it look more like a normal distribution

lambda <- features(vic_bulls, Count, "guerrero") %>% pull(lambda_guerrero)

vic_bulls <- vic_bulls %>% 
  mutate(transformed = box_cox(Count, lambda))

ggplot(vic_bulls, aes(x = transformed)) +
  geom_histogram()

# By looking at the distribution of the transformed variable now you
# can see that it mirrors more closely a normal distribution compared
# to the original variable.

# (c) Victorian Electricity Demand from vic_elec.

autoplot(vic_elec)

gg_season(vic_elec, period = "1w")

# given that the time series exhibit seasonality that is changing
# with the level of the time series, it is fundamental to use a box-cox
# transformation to make sure that this seasonal pattern is stabilized

lambda <- features(vic_elec, Demand, "guerrero") %>% pull(lambda_guerrero)

vic_elec_transformed <- vic_elec %>% 
  mutate(demand_transformed = box_cox(Demand, lambda))

gg_season(vic_elec_transformed, demand_transformed, period = "1w")

# as expected, by applying the box-cox the variance is stabilized
# across the entire time series. This can be noticed by the absence
# of extreme observations (outliers) and the reduced y-axis scale.

###### EX 3
# Why is a Box-Cox transformation unhelpful for the canadian_gas data?

autoplot(canadian_gas)

# By looking at this specific time series it is possible to see that
# the variance of the seasonal patters is not directly proportional
# to the level of the time series. Indeed, we can see that the variance
# is intensified in the middle of the time series while getting smaller
# at the end. This is not compatible with the assumptions that box-cox
# needs to work properly, so using a box-cox transformation here would
# not be useful at all!

###### EX 4
# What Box-Cox transformation would you select for your retail data
# (from Exercise 7 in Section 2.10)?

set.seed(34)
# set.seed(14062026)
myseries <- aus_retail |>
  filter(`Series ID` == sample(aus_retail$`Series ID`,1))

autoplot(myseries)
gg_season(myseries)
gg_subseries(myseries)  

# For this specific time series I would not use a box cox transformation
# for the same reason explained in EX 3.

##### EX 5

# (a) tobacco
tobacco <- aus_production %>%
  select(Tobacco)

autoplot(tobacco)
gg_season(tobacco)

# Seasonal patterns are inverted in the middle of the time series.
# Also seasonal patterns do not have a variance that strictly increases

# (b) ansett

View(ansett)

ansett %>% 
  distinct(Airports)

passengers <- ansett %>% 
  filter(Airports == "MEL-SYD", Class == "Economy")


autoplot(passengers)
gg_season(passengers)

# this time series could be improved by applying a box cox transformation.
# however there are some preliminary steps that need to be addressed.
# first, since this is a log transformation and logs are defined only
# for strictly positive non-zero inputs the fact that this time series
# exhibit observations that are exactly 0 make the box-cos not applicable to the
# time series as is.

# Therefore we remove the weeks were the passenger count was 0 and
# we specify that those datapoints should be treated as missing values.

passengers <- passengers %>% 
  filter(Passengers != 0)

passengers <- fill_gaps(passengers)

autoplot(passengers)

gg_season(passengers, period = "1y")

# we can also confirm that there is the presence of a yearly pattern

# we can now move on to estimate the lambda for the box-cox transformation

lambda <- features(passengers, Passengers, "guerrero") %>% pull(lambda_guerrero)
passengers <- passengers %>% 
  mutate(transformed = box_cox(Passengers, lambda))

autoplot(passengers, transformed)

# we can see now that the fluctuations for the seasonal periods
# are stabilized.

# (c) Pedestrian counts at Southern Cross Station from pedestrian
View(pedestrian)
pedestrian %>% 
  distinct(Sensor)

scs_pedestrian <- pedestrian %>% 
  filter(Sensor == "Southern Cross Station")

autoplot(scs_pedestrian, Count)
# too dense to actually make sense of the data, we can try
# with other seasonality-specific visualizations.

scs_pedestrian <- fill_gaps(scs_pedestrian)

gg_season(scs_pedestrian, Count, period = "1d")
# at the daily level we see strong seasonality and the same
# patterns emerge
gg_season(scs_pedestrian, Count, period = "1w")
# at the weekly level wee see the patterns at the daily level are repeated
# for all weekdays, whereas during the weekend there is not
# a clear pattern other that on Saturday night people use the station more
# and also on Sunday mornings.

# gg_subseries(scs_pedestrian, Count, period = "1d")
# subseries plot is too dense to interpret correctly
# still the mean provides evidence of how peak hours influence
# the number of people passing through the station (higher mean overall).


# from the plot above it is possible to see that more people
# are going through the selected station at peak hours, as expected

# cnt_pedestrian <- scs_pedestrian %>% 
#   as_tibble() %>% 
#   group_by(Time) %>% 
#   summarize(avg_cnt = mean(Count, na.rm = T))
# 
# ggplot(cnt_pedestrian, aes(x = Time, y = avg_cnt)) +
#   geom_bar(stat = "identity")

# given the considerations above this time series exhibits a strong
# seasonal pattern at the daily level. The variance of the time
# series also does not seem to be stable considering that there are period in time
# where the station was not used that much but then later on it was used much more.

# again remember that before we apply the box cox transformation we need to make sure
# that the data we are transforming is strictly > 0. This is not always the case with
# count data where we might have an observation that is exactly 0.
# So we shift the entire time series by adding a constant (usually 1)

scs_pedestrian <- scs_pedestrian %>% 
  mutate(shifted_ts = Count + 1)

lambda <- features(scs_pedestrian, shifted_ts, "guerrero") %>% pull(lambda_guerrero)

scs_pedestrian <- scs_pedestrian %>% 
  mutate(transformed = box_cox(shifted_ts, lambda))

gg_season(scs_pedestrian, transformed, period = "1w")

# We immediately notice two things:
# 1: the variance is correctly stabilized for the weekdays
# 2: however, the variance is still high for the weekends!
# this suggests that this time series should be treated as exhibiting
# a bi-modal seasonality whereby a seasonal pattern exists for weekdays
# and another pattern is exhibited on weekends.

##### EX 6
# Show that a 3 × 5 MA is equivalent to a 7-term weighted moving average 
# with weights of 0.067, 0.133, 0.200, 0.200, 0.200, 0.133, and 0.067.

# in a 3x5 MA we first compute the 5-MA of the series
# then we obtain that the observations in the window have all equal weight of 1/5
# then applying this further, we get that if we now apply a 3-MA to the 5-MA just computed
# the weights will be then given by 1/3 * 1/5 * N
# where N is the number of times that same observation is considered in the window.
# For instance: in the 5-MA you have t-2, t-1, t, t+1, t+2 in each window.
# Intuitively, it makes sense that the observations at the boundaries (i.e., t-2 and t+2)
# will be considered less often as the MA is computed. This is by construction.
# Therefore, the N we are using represents the frequency that a specific value is represented
# in the window.

##### EX 7

# (a)
gas <- tail(aus_production, 5*4) |> select(Gas)

View(gas)

autoplot(gas, Gas)

# considering the systematic pattern that is revealed by plotting the time series
# we can clearly see a seasonal pattern of yearly recurrence.
# There are also hints of a positive trend in the data (see the peaks
# getting higher as we move closer to the present, same thing with the
# troughs.)

# Let's inspect the data more to make sure that we are right.
gg_season(gas, Gas, period="1y")

# (b)

dcmp <- gas %>% 
  model(
    mult = classical_decomposition(Gas ~ season(4), type = "multiplicative")
  )

# (c)
dcmp %>% 
  components()

# Looking at the report from the components we can see that those are consistent
# with the interpretation of the plot above. For instance, the estimated seasonal
# component for Q1 and Q4 is < 1 whereas the estimated seasonal components are > 1 
# for Q2 and Q3 highlighting how these quarters tend to be higher than Q1 and Q4.
# We cannot estimate the first and last two trend components because of the fact that
# a 2x4-MA is performed on this data to estimate the trend.

# (d) Compute and plot the seasonally adjusted data.

dcmp %>% 
  components() %>% 
  select(season_adjust) %>% 
  autoplot(season_adjust)

autoplot(select(components(dcmp), season_adjust))

# (e)

modified <- gas
modified$Gas[1] <- modified$Gas[1] + 300

dcmp_out <- modified %>% 
  model(
    mult = classical_decomposition(Gas ~ season(4), type = "multiplicative")
  )

dcmp_out %>% 
  components() %>% 
  select(season_adjust) %>% 
  autoplot(season_adjust)

# We can see that the decomposition is greatly affected by the presence
# of outliers as this new seasonally adjusted plot completely reverses the
# trend observed in the original time series.

# (f) Does it make any difference if the outlier is near the end rather 
# than in the middle of the time series?

# Yes, in fact as noticed before if an outlier is presented at either one of the
# boundaries (beginning or end) it will only affect the seasonal component. It will
# not affect the trend component because as explained usually the very first and last 
# observations will not be considered when computing the trend due to how moving averages
# work. Instead, when an outlier is present in the middle it will affect both the trend and the seasonality.

##### EX 9

# (a)
# 
# Looking at the results from the STL decomposition we can extract some valuable information.
# First, the time series exhibits a positive trend meaning that across time the overall number
# of people in the civilian labour force in Australia grew.
# Second, we can see that the seasonality is present but is relatively small as highlighted by
# the very large bar to the left of the plot. This means that in terms of magnitude, the estimated seasonal 
# effect is very small.
# Third, the remainder shows a clear indication of the fact that the decomposition failed to detect
# an exogenous shock.
# Finally, the subseries plot provides evidence of the seasonal pattern detected in the plot above.
# This is visible from the fact that different months exhibit very different mean values.

# (b)

# yes, see explanation about the remainder component in (a)

##### EX 10

# (a)

autoplot(canadian_gas)

gg_subseries(canadian_gas)
gg_season(canadian_gas)


# (b)

dcmp <- canadian_gas %>% 
  model(
    stl = STL(Volume ~ trend(window = 11) + season(window = 7))
  )

dcmp %>% 
  components() %>% 
  autoplot()

# (c)

dcmp %>% 
  components() %>% 
  select(season_year) %>% 
  gg_season()

gg_season(select(components(dcmp), season_year))

# over time the seasonal shape moved from being a smooth parabola-like curve to 
# a more ragged line with sharp edges. This indicates that the seasonal patterns
# underwent severe amplification throughout the years.

# (d)

# No it is not possible to use the decomposition as is to produce a seasonally-adjusted time series.
# The reason for that is that STL is strictly additive and does not support features of multiplicative decompositions
# such as different variances for seasonal periods. For these reasons if we want to use this decomposition 
# to produce a seasonally-adjusted time series we need to rely on a box-cox transformation before.


lambda <- features(canadian_gas, Volume, "guerrero") %>% pull(lambda_guerrero)
dcmp <- canadian_gas %>%
  model(STL(box_cox(Volume, lambda) ~ trend(window = 11) + season(window = "periodic")))
# here the window of the season is periodic because it assumes that the box cox transformation stabilized the variance
# of the seasonal patterns

# finally plot the seasonally-adjusted time series

dcmp %>% 
  components() %>% 
  select(season_adjust) %>% 
  autoplot()
