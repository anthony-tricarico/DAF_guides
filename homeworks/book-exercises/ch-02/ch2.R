library(fpp3)
# EX 2: Use filter() to find what days corresponded to the peak closing price for each of the four stocks in gafa_stock.

# assign dataset to `gafa` to avoid overwriting it
gafa <- gafa_stock

# the peak closing price correspond to the days where the closing price reached its maximum
# intuitively, we have to first compute what the maximum for the four stocks was
# then filter for these maximum values

# group the data by stock symbol, then filter for the max close price
gafa %>% 
  group_by(Symbol) %>% 
  filter(Close == max(Close))

autoplot(gafa, Close)

# EX 4

# (a) install USgas package (skip if you already have it installed)
# install.packages("USgas")
library(USgas)
# (b) Create a tsibble from us_total with year as the index and state as the key.
gas_ts <- as_tsibble(USgas::us_total, index = year, key = state)

# (c) Plot the annual natural gas consumption by state for the New England area 
# (comprising the states of Maine, Vermont, New Hampshire, Massachusetts, Connecticut and Rhode Island).

gas_ts %>% 
  filter(
    state %in% c(
      "Maine", 
      "Vermont", 
      "New Hampshire", 
      "Massachusetts", 
      "Connecticut", 
      "Rhode Island"
      )
    ) %>% 
  autoplot(y)

# EX 5

# (a) Download tourism.xlsx from the book website and read it into R using readxl::read_excel()
# we need to import this package if we want to use the read_excel function
library(readxl)
tourism_xlsx <- read_excel("homeworks/book-exercises/tourism.xlsx")

# inspecting the structure of the dataset to determine the types of the different
# columns

str(tourism_xlsx)

# notice that quarter is not encoded as a proper date type but it is encoded
# as a chr, meaning it is just a string (a sequence of characters)

# (b) Create a tsibble which is identical to the tourism tsibble from the tsibble package.

# first let's understand what tourism looks like
tourism

# upon inspection we notice it has five columns: Quarter, Region, State, Purpose, Trips
# 24,320 rows
# the index is quarter
# the key is composite and it is made of the combination of the Region, State, Purpose columns

tourism_xlsx_new <- tourism_xlsx %>%  # start from the original dataset we imported above
  mutate(Quarter = yearquarter(Quarter)) %>%  # convert the Quarter column to a proper date
  as_tsibble(key = c(Region, State, Purpose), index = Quarter) # set the key and index of the new tsibble to mimic the original tourism tsibble

tourism
tourism_xlsx_new

# upon inspections now the two datasets look the same

# (c) Find what combination of Region and Purpose had the maximum number of overnight trips on average.

tourism_xlsx_new %>% 
  as_tibble() %>% # Drop the tsibble temporal structure, otherwise summarize will not work appropriately
  group_by(Region, Purpose) %>% 
  summarize(avg_trips = mean(Trips), .groups = "drop") %>% # Calculate mean and cleanly ungroup
  filter(avg_trips == max(avg_trips)) %>% # Idiomatic way to get the top row
  arrange(desc(avg_trips)) %>% 
  head(1)

# (d) Create a new tsibble which combines the Purposes and Regions, and just has total trips by State.

tourism_xlsx_new %>% 
  group_by(State) %>% 
  summarize(total_trips = sum(Trips))

# EX 6

# The aus_arrivals data set comprises quarterly international arrivals to Australia from Japan, New Zealand, UK and the US.

# Use autoplot(), gg_season() and gg_subseries() to compare the differences between the arrivals from these four countries.

autoplot(aus_arrivals, Arrivals)
# presence of seasonality irrespective of the country of origin
# although this seasonality does not follow the same pattern in all countries
# for instance, UK and Japan seem to have peaks and troughs reverted compared
# to each other (i.e., when UK has a spike Japan has a trough).

# clear trend for new zealand which has increased throughout the years the number
# of visits to australia.
# Japan's visit to Australia increased steadily up until 2000 when the trend was
# reverted and decreased.


gg_season(aus_arrivals, Arrivals)
# strong seasonality for UK where the most common months to visit Australia are
# in the first and last quarter.

gg_subseries(aus_arrivals, Arrivals)
# confirmed reversed trend for Japan, where Q1 is also the most favorite month
# to visit AU
# constantly upward trend for NZ
# trend seems to be in reversal also for UK. Confirmed strong seasonality in Q1
# and Q4 as the most popular months to visit AU.


# EX 9

# 3-D
# 1-A
# 2-C
# 4-B

# EX 10

# The aus_livestock data contains the monthly total number of pigs slaughtered 
# in Victoria, Australia, from Jul 1972 to Dec 2018. Use filter() to extract pig
# slaughters in Victoria between 1990 and 1995. 
# Use autoplot() and ACF() for this data. How do they differ from white noise?
# If a longer period of data is used, what difference does it make to the ACF?
distinct(aus_livestock, Animal)
distinct(aus_livestock, State)
pig_slaughters <- aus_livestock %>% 
  filter(
    Animal == "Pigs",
    year(Month) %in% 1990:1995,
    State == "Victoria")

autoplot(pig_slaughters, .vars = Count)

autoplot(ACF(pig_slaughters, Count))

# The data seem to exhibit a high degree of autocorrelation that is highly visible
# up to lag 15 approximately. The presence of autocorrelation makes it so that 
# we cannot consider this series as white noise (i.e., the patterns make it so this time series is different
# from a random distribution)





