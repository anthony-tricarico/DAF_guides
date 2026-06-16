
library(fpp3)
install.packages("tswge")
library(tswge)

df <- freight
str(df)
df
dates <- seq.Date(from = as.Date("2011-01-01"), to = as.Date("2020-12-01"), by = "month")
hist(df)
df_ts <- tsibble(
  "series" = df,
  "dates" = dates,
  index = "dates"
) %>% 
  mutate(dates = yearmonth(dates))

