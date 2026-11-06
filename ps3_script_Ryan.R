library(tidyverse)

retail_raw <- read_csv("data/retail_sales_raw.csv")

head(retail_raw)

#The class is a date

retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)

retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")

head(retail_raw_long)

library(fpp3)

retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)

head(retail_ts)
has_gaps(retail_ts)

head(canadian_gas)

key_vars(canadian_gas)
index_var(canadian_gas)

can_gas <- canadian_gas |>
  select(Month, Volume)

has_gaps(can_gas)

head(can_gas)

can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))

head(can_gas_recent)

can_gas_recent <- can_gas_recent |>
  mutate(year    = year(Month),
         month   = month(Month, label = TRUE),
         quarter = quarter(Month))

can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))

can_gas_recent |>
  mutate(next_month           = Month + 1,
         same_month_last_year = Month - 12) |>
  head()

#December has the highest average production and June has the lowest.


can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)

can_gas_q |> autoplot(Volume)

#We sum the values to get total quarterly production and yes, the graph still 
#appears to be seasonal.

retail_ts |> autoplot(sales)

retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()

# In the NSA series I see a massive spike in sales every December each year. 
# This spike is not present in the seasonally adjusted data. 
# The two series generally follow the same trend over the long run, 
# although spikes and drops are much higher for the NSA series like during 2008. 

can_gas |> autoplot(Volume) + labs(
  title = "Canadian Gas Production",
  x = NULL, y = "Cubic Meters (Billions)", color = NULL
) + theme_minimal() 

retail_ts |> autoplot(sales) + labs(
  title = "U.S. Retail and Food Service Sales",
  subtitle = "Seasonally adjusted vs. not seasonally adjusted",
  x = NULL, y = "Millions of dollars", color = NULL
) + theme_minimal() +  facet_wrap(~ series_id, scales = "free_y")

#I created a separate plot for each series for the Food Service Sales to really compare 
#how different the seasonally adjusted data is. 

#The seasonal swings get larger as the series grows but only up to a point, past around 1990
#the seasonal swings actually get smaller as overall production gets largeer

can_gas <- can_gas |> 
  mutate(log_volume = log(Volume))

can_gas |> autoplot(log_volume) + labs(
  title = "Canadian Gas Production",
  x = NULL, y = "Log of Cubic Meters (Billions)", color = NULL
) + theme_minimal()

#The trend now has more consistent seasonal swings though they till decrease in 
#size after 1990. I think that the logs are a good idea, because of how large 
#the seasonal swings are in the not logged version. 

retail_nsa <- retail_ts |> filter(series_id == "RSAFSNA") |> mutate(log_sales = log(sales))

retail_nsa |> autoplot(sales) + labs(title = "NSA Retail Sailes", x = NULL, y = "Millions of dollars") + theme_minimal()

retail_nsa |> autoplot(log_sales) + 
  labs(title = "Logged NSA Retail Sales", x = NULL, y = "Log of Millions of dollars") 
+ theme_minimal()

# I think the case for logging the data is weaker for the retail sales data,
# because the seasonal swings are not as large as the gas production data.

can_gas <- can_gas |>
  mutate(
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )

can_gas |> autoplot(growth_direct)

can_gas <- can_gas |>
  mutate(
    growth_gap = abs(growth_direct - growth_logdiff)
  )

can_gas |> autoplot(growth_gap)

# The log difference approximation seems reasonably accurately, but does differ by
# over 3 in some cases

retail_ts <- retail_ts |>
  group_by_key() |>
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()

retail_ts |>
  autoplot(growth_mom) +
  labs(
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL, y = "Growth rate (%)", color = NULL
  ) +
  theme_minimal()

# The RASFSNA is much noiser than its seasonally adjusted counterpart.
# This is because the NSA data has seasonal swings that weren't taken out of the
# data and therefore shows sales varying wildly from month to month.

retail_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

retail_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

# The seasonal plot in the Seasonally Adjusted series has mostly straight line and therefore
# constant growth, no matter what month. The main dip in the RSAFS data is due to COVID.
# On the other hand the growth for the NSA data varies much more from month to month.

can_gas |> gg_season(log_volume) + labs(
  title = "Seasonal Pattern in Canadian Gas Production", y = "log(Volume")

# The winter months tend to be higher for natural gas production, which makes sense
# as it is used for heating.

can_gas |>
  gg_subseries(log_volume) +
  labs(
    title = "Subseries Plot: Canadian Gas Production",
    y = "log(Volume)"
  )

install.packages("seasonal")

retail_nsa_x13 <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()

retail_nsa_x13 |> autoplot()

sa_compare <- retail_nsa_x13 |>
  as_tibble() |>
  select(Month, mine = season_adjust) |>
  left_join(
    retail_ts |>
      filter(series_id == "RSAFS") |>
      as_tibble() |>
      transmute(Month, census = sales),
    by = "Month"
  ) |>
  mutate(pct_diff = 100 * (log(mine) - log(census)))

sa_compare |>
  ggplot(aes(x = Month, y = pct_diff)) +
  geom_line() +
  labs(
    title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",
    x = NULL, y = "Difference (%)"
  ) +
  theme_minimal()

# The adjustment seems to match relatively well, with a average percentage difference
# well under 1%. However this rapidly changes past 2020, where tht differences between
# the two go over 1%.

can_gas_fit <- can_gas |>
  model(TSLM(log_volume ~ trend() + season()))

report(can_gas_fit)

can_gas_fit |>
  tidy() |>
  select(term, estimate, std.error, statistic, p.value) |>
  knitr::kable(digits = 3)

# The average monthly growth rate of gas production is about 4%. The most significant
# seasonal effect appears to be June (month 6)

can_gas_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = log_volume, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
       y = "log(Volume)", color = NULL) +
  theme_minimal()

# Our model gets the  increasing trend correct, but it is linear isntead of exponential.
# Therefore we don't get the massive increase in the beginning of the graph for Canadian gas production

can_gas_fit |> gg_tsresiduals()

can_gas_fit |>
  augment() |>
  features(.innov, ljung_box, lag = 24)

# The residuals do not look like white noise and there is definitely visible structure
# that should be modeled further. Since the p value is below .05, we reject the null hypothesis at the
# 5% level, meaning that the resduals are not in fact white noise.

gas_cv_fc <- can_gas |>
  select(Month, Volume) |>
  stretch_tsibble(.init = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_fc |> accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))

# The First date is January 1980 and the final date is March of 2005.The MAE is .562 for the
# SNAIVE model and 4.01 for the TSLM model. The RMSE is .702 for the SNAIVE model and 4.36 for the TSLM model.
# The SNAIVE forecasts much more accurately.

errs <- gas_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(can_gas) |> select(Month, actual = Volume),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e) |>
  na.omit()

forecast::dm.test(errs$snaive, errs$tslm, h = 1, power = 1)

# The DM is -31.278 and the p-value is 2.2e^-16, much smaller than .05. Therefore we reject the null hypothesis
# at the 5% level of equal forecast accuracy. One forecast (the seasonal random walk) is more accurate than the other.
# 

gas_cv_fc <- can_gas |>
  select(Month, Volume) |>
  slide_tsibble(.size = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_fc |> accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))

# While the Seasonal Random Walk error values remain the same, the TSLM model's errors are much smaller.
# This because as the TSLM model is more trained on recent data and it can adapt better to recent trends.
# The older data might not fit the current pattern.



