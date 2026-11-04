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

#I think the case for logging the data is weaker for the retail sales data, 
#because the seasonal swings are not as large as the gas production data.

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

#The log difference approximation seems reasonably accurately, but does differ by
#over 3 in some cases

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

#The RASFSNA is much noiser than its seasonally adjusted counterpart. 
#This is because the NSA data has seasonal swings that weren't taken out of the
#data and therefore shows sales varying wildly from month to month.



