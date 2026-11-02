# Reading csv file from FRED
library(tidyverse)

retail_raw <- read_csv("data/retail_sales_raw.csv")

head(retail_raw)

# Makig sure it reads as Y/M/D
retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)

# From Wide format to Long fornat
retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")

head(retail_raw_long)

# Creating Tsibble to get dates correct
library(fpp3)

retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)

head(retail_ts)
# Finding gaps
has_gaps(retail_ts)

# canadian Gas comparison tibble
head(canadian_gas)
key_vars(canadian_gas)
index_var(canadian_gas)

# Checking gaps in canadian gas
can_gas <- canadian_gas |>
  select(Month, Volume)

has_gaps(can_gas)

head(can_gas)

# Canadian Gas 1990 and onwards
can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))

head(can_gas_recent)

# Adding Month and Quarters to canadian gas
can_gas_recent <- can_gas_recent |>
  mutate(year    = year(Month),
         month   = month(Month, label = TRUE),
         quarter = quarter(Month))

# Volume in each calendar month of canadian gas
can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))

# Arithmetic in months
can_gas_recent |>
  mutate(next_month           = Month + 1,
         same_month_last_year = Month - 12) |>
  head()

# Monthy to Quarterly data of Canadian gas (Also known as lower frequency)
can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)

can_gas_q |> autoplot(Volume)

# Using autoplot to get auto generated plot of retail sales
retail_ts |> autoplot(sales)

# Creating ggplot of retail sales
retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()
"1. The NSA line has spikes at the end of each year most likely due to the holidays while the SA one does not have this as it is seasonally adjsuted"
"2. No both lines follow the same trend, they follow the same line we can just see when not seasonally adjusted large spikes in the holidays"

can_gas |>
  autoplot(Volume) +
  labs(
    title = "Canadian Gas Production",
    x = "Year",
    y = "Gas Production (Volume)"
  ) +
  theme_minimal()

# Adding geom smooth gg plot to can_gas
can_gas |>
  autoplot(Volume) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    title = "Canadian Gas Production",
    x = "Year",
    y = "Gas Production (Volume)"
  ) +
  theme_minimal()
# Did this to to highlight overall trend making it easier to see long term trend as can_gas spikes a lot

# Part 3
# Part 3.1
# Yes the seasonal swings do get larger however it does not hold through the entire sample especially after aroun 1990 when the swings get smaller.
# Add natural log of Volume
can_gas <- can_gas %>%
  mutate(log_volume = log(Volume))

# Plot logged Canadian gas production
can_gas %>%
  autoplot(log_volume) +
  labs(
    title = "Log of Canadian Gas Production",
    x = "Year",
    y = "Log of Volume"
  ) +
  theme_minimal()

# I do think logging canadian gas is useful because it stabilizes the seasonal swings in the earlier years
# part 3.1c
# Filter for NSA retail sales and add log column
retail_nsa <- retail_ts %>%
  filter(series_id == "RSAFSNA") %>%
  mutate(log_sales = log(sales))

# Plot original retail sales
retail_nsa %>%
  autoplot(sales) +
  labs(
    title = "NSA Retail Sales",
    x = "Year",
    y = "Retail Sales"
  ) +
  theme_minimal()

# Plot logged retail sales
retail_nsa %>%
  autoplot(log_sales) +
  labs(
    title = "Log of NSA Retail Sales",
    x = "Year",
    y = "Log of Retail Sales"
  ) +
  theme_minimal()

# more useful because it makes teh swings more consistent in size throughout the years, especially because it is consistent over the whole sample

# Part 3.2a
# Calculate both growth rates
can_gas <- can_gas |>
  mutate(
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )

# Plot direct growth rate
can_gas |>
  autoplot(growth_direct) +
  labs(
    title = "Canadian Gas Monthly Growth Rate",
    x = "Year",
    y = "Growth Rate (%)"
  ) +
  theme_minimal()

# Calculate absolute difference between growth rates
can_gas <- can_gas |>
  mutate(
    growth_gap = abs(growth_direct - growth_logdiff)
  )

# Plot growth gap
can_gas |>
  autoplot(growth_gap) +
  labs(
    title = "Difference Between Growth Rate Methods",
    x = "Year",
    y = "Absolute Difference (Percentage Points)"
  ) +
  theme_minimal()

# The log difference approximation is pretty accurate because the difference between the growth rates are smal, becomes less accurate when the growth rates are huge.

# Month over Month log difference
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

# The NSA series is much nosier in month-over-month growth rates because it includes the seasonality, especially in december/holidays. SA is much smaller because it has removed these predictable patterns and is more stable.

# Filter to NSA series
retail_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

# Filter to SA series
retail_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in SA Retail Sales",
    x = "Month",
    y = "Millions of Dollars"
  ) +
  theme_minimal()

# The NSA series shows a clear seasonal pattern with the sales increasing at the end of each year, SA series has a much more consistent pattern.

# Part 4.1c
# Season plot for Canadian gas production
can_gas |>
  gg_season(log_volume) +
  labs(
    title = "Seasonal Pattern in Canadian Gas Production",
    x = "Month",
    y = "Log of Volume"
  ) +
  theme_minimal()

# Canadian gas tends to be highest during the winter and low during the summer. Makes sense because natural gas usually used to heat homes so I would expect this.

# Part 4.2
can_gas |>
  gg_subseries(log_volume) +
  labs(
    title = "Subseries Plot: Canadian Gas Production",
    y = "log(Volume)"
  )

# part 4.3a
retail_nsa_x13 <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()

retail_nsa_x13 |> autoplot()

# Seasonal Factor is highest in December, typically around 20% higher in those months. Seasonal swings seem consistent over time maybe slightly smaller in later years.

# Part 4.3b
# Compare my seasonal adjustment to the Census Bureau's
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

# Plot percentage difference
sa_compare |>
  ggplot(aes(x = Month, y = pct_diff)) +
  geom_line() +
  labs(
    title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",
    x = NULL,
    y = "Difference (%)"
  ) +
  theme_minimal()

# My X-13 seasonal adjustment matches the Census Bureau's fairly closely, with most differences staying within about 0.5%. The largest differences occur around 2021–2024, where some spikes exceed 1.5%, possibly due to unusual changes in retail sales during and after the COVID-19 pandemic.

# Part 4.4a
# Fit linear trend and seasonal dummy model
can_gas_fit <- can_gas |>
  model(TSLM(log_volume ~ trend() + season()))

# View estimated coefficients
report(can_gas_fit)

# Create table of coefficients
can_gas_fit |>
  tidy() |>
  select(term, estimate, std.error, statistic, p.value) |>
  knitr::kable(digits = 3)

# Trend coefficient 0.003973 x 100 = 0.3973% which is the estimated average monthly growth rate of candian gas.
# June has the most significant seasonal effect with a t-statistic of -4.777

# Part 4.4b
# Compare actual and fitted Canadian gas production
can_gas_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = log_volume, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(
    title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
    x = "Year",
    y = "log(Volume)",
    color = NULL
  ) +
  theme_minimal()

# Fits the overall upward trend decently, fails badly in the early years (1960s), does well froom 1990 - 2000.

# Part 4.4c
# Plot residuals to check for white noise
can_gas_fit |>
  gg_tsresiduals()

# Ljung-Box test for autocorrelation
can_gas_fit |>
  augment() |>
  features(.innov, ljung_box, lag = 24)

#We can see that the residuals are not simply random white noise as there is still some predictability left in the data not accounted for by our model. The Ljung-Box test produces a p-value of approximately 0, which is significantly less than 0.05 therefore we can reject the null hypothesis. We can also then conclude that there is still significant autocorrelation in our residuals.

