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


