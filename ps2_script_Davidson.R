#----------3.1 & 3.2----------
#NOTE: I forgot to save this when committing the first time 
#so it might now show, I'll make sure to have an extra commit.
library(tidyquant)
library(tidyverse)

# unemp_raw <- tq_get(
#   c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
#   get  = "economic.data",
#   from = "1976-01-01"
# )
# 
# head(unemp_raw)
# 
# dir.create("data", showWarnings = FALSE)
# 
# write_rds(unemp_raw, "data/unemp_raw.rds")

unemp_raw = read_rds("data/unemp_raw.rds")

#----------4.1----------
unemp <- unemp_raw |>
rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR"   ~ "Oregon",
      symbol == "CAUR"   ~ "California",
      symbol == "TXUR"   ~ "Texas",
      symbol == "NYUR"   ~ "New York"
    )
  )

head(unemp)

#----------4.2----------
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |>
  ungroup()

unemp |>
  filter(date %in% as.Date(c("2025-09-01", "2025-10-01"))) |>
  select(region, date, unemployment_rate) |>
  arrange(region, date)

# Calculate unemployment rate changes month to month
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate_change = unemployment_rate - lag(unemployment_rate)
  ) |>
  ungroup()

unemp |>
  filter(
    region == "Oregon",
    date >= as.Date("2025-08-01"),
    date <= as.Date("2025-11-01")
  ) |>
  select(region, date, unemployment_rate, unemployment_rate_change)

unemp_recent <- unemp |>
  filter(date >= as.Date("2015-01-01"))


# Summary for all data
unemp_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate = min(unemployment_rate, na.rm = TRUE),
    max_rate = max(unemployment_rate, na.rm = TRUE)
  )

unemp_summary

# Summary for 2015 on
unemp_recent_summary <- unemp_recent |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate = min(unemployment_rate, na.rm = TRUE),
    max_rate = max(unemployment_rate, na.rm = TRUE)
  )

unemp_recent_summary

#----------4.3----------
# Biggest unemployment increases
unemp_top_increases <- unemp |>
  slice_max(
    order_by = unemployment_rate_change,
    n = 5,
    with_ties = TRUE
  )

unemp_top_increases

# Biggest unemployment decreases
unemp_top_decreases <- unemp |>
  slice_min(
    order_by = unemployment_rate_change,
    n = 5,
    with_ties = TRUE
  )

unemp_top_decreases

#----------4.4----------
#Biggest unemployment increases no 2020
unemp_top_increases_no_covid <- unemp |>
  filter(lubridate::year(date) != 2020) |>
  slice_max(
    order_by = unemployment_rate_change,
    n = 5,
    with_ties = TRUE
  )

unemp_top_increases_no_covid

#Biggest unemployment decreases no 2020
unemp_top_decreases_no_covid <- unemp |>
  filter(lubridate::year(date) != 2020) |>
  slice_min(
    order_by = unemployment_rate_change,
    n = 5,
    with_ties = TRUE
  )

unemp_top_decreases_no_covid

#----------5.1----------
ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  labs(
    title = "Unemployment Rates by Region (1976-2026)",
    x = "Year",
    y = "Unemployment Rate (%)",
    color = "Region"
  ) +
  theme_minimal()

#----------5.2----------
ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  facet_wrap(~ region) +
  labs(
    title = "Unemployment Rates by Region (1976-2026)",
    x = "Year",
    y = "Unemployment Rate (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#----------5.3----------
ggplot(unemp_summary, aes(x = fct_reorder(region, mean_rate), y = mean_rate)) +
  geom_col(fill = "blue") +
  labs(
    title = "Average Unemployment Rate by Region (1976-2026)",
    x = "Region",
    y = "Average Unemployment Rate (%)"
  ) +
  theme_minimal()

#----------5.4----------
ggplot(
  unemp |> filter(date >= as.Date("2019-01-01"),
                  date <= as.Date("2021-12-01")),
  aes(x = date, y = unemployment_rate_change, fill = region)
) +
  geom_col() +
  facet_wrap(~ region) +
  labs(
    title = "Monthly Unemployment Rate Changes (2019-2021)",
    x = "Year",
    y = "Monthly Change (Percentage Points)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#I chose to graph monthly changes in unemployment rates from 2019 to 2021 because 
#I wanted to see how COVID affected different regions. The graph shows the big 
#increases in unemployment during the beginning of the pandemic, then decreases 
#as the economy began recovering.
