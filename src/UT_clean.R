
library(dplyr)
library(readr)
library(lubridate)

# Find all Utah raw data files
files <- list.files(
  "raw-data",
  pattern = "^[0-9]{8}_UT_Regional_Flu_Only\\.csv$",
  full.names = TRUE
)

# Select the file with the most recent date
latest_file <- sort(files, decreasing = TRUE)[1]

# Read the latest file
raw <- read.csv(latest_file)


# Create latest-data file ------------------------------------------------------

# Clean
latest <- raw |>
  mutate(
    location = case_when(
      region %in% "1 Bear River" ~ "bear-river",
      region %in% "2 Central" ~ "central",
      region %in% "3 Davis" ~ "davis",
      region %in% "4 San Juan" ~ "san-juan",
      region %in% "5 Salt Lake" ~ "salt-lake",
      region %in% "6 Southeast" ~ "southeast",
      region %in% "7 Southwest" ~ "southwest",
      region %in% "8 Summit" ~ "summit",
      region %in% "9 Tooele" ~ "tooele",
      region %in% "10 TriCounty" ~ "tri-county",
      region %in% "11 Utah County" ~ "utah-county",
      region %in% "12 Wasatch" ~ "wasatch",
      region %in% "13 Weber-Morgan" ~ "weber-morgan",
      region %in% "utah-state" ~ "utah"),

    target = "Flu ED visits pct",
    time_value = format(mdy(time_value), "%Y-%m-%d")
    ) |>

  rename(observation = value,
         target_end_date = time_value) |>

  select(target_end_date, location, target, observation)

# Check
table(latest$location, useNA = "always")


# Read in latest-data.csv from repo
latest_all <- read.csv("./target-data/latest-data.csv")

# Overwrite any rows in latest_all with the same target_end_date and location as in latest
latest_join <- latest_all |>
  anti_join(latest, by = c("target_end_date", "location", "target")) |>
  bind_rows(latest)

# Export
write.csv(latest_join,
          "./target-data/latest-data.csv",
          row.names = FALSE,
          quote = c(2, 3)) # so dates don't get messed up by quotes


# Update time-series file ------------------------------------------------------

# Define as_of date
as_of_date <- as.Date(
  substr(basename(latest_file), 1, 8),
  format = "%Y%m%d"
)

# Select key variables from latest-data
time_series <- latest |>
  mutate(as_of = format(as_of_date, "%Y-%m-%d")) |>
  select(as_of, location, target, target_end_date, observation)

# Read existing time-series data
time_series_all <- read.csv(
  "./target-data/time-series.csv"
)

# Remove existing observations that match the incoming
# as_of date, location, target, and target_end_date
time_series_join <- time_series_all |>
  anti_join(
    time_series,
    by = c("as_of", "location", "target", "target_end_date")
  ) |>
  bind_rows(time_series)

# Export
write.csv(time_series_join,
          "./target-data/time-series.csv",
          row.names = FALSE,
          quote = FALSE)


# Update oracle-output file ----------------------------------------------------

# Read in oracle-output.csv from repo
oracle <- read.csv("./target-data/oracle-output.csv")

# Format oracle data
oracle_UT <- time_series |>
  rename(oracle_value = observation) |>
  filter(target_end_date >= "2026-10-17") |>
  select(target_end_date, location, target, oracle_value)

# Overwrite any rows in latest_all with the same target_end_date and location
# and append new data
oracle_join <- oracle |>
  anti_join(oracle_UT, by = c("target_end_date", "location", "target")) |>
  bind_rows(oracle_UT)

# Export
write.csv(oracle_join,
          "./target-data/oracle-output.csv",
          row.names = FALSE,
          quote = c(2, 3)) # so dates don't get messed up by quotes

