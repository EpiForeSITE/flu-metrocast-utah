
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
    time_value = time_value
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
  rows_update(
    semi_join(
      latest,
      latest_all,
      by = c("target_end_date", "location", "target")
    ),
    by = c("target_end_date", "location", "target")
  )

# Append only genuinely new observations
new_rows <- latest |>
  anti_join(
    latest_all,
    by = c("target_end_date", "location", "target")
  )

latest_join <- bind_rows(latest_join, new_rows)

# Check whether non-Utah observations are preserved
non_utah_before <- latest_all |>
  filter(!location %in% unique(latest$location))

non_utah_after <- latest_join |>
  filter(!location %in% unique(latest$location))

stopifnot(identical(non_utah_before, non_utah_after))

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
  "./target-data/time-series.csv",
  colClasses = "character"
)

# Identify observations that are not already present
new_rows <- time_series |>
  mutate(across(everything(), as.character)) |>
  anti_join(
    time_series_all,
    by = c("as_of", "location", "target", "target_end_date")
  )

# Append only new observations
if (nrow(new_rows) > 0) {

  write.table(
    new_rows,
    "./target-data/time-series.csv",
    sep = ",",
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE,
    append = TRUE
  )

  message(nrow(new_rows), " new observations appended.")

} else {

  message("No new observations to append.")

}

# Update oracle-output file ----------------------------------------------------

# Read in oracle-output.csv from repo
oracle <- read.csv("./target-data/oracle-output.csv")

# Format oracle data
oracle_UT <- time_series |>
  rename(oracle_value = observation) |>
  filter(target_end_date >= "2026-10-24") |>
  select(target_end_date, location, target, oracle_value)

# Overwrite any rows in latest_all with the same target_end_date and location
# and append new data
oracle_join <- oracle |>
  rows_update(
    semi_join(
      oracle_UT,
      oracle,
      by = c("target_end_date", "location", "target")
    ),
    by = c("target_end_date", "location", "target")
  )

# Append only genuinely new observations
new_rows <- oracle_UT |>
  anti_join(
    oracle,
    by = c("target_end_date", "location", "target")
  )

oracle_join = bind_rows(oracle_join, new_rows)

# Export
write.csv(oracle_join,
          "./target-data/oracle-output.csv",
          row.names = FALSE,
          quote = c(2, 3)) # so dates don't get messed up by quotes
