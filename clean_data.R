#!/usr/bin/env Rscript
# Clean full_prices.csv into a simple CSV for the C++ app.
# Output: full_prices_cleaned.csv
#   Columns: from,to,date,dep,arr,carrier,duration_mins,stops,stop_codes,layover,pp_aud,total_aud

library(tidyverse)
library(lubridate)

raw <- read_csv("full_prices.csv", show_col_types = FALSE, locale = locale(encoding = "UTF-8"))

names(raw) <- str_trim(names(raw))
if (str_detect(names(raw)[1], "^\uFEFF")) {
  names(raw)[1] <- str_remove(names(raw)[1], "^\uFEFF")
}

cleaned <- raw %>%
  transmute(
    from      = str_extract(Route, "^[A-Z]+"),
    to        = str_extract(Route, "[A-Z]+$"),
    date      = format(parse_date(paste0(Date, " 2026"), format = "%a, %b %d %Y"), "%Y-%m-%d"),
    dep       = Dep,
    arr       = Arr,
    carrier   = Carrier,
    duration_mins = {
      h <- as.integer(str_extract(Duration, "\\d+(?=h)"))
      m <- as.integer(str_extract(Duration, "\\d+(?=m)"))
      replace_na(h, 0L) * 60L + replace_na(m, 0L)
    },
    stops     = if_else(Stops == "Direct", 0L, as.integer(Stops)),
    stop_codes = replace_na(`Stop Codes`, ""),
    layover   = replace_na(Layover, ""),
    pp_aud    = `Per Person`,
    total_aud = Total
  ) %>%
  filter(!is.na(date))

write_csv(cleaned, "full_prices_cleaned.csv")
cat(sprintf("Wrote %d rows to full_prices_cleaned.csv\n", nrow(cleaned)))
