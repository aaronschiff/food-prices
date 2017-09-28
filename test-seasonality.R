# Check for seasonality and trend in each price series

# -----------------------------------------------------------------------------
# Setup
rm(list = ls())
library(magrittr)
library(lubridate)
library(tidyverse)
library(seasonal)
data_dir <- "data/"
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# Load data and clean up
# Food price index 
prices <- read_csv(paste0(data_dir, "CPI324701_20170918_033059_56.csv"), skip = 1) %>%
  rename(original_date = X1) %>%
  mutate_at(vars(-original_date), as.numeric)
prices_table_junk_row <- which(prices$original_date == "Table information:")
prices <- prices[1:(prices_table_junk_row - 1), ]
prices %<>%
  separate(col = original_date, 
           into = c("year", "month"), 
           sep = "M", 
           remove = FALSE, 
           convert = TRUE) %>%
  mutate(date = ymd(paste(year, month, "1", sep = "-"))) %>%
  filter(year > 2006) %>%
  gather(key = "food", 
         value = "price", 
         -original_date, -year, -month, -date)
rm(prices_table_junk_row)
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# Convert prices data to a list of time series
foods <- unique(prices$food)
prices_ts <- vector("list", length(foods))
names(prices_ts) <- foods
for (f in foods) {
  pf <- filter(prices, food == f)
  pf_ts <- ts(pf$price, 
              start = c(year(min(pf$date)), month(min(pf$date))), 
              end = c(year(max(pf$date)), month(max(pf$date))), 
              frequency = 12)
  prices_ts[[f]] <- pf_ts
}
rm(pf, pf_ts, f)
# -----------------------------------------------------------------------------




