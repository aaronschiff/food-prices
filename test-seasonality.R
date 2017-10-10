# Check for seasonality and trend in each price series

# -----------------------------------------------------------------------------
# Setup
rm(list = ls())
library(magrittr)
library(lubridate)
library(tidyverse)
library(sandwich)
library(lmtest)
library(ggplot2)
library(ggthemes)
source("clean-ggplot-theme.R")
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
  mutate(date = paste(year, month, "1", sep = "-")) %>%
  mutate(date = ymd(date)) %>%
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

# Smooth with loess or something
# De-trend against smoothed values
# Estimate monthly dummy variables with HAC standard errors
# F-test significance of monthly dummies 
# If jointly significant, conclude it is seasonal


# -----------------------------------------------------------------------------
# Test seasonality of a time series
seasonality_test <- function(y) {
  # Smooth with loess
  y <- na.omit(y)
  y_smooth <- loess(y ~ time(y), span = 3/12)
  y_smooth_ts <- ts(predict(y_smooth), 
                    start = start(y), 
                    end = end(y), 
                    frequency = frequency(y))
  
  # De-trend using smoothed values
  y_detrended <- y - y_smooth_ts
  
  # Regress on monthly dummy variables
  m <- lm(y_detrended ~ as.factor(cycle(y_detrended)))
  
  # Test significance of seasonal dummies with HAC standard errors
  c_t <- coeftest(m, vcov = vcovHAC(m), test = "F")
  w_t <- waldtest(m, vcov = vcovHAC(m), test = "F")
  
  # Set up tibble for plotting
  plot_dat <- tibble(date = seq(ymd(paste(start(y)[1], start(y)[2], "1", sep = "-")), 
                                ymd(paste(end(y)[1], end(y)[2], "1", sep = "-")), 
                                by = "1 month"), 
                     y = y, 
                     y_detrended = as.numeric(y_detrended), 
                     y_smooth = as.numeric(y_smooth_ts), 
                     month = cycle(y)) 
  
  # Plot 1: Original data and trend line
  p1 <- ggplot(plot_dat) + 
    geom_line(aes(x = date, y = as.numeric(y)), col = rgb(200/255, 200/255, 200/255)) + 
    geom_line(aes(x = date, y = y_smooth), col = rgb(0/255, 0/255, 0/255)) + 
    scale_x_date() + 
    scale_y_continuous(limits = c(0, NA)) + 
    xlab("") + 
    ylab("") + 
    clean_theme()
  
  # Plot 2: Variation around trend, by month
  p2 <- ggplot(plot_dat) + 
    geom_point(aes(x = as.factor(month), y = y_detrended), 
               colour = "black", 
               alpha = 0.2) + 
    geom_point(aes(x = as.factor(month), y = mean_y_detrended),
               colour = "red", 
               data = plot_dat %>%
                 group_by(month) %>%
                 summarise(mean_y_detrended = mean(y_detrended, na.rm = TRUE))) + 
    xlab("") + 
    ylab("") + 
    ggtitle("Monthly variation around the trend") + 
    scale_y_continuous() + 
    clean_theme()
  
  # Do a two panel plot
  # Top panel: Original data and smoothed line
  # Bottom panel: Beeswarm by month of variations around the trend with marker for median
  # A monthly beeswarm would be cooler than a boxplot, swarming around the mean
  
  return(list(original = y, 
              smoothed = y_smooth_ts, 
              detrended = y_detrended, 
              model = m, 
              coef_test = c_t, 
              wald_test = w_t, 
              p1 = p1, 
              p2 = p2))
}


# Testing
z <- seasonality_test(prices_ts[["Hot chips, hot wedges"]])
print(z$p1)
print(z$p2)
