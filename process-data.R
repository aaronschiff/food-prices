# Process food prices data for app

# -----------------------------------------------------------------------------
# Setup
rm(list = ls())
library(magrittr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(scales)
source("clean-ggplot-theme.R")

# Directories
data_dir <- "data/"
template_dir <- "template/"
html_dir <- "html/"
img_dir <- "img/"

# Styles
chart_width <- 1600
chart_height <- 500

# Other stuff
current_date <- Sys.Date()
month_names <- tibble(
  month_number = 1:12, 
  month_name = c("January", "February", "March", "April", "May", "June", 
                 "July", "August", "September", "October", "November", "December")
)
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# Load data and cleaning

# Food price index 
prices <- read_csv(paste0(data_dir, "CPI324701_20180125_105035_9.csv"), skip = 1) %>%
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

# Categories and correspondence between foods and categories
categories <- read_csv(paste0(data_dir, "categories.csv")) %>%
  arrange(category_order)
food_categories <- read_csv(paste0(data_dir, "food-categories.csv")) %>%
  arrange(food) %>%
  mutate(food_id = paste0("F", 1:nrow(.)))

# Data joins and filtering
food_categories %<>% left_join(categories, by = "category")
prices %<>% left_join(food_categories, by = "food") %>%
  filter(!is.na(category)) %>%
  arrange(category_id, food_id, date) %>%
  left_join(month_names, by = c("month" = "month_number"))
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# HTML output helper functions
output_html <- function(content, output_filename) {
  # Load HTML template parts
  template_header <- read_file(paste0(template_dir, "header.html"))
  template_footer <- read_file(paste0(template_dir, "footer.html"))
  
  output <- paste(template_header, content, template_footer, sep = "\n")
  write_file(output, paste0(html_dir, output_filename))
}

wrap_html_tag <- function(x, tag, params = NULL) {
  output <- paste0("<", tag)
  if (!is.null(params)) output <- paste(output, paste(params, collapse = " "))
  output <- paste0(output, ">", x, "</", tag, ">")
  return(output)
}

build_content <- function(current = NULL, new) {
  return(paste(current, new, sep = "\n"))
}
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# Clean up output folders
do.call(file.remove, list(list.files(paste0(html_dir, img_dir), full.names = TRUE)))
do.call(file.remove, list(list.files(html_dir, full.names = TRUE)))
dir.create(paste0(html_dir, "img"))
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Generate index page (categories menu)
content <- "<h1>Food prices in New Zealand</h1>" %>%
  build_content(wrap_html_tag("Select a category:", "p"))
for (k in 1:nrow(categories)) {
  content %<>% build_content(
    wrap_html_tag(categories[k, "category"], 
                  "a", 
                  params = c(paste0("href = '", 
                                    categories[k, "category_id"], 
                                    ".html'"), 
                             paste0("class = 'pure-button category-button ", 
                                    categories[k, "category_id"], 
                                    "'"))) %>%
      wrap_html_tag("p")
  )
}
content %<>% 
  wrap_html_tag("div", params = "class = 'category-list'") %>%
  build_content(
    wrap_html_tag("Made by <a href = 'http://schiff.co.nz'>Aaron Schiff</a> using data from <a href = 'http://www.stats.govt.nz/browse_for_stats/economic_indicators/prices_indexes/food-price-index-info-releases.aspx'>Statistics New Zealand</a>.", "p")
  ) %>%
  build_content(
    wrap_html_tag(paste0("Prices are averages across many retailers and geographic locations. ", 
                         "Prices have not been adjusted for inflation. ", 
                         "The most recent data is for <b>", month.name[month(max(prices$date))], " ", year(max(prices$date)), "</b>."), 
                  "p")
  )

output_html(content, "index.html")
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# Generate menu for each category
for (k in 1:nrow(categories)) {
  content <- wrap_html_tag(categories[k, "category"], "h1")
  
  items <- food_categories %>% 
    filter(category == as.character(categories[k, "category"])) %>%
    arrange(short_name)
  
  # List of items
  for (i in 1:nrow(items)) {
    content %<>% build_content(
      wrap_html_tag(items[i, "short_name"],
                    "a", 
                    params = c(paste0("href = '", 
                                      items[i, "food_id"], 
                                      ".html'"), 
                               paste0("class = 'pure-button category-button ", 
                                      categories[k, "category_id"], 
                                      "'"))) %>%
        wrap_html_tag("p")
    )
  }
  
  # Back button
  content %<>% build_content(
    wrap_html_tag("&lsaquo; Categories list", 
                  "a", 
                  params = c("href = 'index.html'", 
                             "class = 'pure-button category-button back-button'"))
  )
  output_html(content, paste0(categories[k, "category_id"], ".html"))
}

# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# Generate page for each food item
valid_food <- food_categories %>%
  filter(!is.na(category))
for (f in 1:nrow(valid_food)) {
  # Setup
  current_month_name <- month_names %>% 
    filter(month_number == month(current_date)) %>%
    pull(month_name)
  
  # Filter price data
  item_prices <- prices %>%
    filter(food == as.character(valid_food[f, "food"])) %>%
    arrange(date)
  item_prices_12_months <- tail(item_prices, 12)
  
  # Create ts object of prices, loess trend, and detrended values
  item_prices_ts <- ts(item_prices$price, 
                       start = c(year(min(item_prices$date)), month(min(item_prices$date))), 
                       end = c(year(max(item_prices$date)), month(max(item_prices$date))), 
                       frequency = 12)
  item_prices_ts <- na.omit(item_prices_ts)   # Remove any NA values at beginning/end of series
  # NB this will fail if any NA inside the series (shouldn't be)
  item_prices_trend <- loess(item_prices_ts ~ time(item_prices_ts), 
                             span = 3/12)
  item_prices_trend_ts <- ts(predict(item_prices_trend), 
                             start = start(na.omit(item_prices_ts)), 
                             end = end(na.omit(item_prices_ts)), 
                             frequency = frequency(item_prices_ts))
  item_prices_detrended <- item_prices_ts - item_prices_trend_ts
  
  # Set up extra plotting data
  plot_dat <- tibble(price_trend = as.numeric(item_prices_trend_ts), 
                     price_detrended = as.numeric(item_prices_detrended), 
                     date = seq(from = ymd(paste(start(item_prices_trend_ts)[1], 
                                                 start(item_prices_trend_ts)[2], 
                                                 "1", 
                                                 sep = "-")), 
                                to = ymd(paste(end(item_prices_trend_ts)[1], 
                                               end(item_prices_trend_ts)[2], 
                                               "1", 
                                               sep = "-")), 
                                by = "1 month")) %>%
    mutate(date_factor = factor(month(date), 
                                levels = 1:12, 
                                labels = c("Jan", "Feb", "Mar", "Apr", 
                                           "May", "Jun", "Jul", "Aug", 
                                           "Sep", "Oct", "Nov", "Dec")))
  
  # Create price trend chart
  item_chart <- ggplot(item_prices) + 
    geom_line(aes(x = date, y = price), 
              size = 3, 
              col = rgb(200/255, 200/255, 200/255)) + 
    geom_line(aes(x = date, y = price_trend), 
              size = 3, 
              col = "black", 
              data = plot_dat) + 
    xlab("") + 
    ylab("") + 
    scale_x_date(breaks = date_breaks("1 years"), 
                 labels = date_format("%Y")) +
    scale_y_continuous(limits = c(0, NA), 
                       labels = scales::dollar) + 
    clean_theme(base_size = 46, 
                axis.ticks.x = element_blank(), 
                axis.text = element_text(face = "bold", 
                                         size = rel(1.1)), 
                panel.grid.major.x = element_line(colour = "#bbbbbb"),
                panel.grid.major.y = element_line(colour = "#bbbbbb"),
                plot.margin = unit(c(1, 3, 0, 1), "lines"))
  png(paste0(html_dir, img_dir, valid_food[f, "food_id"], ".png"), width = chart_width, height = chart_height)
  print(item_chart)
  dev.off()
  
  # Create monthly price deviation chart
  seasonality_chart <- ggplot(plot_dat) + 
    geom_hline(yintercept = 0, colour = "black", size = 2) + 
    geom_point(aes(x = date_factor, y = price_detrended), 
               colour = rgb(0, 0, 0, 0.3), 
               size = 8, 
               shape = 16) + 
    geom_point(aes(x = date_factor, y = mean_price_detrended),
               colour = rgb(40/255, 40/255, 40/255), 
               size = 56, 
               shape = "-", 
               data = plot_dat %>%
                 group_by(date_factor) %>%
                 summarise(mean_price_detrended = mean(price_detrended, na.rm = TRUE))) +
    xlab("") + 
    ylab("") + 
    scale_y_continuous(labels = scales::dollar) + 
    clean_theme(base_size = 46, 
                axis.ticks.x = element_blank(), 
                axis.text = element_text(face = "bold", 
                                         size = rel(1.1)), 
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(colour = "#bbbbbb"),
                plot.margin = unit(c(1, 3, 0, 1), "lines"))
  png(paste0(html_dir, img_dir, valid_food[f, "food_id"], "-seas.png"), width = chart_width, height = chart_height)
  print(seasonality_chart)
  dev.off()
  
  # Title and subtitle
  content <- wrap_html_tag(valid_food[f, "short_name"], "h1") %>%
    build_content(
      paste0(valid_food[f, "units"], 
             ifelse(!is.na(valid_food[f, "subtitle"]), 
                    paste0(", ", valid_food[f, "subtitle"]), 
                    "")) %>%
        wrap_html_tag("h2")
    )
  
  # Price in same month last year and range over the past year
  content %<>%
    build_content(
      wrap_html_tag(
        paste0("<b>", 
               sprintf("$%0.2f", 
                       item_prices %>%
                         filter(year == year(current_date) - 1, 
                                month == month(current_date)) %>%
                         pull(price)), 
               "</b> last ", current_month_name), 
        "p", 
        params = "class = 'previous-price'")
    ) %>%
    build_content(
      wrap_html_tag(
        paste0("Between ", 
               sprintf("$%0.2f", min(item_prices_12_months$price, na.rm = TRUE)), 
               " and ", 
               sprintf("$%0.2f", max(item_prices_12_months$price, na.rm = TRUE)), 
               " in the past 12 months."), 
        "p", 
        params = "class = 'previous-price'"
      )
    )
  
  # # Price variability table
  # content %<>%
  #   build_content(
  #     paste(wrap_html_tag("", "th"), 
  #           wrap_html_tag("Lowest", 
  #                         "th", 
  #                         params = "class = 'align-right'"), 
  #           wrap_html_tag("Highest", 
  #                         "th", 
  #                         params = "class = 'align-right'")) %>%
  #       wrap_html_tag("tr") %>%
  #       wrap_html_tag("thead") %>%
  #       build_content(
  #         paste(wrap_html_tag(current_month_name, "td"), 
  #               wrap_html_tag(sprintf("$%0.2f", 
  #                                     item_prices %>%
  #                                       filter(month == month(current_date)) %>%
  #                                       summarise(min_price = min(price, na.rm = TRUE)) %>%
  #                                       pull(min_price)), 
  #                             "td", 
  #                             params = "class = 'align-right'"), 
  #               wrap_html_tag(sprintf("$%0.2f", 
  #                                     item_prices %>%
  #                                       filter(month == month(current_date)) %>%
  #                                       summarise(max_price = max(price, na.rm = TRUE)) %>%
  #                                       pull(max_price)), 
  #                             "td", 
  #                             params = "class = 'align-right'")) %>%
  #           wrap_html_tag("tr") %>%
  #           build_content(
  #             paste(wrap_html_tag("Past 12 months", "td"), 
  #                   wrap_html_tag(sprintf("$%0.2f", min(item_prices_12_months$price, na.rm = TRUE)), 
  #                                 "td", 
  #                                 params = "class = 'align-right'"), 
  #                   wrap_html_tag(sprintf("$%0.2f", max(item_prices_12_months$price, na.rm = TRUE)), 
  #                                 "td", 
  #                                 params = "class = 'align-right'")) %>%
  #               wrap_html_tag("tr")
  #           ) %>%
  #           wrap_html_tag("tbody")
  #       ) %>%
  #       wrap_html_tag("table", 
  #                     params = "class = 'pure-table pure-table-horizontal'")
  #   )
  
  # Price charts
  content %<>%
    build_content(
      wrap_html_tag("<b>Price trend (not adjusted for inflation)</b>", 
                    "p", 
                    params = "class = 'chart-title'")
    ) %>%
    build_content(
      wrap_html_tag(
        paste0("<img class = 'pure-img price-chart' src = '", img_dir, valid_food[f, "food_id"], ".png' />"), 
        "div", 
        params = "class = 'chart'"
      )
    ) %>%
    build_content(
      wrap_html_tag("<b>Monthly variation around the trend</b>", 
                    "p", 
                    params = "class = 'chart-title'")
    ) %>%
    build_content(
      wrap_html_tag(
        paste0("<img class = 'pure-img price-chart' src = '", img_dir, valid_food[f, "food_id"], "-seas.png' />"), 
        "div", 
        params = "class = 'chart'"
      )
    )
  
  # Back buttons
  content %<>% 
    build_content(
      wrap_html_tag(paste0("&lsaquo; ", valid_food[f, "category"]), 
                    "a", 
                    params = c(paste0("href = '", valid_food[f, "category_id"], ".html'", 
                                      paste0("class = 'pure-button category-button back-button ", 
                                             valid_food[f, "category_id"], 
                                             "'")))) %>%
        wrap_html_tag("p")
    ) %>%
    build_content(
      wrap_html_tag("&lsaquo; Categories list", 
                    "a", 
                    params = c("href = 'index.html'", 
                               "class = 'pure-button category-button back-button'")) %>%
        wrap_html_tag("p")
    )
  
  # Write output
  output_html(content, paste0(valid_food[f, "food_id"], ".html"))
}
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# Copy template items to html output folder
file.copy(from = paste0(template_dir, "style.css"), 
          to = paste0(html_dir, "style.css"), 
          overwrite = TRUE)
file.copy(from = paste0(template_dir, "apple-touch-icon.png"), 
          to = paste0(html_dir, "apple-touch-icon.png"), 
          overwrite = TRUE)
# -----------------------------------------------------------------------------
