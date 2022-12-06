library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(scales)

data <- read_csv("./Album.csv")
options("scipen" = 100, "digits" = 4)

prepared_data <- data %>%
    group_by(Year, Genre) %>%
    summarise(TotalSales = sum(Sales))

genres <- unique(data$Genre)
years <- unique(data$Year)

MAX_DECIMALS <- 1
MAX_GENRES <- 3

max_sold <- max(prepared_data$TotalSales)
y_breaks <- seq(0, max_sold, by = 5000000)
y_labels <- paste0(y_breaks / 1000000, "M")
# 

calculate_abbriviation <- function(sales) {
  sales <- as.numeric(sales)
  count <- ifelse(sales <= 0, paste0(""), paste0(round(sales / 1e6, MAX_DECIMALS), "M"))
  return(count)

}

# identify the top 8 genres
top_genres <- prepared_data %>%
    group_by(Genre) %>%
    summarise(TotalSales = sum(TotalSales)) %>%
    arrange(desc(TotalSales)) %>%
    slice(1:MAX_GENRES) %>%
    pull(Genre)

# populate the genres that are not existent every year with 0
for (genre in genres) {
  for (year in years) {
    if (nrow(prepared_data[prepared_data$Year == year & prepared_data$Genre == genre, ]) == 0) {
      prepared_data <- rbind(prepared_data, data.frame(Year = year, Genre = genre, TotalSales = 0))
    }
  }
  if (!(genre %in% top_genres)) {
    prepared_data <- prepared_data %>% mutate(Genre = ifelse(Genre == genre, "Other", Genre))
  }
}

# sort the data by year
prepared_data <- prepared_data[order(prepared_data$Year),]

#print full data
print(prepared_data)

#print the genres that are not in the top 8
print("top genres:")
print(top_genres)
print("others:")
print(setdiff(genres, top_genres))

#plot the data
ggplot(prepared_data, aes(x = Year, y = TotalSales, by = 1, color = Genre)) +
    geom_line(stat = "identity",) +
    geom_point() +
    geom_text(aes(label = calculate_abbriviation(TotalSales)), vjust = -0.5, hjust = 0.5) +
# geom_smooth(se = FALSE) +
# scale_fill_brewer(palette = "Set1") +
# scale_x_discrete(date_breaks = "1 year", date_labels = "%Y") +
scale_x_continuous(name = 'Year', breaks = years) +
scale_y_continuous(name = 'Total Sales', breaks = seq(0, max_sold, by = 5000000), labels = y_labels) +
theme_bw() +
  theme(axis.line = element_line(color = 'black'),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    plot.background = element_blank(),
    panel.grid.minor = element_blank(),

    panel.border = element_blank())

ui <- shinyUI(fluidPage(
    titlePanel("Album Length"),


))