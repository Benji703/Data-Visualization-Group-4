library(dplyr)
library(DT)
library(readr)
library(ggplot2)
library(tidyr)
library(forcats)

Albums<-read_csv("./Album.csv")


genres <- factor(Albums$Genre)

salesByYear <- Albums %>%
  complete(Year, Genre, fill = list(Sales = 0)) %>%
  mutate(Genre = fct_lump_n(Genre, n = 4)) %>%
  group_by(Year, Genre) %>%
  summarise(TotalSales = sum(Sales)) 

salesByYearInTotal <- salesByYear %>%
  group_by(Genre) %>%
  summarise(TotalSales = sum(TotalSales)) %>%
  arrange(desc(TotalSales))

table(salesByYearInTotal)
salesByYear$Genre <- factor(salesByYear$Genre, levels=sort(salesByYearInTotal$Genre))

test2 <- fct_lump_n(genres, n = 5)
table(test2)

test <- spread(Albums, Genre, Year, fill = 0)

first_years <- group_by(salesByYear, Genre) %>%
  mutate(TotalSales = 0)
first_years

data_complete <- bind_rows(salesByYear, first_years) %>%
  arrange(Year, Genre, TotalSales)

data_complete

ggplot(salesByYear) +
  geom_bar(aes(x = Year, y = TotalSales), stat="identity")
  


arrows <- 
  c(
    x1 = c(2007),
    x2 = c(2008),
    y1 = c(120687708), 
    y2 = c(87687708+2000000)
  )

t2 <- arrows$x1

ggplot(data = salesByYear) + 
  geom_line(mapping = aes(x = Year, y = TotalSales)) +
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
    color = "gray20", curvature = -0.3) +
  annotate("text", x = 2007, y = 120687708, label = "Test")

t <- which(salesByYear$Year == 2008, arr.ind = TRUE)

df <- data.frame(
  g = c("a", "a", "a", "b", "b", "b"),
  x = c(1, 3, 5, 2, 4, 6),
  y = c(2, 5, 1, 3, 6, 7)
)
a <- ggplot(df, aes(x, y, fill = g)) +
  geom_area()
