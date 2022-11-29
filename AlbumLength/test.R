library(dplyr)
library(DT)
library(readr)
library(ggplot2)


Albums<-read_csv("./Album.csv")


salesByYear <- Albums %>%
  group_by(Year) %>%
  summarise(TotalSales = sum(Sales))

ggplot(salesByYear) +
  geom_bar(aes(x = Year, y = TotalSales), stat="identity")
  
ggplot(data = salesByYear) + 
  geom_line(mapping = aes(x = Year, y = TotalSales))

