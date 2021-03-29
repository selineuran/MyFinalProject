#To do on the final project

library(ggplot2)
library(readxl)
library(RColorBrewer)

#Change the scale on the below graph on the Y axis

data2 <- read_excel("~/myfinalproject/processed/data2.xlsx")
View(data2)

d2<- ggplot(data2, aes(x = factor(years), y = number, fill = Country, 
                       group = Country)) +
  geom_line() +
  geom_point(size = 2, shape = 21) +
  labs(y="Number of People", x = "Years") +
  labs(title = "Number of Rough Sleepers in UK by region")

d2

#Look at the data wrangling recording at how to manipulate the data

rawdata <- read_excel("~/myfinalproject/raw/rs_statistics.xlsx")
View(rawdata)


