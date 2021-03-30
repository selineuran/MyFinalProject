#To do on the final project

library(ggplot2)
library(readxl)
library(RColorBrewer)
library(dplyr)

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

rd1 <- data.frame(a = 1:10, b = 2:11, c = 3:12)
df <- subset(df, select = c(a, c))

rd1<- rawdata %>% slice(1:11)

rd1$`Local authority ONS code`<-NULL
rd1$`Local authority`<-NULL
rd1$`Region ONS code`<- NULL

View(rd1)


rd2<- rawdata %>% slice(12:325)

rd2$`Local authority ONS code`<-NULL
rd2$`Region ONS code`<-NULL

View(rd2)

#Look at how to convert from long to wide format



