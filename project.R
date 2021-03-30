#Working project-----------------------------

library(ggplot2)
library(readxl)
library(RColorBrewer)
library(scales)
library(dplyr)
library(tidyr)
library(tidyverse)

rawdata <- read_excel("~/myfinalproject/processed/rs_statistics.xlsx")

rs1<- rawdata %>% slice(1:2)

rs1$`Local authority ONS code`<-NULL
rs1$`Region ONS code`<-NULL
rs1$`Local authority`<-NULL

rs1_long <- rs1 %>% 
  gather(Year, Number, -Region)
rs1_long

rs1plot <- ggplot(rs1_long, aes(x = Year, y = Number, color = Region, group = Region)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = 'Dark2') +
  theme_classic(base_size = 12)

rs1plot
ggsave("figures/rs1plot.png")

rawdata <- read_excel("~/myfinalproject/processed/rs_statistics.xlsx")
View(rawdata)

rs2<- rawdata %>% slice(1:11)

rs2$`Local authority ONS code`<-NULL
rs2$`Local authority`<-NULL
rs2$`Region ONS code`<- NULL

rs2<- rs2[-c(1:3), ]

rs2_long <- rs2 %>% 
  gather(Year, Number, -Region)
rs2_long

rs2plot <-ggplot(rs2_long, aes(x = Year, y = Number, color = Region, group = Region)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = 'Dark2') +
  theme_classic(base_size = 12)

rs2plot

ggsave("figures/rs2plot.png")


data4 <- read_excel("~/myfinalproject/processed/data4.xlsx")
View(data4)

d4 <- data.frame(
  Group = c("Leeds", "Sheffield", "Kingston upon Hull, City of", "Doncaster", 
            "East Riding of Yorkshire", "Scarborough", "Barnsley"),
  value = c(35, 24, 19, 13, 11, 11, 10)
)

# Barplot
bp<- ggplot(d4, aes(x="", y=value, fill=Group))+
  geom_bar(width = 1, stat = "identity")

pie <- bp + coord_polar("y", start=0)

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

#The piechart....
pie<- pie + scale_fill_brewer(palette="Dark2") + blank_theme +
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                label = percent(value/100)), size=0)

pie



#To do list for the project--------

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

rs2<- rawdata %>% slice(1:11)

#Getting rid of columns

rs2$`Local authority ONS code`<-NULL
rs2$`Local authority`<-NULL
rs2$`Region ONS code`<- NULL

#And rows

rs2<- rs2[-c(1:3), ]
View(rs2)

#Converting from wide to long format

rs2_long <- rs2 %>% 
  gather(Year, Number, -Region)
rs2_long
view(rs2_long)

rs2plot <-ggplot(rs2_long, aes(x = Year, y = Number, color = Region, group = Region)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = 'Dark2') +
  theme_classic(base_size = 12)

rs2plot

#and the same for the other plot

rs1<- rawdata %>% slice(1:2)

rs1$`Local authority ONS code`<-NULL
rs1$`Region ONS code`<-NULL
rs1$`Local authority`<-NULL

rs1_long <- rs1 %>% 
  gather(Year, Number, -Region)
rs1_long

view(rs1_long)

rs1plot <- ggplot(rs1_long, aes(x = Year, y = Number, color = Region, group = Region)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = 'Dark2') +
  theme_classic(base_size = 12)

rs1plot