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

#A piechart

data4 <- read_excel("~/myfinalproject/processed/data4.xlsx")
View(data4)

d4 <- data.frame(
  Group = c("Leeds", "Sheffield", "Kingston upon Hull, City of", "Doncaster", 
            "East Riding of Yorkshire", "Scarborough", "Barnsley", 
            "North Lincolnshire", "North East Lincolnshire"),
  value = c(35, 24, 19, 13, 11, 11, 10, 9, 7)
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
pie<- pie + scale_fill_brewer("Greens") + blank_theme +
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
                label = percent(value/100)), size=0)

pie
