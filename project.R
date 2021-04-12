#Developing the project-----------------------------
library(ggplot2)
library(readxl)
library(RColorBrewer)
library(scales)
library(dplyr)
library(tidyr)
library(tidyverse)
library(gridExtra)


#------------------------------------------------------------------------------
#The first main showcase visualisation 

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
  geom_line(position = position_dodge(0.4)) +
  scale_color_brewer(palette = 'Dark2') +
  theme_classic(base_size = 12) + 
  theme(axis.text.x = element_text(face= "bold", color="black", 
                                   size=7),
        axis.text.y = element_text(face= "bold", color="black", 
                                   size=7),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid")) +
  labs(title="Number of Rough Sleepers in each Region of England (2010 - 2020)",
       x="Year", y = "Number of Rough Sleepers") + 
  scale_y_continuous(breaks=c(0, 300, 600, 900, 1200), limits=c(0, 1200))

rs2plot

ggsave("figures/rs2plot.png")

#------------------------------------------------------------------------------
#The supporting graphs - 
#piechart 1

rawdata <- read_excel("~/myfinalproject/processed/rs_statistics.xlsx")

#First the data preparation...
rs3<- rawdata %>% slice(1:11)
rs3<- rs3[-c(1:3), ]
rs3$`Local authority ONS code`<-NULL
rs3$`Local authority`<-NULL
rs3$`Region ONS code`<- NULL
rs3$`2010`<-NULL
rs3$`2011`<-NULL
rs3$`2012`<- NULL
rs3$`2013`<-NULL
rs3$`2014`<-NULL
rs3$`2015`<- NULL
rs3$`2016`<-NULL
rs3$`2017`<-NULL
rs3$`2018`<- NULL
rs3$`2020`<-NULL

view(rs3)

rs3_long <- rs3 %>% 
  gather(Year, Number, -Region)

view(rs3_long)

#assign prepared data as a dataframe to a variable
rs2pie<- data.frame(rs2_long)

#create a barplot of the data
bp<- ggplot(rs2pie, aes(x="", y=Number, fill=Region))+
  geom_bar(width = 1, stat = "identity") + 
  geom_text(aes(label = paste(round(Number / sum(Number) * 100, 1), "%"), x = 1.7), 
            position = position_stack(vjust = 0.5)) +
  ggtitle("help") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        legend.position="none",
        line = element_blank()) + #template theme is added
  scale_fill_brewer(palette="Dark2") #colour scheme is added matching plot above


#barplot is converted into a piechart
piechart<- bp + coord_polar(theta = "y", start=0) +
  theme_void() +
  ggtitle("Number of Rough Sleepers in each Region of England (2019)")

#saves the graph as an image in figs folder
ggsave("figures/piechart.png") 

#------------------------------------
#piechart2

rawdata2<-read_excel("~/myfinalproject/processed/population.xlsx")
#the rows and columns of interest are selected
rs3<- rawdata2 %>% slice(9, 23, 67, 92, 138, 173, 258, 329)
rs3<- rs3 %>% select(2, 5)

#colums are renamed appropriately
rs3<- rs3 %>% 
  rename(
    Region = ...2,
    "2019" = ...5)
#convert the data to 'long' format
rs3_long <- rs3 %>% 
  gather(Year, Number, -Region)

#the Number column is converted to a numeric variable
rs3_long$Number <- as.numeric(as.character(rs3_long$Number))

#dataset containing population of each region is reordered to match the other data set
rs3_long<- rs3_long %>% arrange(Region)

#column of interest is then selected
rs3_long<- rs3_long %>% select(3)

#the column in renamed to an appropriate name
rs3_long<- rs3_long %>%
  rename(Population = Number)

rs3pie<- data.frame(rs3_long)

#assign prepared data as a dataframe to a variable
rs3pie<- data.frame(rs3_long)

#create a barplot of the data
bp<- ggplot(rs3pie, aes(x="", y=Number, fill=Region))+
  geom_bar(width = 1, stat = "identity") + 
  geom_text(aes(label = paste(round(Number / sum(Number) * 100, 1), "%"), x = 1.7), 
            position = position_stack(vjust = 0.5)) +
  ggtitle("help") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        legend.position="none",
        line = element_blank()
  ) + scale_fill_brewer(palette="Dark2")

#barplot is converted into a piechart
piechart2<- bp + coord_polar("y", start=0) + 
  theme_void() + # remove background, grid, numeric labels
  ggtitle("Population of each Region in England (2019)")

#saves the graph as an image in figs folder
ggsave("figures/piechart2.png") 


#To do list for the project----------------------------------------------------

#Change the scale on the below graph on the Y axis, adjust the points on the axis
#Add a title to the graph 
#Distribute clustered data points using the dodge function - 

geom_line(position = position_dodge(0.2)) +           # Dodge lines by 0.2
  geom_point(position = position_dodge(0.2), size = 4)  # Dodge points by 0.2


rs2plot <-ggplot(rs2_long, aes(x = Year, y = Number, 
  color = Region, group = Region)) +
  geom_point() +
  geom_line(position = position_dodge(0.4)) +
  scale_color_brewer(palette = 'Dark2') +
  theme_classic(base_size = 12) +
  theme(axis.text.x = element_text(face= "bold", color="black", 
                                 size=7),
        axis.text.y = element_text(face= "bold", color="black", 
                                  size=7),
        axis.line = element_line(colour = "black", 
                    size = 1, linetype = "solid")) +
  labs(title="Number of Rough Sleepers in each Region of England",
       x="Number of Rough Sleepers", y = "Year") + 
  scale_y_continuous(breaks=c(0, 300, 600, 900, 1200), limits=c(0, 1200))
        
rs2plot

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

#Replace data currently in pie chart with the RS in each region of England data

rs3 <- read_excel("~/myfinalproject/processed/rs_statistics.xlsx")
rs3<- rawdata %>% slice(1:11)

rs3<- rs3[-c(1:3), ]

rs3$`Local authority ONS code`<-NULL
rs3$`Local authority`<-NULL
rs3$`Region ONS code`<- NULL

view(rs3)

rs3$`2010`<-NULL
rs3$`2011`<-NULL
rs3$`2012`<- NULL
rs3$`2013`<-NULL
rs3$`2014`<-NULL
rs3$`2015`<- NULL
rs3$`2016`<-NULL
rs3$`2017`<-NULL
rs3$`2018`<- NULL
rs3$`2019`<-NULL

view(rs3)

rs3_long <- rs3 %>% 
  gather(Year, Number, -Region)

view(rs3_long)

rs3pie<- data.frame(rs3_long)

view(rs3pie)

bp<- ggplot(rs3pie, aes(x="", y=Number, fill=Region))+
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

pie<- pie + scale_fill_brewer(palette="Dark2") + blank_theme +
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = Number/3 + c(0, cumsum(Number)[-length(Number)]), 
                label = percent(Number/100)), size=0)
pie

#Add piechart of rough sleepers as a percentge of region population
view(rs3_long)
view(rs4_long)

rs4_l<- rs4_long %>% select(3)
rs4_l<- rs4_l %>%
  rename(Population = Number)

total<- cbind(rs3_long, rs4_l)
total <- transform(total, percentage = Number / Population)
total <- total %>% select(1,2,5)

rs3pie<- data.frame(total)

bp<- ggplot(rs3pie, aes(x="", y=percentage, fill=Region))+
  geom_bar(width = 1, stat = "identity") + 
  geom_text(aes(label = paste(round(percentage / sum(percentage) * 100, 1), "%"), x = 1.7), 
            position = position_stack(vjust = 0.5)) +
  ggtitle("help") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        legend.position="none",
        line = element_blank()) + #template theme is added
  scale_fill_brewer(palette="Dark2") #colour scheme is added matching plot above


#barplot is converted into a piechart
piechart<- bp + coord_polar(theta = "y", start=0) +
  theme_void() +
  ggtitle("Rough Sleepers as a proportion of the Region
          population (2019)")

piechart

#Add London to the visualisations
