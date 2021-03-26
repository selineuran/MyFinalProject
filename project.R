#Notes on the final project

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
