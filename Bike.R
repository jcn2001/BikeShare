library(tidyverse)
library(tidymodels)
library(vroom)
library(ggplot2)
library(patchwork)
install.packages("DataExplorer","skimr")
install.packages("GGally")

bikedata <- vroom("C:/Users/Josh/Documents/stat348/BikeShare/bike-sharing-demand/train.csv")

dplyr::glimpse(bikedata)
skimr::skim(bikedata)
DataExplorer::plot_intro(bikedata)
DataExplorer::plot_correlation(bikedata)
DataExplorer::plot_bar(bikedata)
DataExplorer::plot_histogram(bikedata)
DataExplorer::plot_missing(bikedata)
GGally::ggpairs(bikedata) 

frequency_table <- table(bikedata$weather)

plot1 <- ggplot(data=bikedata, aes(x=weather)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Weather", x = "Type of Weather", y = "Count")

plot2 <- ggplot(data=bikedata, aes(x=temp, y = count)) +
  geom_point() +
  geom_smooth() +
  theme_minimal() +
  labs(title= "Temperature and Total Rentals", x = "Temperature(Degrees Celsius)", y = "Total Rentals")

plot3 <- ggplot(data=bikedata, aes(x=factor(season))) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Season", x = "Season", y = "Count")

plot3 <- season + scale_x_discrete(labels = c("Spring", "Summer", "Fall", "Winter"))

plot4 <- ggplot(data=bikedata, aes(x=humidity, y=count)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Humidity and Total Rentals", x = "Humidity", y = "Total Rentals")


hw3 <- (plot1 + plot2) / (plot3 + plot4)

ggsave("C:/Users/Josh/Documents/stat348/BikeShare/hw3.png")
