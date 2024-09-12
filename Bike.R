library(tidyverse)
library(tidymodels)
library(vroom)
library(ggplot2)
library(patchwork)
#install.packages("DataExplorer","skimr")
#install.packages("GGally")

trainData <- vroom("C:/Users/Josh/Documents/stat348/BikeShare/bike-sharing-demand/train.csv")

# some exploratory graphs
dplyr::glimpse(trainData)
skimr::skim(trainData)
DataExplorer::plot_intro(trainData)
DataExplorer::plot_correlation(trainData)
DataExplorer::plot_bar(trainData)
DataExplorer::plot_histogram(trainData)
DataExplorer::plot_missing(trainData)
GGally::ggpairs(trainData) 

frequency_table <- table(trainData$weather)

# graph of two scatterplots and two barplots
plot1 <- ggplot(data=trainData, aes(x=weather)) +
  geom_bar(fill = "darkblue") +
  theme_minimal() +
  labs(title = "Weather", x = "Type of Weather", y = "Count")

plot2 <- ggplot(data=trainData, aes(x=temp, y = count)) +
  geom_point(color = "darkblue") +
  geom_smooth(color = "tomato1") +
  theme_minimal() +
  labs(title= "Temperature and Total Rentals", x = "Temperature(Degrees Celsius)", y = "Total Rentals")

plot3 <- ggplot(data=trainData, aes(x=factor(season))) +
  geom_bar(fill = "darkblue") +
  theme_minimal() +
  labs(title = "Season", x = "Season", y = "Count") +
  scale_x_discrete(labels = c("Spring", "Summer","Fall","Winter"))

plot4 <- ggplot(data=trainData, aes(x=humidity, y=count)) +
  geom_point(color = "darkblue") +
  geom_smooth(color = "tomato1") +
  theme_minimal() +
  labs(title = "Humidity and Total Rentals", x = "Humidity", y = "Total Rentals")

# put them all back together and save
hw3 <- (plot1 + plot2) / (plot3 + plot4)
ggsave("C:/Users/Josh/Documents/stat348/BikeShare/hw3.png")

plot5 <- ggplot(data=trainData, aes(x=humidity, y=temp)) +
  geom_point(color = "darkblue") +
  geom_smooth(color = "tomato1") +
  theme_minimal() +
  labs(title = "Humidity and Temperature", x = "Humidity", y = "Temperature")

cor(trainData$temp,trainData$humidity)

# counts for season and weather
table(trainData$weather)
table(trainData$season)

# setup and fit the linear regresssion model
my_linear_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression") %>% # regression just means quantitative response
  fit(formula=log(count)~temp+humidity+windspeed+holiday+workingday, data=trainData)

# Generate predictions using linear model
testData <- vroom("C:/Users/Josh/Documents/stat348/BikeShare/bike-sharing-demand/test.csv")

bike_predictions <- predict(my_linear_model,
                            new_data=testData)

bike_predictions <- exp(bike_predictions)

kaggle_submission <- bike_predictions %>%
  bind_cols(.,testData) %>%
  select(datetime, .pred) %>%
  rename(count=.pred) %>%
  mutate(count=pmax(0,count)) %>%
  mutate(datetime=as.character(format(datetime)))

vroom_write(x=kaggle_submission, file ="./LinearPreds.csv", delim=",")
          
  
  

