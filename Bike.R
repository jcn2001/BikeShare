library(tidyverse)
library(tidymodels)
library(vroom)
library(ggplot2)
library(patchwork)
install.packages("rpart")
#install.packages("DataExplorer","skimr")
#install.packages("GGally")
install.packages("glmnet")
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


# Feature Engineering (hw 6)
# remove the casual and registered variable and change to log count
CleanTrainData <- trainData %>%
  select(-casual,-registered) %>%
  mutate(count=log(count))

# define my recipe
bike_recipe <- recipe(count~.,data=CleanTrainData) %>%
  step_mutate(season=factor(season, levels=c(1,2,3,4),labels = c("spring","summer","fall","winter"))) %>%
  step_mutate(weather=ifelse(weather==4,3,weather)) %>%
  step_mutate(weather=factor(weather,levels=c(1,2,3),labels=c("cloudy","misty","rain"))) %>%
  step_mutate(temp_windspeed = temp*windspeed) %>%
  step_time(datetime, features=c("hour","minute")) %>%
  step_date(datetime, features=c("dow")) %>%
  step_mutate(datetime_hour=as.factor(datetime_hour)) %>%
  step_zv(all_predictors()) %>%
  step_poly(temp, degree=3) %>%
  step_dummy(all_nominal_predictors())

prepped_recipe <- prep(bike_recipe)
bake(prepped_recipe, new_data=CleanTrainData)

# define a model
lin_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# combine into a workflow and fit
bike_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(lin_model) %>%
  fit(data=CleanTrainData)

## run all the steps on test data and put it in the right form to submit
lin_preds <- predict(bike_workflow, new_data = testData)
lin_preds <- exp(lin_preds)

data_engineering_submission <- lin_preds %>%
  bind_cols(.,testData) %>%
  select(datetime, .pred) %>%
  rename(count=.pred) %>%
  mutate(count=pmax(0,count)) %>%
  mutate(datetime=as.character(format(datetime)))

vroom_write(x=data_engineering_submission, file ="C:/Users/Josh/BikeShare/bike-sharing-demand/DataEngineeringPreds.csv", delim=",")



# setup and fit the linear regression model
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

vroom_write(x=kaggle_submission, file ="C:/Users/Josh/BikeShare/bike-sharing-demand/LinearPreds.csv", delim=",")
          
# now we'll fit the poisson model
library(poissonreg)

my_poisson_model <- poisson_reg() %>%
  set_engine("glm") %>%
  set_mode("regression") %>%
  fit(formula=count~temp+humidity+windspeed+holiday+workingday,data=trainData)

bike_poisson_predictions <- predict(my_poisson_model,
                                   new_data=testData)
bike_poisson_predictions

# format the predictions for submission to kaggle
pois_kaggle_submission <- bike_poisson_predictions %>%
  bind_cols(., testData) %>%
  select(datetime, .pred) %>%
  rename(count=.pred) %>%
  mutate(datetime=as.character(format(datetime)))

# write out the file
vroom_write(x=pois_kaggle_submission, file="C:/Users/Josh/BikeShare/bike-sharing-demand/PoissonPreds.csv", delim=",")


## penalized regression section (hw 7)
# create the recipe
bike_recipe_pregression <- recipe(count~.,data=CleanTrainData) %>%
  step_mutate(season=factor(season, levels=c(1,2,3,4),labels = c("spring","summer","fall","winter"))) %>%
  step_mutate(weather=ifelse(weather==4,3,weather)) %>%
  step_mutate(weather=factor(weather,levels=c(1,2,3),labels=c("cloudy","misty","rain"))) %>%
  step_mutate(temp_windspeed = temp*windspeed) %>%
  step_time(datetime, features=c("hour","minute")) %>%
  step_date(datetime, features=c("dow")) %>%
  step_mutate(datetime_hour=as.factor(datetime_hour)) %>%
  step_rm(datetime) %>%
  step_zv(all_predictors()) %>%
  step_poly(temp, degree=3) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize

# penalized regression model
preg_model <- linear_reg(penalty=1,mixture=0) %>%
  set_engine("glmnet")

preg_wf <- workflow() %>%
  add_recipe(bike_recipe_pregression) %>%
  add_model(preg_model) %>%
  fit(data=CleanTrainData)

predict(preg_wf, new_data=testData)

# format the predictions for kaggle
preg_preds <- predict(preg_wf, new_data = testData)

preg_kaggle_submission <- preg_preds %>%
  bind_cols(., testData) %>%
  select(datetime, .pred) %>%
  rename(count=.pred) %>%
  mutate(count=exp(count)) %>%
  mutate(datetime=as.character(format(datetime)))

# write out the file
vroom_write(x=preg_kaggle_submission, file="C:/Users/Josh/BikeShare/bike-sharing-demand/PregPreds.csv", delim=",")




# Cross-Validation (HW 7)
# penalized regression model
preg_model <- linear_reg(penalty=tune(),
                         mixture=tune()) %>%
  set_engine("glmnet")

# set workflow
preg_wf <- workflow() %>%
  add_recipe(bike_recipe_pregression) %>%
  add_model(preg_model)

# Grid of values to tune over
grid_of_tuning_params <- grid_regular(penalty(),
                                      mixture(),
                                      levels = 5)

# split data for CV
folds <- vfold_cv(CleanTrainData, v = 10, repeats=1)

# Run the CV
CV_results <- preg_wf %>%
  tune_grid(resamples=folds,
            grid=grid_of_tuning_params,
            metrics=metric_set(rmse,mae,rsq))

# plot results
collect_metrics(CV_results) %>%
  filter(.metric=="rmse") %>%
  ggplot(data=.,aes(x=penalty,y=mean,color=factor(mixture))) +
  geom_line()

# Find best tuning parameters
bestTune <- CV_results %>%
  select_best(metric = "rsq")

# Finalize the workflow and fit it
final_wf <- preg_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data=CleanTrainData)

# Predict
cv_preds <- predict(final_wf, new_data = testData)


cv_kaggle_submission <- cv_preds %>%
  bind_cols(., testData) %>%
  select(datetime, .pred) %>%
  rename(count=.pred) %>%
  mutate(count=exp(count)) %>%
  mutate(datetime=as.character(format(datetime)))

# write out the file
vroom_write(x=cv_kaggle_submission, file="C:/Users/Josh/BikeShare/bike-sharing-demand/CVPreds.csv", delim=",")



# Regression Tree (HW 9)
install.packages("rpart")

# new recipe
bike_recipe_tree <- recipe(count~.,data=CleanTrainData) %>%
  step_mutate(season=factor(season, levels=c(1,2,3,4),labels = c("spring","summer","fall","winter"))) %>%
  step_mutate(weather=ifelse(weather==4,3,weather)) %>%
  step_mutate(weather=factor(weather,levels=c(1,2,3),labels=c("cloudy","misty","rain"))) %>%
  step_mutate(temp_windspeed = temp*windspeed) %>%
  step_time(datetime, features=c("hour","minute")) %>%
  step_date(datetime, features=c("year","dow")) %>%
  step_mutate(datetime_hour=as.factor(datetime_hour)) %>%
  step_mutate(dow_hour = datetime_hour*datetime_dow) %>%
  step_mutate(holiday_dow = holiday*datetime_dow) %>%
  step_rm(datetime) %>%
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal_predictors())

bike_tree <- decision_tree(tree_depth = tune(),
                           cost_complexity = tune(),
                           min_n=tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")

# set workflow
tree_wf <- workflow() %>%
  add_recipe(bike_recipe_tree) %>%
  add_model(bike_tree)

# grid of values to tune over
grid_of_tree_tuning_params <- grid_regular(tree_depth(),
                                      cost_complexity(),
                                      min_n(),
                                      levels = 5)

# split data for CV
tree_folds <- vfold_cv(CleanTrainData, v = 10, repeats=1)

# Run the CV
tree_CV_results <- tree_wf %>%
  tune_grid(resamples=folds,
            grid=grid_of_tree_tuning_params,
            metrics=metric_set(rmse,mae,rsq))

# Find best tuning parameters
best_treeTune <- tree_CV_results %>%
  select_best(metric = "rmse")

# Finalize the workflow and fit it
final_tree_wf <- tree_wf %>%
  finalize_workflow(best_treeTune) %>%
  fit(data=CleanTrainData)

tree_preds <- predict(final_tree_wf, new_data = testData)


tree_kaggle_submission <- tree_preds %>%
  bind_cols(., testData) %>%
  select(datetime, .pred) %>%
  rename(count=.pred) %>%
  mutate(count=exp(count)) %>%
  mutate(datetime=as.character(format(datetime)))

# write out the file
vroom_write(x=tree_kaggle_submission, file="C:/Users/Josh/BikeShare/bike-sharing-demand/TreePreds.csv", delim=",")


## Random Forest (Homework 10)
install.packages("ranger")
my_randomforest_model <- rand_forest(mtry = tune(),
                                     min_n=tune(),
                                     trees=500) %>%
  set_engine("ranger") %>%
  set_mode("regression")

# set workflow
randomforest_wf <- workflow() %>%
  add_recipe(bike_recipe_tree) %>%
  add_model(my_randomforest_model)

# grid of values to tune over
grid_of_randomforest_tuning_params <- grid_regular(mtry(range=c(1,10)),
                                           min_n(),
                                           levels = 5)

# split data for CV
randomforest_folds <- vfold_cv(CleanTrainData, v = 10, repeats=1)

# Run the CV
randomforest_CV_results <- randomforest_wf %>%
  tune_grid(resamples=randomforest_folds,
            grid=grid_of_randomforest_tuning_params,
            metrics=metric_set(rmse,mae,rsq))

# Find best tuning parameters
best_randomforestTune <- randomforest_CV_results %>%
  select_best(metric = "rmse")

# Finalize the workflow and fit it
final_randomforest_wf <- randomforest_wf %>%
  finalize_workflow(best_randomforestTune) %>%
  fit(data=CleanTrainData)

randomforest_preds <- predict(final_randomforest_wf, new_data = testData)

randomforest_kaggle_submission <- randomforest_preds %>%
  bind_cols(., testData) %>%
  select(datetime, .pred) %>%
  rename(count=.pred) %>%
  mutate(count=exp(count)) %>%
  mutate(datetime=as.character(format(datetime)))

# write out the file
vroom_write(x=randomforest_kaggle_submission, file="C:/Users/Josh/BikeShare/bike-sharing-demand/RandomForestPreds.csv", delim=",")



# stacking (hw 11)
library(stacks)

# split the data for CV
folds <- vfold_cv(CleanTrainData, v = 10, repeats = 1)

# create a control grid
untunedModel <- control_stack_grid() # if tuning over a grid
tunedModel <- control_stack_resamples() # if not tuning a model

# penalized regression model
preg_stacking_model <- linear_reg(penalty=tune(),
                                  mixture=tune()) %>%
  set_engine("glmnet")

# set workflow
preg_stack_wf <- workflow() %>%
  add_recipe(bike_recipe_pregression) %>%
  add_model(preg_stacking_model)

# grid of values to tune over
preg_tuning_grid <- grid_regular(penalty(),
                                 mixture(),
                                 levels = 5)

# run the cv
preg_models <- preg_stack_wf %>%
  tune_grid(resamples=folds,
            grid=preg_tuning_grid,
            metrics=metric_set(rmse,mae,rsq),
            control = untunedModel)

linreg_stack_model <-
  linear_reg() %>%
  set_engine("lm")

lin_stack_reg_wf <- 
  workflow() %>%
  add_model(linreg_stack_model) %>%
  add_recipe(bike_recipe)

lin_reg_model <-
  fit_resamples(
    lin_stack_reg_wf,
    resamples = folds,
    metrics = metric_set(rmse,mae,rsq),
    control = tunedModel
  )


random_forest_models <- randomforest_wf %>%
  tune_grid(resamples = folds,
            grid=grid_of_randomforest_tuning_params,
            metrics=metric_set(rmse,mae,rsq),
            control=untunedModel)



# specify which models to include
my_stack <- stacks() %>%
  add_candidates(preg_models) %>%
  add_candidates(lin_reg_model) %>%
  add_candidates(random_forest_models) 

# fit the stacked model
stack_model <- my_stack %>%
  blend_predictions() %>%
  fit_members()

# use the stacked data to get a prediction
stack_preds <- stack_model %>% predict(new_data=testData)

stack_kaggle_submission <- stack_preds %>%
  bind_cols(., testData) %>%
  select(datetime, .pred) %>%
  rename(count=.pred) %>%
  mutate(count=exp(count)) %>%
  mutate(datetime=as.character(format(datetime)))

# write out the file
vroom_write(x=stack_kaggle_submission, file="C:/Users/Josh/BikeShare/bike-sharing-demand/StackPreds.csv", delim=",")



## hw 12 (pick a model)
## set up the bart model
my_bart_model <- bart(
  trees = 300,
  prior_terminal_node_coef = NULL,
  prior_terminal_node_expo = NULL,
  prior_outcome_range = NULL
) %>% 
  set_engine("dbarts") %>% 
  set_mode("regression") %>% 
  translate()

# set workflow
bart_wf <- workflow() %>%
  add_recipe(bike_recipe_tree) %>%
  add_model(my_bart_model) %>%
  fit(data=CleanTrainData)

# make the predictions
bart_preds <- predict(bart_wf, new_data=testData)

# format for kaggle
bart_kaggle_submission <- bart_preds %>%
  bind_cols(., testData) %>%
  select(datetime, .pred) %>%
  rename(count=.pred) %>%
  mutate(count=exp(count)) %>%
  mutate(datetime=as.character(format(datetime)))

# write out the file
vroom_write(x=bart_kaggle_submission, file="C:/Users/Josh/BikeShare/bike-sharing-demand/BartPreds.csv", delim=",")