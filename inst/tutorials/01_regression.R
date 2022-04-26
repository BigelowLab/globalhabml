## Regression using Machine Learning
## Global HAB Workshop - University of Strathclyde

devtools::load_all()
library(globalhabml)

library(readr)
library(dplyr)
library(tidymodels)
library(ggplot2)

hab <- readr::read_csv("inst/input_data/common_species.csv")

nrow(hab)
summary(hab)


##### Select one (only load one `data` variable) data option below #####

## Predict Plagioselmis prolonga with Chlorophyll, Salinity, and Temperature
data <- hab %>% 
  select(Chl, Temp, Sal, `Plagioselmis prolonga`) %>% 
  tidyr::drop_na()

## Predict Eucapsis microscopica with Chlorophyll, Salinity, and Temperature
data <- hab %>% 
  select(Chl, Temp, Sal, `Eucapsis microscopica`) %>% 
  tidyr::drop_na()


data <- hab %>% 
  select(Chl, Temp, Sal, PO4, NO3,  NH4, `Plagioselmis prolonga`) %>% 
  tidyr::drop_na()


### Split the data into training and testing sets 
### Train the model with 90% of the examples then test the model with the other 10%
data_split <- initial_split(data, prop=9/10)

data_split

data_test <- testing(data_split)
data_train <- training(data_split)

### The recipe in tidymodels describes which variavles are features and which are labels

## Features
## Labels

hab_recipe <- recipe(`Plagioselmis prolonga` ~ ., data=data)


hist(data$Chl)
hist(log10(data$Chl+1))

## We can also add steps to transform our data before putting it into our models
hab_recipe <- hab_recipe %>% 
  step_log(Chl) %>% 
  step_normalize(all_numeric_predictors())

hab_recipe_prepped <- hab_recipe %>%
  prep()

prepped_training <- juice(hab_recipe_prepped)

prepped_testing <- bake(hab_recipe_prepped, data_test)



#### Linear Regression ####

show_engines("linear_reg")

lr <- linear_reg() %>% 
  set_mode("regression") %>% 
  set_engine("lm")

lr

#can we pass in a recipe here?
lr_fit <- lr %>% 
  fit(`Plagioselmis prolonga` ~ ., data=prepped_training)

lr_fit


## Predict on the test set using 
predict(lr_fit, prepped_testing)

lr_pred <- augment(lr_fit, prepped_testing)

lr_metrics <- metrics(lr_pred, truth = `Plagioselmis prolonga`, estimate=.pred)

lr_metrics


#### K-nearest Neighbors ####
## Requires "kknn" install

show_engines("nearest_neighbor")

neighbor <- nearest_neighbor(mode = "regression",
                             engine = "kknn")

n_fit <- neighbor %>% 
  fit(`Plagioselmis prolonga` ~ ., data=prepped_training)

n_fit

n_pred <- augment(n_fit, prepped_testing)

n_metrics <- metrics(n_pred, truth = `Plagioselmis prolonga`, estimate=.pred)

lr_metrics
n_metrics


#### Random Forest ####

rf <- rand_forest(mode = "regression",
                  engine="ranger")

rf_fit <- rf %>% 
  fit(`Plagioselmis prolonga` ~ ., data=prepped_training)

rf_fit

rf_pred <- augment(rf_fit, prepped_testing)

rf_metrics <- metrics(rf_pred, truth = `Plagioselmis prolonga`, estimate=.pred)

rf_metrics

rf_pred <- rf_pred %>% 
  mutate(row = 1:nrow(rf_pred))


ggplot2::ggplot(data = rf_pred, ggplot2::aes(x=`Plagioselmis prolonga`, y=.pred)) + 
  ggplot2::geom_point() + 
  ggplot2::geom_abline(slope=1, intercept=0)



## SVM

svm <- svm_poly()

svm

svm_fit <- svm %>% 
  fit(`Plagioselmis prolonga` ~ ., data=prepped_training)


data <- hab %>% 
  select(Chl, Temp, Sal, PO4, NO3,  NH4, `Plagioselmis prolonga`) %>% 
  tidyr::drop_na()



