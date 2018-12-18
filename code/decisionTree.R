library(simpleSetup)

# SETUP-------

# this package helps to install and load packages at the same time.
library(simpleSetup)

# Load packages, install if needed
packages <- c('tidyverse', 'skimr', "GGally", "purrr", "repurrrsive",
              "nycflights13","gmodels", "stringr", "DataExplorer",
              "summarytools", "recipes", "broom", "modelr", "magrittr",
              "corrr", "viridis", "readr", "caret", "forcats", "tidyquant",
              "rpart", "rpart.plot", "ipred", "rsample")

library_install(packages)


churn <- read_csv("data/WA_Fn-UseC_-Telco-Customer-Churn.csv")

# Load data

# split training and testing data sets
train.index <- createDataPartition(churn$Churn, p = .7, list = FALSE)
train <- churn[ train.index,]
test  <- churn[-train.index,]
#drop missing values
test %<>%
  select(-customerID) %>%
  drop_na()

train %<>%
  select(-customerID) %>%
  drop_na()

# set seed for reproducibility
set.seed(123)

# Feature Engineering -----------------------------------------------------

# use recipes to quickly turn features into formats that are favourable for logistic regression

recObj <- recipe(Churn ~ ., data = train) %>% # chose Churn as the response variable and the rest are explainatory variables
  step_dummy(all_nominal()) %>% # convert string to factor
  step_BoxCox(all_numeric()) %>% # normalize data that is not normal distribution
  prep(data = train) # prepare for this data

bakedTrain <- bake(recObj, new_data = train) # use bake to transform according to recipes
bakedTest <- bake(recObj, new_data = test)


# modelling  --------------------------------------------------------------

# we will use bagging for our modelling since the original method suffer from high variance
# I have tried many different methods but treebag turns out to be the most useful one because (1) it is easier to do cross validation and (2) we can access variable importance

# Specify 10-fold cross validation
ctrl <- trainControl(method = "cv",  number = 10)

# CV bagged model
bagged_cv <- train(
  Churn_Yes ~ .,
  data = bakedTrain,
  method = "treebag",
  trControl = ctrl,
  importance = TRUE
)

# assess results
bagged_cv


# plot most important variables
plot(varImp(bagged_cv), 20)

# organize the data set then draw the plot using ggplot
col_index <-
  data.frame(varImp(bagged_cv))

ggplot(col_index, aes(x = names, y = Overall)) +
  geom_segment(aes(xend = names, yend = 0)) +
  geom_point() +
  coord_flip() +
  scale_color_viridis() +
  theme_linedraw()

# use the training data to test on the new data set
pred <- predict(bagged_cv, bakedTest)

# measure the RMSE value
RMSE(pred, bakedTest$Churn_Yes)
